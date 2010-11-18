open Unix;;
open Printf;;

open Lwt;;
open Ocsigen_stream;;
open Ocsigen_http_frame;;
open Ocsigen_http_client;;

open Json_io;;


let default_host = "127.0.0.1";;
let default_port = 5984;;

let localhost_entry = gethostbyname "127.0.0.1";;
let local_addr = localhost_entry.h_addr_list.(0);;


type couch_error = {
  error: string;
  reason: string
} with json;;

type couch_success = {
  ok: bool;
  id: string;
  rev: string
} with json;;

type couch_result = 
    Error of couch_error
  | Success of couch_success
  | Result of Json_type.t;;


let to_couch_result json = 
  try
    return (Error(couch_error_of_json json))
  with _ ->
    (try
      return(Success(couch_success_of_json json))
    with _ ->
      return(Result(json)));;


let call = 
  Ocsigen_http_client.raw_request
    ~host:default_host
    ~port:default_port
    ~inet_addr:local_addr;;

let get_call =
  call ~http_method:Http_header.GET;;

let put_call = 
  call ~http_method:Http_header.PUT;;

let delete_call =
  call ~http_method:Http_header.DELETE;;


let raw_result frame =
  match frame.frame_content with
      Some c ->
        (Ocsigen_stream.string_of_stream (Ocsigen_stream.get c))
    | None ->
        return (" { \"error\" : \"network error\", \"reason\" : \"Unknown (database down?)\" }");;

let result_to_json frame = 
  (raw_result frame)
  >>=
    (fun json_string -> 
       return (Json_io.json_of_string
         ~allow_comments:true
         ~allow_nan:true
         ~recursive:false
         json_string));;
   

let create_doc db_name doc_id doc_json= 
  let url = (Printf.sprintf "/%s/%s" db_name doc_id) in
    (put_call 
      ~uri:url 
      ~content:(Some (Ocsigen_stream.of_string doc_json)) 
      ()())
    >>=
      result_to_json
    >>=
        to_couch_result;;


let write_doc db_name doc_id doc_json = 
  let url = (Printf.sprintf "/%s/%s" db_name doc_id) in
    (put_call
       ~uri:url
       ~content:(Some (Ocsigen_stream.of_string doc_json))
       ()())
    >>=
      result_to_json;;


let delete_doc_raw db_name doc_id doc_rev =
  let url = (sprintf "/%s/%s?rev=%s" db_name doc_id doc_rev) in
    (delete_call
       ~uri:url
       ~content:None
       ()())
    >>=
      result_to_json;;

let delete_doc db_name doc_id doc_rev =
  (delete_doc_raw db_name doc_id doc_rev)
    >>=
        to_couch_result;;


let get_db db_name = 
  (Ocsigen_http_client.raw_request
    ~host:default_host
    ~port:default_port
    ~http_method:Http_header.GET
    ~inet_addr:local_addr
    ~uri:("/"^db_name)
    ~content:None
    () ())
  >>=
    result_to_json;;


let get url = 
  get_call ~uri:url ~content:None () () >>= result_to_json;;


let get_doc dbname docid =
  let url = sprintf "/%s/%s" dbname docid in
    get url >>= to_couch_result;;

let get_json dbname docid =
  let url = sprintf "/%s/%s" dbname docid in
    get url;;

let view view_name p =
  let url = sprintf "_view/%s?id=%s" view_name p in
    get url;;



let select_from_view_raw dbname designdoc viewname key = 
  let url = sprintf "/%s/_design/%s/_view/%s?key=\"%s\"" dbname designdoc viewname key in
    get url;;

let select_from_view dbname designdoc viewname key = 
  let url = sprintf "/%s/_design/%s/_view/%s?key=%s" dbname designdoc viewname key in
    get url >>= to_couch_result;;
    
