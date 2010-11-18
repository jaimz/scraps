open Str;;

open Lwt;;
open Eliom_services;;
open Eliom_parameters;;
open Eliom_predefmod;;
open Json_io;;

open Couchdb;;
open Account;;


let to_json_result jsont =
  return (
    (Json_io.string_of_json jsont),
    "application/javascript"
  );;


let account =
  Text.register_new_service
    ~path:["account"]
    ~get_params:unit
    (fun sp () () ->
       (Account.get_active_account sp) 
       >>=
         (fun cr -> return (Account.json_of_client_response cr))
       >>=
           to_json_result
    );;


let register =
  Text.register_new_post_service
    ~fallback:account
    ~post_params:(string "email" ** string "name" ** string "passhash")
    (fun sp () (email, (name, (passhash))) ->
       (Account.create_permanent sp email name passhash)
       >>=
         (fun cr -> return (Account.json_of_client_response cr))
       >>=
           to_json_result);;


let prevent_get_call =
  (fun sp () () ->
     (return
        (Account.json_of_client_response
           { Account.anon with error = "Wrong call type"; message = "GET not allowed to this location" }))
     >>=
       to_json_result);;
  

let authenticate_fallback = 
  Text.register_new_service
    ~path:["authenticate"]
    ~get_params:unit
    prevent_get_call;;
  

let authenticate =
  Text.register_new_post_service
    ~fallback:authenticate_fallback
    ~post_params:(string "email" ** string "passhash" ** string "timestamp")
    (fun sp () (email, (passhash, timestamp)) ->
       (Account.authenticate sp email passhash timestamp)
       >>=
         (fun cr -> 
            return (Account.json_of_client_response cr))
       >>=
           to_json_result);;


let signout_fallback = 
  Text.register_new_service
    ~path:["signout"]
    ~get_params:unit
    prevent_get_call;;


let signout = 
  Text.register_new_post_service
    ~fallback:signout_fallback
    ~post_params:unit
    (fun sp () () ->
       (Account.deactivate sp) 
       >>=
         (fun cr ->
            return (Account.json_of_client_response cr))
       >>=
           to_json_result);;


let notes = 
  Text.register_new_service
    ~path:["notes"]
    ~get_params:(suffix (all_suffix "note_id"))
    (fun sp note_id () ->
       (match note_id with
          | [] -> (Account.get_notes sp)
          | id :: _ -> (Account.lookup_note sp id)
       )
       >>=
         to_json_result
    );;

let delete_fallback = 
  Text.register_new_service
    ~path:["delete"]
    ~get_params:unit
    prevent_get_call;;

let delete = 
  Text.register_new_post_service
    ~fallback:delete_fallback
    ~post_params:(string "id" ** string "rev")
    (fun sp () (id, rev) -> 
       (Account.delete_note sp id rev)
       >>= 
         to_json_result);;
       


let save_note = 
  Text.register_new_post_service
    ~fallback:notes
    ~post_params:(string "_id" ** 
                    string "_rev" ** 
                    string "title" ** 
                    string "content" ** 
                    string "tags" **
                    string "authors" **
                    string "created" **
                    string "modified" **
                    string "selection")
    (fun sp note_id (_id, (_rev, (title, (content, (tags, (authors, (created, (modified, selection)))))))) ->
       (match note_id with
         | [] -> 
             return (
               Couchdb.json_of_couch_success 
                 { ok = true;
                   id = "";
                   rev = "" })
         | note_id :: _ ->
             let nlexp = Str.regexp "[\n]" in
             let tag_list = Str.split nlexp tags in
             let author_list = Str.split nlexp authors in
               Account.save_note sp note_id _rev title content tag_list created author_list)
         >>=
         to_json_result
    );;


let bulk_update_fallback = 
  Text.register_new_service
    ~path:["notes"; "bulk_update"]
    ~get_params:unit
    prevent_get_call;;

let bulk_update = Text.register_new_post_service
  ~fallback:bulk_update_fallback =
  ~post_params:(string "notes")
    (fun sp () notes ->
       Account.bulk_update sp notes >>= to_json_result);;

    
let volatile_bulk_update_fallback = 
  Text.register_new_service
    ~path:["notes"; "volatile_bulk_update"]
    ~get_params:unit
    prevent_get_call;;


let volatile_bulk_update = Text.register_new_post_service
  ~fallback:volatile_bulk_update_fallback
  ~post_params:(string "notes")
  (fun sp () notes ->
     Account.volatile_bulk_update sp notes >>- to_json_result);;
       
