open Unix;;

open Lwt;;
open Json_type;;
open Json_type.Build;;

module Creation = struct
  type t = {
    _id: string;
    
    title: string;
    content: string;
    tags: string list;
    authors: string list;
    
    created: string;
    modified: float
  } with json;;
  
  let make id title content tags authors created modified =
    { _id = id; 
      title = title; 
      content = content; 
      tags = tags; 
      authors = authors; 
      created = created; 
      modified = modified };;
end;;

type t = {
  _id: string;
  _rev: string;

  title: string;
  content: string;
  tags: string list;
  authors: string list;
  
  created: string;
  modified: float;
} with json;;


let (|>) f g = g f;;


let make_view_entry note_spec = 
  Json_type.Build.objekt [
    ("id", (Json_type.Build.string note_spec.Creation._id));
    ("key", (Json_type.Build.string ""));
    ("value", (Creation.json_of_t note_spec))
  ];;


let list_view note_list = 
  let json_list = Json_type.Build.list make_view_entry note_list in
    (Json_type.Build.objekt [ 
       ("total_rows", (Json_type.Build.int (List.length note_list)));
       ("offset", (Json_type.Build.int 0));
       ("rows", json_list)
     ]);;


let notes_for_account account_id = 
  Couchdb.select_from_view_raw "notes" "notes" "by_author" account_id;;

let replace_volatile_note creator others = 
  creator :: (List.filter (fun c -> c.Creation._id <> creator.Creation._id) others);;



let delete_permanent note_id rev author_id = 
  Couchdb.get_doc "notes" note_id >>=
    (fun cr ->
       match cr with
         | Couchdb.Result note_json ->
             (try
               let note = t_of_json note_json in
                 try
                   ignore (List.find (fun s -> s = author_id) note.authors);
                   Couchdb.delete_doc_raw "notes" note_id rev
                 with Not_found ->
                   return (Couchdb.json_of_couch_error ({ Couchdb.error = "disallowed"; Couchdb.reason = "You are not an author of this note" }))
             with Json_error e ->
               return (Couchdb.json_of_couch_error ({ Couchdb.error = "internal json error"; Couchdb.reason = e })))
         | Couchdb.Error e ->
             return (Couchdb.json_of_couch_error e)
         | Couchdb.Success s ->
             return (Couchdb.json_of_couch_success s));;


let update_last_author note author =
  return ({ note with authors = (author :: (List.filter (fun s -> s <> author) note.authors)) });;

let save_note note = 
  Couchdb.write_doc "notes" note._id (Json_io.string_of_json (json_of_t note));;
