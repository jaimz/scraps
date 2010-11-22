open Printf;; (* DEBUG *)

open Lwt;;
open Eliom_sessions;;
open Cryptokit;;

open Json_io;;
open Json_type;;
open Json_type.Browse;;
open Json_type.Build;;

open Couchdb;;

open Note;;

type session_t = 
  | Notes of Note.Creation.t list
  | Acc_id of string;;


type account_state = [
  | `Anon
  | `Active
  | `Tentative ] with json;;


let account_state_of_string s = 
  match s with 
    | "Anon" -> `Anon
    | "Active" -> `Active
    | "Tentative" -> `Tentative
    | _ -> raise (Failure ("Unexpected account state: "^s));;


type client_response = {
  name: string;
  error: string;
  message: string;
  state: account_state
} with json;;


let anon = {
  name = "anonymous";
  state = `Anon;
  error = "";
  message = "";
};;


let expired = {
  anon with
    error = "Expired"
};;


let session_table = Eliom_sessions.create_volatile_table ();;


let client_response_of_couch_entry couch_entry =
  match couch_entry with
    | Json_type.Object o ->
        (try
           let table = make_table o in
           let name = Json_type.Browse.string (field table "name") in
           let state = 
             account_state_of_string (
               Json_type.Browse.string (field table "state")) in
             { name = name; state = state; error  = ""; message = "" }
         with
             Json_error s ->
               { anon with error = "DB format error"; message = s })
    | _ ->
        { anon with error = "DB format error"; message = "Unexpected JSON type from CouchDB" };;


let lookup id =
  (Couchdb.get_doc "note_accounts" id) >>=
    (fun couch_result ->
       match couch_result with
         | Error e ->
             return ({ anon with error = "DB error"; message = (Json_io.string_of_json (Couchdb.json_of_couch_error e)) })
         | Success s ->
             return ({ anon with error = "Unexpected DB result"; message = (Json_io.string_of_json (Couchdb.json_of_couch_success s)) })
         | Result r ->
             return (client_response_of_couch_entry r));;


let get_active_account ?(create = false) sp = 
  let si = get_volatile_session_data ~table:session_table ~sp:sp () in
    match si with
      | Data_session_expired ->
          (if create = true then
            set_volatile_session_data ~table:session_table ~sp (Notes []));
          return expired
      | No_data ->
          (if create = true then
             set_volatile_session_data ~table:session_table ~sp (Notes []));
          return anon
      | Data s ->
          (match s with
             | Acc_id id ->
                 lookup id
             | Notes _ ->
                 return anon);;

let persist_volatile_notes sp author_id =
  let si = get_volatile_session_data ~table:session_table ~sp:sp () in
    match si with
      | Data s ->
          (match s with
            | Notes notes ->
                Lwt_list.iter_s
                  (fun n ->
                     let new_authors = author_id :: (List.filter (fun s -> s <> author_id) n.Creation.authors) in
                     let new_note = { n with Note.Creation.authors = new_authors } in
                       return (
                         ignore(
                           Couchdb.create_doc 
                             "notes" 
                             n.Creation._id 
                             (Json_io.string_of_json (Note.Creation.json_of_t new_note))))
                  )
                  notes
            | Acc_id id ->
                return ())
      | _ ->
          return  ();;


let create_permanent sp email name pass_hash =
  let create_templ = Json_type.Build.objekt [ 
    ("name", (Json_type.Build.string name)); 
    ("pass_hash", (Json_type.Build.string pass_hash));
    ("state", (Json_type.Build.string "Tentative")) ] in
  (Couchdb.create_doc "note_accounts" email (Json_io.string_of_json create_templ)) >>=
    (fun couch_result ->
       match couch_result with
         | Error e ->
             return ({ anon with error = "DB error"; message = (Json_io.string_of_json (Couchdb.json_of_couch_error e)) })
         | Result r ->
             return ({ anon with error = "DB error"; message = (Json_io.string_of_json r) })
         | Success s ->
             ignore(persist_volatile_notes sp email);
             set_volatile_session_data ~table:session_table ~sp (Acc_id email);
             return ({ name = name; state = `Tentative; error = ""; message = "" }));;



let check_hash supplied_hash stored_hash timestamp =
  let hex_transform = Cryptokit.Hexa.encode () in
  let alg = Cryptokit.MAC.hmac_sha1 stored_hash in
  let our_hash = Cryptokit.hash_string alg timestamp in
  let our_hash_hex = Cryptokit.transform_string hex_transform our_hash in
    our_hash_hex = supplied_hash;;



let authenticate sp email pass_hash timestamp =
  (Couchdb.get_doc "note_accounts" email) >>=
    (fun couch_result ->
       match couch_result with
         | Error e ->
             return { anon with error = "Not found"; message = "Account "^email^" not found" }
         | Success s ->
             return { anon with error ="Unexpected DB result"; message = "Did not expect success message" }
         | Result r ->
             try
               match r with
                 | Json_type.Object o -> 
                     (let table = Json_type.Browse.make_table o in
                      let stored_hash = Json_type.Browse.string (field table "pass_hash") in
                        if (check_hash pass_hash stored_hash timestamp) then
                          (ignore(persist_volatile_notes sp email);
                           set_volatile_session_data ~table:session_table ~sp (Acc_id email);
                          return (client_response_of_couch_entry r))
                        else
                          return ({ anon with error = "Bad password"; message = "Password incorrect" }))
                 | _ ->
                     return ({ anon with error = "DB format error"; message = "Unexpected JSON type from Couch DB when retrieving account to authenticate" })
             with
                 Json_error msg ->
                   return ({ anon with error = "DB format error"; message = msg }));;


let get_notes sp =
  let si = get_volatile_session_data ~table:session_table ~sp:sp () in
    match si with
      | Data_session_expired | No_data ->
          return (Note.list_view [])
      | Data s ->
          (match s with
             | Acc_id id ->
                 Note.notes_for_account id
             | Notes n ->
                 return (Note.list_view n));;


let lookup_note sp id = 
  let si = get_volatile_session_data ~table:session_table ~sp:sp () in
    match si with
      | Data_session_expired ->
          return (Couchdb.json_of_couch_error (
                    { 
                      Couchdb.error = "not_found"; 
                      Couchdb.reason = "session expired" 
                    }))
      | No_data ->
          return (Couchdb.json_of_couch_error { Couchdb.error = "not_found"; Couchdb.reason = "no session" })
      | Data s ->
          match s with
            | Notes notes ->
                (try
                   return (Note.Creation.json_of_t (
                             List.find (fun curr -> curr.Creation._id = id) notes
                           ))
                 with
                     Not_found ->
                       return (Couchdb.json_of_couch_error { Couchdb.error = "not_found"; Couchdb.reason = "missing" }))
            | Acc_id accid ->
                (* XXX Permissions *)
                Couchdb.get_json "notes" id;;
                 

let create_note_persisted note_id title content tags created authors =
  let creator = {
    Note.Creation._id = note_id;
    Note.Creation.title = title;
    Note.Creation.content = content;
    Note.Creation.tags = tags;
    Note.Creation.authors = authors;
    Note.Creation.created = created;
    Note.Creation.modified = (Unix.gettimeofday ())
  } in
    Couchdb.write_doc "notes" note_id (Json_io.string_of_json (Note.Creation.json_of_t creator));;


let save_note_persisted note_id rev title content tags created authors =
  let note = {
    _id = note_id;
    _rev = rev;

    title = title;
    content = content;
    tags = tags;
    authors = authors;

    created = created;
    modified = (Unix.gettimeofday ())
  } in
    Couchdb.write_doc "notes" note_id (Json_io.string_of_json (Note.json_of_t note));;



let save_note sp note_id rev title content tags created authors =
  let tm = Unix.gettimeofday () in
    match (get_volatile_session_data ~table:session_table ~sp:sp ()) with
      | Data_session_expired ->
          return (Couchdb.json_of_couch_error (
                    {
                      Couchdb.error = "not_found";
                      Couchdb.reason = "session expired"
                    }))
      | No_data ->
          let creator = (Note.Creation.make note_id title content tags authors created tm) in
            set_volatile_session_data ~table:session_table ~sp (Notes [ creator ]);
            return (
              Couchdb.json_of_couch_success 
                {
                  ok = true;
                  id = note_id;
                  rev = rev;
                }
            )
      | Data s ->
          match s with
              Acc_id acc ->
                let new_authors = acc :: (List.filter (fun s -> s <> acc) authors) in
                  if rev = "new" then
                    create_note_persisted note_id title content tags created new_authors
                  else
                    save_note_persisted note_id rev title content tags created new_authors
            | Notes notes ->
                try
                  let creator = {
                    Note.Creation._id = note_id;
                    Note.Creation.title = title;
                    Note.Creation.content = content;
                    Note.Creation.tags = tags;
                    Note.Creation.authors = authors;
                    Note.Creation.created = created;
                    Note.Creation.modified = Unix.gettimeofday ()
                  } in
                    set_volatile_session_data 
                      ~table:session_table 
                      ~sp (Notes (Note.replace_volatile_note creator notes));
                    return (
                      Couchdb.json_of_couch_success
                        {
                          ok = true;
                          id = note_id;
                          rev = rev; (* XXX *)
                        }
                  )
                with
                    Failure f ->
                      return (
                        Couchdb.json_of_couch_error {
                          Couchdb.error = "Cant save volatile";
                          Couchdb.reason = f
                        });;


let delete_note sp note_id rev =
  let si = get_volatile_session_data ~table:session_table ~sp:sp () in
    match si with
      | Data_session_expired ->
          return (Couchdb.json_of_couch_error ({ Couchdb.error = "not_found"; Couchdb.reason = "session has expired" }))
      | No_data ->
          return (Couchdb.json_of_couch_error ({ Couchdb.error = "not_found"; Couchdb.reason = "no session data" }))
      | Data s ->
          match s with
              Acc_id id ->
                Note.delete_permanent note_id rev id
            | Notes creators ->
                set_volatile_session_data ~table:session_table ~sp (Notes (List.filter (fun c -> c.Note.Creation._id <> note_id) creators));
                return (Couchdb.json_of_couch_success ({ Couchdb.ok = true; Couchdb.id = "volatile"; Couchdb.rev = "volatile" }));;


  
let bulk_update_volatile sp update_str = 
  let si = get_volatile_session_data ~table:session_table ~sp:sp () in
    match si with
      |Data_session_expired ->
         return (Couchdb.make_bulk_update_failure "not_found" "Session expired")
      | No_data ->
          return (Couchdb.make_bulk_update_failure "note_found" "No session data")
      | Data d ->
          match d with
            | Notes notes ->
                (try
                   let j_updates = Json_io.json_of_string update_str in
                     match j_updates with 
                       | Json_type.Array _ ->
                           let updates = (Json_type.Browse.list (Note.Creation.t_of_json) j_updates) in
                           let new_creators = 
                             List.map (
                               fun existing ->
                                 try
                                   List.find (fun update -> update.Note.Creation._id = existing.Note.Creation._id) updates
                                 with Not_found ->
                                   existing
                             ) notes
                           in
                             set_volatile_session_data ~table:session_table ~sp (Notes new_creators);
                             return (Json_type.Build.array [])
                       | _ ->
                           return (Couchdb.make_bulk_update_failure "wrong_json_type" "Expected an array")
                 with Json_error s ->
                   return (Couchdb.make_bulk_update_failure "json_parse_error" s))
            | Acc_id _ ->
                return (Couchdb.make_bulk_update_failure "wrong_session_type" "Trying to save volatile when signed in");;



(* returns string Lwt.t - which is the JSON encoding of a Couchdb bulk update result *)
let bulk_update_persistent sp update_str =
  let si = get_volatile_session_data ~table:session_table ~sp:sp () in
    match si with
      | Data_session_expired ->
          return (Json_io.string_of_json (Couchdb.make_bulk_update_failure "not_found" "Session expired"))
      | No_data ->
          return (Json_io.string_of_json (Couchdb.make_bulk_update_failure "not_found" "No session data"))
      | Data d ->
          match d with
            | Acc_id id ->
                (try
                   let j_updates = Json_io.json_of_string update_str in
                     match j_updates with
                       | Json_type.Array _ ->
                           let updates = (Json_type.Browse.list (Note.t_of_json) j_updates) in
                             (Lwt_list.map_s
                                (fun note ->
                                   Note.update_last_author note id)
                                updates) 
                             >>=
                               (fun note_list ->
                                  let json_note_list = Json_type.Build.list Note.json_of_t note_list in
                                  let json_request_data = Json_type.Build.objekt [ ( "all_or_nothing", (Json_type.Build.bool false)); ( "docs", json_note_list )] in
                                    Couchdb.write_bulk "notes" (Json_io.string_of_json json_request_data)
                               )
                       | _ ->
                           return (Json_io.string_of_json (Couchdb.make_bulk_update_failure "wrong_json_type" "Expected a list"))
                 with Json_error s ->
                   return (Json_io.string_of_json (Couchdb.make_bulk_update_failure "json_parse_error" s)))
            | Notes _ ->
                return (
                  Json_io.string_of_json (
                    Couchdb.make_bulk_update_failure "wrong_session_type" "Only signed in users can persist notes" 
                  ));;




let deactivate sp = 
  let si = get_volatile_session_data ~table:session_table ~sp:sp () in
    (match si with
      | Data s ->
          (match s with 
             | Acc_id id ->
                 set_volatile_session_data ~table:session_table ~sp (Notes []);
             | _ ->
                 ())
      | _ ->
          ());
    return anon;;
