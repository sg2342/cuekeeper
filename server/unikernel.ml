(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt
open V1_LWT

let () = Log.(set_log_level WARN)

let task s = Irmin.Task.create ~date:0L ~owner:"User" s

(* TODO: disk store *)
module Store = Irmin.Basic(Irmin_mem.Make)(Irmin.Contents.String)
module OptHead = Tc.Option(Store.Head)
(* This is a work-around for https://github.com/mirage/irmin/issues/204 *)
module SliceIO = struct
  module Make_list (K: Tc.S0)(V: Tc.S0) = Tc.List( Tc.Pair(K)(V) )
  module Ct = Make_list(Store.Private.Contents.Key)(Tc.String)
  module No = Make_list(Store.Private.Node.Key)(Store.Private.Node.Val)
  module Cm = Make_list(Store.Private.Commit.Key)(Store.Private.Commit.Val)
  module T = Tc.Triple (Ct)(No)(Cm)
end

module Bundle = Tc.Pair(SliceIO.T)(Store.Head)

let show_head = function
  | None -> "(none)"
  | Some head -> String.sub (Irmin.Hash.SHA1.to_hum head) 0 6

module Main (C:CONSOLE) (S:Cohttp_lwt.Server) = struct
  let respond_json json =
    let body = Ezjsonm.to_string json in
    S.respond_string ~status:`OK ~body ()

  let respond_static segments =
    let path = String.concat "/" segments in
    match Static.read path with
    | Some body -> S.respond_string ~status:`OK ~body ()
    | None -> S.respond_not_found ()

  let start c http =
    Store.create (Irmin_mem.config ()) task >>= fun s ->

    (* Split a URI into a list of path segments *)
    let split_path uri =
      let path = Uri.path uri in
      let rec aux = function
        | [] | [""] -> []
        | hd::tl -> hd :: aux tl
      in
      List.filter (fun e -> e <> "")
        (aux (Re_str.(split_delim (regexp_string "/") path)))
    in

    let accept_push body =
      let s = s "import" in
      Cohttp_lwt_body.to_string body >>= fun body ->
      let buf = Mstruct.of_string (B64.decode body) in
      let (slice, head) = Bundle.read buf in
      Store.head s >>= function
      | Some server_head when server_head = head ->
          S.respond_string ~status:`OK ~body:"ok" ()
      | server_head ->
      Store.import s (Store.Private.Slice.implode slice) >>= fun () ->
      let commit_store = Store.Private.commit_t s in
      Store.Private.Commit.mem commit_store head >>= function
      | false ->
          let msg = "New head not found after import!" in
          C.log_s c msg >>= fun () ->
          S.respond_string ~status:`Bad_request ~body:msg ()
      | true ->
      Store.fast_forward_head s head >>= function
      | false ->
          let msg = Printf.sprintf "Non-fast-forward push attempted: %s -> %s"
            (show_head server_head) (show_head (Some head)) in
          C.log_s c msg >>= fun () ->
          S.respond_string ~status:`OK ~body:"not-fast-forward" ()
      | true ->
          let msg = Printf.sprintf "Update master %s -> %s" (show_head server_head) (show_head (Some head)) in
          C.log_s c msg >>= fun () ->
          S.respond_string ~status:`OK ~body:"ok" () in

    let fetch last_known =
      let s = s "export" in
      Store.head s >>= function
      | None -> S.respond_string ~status:`OK ~body:"empty" ()
      | Some head ->
      let basis =
        match last_known with
        | None -> []
        | Some c -> [Irmin.Hash.SHA1.of_hum c] in
      Store.export s ~min:basis ~max:[head] >>= fun slice ->
      let slice = Store.Private.Slice.explode slice in
      let bundle = (slice, head) in
      let buf = Cstruct.create (Bundle.size_of bundle) in
      let rest = Bundle.write bundle buf in
      assert (Cstruct.len rest = 0);
      let body = Cstruct.to_string buf |> B64.encode in
      S.respond_string ~status:`OK ~body () in

    (* HTTP callback *)
    let callback conn_id request body =
      Lwt.catch (fun () ->
        match S.Request.meth request, split_path (S.Request.uri request) with
        | `GET, ["fetch"] -> fetch None
        | `GET, ["fetch"; last_known] -> fetch (Some last_known)
        | `POST, ["push"] -> accept_push body
        | `GET, ([] | [""]) -> respond_static ["index.html"]
        | `GET, segments -> respond_static segments
        | _ -> S.respond_error ~status:`Method_not_allowed ~body:"Invalid request" ()
      ) (fun ex ->
        C.log_s c (Printexc.to_string ex) >>= fun () ->
        fail ex
      )
    in
    let conn_closed (_,conn_id) =
      let cid = Cohttp.Connection.to_string conn_id in
      C.log c (Printf.sprintf "conn %s closed" cid)
    in
    http (S.make ~conn_closed ~callback ())

end
