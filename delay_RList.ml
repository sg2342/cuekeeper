(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open ReactiveData.RList
open Lwt

module type CLOCK = sig
  val now : unit -> float
  val sleep : float -> unit Lwt.t
end

type pending = {
  time : float;       (* When to actually delete it *)
  mutable i : int;    (* Current index in list. *)
}

type set_state = [ `Current | `Removed ] -> unit

type 'a item = {
  set_state : set_state;
  state : [ `Current | `Removed ] React.S.t;
  data : 'a;
}

let current = React.S.const `Current
let fixed data = {
  state = current; set_state = ignore; data
}

let make_item data =
  let state, set_state = React.S.create `Current in
  { state; set_state; data }

module Make (C : CLOCK) = struct
  let make ~delay src =
    let src_events = ReactiveData.RList.event src in
    let delete_events, send_delete = React.E.create () in
    let pending_deletes = ref [] in
    let adjust i =
      let i =
        if i < 0 then i + List.length (value src) else i in
      (* Source wants to change index i. Undo changes due to pending deletes. *)
      let rpending = List.rev !pending_deletes in
      let rec aux i = function
        | [] -> i
        | d::ds ->
            if i >= d.i then aux (i + 1) ds
            else aux i ds in
      aux i rpending in

    let thread = ref None in
    let rec schedule () : unit =
      match !thread, !pending_deletes with
      | None, d::ds ->
          let interval = d.time -. C.now () in
          if interval <= 0.0 then (
            send_delete (Patch [R d.i]);
            pending_deletes := ds;
            schedule ()
          ) else (
            thread := Some (C.sleep interval >|= fun () ->
              thread := None;
              schedule ()
            )
          )
      | _ -> () in

    let current_dst = ref ReactiveData.RList.empty in

    let filtered_events = src_events |> React.E.map (function
      | Set s ->
          pending_deletes := [];
          begin match !thread with
          | None -> ()
          | Some t ->
              Lwt.cancel t;
              thread := None
          end;
          Set (s |> List.map make_item)
      | Patch ps ->
          let ps = ps |> Ck_utils.filter_map (function
            | I (i, x) ->
                let i = adjust i in
                !pending_deletes |> List.iter (fun d ->
                  if d.i >= i then d.i <- d.i + 1
                );
                Some (I (i, make_item x))
            | R i ->
                let i = (* Need to add one as src has already been shortened. *)
                  if i < 0 then i + List.length (value src) + 1 else i in
                let i = adjust i in
                pending_deletes := !pending_deletes @ [{ time = C.now () +. delay; i}];
                (List.nth (value !current_dst) i).set_state `Removed;
                None
            | U (i, x) ->
                let i = adjust i in
                Some (U (i, make_item x))
            | X (i, offset) ->
                let dst = adjust (i + offset) in
                let i = adjust i in
                if offset > 0 then (
                  !pending_deletes |> List.iter (fun d ->
                    if d.i > i && d.i < dst then d.i <- d.i - 1
                  )
                ) else (
                  !pending_deletes |> List.iter (fun d ->
                    if d.i > dst && d.i < i then d.i <- d.i + 1
                  )
                );
                Some (X (i, dst - i))
          ) in
          schedule ();
          Patch ps
    ) in
    (* (since delete_events always fires from a Lwt async thread, it can't overlap with filtered_events) *)
    let result = ReactiveData.RList.make_from (value src |> List.map make_item) (React.E.select [filtered_events; delete_events]) in
    current_dst := result;
    result
end
