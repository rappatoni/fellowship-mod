(* machine.ml – minimal toggle & dummy snapshot *)

open Core
let wrap_decl d = Printf.sprintf "(%s)" d

(** Global flag – set via environment variable FSP_MACHINE. *)
let machine_mode : bool ref = ref (
  match Sys.getenv_opt "FSP_MACHINE" with
  | Some ("0" | "false" | "no") -> false
  | _ -> true)

(** Return an *ASCII* placeholder payload for now – will be filled later. *)
(*let snapshot (_c : Core.cairn) : string =
  "(state (mode idle))"*)

let esc s =
  let buf = Buffer.create (String.length s + 8) in
  String.iter (function
    | '\"' -> Buffer.add_string buf "\\\""
    | '\n' | '\r' -> Buffer.add_char buf ' '
    | c -> Buffer.add_char buf c) s;
  Buffer.contents buf

let sexp_string s = "\"" ^ esc s ^ "\""
let sexp_prop   p = sexp_string (pretty_prop p)
let sexp_sort   s = sexp_string (pretty_sort s)
let sexp_bool   b = if b then "t" else "nil"

(*--------------------------------------------------------------------*)
(* 3.  Encode a single goal – we keep it minimal for now               *)
(*--------------------------------------------------------------------*)
let sexp_hyp (nm, pr, vis) =
  Printf.sprintf "((name %s)(prop %s)(visible %s))"
    (sexp_string nm) (sexp_prop pr) (sexp_bool vis)

let sexp_env (nm, srt) =
  Printf.sprintf "((name %s)(sort %s))" (sexp_string nm) (sexp_sort srt)

let sexp_goal (meta, g : Core.metaid * Core.goal) =
  let side = match fst g.active with LeftHandSide -> "lhs" | RightHandSide -> "rhs" in
  let hyps = String.concat " " (List.map sexp_hyp g.hyp) in
  let ccls = String.concat " " (List.map sexp_hyp g.ccl) in
  let env  = String.concat " " (List.map sexp_env  g.env)  in
  Printf.sprintf "(goal (meta %s)(side %s)(active-prop %s)(hyps %s)(ccls %s)(env %s))"
    (sexp_string meta) side (sexp_prop (snd g.active)) hyps ccls env

(*--------------------------------------------------------------------*)
(* 4.  Snapshot builder                                                *)
(*--------------------------------------------------------------------*)
let hash s = Digest.to_hex (Digest.string s)

let snapshot (cairn : Core.cairn) : string =
  (* force ASCII pretty‑printing temporarily *)
  let saved_ascii = !ascii in
  ascii := true;

  let st = Core.get_state cairn in

  (* mode ------------------------------------------------------------*)
  let mode =
    if Core.isCptScs cairn then "success"
    else if Core.isExn cairn then "exception"
    else if Core.isOngoing cairn then "subgoals"
    else "idle" in

  (* goals -----------------------------------------------------------*)
  let goals_sexp = String.concat " " (List.map sexp_goal st.goals) in

  (* proof term & hash ----------------------------------------------*)
  let pt_ascii = pretty_t3rm st.pt in
  let pt_hash  = hash pt_ascii in

  (* declarations – fold signature + theorems -----------------------*)
  let decls_sexp =
    let sig_sexp = Core.Coll.fold (fun nm srt acc ->
      let d = Printf.sprintf "((name %s)(kind sort)(sort %s))"
                (sexp_string nm) (sexp_sort srt) in
      wrap_decl d :: acc)
        st.sign [] in
    let thm_sexp = Core.Coll.fold (fun nm pr acc ->
      let d = Printf.sprintf "((name %s)(kind prop)(prop %s))"
                (sexp_string nm) (sexp_prop pr) in
      wrap_decl d :: acc)   
        st.thms [] in
    String.concat " " (List.rev_append sig_sexp thm_sexp) in

  (* restore user ascii preference ----------------------------------*)
  ascii := saved_ascii;

  (* assemble final one‑liner ---------------------------------------*)
  Printf.sprintf
    "(state (mode %s)(current-goal-index %d)(goals %s)(proof-term %s)(proof-term-hash %s)(decls %s))"
    mode st.index goals_sexp (sexp_string pt_ascii) (sexp_string pt_hash) decls_sexp
