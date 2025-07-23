(* machine.ml – minimal toggle & dummy snapshot *)

open Core

(** Global flag – set via environment variable FSP_MACHINE. *)
let machine_mode : bool ref = ref (
  match Sys.getenv_opt "FSP_MACHINE" with
  | Some ("0" | "false" | "no") -> false
  | _ -> true)

(** Return an *ASCII* placeholder payload for now – will be filled later. *)
let snapshot (_c : Core.cairn) : string =
  "(state (mode idle))"
