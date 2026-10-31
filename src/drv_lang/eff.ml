type experiments = [ `pipe ]

type _ Effect.t += Experimental_feature : (experiments * string) -> unit Effect.t

module Import_error =
  struct

  module Resolve_error =
    struct

    type t =
      | Find_file_error of [
        `Lookup_path_not_found of string list
      ]

    end

  type t =
    | Resolve_error of Resolve_error.t

  end

type exn +=
  | Variable_not_found of string
  | Access_not_found of string
  | Import_error of Import_error.t

module Syntax_error =
  struct exception Unfinished_let_in

  end

