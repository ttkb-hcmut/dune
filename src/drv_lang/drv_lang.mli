(** https://nix.dev/manual/nix/2.34/language/index.html *)
(** https://nix.dev/tutorials/nix-language.html *)

type drv
(** [drv], short for "derivation", or "driver", is *)

module Parse :
	sig

	val expression' : string -> (drv Trx.code, string) result
	(** [expression' s] compiles a \nix string representation to \ocaml code. *)

	val expression : Lexing.lexbuf -> (drv Trx.code, string) result
	(** [expression s] compiles a \nix representation to \ocaml code, where input is buffered through a character grouper. *)

	end
