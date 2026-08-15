(** Returns [true] if the input starts with "-*- nix -*-" *)
val is_nix : Lexing.lexbuf -> bool

val eof_reached : Lexing.lexbuf -> bool

