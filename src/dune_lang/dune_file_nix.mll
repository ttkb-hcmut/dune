{
open! Stdune
}

rule is_nix = parse
  | "-*- nix -*-" { true }
  | ""            { false }

and eof_reached = parse
  | eof { true  }
  | ""  { false }

