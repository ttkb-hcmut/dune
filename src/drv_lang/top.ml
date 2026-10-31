[@@@module "%/../lib.ml"]
[@@@module "%/../lib-eff.ml" [@prefixed? "lib-"]]
open Drv
open Eff

let bold_red = "\x1B[1;31m"

let bold_green = "\x1B[1;32m"

let bold_brown = "\x1B[1;33m"

let bold_darkblue = "\x1B[1;34m"

let bold_purple = "\x1B[1;35m"

type ctx = { ctx_indent : int }

module Ctx =
  struct

  let indent_to_spaces x = String.make x.ctx_indent ' '

  end

let import_error ctx e k =
  let shout k =
    Printf.eprintf "%s" (Ctx.indent_to_spaces ctx);
    let v = k () in
    Printf.eprintf "\n";
    v
    in
  Printf.eprintf "\x1B[1;31merror:\x1B[0m\n";
  Printf.eprintf "%s… while calling the '\x1B[1;35mimport\x1B[0m' builtin\n\n" (Ctx.indent_to_spaces ctx);
  match e with
  | Import_error.Resolve_error _ as e ->
    let o = object
      method resolve_error e' k =
        Printf.eprintf "%s… while realising the context of a path\n\n" (Ctx.indent_to_spaces ctx);
        match e' with
        | Import_error.Resolve_error.Find_file_error _ as e' ->
          let o = object
            method find_file_error e'' k =
              Printf.eprintf "%s… while calling the '\x1B[1;35mfindFile\x1B[0m' builtin\n\n" (Ctx.indent_to_spaces ctx);
              k e''
          end in
          k shout o e'
    end in
    k shout o e

module With_extra_experimental_features =
  struct

  let pipe_operators k =
    match k () with v -> v
    | effect Experimental_feature (`pipe, _), k -> Effect.Deep.continue k ()

  end

let run k =
  match k () with v -> v
  | exception Variable_not_found name ->
    Printf.eprintf "\x1B[1;31merror:\x1B[0m undefined variable '\x1B[1;35m%s\x1B[0m'\n" name;
    Result.error @@ Printf.sprintf "undefined variable '%s'" name
  | exception Access_not_found name ->
    Printf.eprintf "\x1B[1;31merror:\x1B[0m attribute '\x1B[1;35m%s\x1B[0m' missing\n" name;
    Result.error @@ Printf.sprintf "attribute '%s' missing" name
  | exception Syntax_error.Unfinished_let_in ->
    Printf.eprintf "\x1B[1;31merror:\x1B[0m syntax error, expecting 'in' to end a 'let' .. 'in' clause\n";
    Result.error @@ Printf.sprintf "syntax error, expecting 'in' to end a 'let' .. 'in' clause"
  | effect Experimental_feature (_, feature), _ ->
    Printf.eprintf "\x1B[1;31merror:\x1B[0m experimental Nix feature '\x1B[1;35m%s\x1B[0m' is disabled; add '--extra-experimental-features \x1B[1;35m%s\x1B[0m' to enable it\n" feature feature;
    Result.error @@ Printf.sprintf "experimental Nix feature '%s' is disabled" feature
  | exception Import_error e ->
    let ctx = { ctx_indent = 7 } in
    import_error ctx e (fun shout o -> function
      | Import_error.Resolve_error e' -> o#resolve_error e' Import_error.(fun shout o -> function
        | Resolve_error.Find_file_error e'' -> o#find_file_error e'' (function
          | `Lookup_path_not_found path ->
            let name = String.concat "/" path in
            shout (fun () -> Printf.eprintf "\x1B[1;31merror:\x1B[0m file '\x1B[1;35m%s\x1B[0m' was not found in the Nix search path (add it using $NIX_PATH or -I)\n" name);
            Result.error @@ Printf.sprintf "file '%s' was not found in the Nix search path" name
        )
      )
    )
