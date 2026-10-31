[@@@module [%unfold "~/u/topcore/cprg"]]
[@@@module "%/../lib-eff.ml" [@prefixed? "lib-"]]
[@@@module "%/../lib-result.ml" [@prefixed? "lib-"]]
[@@@module "%/../lib-option.ml" [@prefixed? "lib-"]]
[@@@module "%/../lib-list.ml" [@prefixed? "lib-"]]
[@@@module "%/../lifts.ml"]
[@@@module (
  assert (Sys.command @@ Filename.quote_command "rm" ["-rf"; "/home/kinten/u/scaffold.0/drv/lib-stanza"] = 0);
  assert (Sys.command @@ Filename.quote_command "git" ["clone"; "/home/kinten/o/ttkb-hcmut/dune"; "/home/kinten/u/scaffold.0/drv/lib-stanza/project"] = 0);
  assert (Sys.command @@ Filename.quote_command "unfold" ["~/u/scaffold.0/drv/mkstanza";
    "/home/kinten/u/scaffold.0/drv/lib-stanza/project/src/dune_rules/stanzas";
    "/home/kinten/u/scaffold.0/drv/lib-stanza";
    "/home/kinten/u/scaffold.0/drv";
    "lib-";
    "stanza";
    "lib-stanza";
    "mod";
    "import"
  ] = 0);
  "/home/kinten/.var/u-drv/ext-runtime.ml"
)]
open Cprg
open Cprg.Syntax

type context = { ctx_cwd : string }

and 'expr closure = { closure_access: string list; closure_apps: ('expr * 'expr closure) list; closure_rescues: 'expr list; closure_bindings: (string * ('expr * ('expr closure))) list ; closure_lettbl: (string, 'expr ref) Hashtbl.t }

type drv = Mod.drv and dunerule = Mod.dunerule

module type Mod = module type of Mod

module Builtins =
  struct

  let map = Iter.map

  let filter = Iter.filter

  let head = Iter.head_exn

  let attr_names (set_ : (string * _) list) = List.map (fun (key, _) -> key) set_

  let length = Iter.length

  let type_of a = failwith "unimplemented"

  end

let dot = char '.'

let comma = char ','

let equal = char '='

let question_mark = char '?'

let slash = char '/'

let sharp = char '#'

let semicolon, colon = char ';', char ':'

let lbrac, rbrac = char '{', char '}'

let lparen, rparen = char '(', char ')'

let langle, rangle = char '<', char '>'

let lsq, rsq = char '[', char ']'

let dquote = char '"'

let ws0, ws1 =
  let whitespace = function ' ' | '\t' | '\n' -> true | _ -> false in
  let ws0 = take_while whitespace in
  let ws1 = take_while1 whitespace in
  ws0, ws1

let or_ = char 'o' *> char 'r' >>| fun _ -> ()

let let_ = char 'l' *> char 'e' *> char 't' >>| fun _ -> ()

let in_ = char 'i' *> char 'n' >>| fun _ -> ()

let rec_ = char 'r' *> char 'e' *> char 'c' >>| fun _ -> ()

let true_ = char 't' *> char 'r' *> char 'u' *> char 'e' >>| fun _ -> true

let false_ = char 'f' *> char 'a' *> char 'l' *> char 's' *> char 'e' >>| fun _ -> false

let inherit_ = char 'i' *> char 'n' *> char 'h' *> char 'e' *> char 'r' *> char 'i' *> char 't' >>| fun _ -> ()

let with_ = char 'w' *> char 'i' *> char 't' *> char 'h' >>| fun _ -> ()

let pipe_forward = char '|' *> char '>' >>| fun _ ->
  let open Effect in
  let open Effect.Deep in
  perform @@ Eff.Experimental_feature (`pipe, "pipe-operators");
  ()

let pipe_backward = char '<' *> char '|' >>| fun _ ->
  let open Effect in
  let open Effect.Deep in
  perform @@ Eff.Experimental_feature (`pipe, "pipe-operators");
  ()

let identifier =
  let first_char k =
    let* c = peek_char_fail in
    match c with
    ('a' .. 'z' | 'A' .. 'Z' | '_') as c -> bind (advance 1) (fun () -> k c)
    | _ -> fail "invalid identifier" in
  let rest first_char = first_char @@ fun c ->
    take_while (function
     'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true | _ -> false)
    >>| function "" -> String.make 1 c | cc -> String.make 1 c ^ cc in
  first_char |> rest

let integer =
  let first_char k =
    let* c = peek_char_fail in
    match c with '1' .. '9' as c -> bind (advance 1) (fun () -> k c)
    | _ -> fail "invalid identifier" in
  let rest first_char = first_char @@ fun c ->
    take_while (function '0' .. '9' -> true | _ -> false)
    >>| (int_of_string % function "" -> String.make 1 c | cc -> String.make 1 c ^ cc) in
  first_char |> rest

let string_lit =
  let* _ = dquote in
  let* content = take_while (function '"' -> false | _ -> true) in
  let* () = advance 1 in
  return content

let bool_lit = true_ <|> false_

let absolute_path =
  slash *>
  (fix @@ fun inner ->
  lift2 List.cons (identifier <* slash) inner
  <|> lift (fun x -> [x]) identifier)

let relative_path =
  dot *> slash *>
  (fix @@ fun inner ->
  lift2 List.cons (identifier <* slash) inner
  <|> lift (fun x -> [x]) identifier)

let lookup_path =
  let inner = fix @@ fun inner ->
    lift2 List.cons (identifier <* slash) inner
    <|> lift (fun x -> [x]) identifier in
  langle *> inner <* rangle

let comment =
  let* _ = sharp in
  let* content = take_while (function '\n' -> false | _ -> true) in
  let* () = advance 1 in
  return content

let ( *> ) a b = a *> ws0 *> b

let ( <* ) a b = a <* ws0 <* b

let lift2 f a b = lift2 f a (ws0 *> b)

let var = identifier

let constant = integer

let strict_list expr =
  let pair expr =
    lift2 (fun a b -> (a, b)) (identifier <* equal) expr in
  let pairlist pair = fix @@ fun pairlist ->
    lift2 List.cons (pair <* semicolon) pairlist <|> (return []) in
  pairlist @@ pair expr

let amend_list expr =
  let nested_id identifier = fix @@ fun nested_id ->
    lift2 List.cons identifier (dot *> nested_id)
    <|> lift List.singleton identifier
    <|> return []
  in
  let seplist x = fix @@ fun seplist ->
    lift2 List.cons x seplist
    <|> lift (fun x -> [x]) x in
  let pair expr =
    lift2 (fun (a:string list) b -> match a with [a] -> [a, b] | _ -> failwith "nested set syntax is not suppoted yet") (nested_id identifier <* equal) expr
    <|> lift (List.map @@ fun id -> id, `var id) (inherit_ *> (seplist identifier))
  in
  let pairlist pair = fix @@ fun pairlist ->
    lift2 List.cons (pair <* semicolon) pairlist
    <|> (return []) in
  lift (List.flatten) @@ pairlist @@ pair expr

let list_expression simple_expr =
  let rec chain = fix @@ fun chain ->
    let ( *> ) = Cprg.Syntax.( *> ) and lift2 = Cprg.lift2 in
    lift2 List.cons (return () *> simple_expr) (ws1 *> chain)
    <|> return [] in
  lsq *> chain <* rsq

let attrset_expression expr =
  lbrac *> (amend_list expr) <* rbrac

let rec_attrset_expression expr =
  rec_ *> attrset_expression expr

module Id =
  struct

  let query_opt (name : string) (o: 'expr closure) : ('expr * ('expr closure)) option =
    List.assoc_opt name o.closure_bindings

  let app closure_bindings closure_lettbl (f : 'expr closure -> 'a) : 'a =
    f { closure_bindings ; closure_lettbl ; closure_rescues = [] ; closure_apps = [] ; closure_access = [] }

  let lettbl_find o key =
    Hashtbl.find o.closure_lettbl key

  let lettbl_add o key value =
    Hashtbl.add o.closure_lettbl key value

  let append bindings o =
    { o with closure_bindings = bindings @ o.closure_bindings ; closure_lettbl = Hashtbl.copy o.closure_lettbl }

  let copy o =
    { o with closure_bindings = o.closure_bindings ; closure_lettbl = Hashtbl.copy o.closure_lettbl }

  let mkcontext_of_bindings args =
    { closure_bindings = args ; closure_lettbl = Hashtbl.create 1 ; closure_rescues = [] ; closure_apps = [] ; closure_access = [] }

  let rescue_append x o =
    { o with closure_rescues = x :: o.closure_rescues }

  let rescue_pop_opt o =
    match o.closure_rescues with
    | [] -> None
    | hd :: xs -> Some (hd, { o with closure_rescues = xs })

  let apps_append x o =
    (* Printf.eprintf "apps_appen before: %d\n" (List.length o.closure_apps); *)
    let o = { o with closure_apps = x :: o.closure_apps } in
    (* Printf.eprintf "apps_appen after: %d\n" (List.length o.closure_apps); *)
    o

  let apps_pop_opt o =
    (* Printf.eprintf "apps_pop before: %d\n" (List.length o.closure_apps); *)
    match o.closure_apps with
    | [] ->
      None
    | hd :: xs ->
      let o = { o with closure_apps = xs } in
      (* Printf.eprintf "apps_pop after: %d\n" (List.length o.closure_apps); *)
      Some (hd, o)

  let apps_inherit o0 o =
    (* Printf.eprintf "apps_inherit before: %d\n" (List.length o.closure_apps); *)
    let o = { o with closure_apps = o0.closure_apps } in
    (* Hashtbl.add_seq o.closure_lettbl (Hashtbl.to_seq o0.closure_lettbl); *)
    (* Printf.eprintf "apps_inherit after: %d\n" (List.length o.closure_apps); *)
    o

  let access_append x o =
    { o with closure_access = x :: o.closure_access }

  let access_pop_opt o =
    match o.closure_access with
    | [] ->
      None
    | hd :: xs ->
      let o = { o with closure_access = xs } in
      Some (hd, o)

  let access_inherit o0 o =
    { o with closure_access = o0.closure_access }

  let ctx_cat a b =
    { closure_bindings = a.closure_bindings @ b.closure_bindings
    ; closure_lettbl = (let x = Hashtbl.copy a.closure_lettbl in Hashtbl.add_seq x (Hashtbl.to_seq b.closure_lettbl); x)
    ; closure_rescues = a.closure_rescues @ b.closure_rescues
    ; closure_apps = a.closure_apps @ b.closure_apps
    ; closure_access = a.closure_access @ b.closure_access
    }

  end

let environment_expression simple_expr =
  lift2 (fun a body -> a, body) (with_ *> simple_expr <* semicolon) simple_expr

let with_attr_access simple_expr =
  let* init = simple_expr in
  ( let* dot = ws0 *> dot and* var = ws0 *> var in
    return @@ `access (init, var)
  )
  <|> (return init)

let reverse_application lhs_simple_expr' simple_expr =
  let* y = ws0 *> pipe_forward *> simple_expr in
  return @@ `app (y, lhs_simple_expr')

let forward_application lhs_simple_expr' simple_expr =
  let* y = ws0 *> pipe_backward *> simple_expr in
  return @@ `app (lhs_simple_expr', y)

let or_expression lhs_simple_expr' simple_expr =
  let* rhs = or_ *> simple_expr in
  return @@ `or_ (lhs_simple_expr', rhs)

let rec with_function_application (type ctx) (simple_expr : (ctx, _) code) = fix @@ fun function_application ->
  let currying v function_application =
    let* z = ws0 *> function_application in
    return @@ `app (v, z) in
  let* x =
    let ( *> ) = Cprg.Syntax.( *> ) in
    (return ()) *> simple_expr in
  or_expression x simple_expr
  <|> reverse_application x simple_expr
  <|> forward_application x simple_expr
  <|>
  ( let* y = lift (fun y -> `app (x, y)) (ws0 *> simple_expr) in
    currying y function_application
    <|> return y
  )
  <|> return x

let simple_expression expr = fix @@ fun simple_expression ->
  with_attr_access (
    (lparen *> expr <* rparen)
    <|> lift (Lifts.Lift_int.lift %> fun x -> `int x) @@ integer
    <|> lift (fun x -> `bool .< x >.) @@ bool_lit
    <|> lift (fun x -> `list x) @@ list_expression simple_expression
    <|> lift (fun x -> `attrset x) @@ attrset_expression expr
    <|> lift (fun x -> `absolute_path x) @@ absolute_path
    <|> lift (fun x -> `relative_path x) @@ relative_path
    <|> lift (fun x -> `lookup_path x) @@ lookup_path
    <|> lift (fun x -> `var x) var
  )

let function_arg_attrset_destructure expr =
  let pair simple_expr =
    lift2 (fun a b -> `arg_with_default (a, b)) (identifier <* question_mark) simple_expr
    <|> lift (fun a -> `arg a) identifier in
  let pairlist pair = fix @@ fun pairlist ->
    lift2 List.cons (pair <* comma) pairlist
    <|> lift (fun x -> [x]) (pair <* comma)
    <|> lift (fun x -> [x]) pair
    <|> return [] in
  lbrac *> (pairlist @@ pair expr) <* rbrac

(** @reference https://nix.dev/tutorials/nix-language.html#functions *)
let function_ simple_expr expr =
  let arg =
    lift (fun args -> `set args) (function_arg_attrset_destructure expr)
    <|> lift (fun a -> `single a) var in
  lift2 (fun a b -> b, fun (input_expr, ctxe) ->
    match a with
    | `single name -> `bindings ([name, `provider (ctxe, input_expr)], b)
    | `set args ->
      let bindings = args |> List.map @@ function
        | `arg name -> name, `access (`provider (ctxe, input_expr), name)
        | `arg_with_default (name, e) -> name, `or_ (`access (`provider (ctxe, input_expr), name), e)
      in
      `bindings (bindings, b)
  ) (arg <* colon) expr

let binding expr =
  let* k = let_ *> strict_list expr in
  lift2 (fun rhs body -> (rhs, body)) (
    ((return k) <* in_)
    <|> (lift (fun () -> raise Eff.Syntax_error.Unfinished_let_in) @@ return ())
  ) expr

let rec toplevel_mkbinding key expr =
  match expr with
  | `int i -> let l = Codelib.genlet ~name:key i in `int l
  | `drv c -> let c = Codelib.genlet ~name:key c in `drv c
  | `dunerule c -> let c = Codelib.genlet ~name:key c in `dunerule c
  | `dunerule_algebra (`mkRule c) -> let c = Codelib.genlet ~name:key c in `dunerule_algebra (`mkRule c)
  | `dunenix_algebra c -> let c = Codelib.genlet ~name:key c in `dunenix_algebra c
  | ex -> ex

and toplevel_eval ?parentkey ctx body =
  match body with
  | `provider (ctxe, body) ->
    toplevel_eval ?parentkey (Id.ctx_cat ctxe ctx) body
  | `bindings (map, body) ->
    let bindings = List.map (fun (key, e) ->
      let e, env = toplevel_eval ~parentkey:key ctx e in
      key, (e, env)
    ) map in
    let ctx = Id.append bindings ctx in
    toplevel_eval ?parentkey ctx body
  | e ->
    let e = match parentkey with Some key -> toplevel_mkbinding key e | None -> e in
    e, ctx

and splice_expr ?(back = false) name ctx type_ expr =
  let eval name ctx expr = match name with
    | Some name when back ->
      (* Printf.eprintf "some name %s\n" name; *)
    ( match Id.lettbl_find ctx name with
      | v ->
        (* Printf.eprintf "cache found unchanged for %s!\n" name; *)
        (!v, ctx)
      | exception Not_found ->
        (* Printf.eprintf "cache not found for %s\n" name; *)
        let expr, ctx = toplevel_eval ~parentkey:name ctx expr in
        Id.lettbl_add ctx name (ref expr);
        expr, ctx
    )
    | _ ->
      (* ( match name with Some name when not back -> Printf.eprintf "WARNING: inconsistent back: %s\n" name | _ -> () ); *)
      (* Printf.eprintf "no name\n"; *)
      toplevel_eval ctx expr
      in
  let expr, ctx = eval name ctx expr in
  let else_ = fun e k ->
    match e with
    | `attrset set_ as expr ->
      ( match Id.access_pop_opt ctx with
      | Some (name_, ctx) ->
        (* Printf.eprintf "trying to listassoc %s in a list of [%s]\n" name_ (set_ |> List.map (fun (k,v) -> k) |> String.concat ";"); *)
        let v = match List.assoc_opt name_ set_ with Some v -> v | None -> raise (Eff.Access_not_found name_) in
        splice_expr name ctx type_ v
      | None -> k expr
      )
    | `lambda (_, f) as expr ->
      Id.apps_pop_opt ctx
      |> Option.map (fun ((arge, ctxe), ctx) -> splice_expr name ctx type_ @@ f (arge, ctxe))
      |> Option.value' ~default:(fun () -> k expr)
    | e -> k e
  in
  match expr with
  | `var name ->
    ( match Id.query_opt name ctx with
    | Some (c, ctx') ->
      let ctx = ctx' |> Id.apps_inherit ctx |> Id.access_inherit ctx in
      type_ (Some name) ctx c (fun e k ->
        match e with
        | (`var _ | `with_binding _ | `app _ | `access _ | `or_ _ | `lambda _ | `attrset _) as expr -> splice_expr (Some name) ctx type_ expr
        | e -> k e
      )
    | None -> 
      ( match Id.rescue_pop_opt ctx with
      | Some (prefix, ctx) -> splice_expr (Some name) ctx type_ @@ `access (prefix, name)
      | None -> raise (Eff.Variable_not_found name)
      )
    )
  | `or_ (a, b) ->
    ( try splice_expr name ctx type_ a with _ -> splice_expr name ctx type_ b )
  | `with_binding (prefix, expr) ->
    let ctx = Id.rescue_append prefix ctx in
    splice_expr name ctx type_ expr
  | `app (f, arg) ->
    (* Printf.eprintf "saving closure %s whose bindings are [%s]\n" (name |> Option.value ~default:"??") (ctx.closure_bindings |> List.map (fun (k,v) -> k) |> String.concat ";"); *)
    let ctx = Id.apps_append (arg, ctx) ctx in
    splice_expr name ctx type_ f
  | `access (e, name_) ->
    let ctx = Id.access_append name_ ctx in
    splice_expr name ctx type_ e
  | expr -> type_ None ctx expr else_

let type_int name _ v k = match v with `int c -> c | e -> k e @@ fun _ -> failwith "type int wrong"

let type_attrset name _ v k = match v with `attrset args -> args | e -> k e @@ fun _ -> failwith "type attrset wrong"

let type_list ~type_el name ctx v k =
  match v with
  | `list es ->
    List.fold_right (fun (i, e) c ->
      let elc = splice_expr None ctx type_el e in
      .< .~elc :: .~c >.
    ) (List.mapi (fun i e -> (i, e)) es) .< [] >.
  | e -> k e @@ fun _ -> failwith "type list wrong"

let rec type_dune_nix_path name ctx v k =
  match v with
  | `attrset _ ->
    fun s () ->
    let open Result.Syntax in
    let* m, ctx =
      Id.query_opt (String.concat "/" ("" :: s)) ctx
      |> Option.map (fun (c, ctx') -> 
        let ctx = ctx' |> Id.apps_inherit ctx |> Id.access_inherit ctx in
        return (c, (ctx : _ closure)))
      |> Option.value ~default:(fail @@ `Lookup_path_not_found s) in
    return (m, ctx)
  | e -> k e @@ fun _ -> failwith "type dune nix path wrong"

and type_import typearg_ name ctx v k =
  match v with
  | `lookup_path s ->
    ( match Id.apps_pop_opt ctx with
    | Some ((arg, argctx), ctx) -> splice_expr None (Id.ctx_cat argctx ctx) typearg_ arg @@ s
    | None -> k v @@ fun _ -> failwith "type import wrong 2"
    )
  | e -> k e @@ fun _ -> failwith "type import wrong"

let rec type_dunerule_algebra name ctx v k =
  match v with
  | `dunerule_algebra mkx ->
    ( match Id.apps_pop_opt ctx with
    | Some ((args, argctx), ctx) ->
      let c = match mkx with
        | `mkRule c ->
          let ctx = Id.ctx_cat argctx ctx in
          let args = splice_expr None ctx type_attrset args in
          let target = List.assoc_opt "target" args |> Option.map (splice_expr None ctx type_int) in
          let action = List.assoc_opt "action" args |> Option.map (splice_expr None ctx type_int) in
          let c : (?target:int ->  ?action:int -> unit -> dunerule) Trx.code = c in
          let c = match target with Some target -> .< .~c ~target:.~target >. | None -> .< .~c ?target:None >. in
          let c = match action with Some action -> .< .~c ~action:.~action >. | None -> .< .~c ?action:None >. in
          let c = .< .~c () >. in
          c
        | `mkExecutable c ->
          let ctx = Id.ctx_cat argctx ctx in
          let args = splice_expr None ctx type_attrset args in
          let name_ = List.assoc "name" args |> splice_expr None ctx type_int in
          let libraries = List.assoc_opt "libraries" args |> Option.map (splice_expr None ctx (type_list ~type_el:type_int)) in
          let c : (name:int ->  ?libraries:(int list) -> unit -> dunerule) Trx.code = c in
          let c = .< .~c ~name:.~name_ >. in
          let c = match libraries with Some libraries -> .< .~c ~libraries:.~libraries >. | None -> .< .~c ?libraries:None >. in
          let c = .< .~c () >. in
          c
      in
      splice_expr ~back:true name ctx type_dunerule @@ `dunerule c
    | None -> k v @@ fun _ -> failwith "type dunerule algebra wrong 2"
    )
  | e -> k e @@ fun _ -> failwith "type dunerule algebra wrong"

and type_dunerule name ctx v k =
  match v with
  | `dunerule c -> c
  | `import type_import -> (* m ::= imported_nix_module agr *)
    Id.apps_pop_opt ctx
    |> Option.map (fun ((args, argctx), ctx) ->
      let resolve = splice_expr None (Id.ctx_cat argctx ctx) type_import args in
      let context, ctx' = match resolve () with Result.Ok x -> x | Result.Error e -> raise @@ Eff.Import_error Eff.Import_error.(Resolve_error Resolve_error.(Find_file_error e)) in
      let ctx = ctx |> Id.access_inherit ctx' |> Id.apps_inherit ctx' in
      splice_expr ~back:true name ctx type_dunerule_algebra context)
    |> Option.value' ~default:(fun () -> k v @@ fun _ -> failwith "type dunerule wrong 2")
  | `dunerule_algebra _ as context ->
    splice_expr ~back:true name ctx type_dunerule_algebra context
  | e -> k e @@ fun _ -> failwith "type dunerule wrong"

let a = ref None

let rec type_drv_algebra name ctx v k =
  match v with
  | `dunenix_algebra derivation ->
    Option.value' ~default:(fun () -> k v @@ fun _ -> failwith "type drv algebra wrong 2")
    @@ Option.map (fun ((args, argctx), ctx) ->
      let args = splice_expr None (Id.ctx_cat argctx ctx) (type_list ~type_el:type_dunerule) args in
      let derivation = .< .~derivation .~args >. in
      splice_expr ~back:true name ctx type_drv @@ `drv derivation)
    @@ Id.apps_pop_opt ctx
  | e -> k e @@ fun _ -> failwith "type drv algebra wrong"

and type_drv name ctx v k =
  match v with
  | `drv c -> c
  | `import type_import ->
    ( match Id.apps_pop_opt ctx with
    | Some ((args, argctx), ctx) ->
      let resolve = splice_expr None (Id.ctx_cat argctx ctx) type_import args in
      (* Printf.eprintf "test %d\n" (List.length ctx.closure_apps); *)
      let context, ctx' = match resolve () with Result.Ok x -> x | Result.Error e -> raise @@ Eff.Import_error Eff.Import_error.(Resolve_error Resolve_error.(Find_file_error e)) in
      let ctx = ctx |> Id.access_inherit ctx' |> Id.apps_inherit ctx' in
      (* k0 ctx type_drv_algebra context *)
      splice_expr ~back:true name ctx type_drv_algebra context
    | None -> k v @@ fun _ -> failwith "type dune nix module wrong 2"
    )
  | `dunenix_algebra _ as context ->
    splice_expr ~back:true name ctx type_drv_algebra context
  | e -> k e @@ fun _ -> failwith "type drv wrong"

let expression () = fix @@ fun expr ->
  lift (fun (arg, body) -> `bindings (arg, body)) @@ binding expr
  <|> lift (fun (a, body) -> `with_binding (a, body)) @@ environment_expression (simple_expression expr)
  <|> lift (fun (a, f) -> `lambda (a, f)) @@ function_ (simple_expression expr) expr
  <|> lift (fun x -> `attrset x) @@ rec_attrset_expression expr
  <|> with_function_application @@ simple_expression expr

let langchain () =
  let open Cprg.Syntax in
  let ctx = Id.mkcontext_of_bindings [] in
  let module_stdenv = `attrset [
    "derivation", `dunenix_algebra .< Mod.derivation >.;
    "mkDerivation", `dunenix_algebra .< Mod.derivation >.;
    "mkRule", `dunerule_algebra (`mkRule .< Mod.mkRule >.);
    "mkExecutable", `dunerule_algebra (`mkExecutable .< Mod.mkExecutable >.);
  ] in
  let ctx = Id.append ["/dune-nix", ( module_stdenv, ctx )] ctx in
  let ctx = Id.append ["builtins", ( `attrset [
    "import", `import (type_import type_dune_nix_path);
  ], ctx )] ctx in
  let* e = ws0 *> expression () <* ws0 in
  let f = splice_expr None ctx (fun name ctx v k ->
    match v with
    | `lambda (_, f) -> splice_expr None ctx type_drv @@ f (`attrset [ "stdenv", module_stdenv ], Id.mkcontext_of_bindings [])
    | e -> k e @@ fun _ -> failwith "type program wrong"
  ) (`with_binding ((`var "builtins"), e)) in
  return ((), f)

module Parse =
  struct

  let expression' s =
    let open Result.Syntax in
    let* body, f = Cprg.parse_string ~consume:`All (langchain ()) s in
    Codelib.print_closed_code Format.std_formatter @@ Codelib.close_code ~csp:CSP_ok f;
    return f

  let expression lb =
    let open Result.Syntax in
    let* body, f = Cprg.parse_buf ~consume:`All (langchain ()) lb in
    Codelib.print_closed_code Format.std_formatter @@ Codelib.close_code ~csp:CSP_ok f;
    return f

  end
