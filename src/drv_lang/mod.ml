open Import

type drv = Drv of { rules : dunerule list }

and dunerule = Dunerule of { repr : Stanza.repr }

and libname  = Libname of { repr : string }

and link_mode = Executables.Link_mode.t

let loc = Loc.none

module Modes =
  struct

  let exe : link_mode = Executables.Link_mode.exe

  end

let derivation rules : drv = Drv { rules }

let mkLibrary () : dunerule = failwith "unimplemented"

let mkRule ?target ?action () : dunerule =
  failwith "unimplemented"

let mkExecutable ~name ?(modes = []) ?(libraries = []) () =
  let modes_map = Executables.Link_mode.Map.(empty |> (fun m -> List.fold_left (fun m mode -> m |> add mode loc) m modes)) in
  let modules =
    Stanza_common.Modules_settings.
    {
    root_module = Some (loc, Module_name.of_string_allow_invalid name);
    modules = Ordered_set_lang.Unexpanded.of_strings ~pos:("", 0, 0, 0) [":standard"];
    modules_without_implementation = Ordered_set_lang.Unexpanded.of_strings ~pos:("", 0, 0, 0) [];
    } in
  let libraries = List.map (function Libname lib -> Lib_dep.Direct (loc, Lib_name.of_local (loc, Option.get (Lib_name.Local.of_string_opt lib.repr)))) libraries in
  let buildable =
    Buildable.
    {
    loc;
    foreign_archives = [];
    foreign_stubs = [];
    modules;
    js_of_ocaml = Js_of_ocaml.{ js = In_buildable.default; wasm = In_buildable.default };
    empty_module_interface_if_absent = false;
    allow_overlapping_dependencies = false;
    preprocessor_deps = [];
    ctypes = None;
    extra_objects = Foreign.Objects.empty;
    flags = Ocaml_flags.Spec.standard;
    libraries
    } in
  let repr = Executables.
    {
    modes;
    buildable
    } in
  Dunerule { repr }
