(* Liftable types
   The types of present-stage values that are OK to use at a future
   stage (values safe to CSP, both in byte- and native-code modes)

   Although polymorphic lift is possible in MetaOCaml and works well
   in byte-code, it is not at all helpful in the native mode.
*)

open Codelib

module Lift_int       : (lift with type t = int)
module Lift_int32     : (lift with type t = int32)
module Lift_int64     : (lift with type t = int64)
module Lift_nativeint : (lift with type t = nativeint)
module Lift_char      : (lift with type t = char)
module Lift_float     : (lift with type t = float)
module Lift_string    : (lift with type t = string)

(* More can, and should be added *)

module Lift_option : functor (L:lift) -> (lift with type t = L.t option)
module Lift_list   : functor (L:lift) -> (lift with type t = L.t list)
module Lift_array  : functor (L:lift) -> (lift with type t = L.t array)

val lift_array : 'a code array -> 'a array code
