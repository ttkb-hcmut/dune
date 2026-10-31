type drv and dunerule and libname and link_mode

module Modes : sig

	val byte : link_mode

	val jsoo : link_mode

end

val mkExecutable : name:string -> ?mode:link_mode -> ?libraries:(libname list) -> unit -> dunerule
