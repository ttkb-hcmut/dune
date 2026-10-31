type experiments = [ `pipe ]

type _ Effect.t += Experimental_feature : (experiments * string) -> unit Effect.t

module Import_error : sig

	module Resolve_error : sig
		type t =
			| Find_file_error of [
				`Lookup_path_not_found of string list
			]
	end

	type t =
		| Resolve_error of Resolve_error.t

end

exception Variable_not_found of string

exception Access_not_found of string

exception Import_error of Import_error.t

module Syntax_error : sig
	exception Unfinished_let_in
end

