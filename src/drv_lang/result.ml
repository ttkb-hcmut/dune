include Result

module Syntax =
  struct

  let ( let* ) = bind

  let return = ok and fail = error

  end
