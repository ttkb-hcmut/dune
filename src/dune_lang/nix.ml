open Import

let syntax =
  Syntax.create
    ~name:"nix"
    ~desc:"experimental support for Nix expressions"
    ~experimental:true
    [ (0, 1), `Since (3, 20) ]
;;

