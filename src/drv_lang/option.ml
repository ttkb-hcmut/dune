include Option

let value' v ~default =
  match v with
  | Some v -> v
  | None -> default ()
