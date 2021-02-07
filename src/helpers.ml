let add_some a b =
  match b with
  | Some b -> a + b
  | None   -> a

let count_some a b = 
  match b with
  | Some _ -> a + 1
  | None   -> a