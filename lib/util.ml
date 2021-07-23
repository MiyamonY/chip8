open StdLabels

let id x = x

let succ x = x + 1

let pred x = x - 1

let (++=) n = List.init ~len:(succ n) ~f:id

let (++<) n = List.init ~len:n ~f:id
