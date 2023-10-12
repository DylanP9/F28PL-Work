let mulX p = 0 :: p

let rec isPoly  p = 
  let rec helper p b = 
    match p with 
    | [] -> not b
    | a :: p1 ->
      if a = 0 then helper p1 true
      else helper p1 false
    in helper p false

let valueAt ps x =
  let rec aux xn ps =
    match ps with
    | [] -> 0.0
    | p :: ps -> p *. xn +. aux (xn *. x) ps
  in aux 1.0 ps;;

let degree ps =
  let rec aux ps =
    match ps with
    | [] -> 0
    | _ :: ps -> 1 + aux ps
  in match ps with
  | [] -> 0
  | _ :: ps -> aux ps;;

  let xs = [3]


let rec length p = 
  match p with
  | [] -> 0
  |a :: p -> 1 + length p

let degree p =
  match p with
  | [] -> 0
  | a :: p -> length(a :: p) - 1