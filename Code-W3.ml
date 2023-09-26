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