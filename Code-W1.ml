

let twice n = n + n

let rec fact n =
  if n = 0
  then 1
  else n * fact (n - 1)

let rec loop _ = loop ()

let rec explode _ = 1 + explode ()

let divides k n =
  n mod k = 0

  let rec gcd a b =
    if b = 0 then a
    else gcd b (a mod b)
;;


  let checkGCD m n = 
        divides (gcd m n ) m
    &&  divides  (gcd m n ) n


let angle m n =
  sqrt (m *. m +. n *. n);;

let isEven n = n mod 2 = 0;;

let isOdd n = not (isEven n);;
 
let myAnd m n =
  if n = m
    then n
else false

let myNot p = 
  if p 
    then false 
else true


