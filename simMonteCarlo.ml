(*PURPOSE: to calculate Pi*)
 
open Random;;
Random.self_init();;


let numTryal = 99999;;

let calcDistance coo =
  match coo with
    (x,y)->
      sqrt (x *. x +. y *. y) ;;

let isInCircle distance = 
  if distance < 1.0 
    then true 
    else false;;

let cood (x,y) = (x,y);;

let calcNumInCircle numInCircle  =
  if isInCircle ( calcDistance (cood (Random.float 1.0 ,Random.float 1.0))) = true
    then numInCircle  +. 1. 
    else numInCircle ;;

let rec calcPi (cnt, res,num) = 
  if cnt > 0 
    then calcPi (cnt - 1, calcNumInCircle res,num) 
    else 4. *. (res /. num  );;

calcPi (numTryal, 0.0,float_of_int numTryal);;