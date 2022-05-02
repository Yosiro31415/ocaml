(*PURPOSE: to calculate Pi*)
 
open Random;;

Random.self_init();;

let calcDistance coo =
  match coo with
    (x,y)->
      sqrt (x *. x +. y *. y) ;;

let isInCircle distance = if distance < 1.0 then true else false;;

let cood (x,y) = (x,y);;

isInCircle ( calcDistance (cood (Random.float 1.0 ,Random.float 1.0)));;

let calcSum sumInCircle = if isInCircle ( calcDistance (cood (Random.float 1.0 ,Random.float 1.0))) = true
  then sumInCircle +. 1. 
  else sumInCircle;;

let rec calcPi (cnt, res,num) = if cnt > 0 then calcPi (cnt - 1, calcSum res,num) else (res /. num /.4. );;

calcPi (9999999 , 0.0,9999999.);;