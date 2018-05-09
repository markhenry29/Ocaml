let rec append l1 l2 = match l1 with
  |[]-> l2
  |x::l1bis -> x::append l1bis l2;;

let rec decoupe entier l = match l with
  |[] -> ([], [])
  |x::lbis ->
    if x<=entier
    then  let (l1, l2) = decoupe entier lbis in (x::l1, l2)
    else  let (l1, l2) = decoupe entier lbis in (l1, x::l2)
;;

let rec tri_rapide l = match l with
  |[]->[]
  |a::lbis->
    let (l1,l2) = decoupe a lbis in append (tri_rapide l1) (a::(tri_rapide l2))
                                                    ;;


let rec fusion l1 l2 = match (l1, l2) with
  |([], l2)-> l2
  |(l1, [])-> l1
  |(x::l1bis, y::l2bis) -> if x < y
                           then x::fusion l1bis l2
                           else y::fusion l1 l2bis;;

(*let rec decoupe2 entier l = match l with
  |[]->[]
  |if entier<=0 then ([], l)*)
    
  
let listA = [3;6];;
let listB = [1;2; 40; 0];;

  tri_rapide listB;;
    
    
