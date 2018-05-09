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


let rec decoupe2 entier l=decoupecouple entier ([],l);;

  
let rec decoupecouple entier (l1,l2) =
  match (l1,l2) with
  |(l1,[])->(l1,[])
  |(l1,x::l)->if entier>0 then decoupecouple (entier-1) (l1@[x],l)
              else (l1,x::l);;

  
let rec tri_fusion l = match l with
  |[]->[]
  |[x]->[x]
  |l->let (l1,l2)= decoupe2 ((longueur l)/2) l in
      fusion (tri_fusion l1) (tri_fusion l2)

;;
                                           

let rec longueur l = match l with
  |[]->0
  |x::lbis->1 + longueur lbis;;

  
let listA = [3;6];;
let listB = [1;2; 40; 4; 7; 0; 8; 9;500;98;5;5;1];;

  tri_fusion listB;;
