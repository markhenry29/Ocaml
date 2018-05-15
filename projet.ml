type fp = X of int | Vrai1 | Faux1 | Et of fp * fp | Ou of fp * fp | Imp
of fp * fp | Equiv of fp * fp | Non of fp ;;
type sas = Y of int | Vrai2 | Faux2 | Si of sas * sas * sas ;;
type env = Vide | Env of int * bool * env;;
  

let rec trad f = match f with
  |X(i)  -> Y(i)
  |Vrai1 -> Vrai2
  |Faux1 -> Faux2
  |Non(p) -> Si(trad p, Faux2, Vrai2)
  |Ou(p, q) -> Si(trad p, Vrai2, Si(trad q, Vrai2, Faux2))
  |Et(p, q) -> Si(trad p, Si(trad q, Vrai2, Faux2), Faux2)
  |Imp(p, q) -> Si(trad p, Si(trad q, Vrai2, Faux2),Vrai2)
  |Equiv(p, q) -> Si(trad p, Si(trad q, Vrai2, Faux2), Si(trad q, Faux2, Vrai2))
;;
                    
let rec mefn sas = match sas with
  |Y(i) -> Y(i)
  |Vrai2 -> Vrai2
  |Faux2 -> Faux2
  |Si(Vrai2, b, c) -> mefn b
  |Si(Faux2, b, c) -> mefn c
  |Si(Si(a,b,c),d,e) -> mefn (Si(a, Si(b,d,e), Si(c,d,e)))
  |Si(Y(i), b,c) -> Si(Y(i), b,c)
;;

let rec eval f e = match f with
  |Vrai2 -> true
  |Faux2 -> false
  |Y(i) -> if(estDans e i)
           then b
           else false
  |Si(g, h, k) -> match g with
                  |Vrai2 -> eval h e
                  |Faux2 -> eval k e
                  |Y(i) -> if(estDans e i)
                           then (if(b) then (eval h e) else (eval k e))
                           else eval h Env(i,true,e)&&(eval k Env(i, false, e))
                                  


let rec estDans environnement i = match environnement with
  |Vide -> false
  |Env(indice, b, e) -> if(i == indice)
                        then true
                        else estDans e i;;

  
let f = Et(X(1),Faux1);;
let res = trad f;;
let formenormale = mefn res;;
let test = Env(3, true, Env(1, false, Vide));;
