type arbre = Vide | Noeud of int * arbre * arbre;;

let rec appartient arbre n = match arbre with
  |Vide          -> false
  |Noeud (m,g,d) -> if(n == m)
                    then true
                    else appartient g n || appartient d n;;

let rec abr_en_liste arbre = match arbre with
  |Vide -> []
  |Noeud (m,g,d) -> (abr_en_liste g)@[m]@abr_en_liste(d);;

let rec inserer_bas arbre n = match arbre with
  |Vide -> Noeud(n, Vide, Vide)
  |Noeud(m, g,d) -> if(n == m)
                    then arbre
                    else
                      if(n < m)
                      then Noeud(m, inserer_bas g n, d)
                      else Noeud(m, g,inserer_bas d n)
;;

let rec couper arbre n = match arbre with
  |Vide ->(Vide,Vide)
  |Noeud(m,g,d) when (n = m) -> (g,d)
  |Noeud(m,g,d) when (n < m) -> let (a,b) = couper g n in (a, Noeud(m, b, d))
  |Noeud(m,g,d) -> let (a,b) = couper d n in (Noeud(m, g, a), b)
 ;;
                                                        
 let inserer_haut arbre n = match arbre with
   |Vide -> Noeud(n, Vide, Vide)
   |Noeud(m,g,d)-> let (a,b) = couper arbre n in Noeud(n,a,b);;

 let rec creer_arbre l acc = match l with
   |[]-> acc
   |x::lbis ->creer_arbre lbis (inserer_haut acc x)
                                        
 ;; 

 let tri_arbre l = abr_en_liste (creer_arbre l Vide) ;;
   
let abr = Noeud(4,Noeud(3, Vide, Vide), Noeud(9, Noeud(7,Vide, Vide), Noeud(11,Vide,Vide)));;

let list = [18; 5; 6; 4; 50; 656];;

tri_arbre list;;
