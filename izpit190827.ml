(* 1. naloga *)
(*(a)*)
let odstej_trojici (x1, x2, x3) (y1, y2, y3) =
  (x1 - y1, x2 - y2, x3 - y3)

(*(b)*)
(*alternativna rešitev: uporaba List.init in uporabimo max*)
let max_rezultat_do_n f n =
  let rec aux acc m =
    if m > n then
      acc
    else if f m > acc then
      aux (f m) (m + 1)
    else
      aux acc (m + 1)
  in aux (f 0) 0

(*(c)*)
let pocisti_seznam l =
  let rec aux acc l' = match l' with
    | [] -> acc
    | None::xs -> aux acc xs
    | x::xs -> aux (x::acc) xs
  in aux [] l

let pocisti2 l =
  let rec aux acc = function
    | [] -> List.rev acc
    | (Some x)::xs -> aux (x::acc) xs
    | _::xs -> aux acc xs
  in aux [] l

(*(d)*)
(*razdelimo na dva dela, uredimo, preverimo, če sta ok*)
(*drugi način: se zapeljemo čez, preverimo, če so števila večja od najmanjšega sodega in manjša od največjega lihega*)
let preveri_urejenost l =
  let rec aux min_sodo max_liho = function
    | [] -> true
    | x::xs -> if x mod 2 = 0 then x > min_sodo && aux x max_liho xs else x < max_liho && aux min_sodo x xs
  in aux -99999990 9999999990 (*slabo, bolje če se zapeljemo čez seznam in poiščemo najmanjše in največje število ali uporabimo int.min, int.max neki takga*)


(*2. naloga*)
type 'a gnezdenje =
  | Element of 'a
  | Podseznam of 'a gnezdenje list
(*(a)*)
let gnezdenje_primer =
  Podseznam [
    Element 1;
    Element 2;
    Podseznam [
      [Element 3; Podseznam [Element 4]; Podseznam []]
    ];
    Podseznam [Element 5]
  ]

(*(b)*)
let rec najvecja_globina_gnezdenja g =
  match g with
  | Element _ -> 0
  | POdseznam xs -> 1 + (List.fold_left max 1 (List.map najvecja globina xs))

let najvecja_globina g_list =
  1 + (List.fold_left max 1 (List.map najvecja globina xs))

(*(c)*)
let rec preslikaj f g = match g with
  | Element x -> Element (f x)
  | Podseznam xs -> Podseznam ((List.map (preslikaj f)) xs)

(*(d)*)
let rec splosci = function
  | Element x -> [x]
  | Podseznam xs ->
    let splosceni = List.map splosci xs in
    List.fold_left (@) [] splosceni

(*(e)*)
let alternirajoci_konstruktorji = function
  | [] -> true
  | [x] -> true
  | Element _ :: Podseznam p::xs ->  alternirajoci_konstruktorji ((Podseznam p)::xs)
  | Podseznam _:: Element p::xs ->  alternirajoci_konstruktorji ((Element p)::xs)
  | _ -> false

(*(f)*)
let rec zlozi_gnezdenje f acc g = match g with
  | Element x -> f acc xs
  | Podseznam l -> 
    List.fold_left (zlozi_gnezdenje f) acc l

let zlozi_preko_gnezdenja f acc g_list =
  zlozi_gnezdenje f acc (Podseznam g_list)