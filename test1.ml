
module type mapable = sig
    type 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
end

let map f (implicit M : mapable) = M.map f
(* let map (implicit M : mapable) ~f = M.map f *)

module type iterable = sig
  type 'a t
  val iter : ('a -> unit) -> 'a t -> unit
end

let iter f (implicit M : iterable) = M.iter f

implicit module L = struct
           type 'a t = 'a list
           include List
end

implicit module A = struct
           type 'a t = 'a array
           include Array
         end

module type scal_mult = sig
  type scal
  type t
  val mult : scal -> t -> t
end

(* RQ : l'implicit module devrait toujours apparaitre en dernier
   dans le type de la fonction (juste avant le resultat... *)
let scal_mult (type u) (type v) (x:u) (implicit A : scal_mult with type scal = u and type t = v) (y:v) = A.mult x y
let scal_mult (type u) (x:u) (implicit A : scal_mult with type scal = u) (y:A.t) = A.mult x y
let scal_mult (type u) (type v) (x:u) (y:v) (implicit A : scal_mult with type scal = u and type t = v) = A.mult x y

implicit module R =
struct
  type scal = float
  type t = float * float

  let mult s (x, y) = (s *. x, s *. y)
end

let _ = List.map (scal_mult 1.2) [ (2.3, 3.4) ] (* CACA ! *)
let _ = List.map (fun x -> scal_mult 1.2 x) [ (2.3, 3.4) ]
let _ = List.map (scal_mult 1.2 (implicit R)) [ (2.3, 3.4) ]

let _ = scal_mult 1.2 (2.3, 3.4)


module type to_string = sig
  type t
  val to_string : string -> t
end

let to_string (implicit M : to_string) x = M.to_string x

implicit module String_tostring = struct
           type t = string
           let to_string x = x
end

let _ = List.map (fun x -> to_string x) [ "1" ; "2" ]
