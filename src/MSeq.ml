type 'a t = 'a Seq.t

let map (f : 'a -> 'b) (s : 'a t) : 'b t =
  Seq.map f s

let return (x : 'a) : 'a t =
  Seq.cons x Seq.empty

let bind (sa : 'a t) (f : 'a -> 'b t) : 'b t =
  Seq.concat_map f sa

let delay (f : unit -> 'a t) : 'a t =
  fun () -> f () ()

let sum (li : 'a t list) : 'a t =
  Seq.concat (List.to_seq li)

let fail : 'a t =
  Seq.empty

let one_of (vs : 'a array) : 'a t =
  Array.to_seq vs

let run (s : 'a t) : 'a Seq.t =
  s
