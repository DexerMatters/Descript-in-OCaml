
type 'a stream = Cons of {rest: 'a stream; head: 'a} | Nils
let explode str = 
  let rec aux p (chars : char stream) = 
    if p = -1 then chars else
      let c = String.get str p in
      aux (p - 1) (Cons {rest = chars; head = c}) in
  aux ((String.length str) - 1) Nils

let rec map f strm = match strm with 
| Nils -> () 
| Cons r -> f r.head; map f r.rest
