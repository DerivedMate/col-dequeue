type 'v t = 
  { maxSize : int
  ; first   : int
  ; last    : int
  ; arr     : ('v option) array
  }
 
exception Filled

module Internal = struct
  let reorder q =
    let rec aux xs i =
      if i != q.last then 
        begin
          xs.(i)           <- xs.(i + q.first) ;
          xs.(i + q.first) <- None ;
          aux xs (i + 1)
        end
      else 
        xs
    in { q with
         arr   = aux q.arr 0 ;
         first = 0 ;
         last  = q.last - 1
       }
end

let make n = 
  { maxSize = n 
  ; first   = 0
  ; last    = 0
  ; arr     = Array.make n None
  } 

let getSize q = q.last - q.first
let isEmpty q = getSize q == 0

let insert q x =
  let aux xs maxSize x = 
    xs.(maxSize) <- Some(x) ;
    xs
  in if getSize q == q.maxSize then 
    raise Filled
  else 
    { q with 
      arr  = aux q.arr q.last x ; 
      last = q.last + 1
    }
  
let dequeue q =
  if getSize q == 0 then ( q , None )
  else 
    begin
      let hd = q.arr.(0) in
      ( 
        Internal.reorder  
          { q with 
            first = 1 ; 
            arr   = begin 
                      q.arr.(0) <- None ; 
                      q.arr 
                    end
          }, 
        hd
      )
    end

let pop q = 
  if q.last == q.first then (q, None)
  else 
    begin
      let tl = q.arr.(q.last - 1) in 
      ({ q with 
         arr  = begin 
                  q.arr.(q.last - 1) <- None ; 
                  q.arr 
                end ;
         last = q.last - 1
       },
       tl
      )
    end