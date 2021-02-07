type 'v t = ('v Deque.t) ref

let make n = ref (Deque.make n)
let getSize q = Deque.getSize (!q)
let isEmpty q = Deque.isEmpty (!q)
let insert q x = 
  q := Deque.insert (!q) x

let dequeue q =
  let (q', hd) = Deque.dequeue (!q)
  in q := q' ;
     hd

let pop q =
  let (q', tl) = Deque.pop (!q)
  in q := q' ;
     tl