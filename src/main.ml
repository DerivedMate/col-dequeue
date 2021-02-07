let test_immutable () = 
  let open Deque in
  let que : int t = Deque.make 3 
  in assert (getSize que = 0)      ;
     assert (isEmpty que)          ;

  let que = insert que 1 in
  let que = insert que 2 in
  let que = insert que 3 
  in assert (not (isEmpty que))    ;
     assert (getSize que = 3)      ;

  let (que, x) = pop que
  in assert (x = Some 3)           ;

  let (que, x) = dequeue que
  in assert (x = Some 1)           ;

  let (que, x)           = pop que 
  in assert (x           = Some 2) ;
     assert (getSize que = 0)      ;
  
  let (que, x) = pop que 
  in assert (x = None)             ;
  
  let que = insert que 7 in
  let que = insert que 8 in
  let que = insert que 9 
  in assert (
       begin
        try insert que 10 != que 
        with Deque.Filled -> true
       end
      )

  
let test_mutable () = 
  let open Deque_mutable in
  let que : int t = make 3
  in assert (getSize que = 0) ;
     assert (isEmpty que) ;

     insert que 1 ;
     insert que 2 ; 
     insert que 3 ;

     assert (not (isEmpty que))         ;
     assert (getSize que = 3)           ;
     assert (pop que     = Some 3)      ;
     assert (getSize que = 2)           ;
     assert (!que.last   = 2)           ;
     assert (Array.fold_left 
             Helpers.count_some 
             0 
             (!que.arr)  = getSize que) ;
     assert (dequeue que = Some 1)      ;
     assert (getSize que = 1)           ;
     assert (!que.last   = 1)           ;
     assert (pop que     = Some 2)      ;
     assert (getSize que = 0)           ;
     assert (!que.last   = 0)           ;
     assert (pop que     = None)        ;
     
     insert que 7 ;
     insert que 8 ; 
     insert que 9 ;
     
     (* Since `insert` returns 'unit', 
        it'll evaluate to 'false' 
        if the insertion succeeds
      *)
     assert (
       begin
        try insert que 10 != () 
        with Deque.Filled -> true
       end
      )


let () = 
  test_immutable () ;
  test_mutable () ;
  Js.log "If you see this, everything went fine :)" ;
  ()