let _ =
  let tree = Cbtree.add "a" (Cbtree.add "ax" Cbtree.empty) in
  if Cbtree.mem "a" tree then
    print_endline "YUP"

  
  
