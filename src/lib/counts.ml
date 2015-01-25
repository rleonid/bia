let acc_upper (a,c,g,t) = function 
    'A' -> (a+1,c,g,t) 
  | 'C' -> (a,c+1,g,t) 
  | 'G' -> (a,c,g+1,t) 
  | 'T' -> (a,c,g,t+1) 
  | x -> 
      begin 
        Printf.eprintf "not nucleotide %c" x;
        (a,c,g,t)
      end ;;
