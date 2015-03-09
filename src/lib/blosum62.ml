(* http://www.ncbi.nlm.nih.gov/Class/FieldGuide/BLOSUM62.txt *)


let blosum62 x y = 
    let amino_acids = 
      [|  'A';  'R';  'N';  'D';  'C';  'Q';  'E';  'G';  'H';  'I';  'L';  'K'
       ;  'M';  'F';  'P';  'S';  'T';  'W';  'Y';  'V';  'B';  'Z';   |]
    in
    let aa_assoc = 
        amino_acids 
        |> Array.mapi (fun i c -> (c,i)) 
        |> Array.to_list
    in
    let idx c = try List.assoc c aa_assoc with Not_found -> 22 in
    let mat = 
    [|
      [|  4; -1; -2; -2;  0; -1; -1;  0; -2; -1; -1; -1; -1; -2; -1;  1;  0; -3; -2;  0; -2; -1;  0; -4 |];
      [| -1;  5;  0; -2; -3;  1;  0; -2;  0; -3; -2;  2; -1; -3; -2; -1; -1; -3; -2; -3; -1;  0; -1; -4 |];
      [| -2;  0;  6;  1; -3;  0;  0;  0;  1; -3; -3;  0; -2; -3; -2;  1;  0; -4; -2; -3;  3;  0; -1; -4 |];
      [| -2; -2;  1;  6; -3;  0;  2; -1; -1; -3; -4; -1; -3; -3; -1;  0; -1; -4; -3; -3;  4;  1; -1; -4 |];
      [|  0; -3; -3; -3;  9; -3; -4; -3; -3; -1; -1; -3; -1; -2; -3; -1; -1; -2; -2; -1; -3; -3; -2; -4 |];
      [| -1;  1;  0;  0; -3;  5;  2; -2;  0; -3; -2;  1;  0; -3; -1;  0; -1; -2; -1; -2;  0;  3; -1; -4 |];
      [| -1;  0;  0;  2; -4;  2;  5; -2;  0; -3; -3;  1; -2; -3; -1;  0; -1; -3; -2; -2;  1;  4; -1; -4 |];
      [|  0; -2;  0; -1; -3; -2; -2;  6; -2; -4; -4; -2; -3; -3; -2;  0; -2; -2; -3; -3; -1; -2; -1; -4 |];
      [| -2;  0;  1; -1; -3;  0;  0; -2;  8; -3; -3; -1; -2; -1; -2; -1; -2; -2;  2; -3;  0;  0; -1; -4 |];
      [| -1; -3; -3; -3; -1; -3; -3; -4; -3;  4;  2; -3;  1;  0; -3; -2; -1; -3; -1;  3; -3; -3; -1; -4 |];
      [| -1; -2; -3; -4; -1; -2; -3; -4; -3;  2;  4; -2;  2;  0; -3; -2; -1; -2; -1;  1; -4; -3; -1; -4 |];
      [| -1;  2;  0; -1; -3;  1;  1; -2; -1; -3; -2;  5; -1; -3; -1;  0; -1; -3; -2; -2;  0;  1; -1; -4 |];
      [| -1; -1; -2; -3; -1;  0; -2; -3; -2;  1;  2; -1;  5;  0; -2; -1; -1; -1; -1;  1; -3; -1; -1; -4 |];
      [| -2; -3; -3; -3; -2; -3; -3; -3; -1;  0;  0; -3;  0;  6; -4; -2; -2;  1;  3; -1; -3; -3; -1; -4 |];
      [| -1; -2; -2; -1; -3; -1; -1; -2; -2; -3; -3; -1; -2; -4;  7; -1; -1; -4; -3; -2; -2; -1; -2; -4 |];
      [|  1; -1;  1;  0; -1;  0;  0;  0; -1; -2; -2;  0; -1; -2; -1;  4;  1; -3; -2; -2;  0;  0;  0; -4 |];
      [|  0; -1;  0; -1; -1; -1; -1; -2; -2; -1; -1; -1; -1; -2; -1;  1;  5; -2; -2;  0; -1; -1;  0; -4 |];
      [| -3; -3; -4; -4; -2; -2; -3; -2; -2; -3; -2; -3; -1;  1; -4; -3; -2; 11;  2; -3; -4; -3; -2; -4 |];
      [| -2; -2; -2; -3; -2; -1; -2; -3;  2; -1; -1; -2; -1;  3; -3; -2; -2;  2;  7; -1; -3; -2; -1; -4 |];
      [|  0; -3; -3; -3; -1; -2; -2; -3; -3;  3;  1; -2;  1; -1; -2; -2;  0; -3; -1;  4; -3; -2; -1; -4 |];
      [| -2; -1;  3;  4; -3;  0;  1; -1;  0; -3; -4;  0; -3; -3; -2;  0; -1; -4; -3; -3;  4;  1; -1; -4 |];
      [| -1;  0;  0;  1; -3;  3;  4; -2;  0; -3; -3;  1; -1; -3; -1;  0; -1; -3; -2; -2;  1;  4; -1; -4 |];
      [|  0; -1; -1; -1; -2; -1; -1; -1; -1; -1; -1; -1; -1; -1; -2;  0;  0; -2; -1; -1; -1; -1; -1; -4 |];
      [| -4; -4; -4; -4; -4; -4; -4; -4; -4; -4; -4; -4; -4; -4; -4; -4; -4; -4; -4; -4; -4; -4; -4;  1 |] 
    |]
    in
    mat.(idx x).(idx y)
