let int_width n =
  let rec width_sub a n =
    if n = 0 then a
    else width_sub (a + 1) ((n - n mod 10) / 10)
  in
  if n <= 0 then width_sub 1 (-1 * n)
  else width_sub 0 n

let widths = Array.map int_width
let mwidth = Array.fold_left max 0

let int_array_printer_widths w m frmttr a =
  let n = Array.length a in
  Format.pp_open_box frmttr 0;
  Format.pp_print_string frmttr "[|";
  for i = 0 to n - 1 do
    for j = 1 to m - w.(i) + 1 do
      Format.pp_print_char frmttr ' ';
    done;
    Format.pp_print_int frmttr a.(i);
    Format.pp_print_char frmttr ';';
  done;
  Format.pp_print_string frmttr "|]";
  Format.pp_close_box frmttr ()

let int_array_printer frmttr a =
  let w = widths a in
  let m = mwidth w in
  int_array_printer_widths w m frmttr a;
  Format.pp_print_flush frmttr ()

let int_array_array_printer frmttr a =
  Format.pp_open_vbox frmttr 0;
  Format.pp_print_newline frmttr ();
  let ws = Array.map widths a in
  let ms = mwidth (Array.map mwidth ws) in
  for i = 0 to Array.length a - 1 do
    int_array_printer_widths ws.(i) ms frmttr a.(i);
    Format.pp_print_newline frmttr ()
  done;
  Format.pp_close_box frmttr ();
  Format.pp_print_flush frmttr ()

(* 
#install_printer int_array_printer ;;
#install_printer int_array_array_printer ;;
*)
