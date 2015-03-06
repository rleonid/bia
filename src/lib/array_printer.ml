module type Array_element = sig
  type t
  val width : t -> int
  val printer : Format.formatter -> t -> unit
end

module Int_Ae = struct
  type t = int
  let width n =
    let rec width_sub a n =
      if n = 0 then a
      else width_sub (a + 1) ((n - n mod 10) / 10)
    in
    if n <= 0 then width_sub 1 (-1 * n)
    else width_sub 0 n

  let printer = Format.pp_print_int

end

module Aligned_array_printer (Ae : Array_element) : sig

  val printer_full : ?flush:bool -> int array -> int ->
                          Format.formatter -> Ae.t array -> unit
  val printer      :      Format.formatter -> Ae.t array -> unit

end = struct

  let printer_full ?(flush=true) w m frmttr a =
    let n = Array.length a in
    Format.pp_open_box frmttr 0;
    Format.pp_print_string frmttr "[|";
    for i = 0 to n - 1 do
      for j = 1 to m - w.(i) + 1 do
        Format.pp_print_char frmttr ' ';
      done;
      Ae.printer frmttr a.(i);
      Format.pp_print_char frmttr ';';
    done;
    Format.pp_print_string frmttr "|]";
    Format.pp_close_box frmttr ();
    if flush then
      Format.pp_print_flush frmttr ()

  let printer frmttr a =
    let w = Array.map Ae.width a in
    let m = Array.fold_left max 0 w in
    printer_full ~flush:true w m frmttr a

end

module Aligned_array_array_printer (Ae : Array_element) : sig

  val printer      :      Format.formatter -> Ae.t array array -> unit

end = struct

  let widths = Array.map Ae.width
  let mwidth = Array.fold_left max 0

  module Local_array_printer = Aligned_array_printer (Ae)

  let printer frmttr a =
    Format.pp_open_vbox frmttr 0;
    Format.pp_print_string frmttr "[|";
    Format.pp_print_newline frmttr ();
    let ws = Array.map widths a in
    let ms = mwidth (Array.map mwidth ws) in
    let n  = Array.length a in
    for i = 0 to n - 2 do
      Local_array_printer.printer_full ~flush:false ws.(i) ms frmttr a.(i);
      Format.pp_print_string frmttr ";";
      Format.pp_print_newline frmttr ()
    done;
    Local_array_printer.printer_full ~flush:false ws.(n-1) ms frmttr a.(n-1);
    Format.pp_print_string frmttr "|]";
    Format.pp_close_box frmttr ();
    Format.pp_print_flush frmttr ()

end

module Int_array_printer = Aligned_array_printer (Int_Ae)
module Int_array_array_printer = Aligned_array_array_printer (Int_Ae)

(*
#install_printer Int_array_printer.printer ;;
#install_printer Int_array_array_printer.printer ;;
*)
