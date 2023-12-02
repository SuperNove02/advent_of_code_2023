open Utils


let regex = Str.regexp {|[0-9]\|one\|two\|three\|four\|five\|six\|seven\|eight\|nine\|zero|}
let string_to_int s =
  match s with 
  | "nine" -> 9
  | "eight" -> 8
  | "seven" -> 7
  | "six" -> 6
  | "five" -> 5
  | "four" -> 4
  | "three" -> 3
  | "two" -> 2
  | "one" -> 1
  | "zero" -> 0
  | _ -> failwith "this is not a valid number"

let data = 
  "inputs/1.txt"
  |> read_lines
  (*|> List.map explode*)


let f (a,b) c  = 
  print_char c;
  print_endline "<- c";
  let c = int_of_char c - int_of_char '0'
  in
  if c > 9 then (a,b)
  else
    begin
    print_endline (string_of_int c);
    if a=(-1)
       then (c,c) 
    else (a,c) 
  end

let fold_helper acc (cl:char list) =
  let (a,b) = List.fold_left f  (-1,-1) cl
  in
  let c = a*10+b
  in
  print_endline (string_of_int (c)) ;
  c+acc

  let rec print_list = function 
  [] -> ()
  | e::l -> print_endline e ; print_string " " ; print_list l
let fold_helper_p2 (acc: string list) (sl: string )=
  let r = (get_all_matches regex sl) 
  in  
  print_list r;
  print_endline "" ;
  r @ acc

let () =
  (*
  let d = List.map explode data
  in
  List.fold_left fold_helper 0 d|> dump_int 1
  *)
  let d = data
in let _ = List.fold_left fold_helper_p2 [] d in  print_endline "hallo"
  



  (*
  |> List.fold_left (fun h acc -> acc)  ' 

  |> dump_int 1
*)