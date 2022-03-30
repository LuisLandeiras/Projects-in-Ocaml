let count = ref 0
let count1 = ref 0
let k = ref 0

let rec s n =
  count := !count + 1;
  match n with
    0. -> 1.
  | 1. -> 2.
  |_  -> (((6. *. (n -. 2.) +. 9.) /. (n +. 1.)) *. (s(n -. 1.))) -. ((n -. 2.) /. (n +. 1.)) *. ((s(n -. 2.)))

  let aux n = 
    int_of_float(s(float_of_int (n)))
  

let rec s1 n =
  count1 := !count1 + 1;
  k := 0;
  match n with
    0 -> 1
  | 1 -> 2
  | 2 -> 3 * s1(n-1)
  |_  -> 
    for i=1 to n-2 do 
      k:=s1(i) * s1(n-i-1) + !k
    done;
    3 * s1(n-1) + !k

open Z

let  rec bigS n =
  match n with
    0 -> 1
  | 1 -> 2
  |_  -> (((6 * (n - 2) + 9) / (n + 1)) * (bigS(n - 1))) - ((n - 2) / (n + 1)) * ((bigS(n - 2)))

let () =
for z=0 to 20 do
  print_string "s-> ";
  print_int (aux z);
  print_string " count ->";
  print_int !count;
  print_newline ();
  count := 0;
  print_string "s1-> ";
  print_int (s1 z);
  print_string " count ->";
  print_int !count1;
  print_newline();
  print_newline();
  count1 := 0
  
done;
print_string "Big S: ";
print_int (bigS 40);
print_newline()
