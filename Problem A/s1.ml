let count = ref 0
let count1 = ref 0
let k = ref 0

let rec s n =
  count := !count + 1;
  match n with
    0 -> 1
  | 1 -> 2
  |_  -> int_of_float((((6. *. (float_of_int (n) -. 2.) +. 9.) /. (float_of_int (n) +. 1.)) *. (float_of_int (s(n-1)))) -. (((float_of_int (n) -. 2.) /. (float_of_int (n) +. 1.)) *. (float_of_int (s(n-2)))))

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

let () =
for z=0 to 20 do
  print_string "s-> ";
  print_int (s z);
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
done