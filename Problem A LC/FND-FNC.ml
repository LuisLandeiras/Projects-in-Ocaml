(* CabeÃ§alho a incluir *)

type formula =
  | Lit of char
  | Neg of char
  | Conj of formula * formula
  | Disj of formula * formula

let rec compare_formula f_1 f_2 =
  match (f_2, f_1) with
  | Lit c_1, Lit c_2 | Neg c_1, Neg c_2 -> Char.compare c_1 c_2
  | Lit c_1, Neg c_2 when c_1 = c_2 -> -1
  | Neg c_1, Lit c_2 when c_1 = c_2 -> 1
  | (Lit c_1 | Neg c_1), (Lit c_2 | Neg c_2) -> Char.compare c_1 c_2
  | (Lit _ | Neg _), (Disj _ | Conj _) -> -1
  | (Disj _ | Conj _), (Lit _ | Neg _) -> 1
  | Conj (f_1_1, f_1_2), Conj (f_2_1, f_2_2)
  | Disj (f_1_1, f_1_2), Disj (f_2_1, f_2_2) ->
      let c = compare_formula f_1_1 f_2_1 in
      if c = 0 then compare_formula f_1_2 f_2_2 else c
  | Conj _, Disj _ | Disj _, Conj _ -> 0

let rec normalize_conjs acc f_1 f_2 =
  match (f_1, f_2) with
  | Conj (f_1_1, f_1_2), Conj (f_2_1, f_2_2) ->
      normalize_conjs (normalize_conjs acc f_1_1 f_1_2) f_2_1 f_2_2
  | (Lit _ | Neg _ | Disj _), Conj (f_1', f_2') ->
      normalize_conjs (normalize_formula f_1 :: acc) f_1' f_2'
  | Conj (f_1', f_2'), (Lit _ | Neg _ | Disj _) ->
      normalize_formula f_2 :: normalize_conjs acc f_1' f_2'
  | _ -> normalize_formula f_2 :: normalize_formula f_1 :: acc

and normalize_disjs acc f_1 f_2 =
  match (f_1, f_2) with
  | Disj (f_1_1, f_1_2), Disj (f_2_1, f_2_2) ->
      normalize_disjs (normalize_disjs acc f_1_1 f_1_2) f_2_1 f_2_2
  | (Lit _ | Neg _ | Conj _), Disj (f_1', f_2') ->
      normalize_disjs (normalize_formula f_1 :: acc) f_1' f_2'
  | Disj (f_1', f_2'), (Lit _ | Neg _ | Conj _) ->
      normalize_formula f_2 :: normalize_disjs acc f_1' f_2'
  | _ -> normalize_formula f_2 :: normalize_formula f_1 :: acc

and normalize_formula = function
  | (Lit _ | Neg _) as f -> f
  | Conj (f_1, f_2) -> (
      match normalize_conjs [] f_1 f_2 |> List.sort compare_formula with
      | x :: xs -> List.fold_left (fun f acc -> Conj (acc, f)) x xs
      | _ -> assert false)
  | Disj (f_1, f_2) -> (
      match normalize_disjs [] f_1 f_2 |> List.sort compare_formula with
      | x :: xs -> List.fold_left (fun f acc -> Disj (acc, f)) x xs
      | _ -> assert false)

exception Malformed

let normalize_disjs f =
  let rec aux acc = function
    | (Lit _ | Neg _ | Conj _) as f -> f :: acc
    | Disj (((Lit _ | Neg _ | Conj _) as f_1), f_2) -> aux (f_1 :: acc) f_2
    | Disj (Disj _, _) -> raise Malformed
  in
  aux [] f |> List.rev

let normalize_conjs f =
  let rec aux acc = function
    | (Lit _ | Neg _ | Disj _) as f -> f :: acc
    | Conj (((Lit _ | Neg _ | Disj _) as f_1), f_2) -> aux (f_1 :: acc) f_2
    | Conj (Conj _, _) -> raise Malformed
  in
  aux [] f |> List.rev

let string_of_formula =
  let rec aux conj disj f = function
    | Lit c -> f (Char.escaped c)
    | Neg c -> f ("!" ^ Char.escaped c)
    | Conj (f_1, f_2) ->
        aux true false
          (fun s_1 ->
            aux true false
              (fun s_2 ->
                f
                  (if conj then s_1 ^ " & " ^ s_2
                  else "(" ^ s_1 ^ " & " ^ s_2 ^ ")"))
              f_2)
          f_1
    | Disj (f_1, f_2) ->
        aux false true
          (fun s_1 ->
            aux false true
              (fun s_2 ->
                f
                  (if disj then s_1 ^ " | " ^ s_2
                  else "(" ^ s_1 ^ " | " ^ s_2 ^ ")"))
              f_2)
          f_1
  in
  aux false false (fun x -> x)

let print_formula f = normalize_formula f |> string_of_formula |> print_endline

(* Escreva a soluÃ§Ã£o do problema a seguir *)