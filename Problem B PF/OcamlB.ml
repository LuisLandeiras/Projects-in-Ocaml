(*
  Trabalho realizado por:
  Luís Alves 46107
  Ana Pires 45723
*)

type 't arvore =
  N of 't arvore * 't * 't arvore * int
  | Folha;;

let altura = function
  | Folha -> 0
  | N (_,_,_,h) -> h

let node l v r = N(l,v,r,1 + max (altura l) (altura r))

(*Função retirada do pdf disponibilizado no Teams*)
let balance l v r =
  let hl = altura l in
  let hr = altura r in
  if hl > hr + 1 then begin (* o problema = à esquerda*)
    match l with
    | N (ll, lv, lr, _) when altura ll >= altura lr ->
    (* caso de uma simples rotação *)
    node ll lv (node lr v r)
    | N (ll, lv, N (lrl, lrv, lrr, _),_) ->
    (* caso precisemos de uma dupla rotação *)
    node (node ll lv lrl) lrv (node lrr v r)
    | _ ->
    (* situação "impossível" mas que temosde considerar *)
    assert false
    end else if hr > hl + 1 then begin (* caso simétrico *)
    match r with
    | N (rl, rv, rr, _) when altura rr >= altura rl ->
      node (node l v rl) rv rr
    | N (N(rll, rlv, rlr, _), rv, rr, _) ->
      node (node l v rll) rlv (node rlr rv rr)
    | _ ->
    assert false
    end else (* caso em que não há rotações por fazer *)
    node l v r

(*Função retirada do pdf disponibilizado no Teams*)
let rec add x = function
  | Folha ->
  N (Folha, x, Folha, 1)
  | N (l, v, r, _) as t ->
  let c = compare x v in
  if c = 0 then t
  else if c < 0 then balance (add x l) v r
  else balance l v (add x r)

(*Guarda o caminho para um elemento pretendido da árvore*)
let rec path t number =
  match t with
  | Folha -> []
  | N (l, v, r, _) ->
    if v = number then [v]
    else if number < v then v :: path l number
    else v :: path r number;;

let aux = ref 0 in

(*Procura a mutação mais próxima e comum entre dois elementos selecionados*)
let mutacao tree number1 number2 =
  if (List.length (path tree number1)) >= (List.length (path tree number2)) then
    (
      for i = 0 to (List.length (path tree number2) - 1) do
        if List.nth (path (tree) number1) i = List.nth (path (tree) number2) i then
          aux := List.nth (path (tree) number1) i
        else
          aux := !aux
      done;
    )
  else
    (
      for i = 0 to (List.length (path tree number1) - 1) do
        if List.nth (path (tree) number1) i = List.nth (path (tree) number2) i then
          aux := List.nth (path (tree) number1) i
        else
          aux := !aux
      done;
    );
    print_int (!aux); print_newline() in

let lista_arvores = ref [] in

(*Procura um elemento da árvore e retorna true se existir false se não existir*)
let rec procura arvore value =
  match arvore with
  | Folha -> false
  | N (left, elemento, right, _) ->
    if elemento = value then
      true
    else if value < elemento then
      procura left value
    else
      procura right value in

let () =
  let forest = read_int() in
  for i = 1 to forest do
    let treesize = read_int () in
    let first = read_int() in
    let tree = ref (node Folha first Folha) in
    for i = 0 to treesize - 2 do
      let number = read_int () in
      tree := add number !tree;
    done;
    lista_arvores := !tree :: !lista_arvores;
  done;
  let b,a = Scanf.sscanf (read_line ()) " %d %d" (fun a b -> a,b) in
  for i = 1 to ((List.length !lista_arvores)) do
    if (procura (List.nth !lista_arvores (List.length !lista_arvores - i)) a) && (procura (List.nth !lista_arvores (List.length !lista_arvores - i)) b) then
      mutacao (List.nth !lista_arvores (List.length !lista_arvores - i)) a b
    else
      aux := !aux;
  done in
  if(!aux = 0) then
   (print_string "NO"; print_newline())
  else
    aux := !aux;