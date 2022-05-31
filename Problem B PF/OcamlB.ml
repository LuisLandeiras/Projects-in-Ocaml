type 't arvore =
  N of 't arvore * 't * 't arvore * int
  | Folha;;
  
let altura = function
  | Folha -> 0
  | N (_,_,_,h) -> h

let node l v r = N(l,v,r,1 + max (altura l) (altura r))

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


let rec add x = function
  | Folha ->
  N (Folha, x, Folha, 1)
  | N (l, v, r, _) as t ->
  let c = compare x v in
  if c = 0 then t
  else if c < 0 then balance (add x l) v r
  else balance l v (add x r)

let rec path t number =
  match t with
  | Folha -> []
  | N (l, v, r, _) ->
    if v = number then [v]
    else if number < v then v :: path l number
    else v :: path r number;;

let g = ref (node Folha 1 Folha) in

g := add 3 !g;
g := add 5 !g;
g := add 4 !g;
g := add 6 !g;

let list1 = path !g 6 in
let list2 = path !g 1 in

let aux = ref 0 in

let mutacao list1 list2 = 
  for i = 0 to (List.length list1 - 1) do
    if (List.nth list1 i = List.nth list2 i) then
      aux := !aux + 1
    else
      aux := !aux
  done;
  print_int (List.nth list1 (!aux - 1))



