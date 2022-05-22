type 'a avl = E | N of 'a avl'a * 'a avl * int


let altura = function
 | E -> 0
 | N (,,,h) -> h
 

 let node l v r = N (l,v,r, 1 + max (altura l) (altura r))

 let balance l v r =
  let hl = altura l in
  let hr = altura r in
   if hl > hr + 1 then begin 
        match l with
        | N (ll, lv, lr, ) when altura ll >= altura lr ->
        node ll lv (node lr v r)
        | N (ll, lv, N (lrl, lrv, lrr, ),) ->
        node (node ll lv lrl) lrv (node lrr v r)
        |  -> assert false
    end 
  else if hr > hl + 1 then 
    begin match r with
   | N (rl, rv, rr, ) when altura rr >= altura rl ->
  node (node l v rl) rv rr
  | N (N(rll, rlv, rlr, ), rv, rr, ) ->
  node (node l v rll) rlv (node rlr rv rr)
  |  -> assert false
  end 
else 
  node l v r
  let rec add x = function
  | E -> N (E, x, E, 1)
  | N (l, v, r, ) as t ->
     let c = compare x v in
     if c = 0 then t
     else if c < 0 then balance (add x l) v r
     else balance l v (add x r)


let rec mem x = function
| Empty ->
false
| Node (l, v, r) ->
  let c = Ord.compare x v in
   c = 0 || if c < 0 then mem x l else mem x6 let balance l v r =
    let hl = altura l in
    let hr = altura r in
    if hl > hr + 1 then begin (* o problema = à esquerda*)
    match l with
    | N (ll, lv, lr, _) when altura ll >= altura lr ->
  
    node ll lv (node lr v r)
    
    | N (ll, lv, N (lrl, lrv, lrr, ),) ->
     (* caso precisemos de uma dupla rotação *)
     node (node ll lv lrl) lrv (node lrr v r)
    
    | _ ->

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
    node l v r r


