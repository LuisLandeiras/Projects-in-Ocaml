(*Endereços de memória onde ficam guardados o número de chamadas das funções iniciais*)
let count = ref 0
let count1 = ref 0

(*Endereço de memória utilizado para guardar o calculo do somatória da função s1*)
let k = ref 0

(*Calculo da sequência de Schroder utilizando a formula que não tem somatórios*)
let rec s n =
  count := !count + 1;
  match n with
    0. -> 1.
  | 1. -> 2.
  | _  -> (((6. *. (n -. 2.) +. 9.) /. (n +. 1.)) *. (s(n -. 1.))) -. ((n -. 2.) /. (n +. 1.)) *. ((s(n -. 2.)))

  let saux n = int_of_float(s(float_of_int (n)))
 
(*Calculo da sequência de Schroder utilizando a fórmula que utiliza somatórios*)
(*O ciclo for é utilizado para fazer o calculo do sumatório*)
let rec s1 n =
  count1 := !count1 + 1;
  k := 0;
  match n with
    0 -> 1
  | 1 -> 2
  | 2 -> 3 * s1(n-1)
  | _ -> 
    (for i=1 to n-2 do 
      k:=s1(i) * s1(n-i-1) + !k
    done;
    3 * s1(n-1) + !k)

open Z
(*Endereços de memória utilizados para armazenar os primeiros 2 valores da sucessão*)
let antAnterior = ref (Z.of_float 1.)
let anterior = ref (Z.of_float 2.)
(*Endereço de memória utilizado para armazenar os restantes valores da sucessão*)
let c = ref (Z.of_float 0.)

(*Função responsável por calcular os valores entre 0 e 10000*)
(*
O ciclo for é utilizado para calcular os valores a cima de 2,
como a função necessita somente de 2 valores anterios para esse calculo
é utilizado os endereços antAnterior e anterios para armazenar esses valores
*)
let bigs n =
    if n = 0 then 
      !antAnterior
    else 
      if n = 1 then 
        !anterior
        else 
          (for i = 2 to n do
            c := ( ((Z.of_float 6. * Z.of_float ( float_of_int i ) - Z.of_float 3.) * !anterior) - ((Z.of_float ( float_of_int i ) - Z.of_float 2.) * !antAnterior ) ) / (Z.of_float ( float_of_int i ) + Z.of_float 1.);
            antAnterior := !anterior;
            anterior := !c
          done;
          !c)

open Scanf
let x,y = Scanf.sscanf(read_line()) " %d %d " (fun x y -> x,y) 

let () =

print_int (s1 x);
print_string " ";
print_int !count1;
count1 := 0;

print_newline();

print_int (saux x);
print_string " ";
print_int !count;
count := 0;


print_newline();
print_string (Z.to_string (bigs y));
print_newline()
