let format input =
 String.split_on_char '\n' input |> List.map String.trim

let numpy_add list list' =
    let rec f acc =
        function
        | ([], []) -> acc
        | (x :: xs, y :: ys) -> f (acc @ [(x + y)]) (xs, ys)
        | _ -> failwith "Function numpy add called on bad variables"
    in
    f [] (list, list')

let split s =
    let rec f acc cnt =
    match cnt with
    | x when x <= 0 -> acc
    | x when x > 0 -> f ((int_of_string (Char.escaped s.[x-1])) :: acc) (x-1)
    | _ -> failwith "Something went wrong in function split"
    in
    f [] (String.length s)

let split' s =
    let rec f acc cnt =
    match cnt with
    | x when x <= 0 -> acc
    | x when x > 0 -> f ((Char.escaped s.[x-1]) :: acc) (x-1)
    | _ -> failwith "Something went wrong in function split'"
    in
    f [] (String.length s)

let count_ones list =
    let rec f acc =
    function
    | [] -> acc
    | x :: xs -> f (numpy_add acc x) xs
    in
    f [0;0;0;0;0;0;0;0;0;0;0;0] list

let more_common list =
    let rec f acc =
    function
    | [] -> acc
    | x :: xs -> f (acc @ [(x/500)]) xs
    in
    f [] list

let less_common list =
    let rec f acc =
    function
    | [] -> acc
    | x :: xs -> f (acc @ [((x/500) + 1) mod 2]) xs
    in
    f [] list

let power x n =
    let x' = float_of_int x and
    n' = float_of_int n in
    x' ** n' |> int_of_float

let to_decimal list =
    let rec f acc exp = 
    function
    | [] -> acc
    | x :: xs when x = 1 -> f (acc + (power 2 exp)) (exp-1) xs
    | x :: xs when x = 0 -> f acc (exp-1) xs (* to avoid 0 ** 0 *)
    | _ -> failwith "to_decimal called on a non decimal number"
    in
    f 0 ((List.length list) - 1) list

let join s list =
    let rec f acc =
    function
    | [] -> acc
    | x :: xs -> f (acc ^ s ^ (string_of_int x)) xs
    in
    f "" list

let join' s list =
    let rec f acc =
    function
    | [] -> acc
    | x :: xs -> f (acc ^ s ^ x) xs
    in
    f "" list

let index_equals index condition =
    let rec f cnt =
    function
    | [] -> failwith "Length exception in index_equals"
    | x :: xs when cnt = index -> x = condition
    | _ :: xs -> f (cnt+1) xs
    in
    f 0

let list_index_at index =
    let rec f cnt =
    function
    | [] -> failwith "Length exception in list_index_at"
    | x :: xs when cnt = index -> x
    | _ :: xs -> f (cnt+1) xs
    in
    f 0

let split_at_index index =
    let rec f i =
    function
    | [] -> failwith "Index out of range"
    | x :: xs when i = index -> xs
    | _ :: xs -> f (i+1) xs
    in
    f 0

let naloga1 vsebina_datoteke =
    let cnt = format vsebina_datoteke |> List.map split |> count_ones in
    string_of_int ((more_common cnt |> to_decimal) * (less_common cnt |> to_decimal))

(* 
let naloga2 vsebina_datoteke =
    let cnt = format vsebina_datoteke |> List.map split in
    let f func list =
        let rec g acc index =
        function
        | [] -> failwith "Something went wrong"
        | x :: [] -> acc ^ (split_at_index (index-1) x |> join "")
        | l -> 
            let i = count_ones l |> func |> list_index_at index |> string_of_int in
            g (acc ^ i) (index+1) (List.filter (index_equals index (int_of_string i)) l)
        in
        g "" 0 list
    in
    ((f less_common cnt) ^ " " ^ (f more_common cnt) ^ " " ^ (count_ones cnt |> join ","))
*)

(*
This is where I realized OCaml is a scumfuck retard language and my time is better used doing literally anything else than writing 6 nested recursions
so after 3 hours of pain and suffering i solved the problem by hand in 5 minutes. In the off chance that anyone is checking this code please tell me
where I went wrong and I hope that trying to make sense of this steaming pile of dogshit is as painfull to you ass it was for me.

PS. Please don't lower my exam score mr. Pretnar
*)


let naloga2 vsebina_datoteke =
    ""

let _ =
    let preberi_datoteko ime_datoteke =
        let chan = open_in_bin ime_datoteke in
        let vsebina = really_input_string chan (in_channel_length chan) in
        close_in chan;
        vsebina
    and izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out_bin ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let vsebina_datoteke = preberi_datoteko "data/day_3.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day3/answer_1.out" odgovor1;
    izpisi_datoteko "day3/answer_2.out" odgovor2
