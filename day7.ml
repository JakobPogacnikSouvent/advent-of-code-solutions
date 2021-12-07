let format input =
 String.split_on_char '\n' input |> List.map String.trim |> List.hd |> String.split_on_char ',' |> List.map int_of_string

let rec s list =
    match list with
    | [] -> 0
    | x :: xs -> x + s xs


let sum_of_n n =
    (n * (n+1)) / 2

let distance x y =
    x-y |> float_of_int |> abs_float |> int_of_float

let fuel (x : int) (list : int list) =
    let rec f acc =
    function
    | [] -> acc
    | y :: ys -> f (acc + distance x y) ys
    in
    f 0 list

let distance' x y =
    x-y |> float_of_int |> abs_float |> int_of_float |> sum_of_n

let fuel' (x : int) (list : int list) =
    let rec f acc =
    function
    | [] -> acc
    | y :: ys -> f (acc + distance' x y) ys
    in
    f 0 list

let fuel_list (list : int list) =
    let rec f acc cnt =
    match cnt with
    | x when x = 2000 -> acc
    | x -> f (fuel x list :: acc) (cnt+1)
    in
    f [] 0


let fuel_list' (list : int list) =
    let rec f acc cnt =
    match cnt with
    | x when x = 2000 -> acc
    | x -> f (fuel' x list :: acc) (cnt+1)
    in
    f [] 0

let find_smallest (list : int list) =
    let rec f smallest =
    function
    | [] -> smallest
    | x :: xs -> 
        if x < smallest then f x xs else f smallest xs
    in
    f 999999999999999 list

let naloga1 vsebina_datoteke =
    format vsebina_datoteke |> fuel_list |> find_smallest |> string_of_int
    
let naloga2 vsebina_datoteke =
    format vsebina_datoteke |> fuel_list' |> find_smallest |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "data/day_7.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day7/answer_1.out" odgovor1;
    izpisi_datoteko "day7/answer_2.out" odgovor2
