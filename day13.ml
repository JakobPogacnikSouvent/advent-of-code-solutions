let dvojcek s =
    let x = String.split_on_char ',' s |> List.map String.trim in
    (List.hd x |> int_of_string, List.tl x |> List.hd |> int_of_string)

let format input =
    String.split_on_char '\n' input |> List.map String.trim |> List.map dvojcek

let fold_along_x n list =
    let rec f acc =
    function
    | [] -> acc
    | x :: xs ->
        if fst x > n then
            f (( (n - fst x) |> float_of_int |> abs_float |> int_of_float |> (-) n, snd x) :: acc) xs
        else if fst x < n then
            f (x :: acc) xs
        else
            f acc xs
    in
    f [] list

let fold_along_y n list =
    let rec f acc =
    function
    | [] -> acc
    | x :: xs ->
        if snd x > n then
            f ((fst x, (n - snd x) |> float_of_int |> abs_float |> int_of_float |> (-) n) :: acc) xs
        else if snd x < n then
            f (x :: acc) xs
        else
            f acc xs
    in
    f [] list

let remove_duplicates list =
    let rec in_list x =
    function
    | [] -> false
    | y :: ys -> if y = x then true else in_list x ys
    in
    let rec f acc =
    function
    | [] -> acc
    | x :: xs ->
        if in_list x acc then f acc xs else f (x :: acc) (xs)
    in
    f [] list

let rec print_list (l : (int * int) list) =
    match l with
    | [] -> ()
    | x :: xs -> 
        print_int (fst x);
        print_string ",";
        print_int (snd x);
        print_string "\n";
        print_list xs

let naloga1 vsebina_datoteke =
    format vsebina_datoteke |> fold_along_x 655 |> remove_duplicates |> List.length |> string_of_int

let naloga2 vsebina_datoteke =
    format vsebina_datoteke |> 
    fold_along_x 655 |>
    fold_along_y 447 |>
    fold_along_x 327 |>
    fold_along_y 223 |>
    fold_along_x 163 |>
    fold_along_y 111 |>
    fold_along_x 81 |>
    fold_along_y 55 |>
    fold_along_x 40 |>
    fold_along_y 27 |>
    fold_along_y 13 |>
    fold_along_y 6 |>
    remove_duplicates |> print_list;
    ""

(* I just popped points into excell and ploted them *)


(* 
fold along x=655
fold along y=447
fold along x=327
fold along y=223
fold along x=163
fold along y=111
fold along x=81
fold along y=55
fold along x=40
fold along y=27
fold along y=13
fold along y=6
*)

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
    let vsebina_datoteke = preberi_datoteko "data/day_13.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day13/answer_1.out" odgovor1;
    izpisi_datoteko "day13/answer_2.out" odgovor2
