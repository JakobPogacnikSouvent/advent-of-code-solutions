let format input =
    String.split_on_char '\n' input |> List.map String.trim

let get_value (s : char) =
    match s with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwith "Something that isn't zaklepaj"

let get_autocomplete_value (s : char) =
    match s with
    | '(' -> 1
    | '[' -> 2
    | '{' -> 3
    | '<' -> 4
    | _ -> failwith "Something that isn't okepaj"


let autocomplete_value (queue : char list) =
    let rec f (acc : int) =
    function
    | [] -> acc
    | x :: xs -> f ((5*acc) + (get_autocomplete_value x)) xs
    in
    f 0 queue

let je_oklepaj (c : char) =
    c = '(' || c = '{' || c = '[' || c = '<'

let pravilno_zaprt (okl : char) (zakl : char) =
    (okl = '(' && zakl = ')') || (okl = '{' && zakl = '}')|| (okl = '[' && zakl = ']') || (okl = '<' && zakl = '>')

let illegal (line : string) =
    let m = String.length line in
    let rec f (queue : char list) (i : int) (s : string) =
    match i with
    | y when y >= m -> 0
    | y ->
        let c = s.[y] in
        if je_oklepaj c then
            f (c :: queue) (y+1) s
        else (* je zaklepaj *)
            match queue with
            | [] -> get_value c
            | x :: xs ->
                if  pravilno_zaprt x c then
                    f xs (y+1) s
                else
                    get_value c
    in
    f [] 0 line

let autocomplete (line : string) =
    let m = String.length line in
    let rec f (queue : char list) (i : int) (s : string) =
    match i with
    | y when y >= m -> autocomplete_value queue
    | y ->
        let c = s.[y] in
        if je_oklepaj c then
            f (c :: queue) (y+1) s
        else (* je zaklepaj *)
            match queue with
            | [] -> get_value c
            | x :: xs ->
                if  pravilno_zaprt x c then
                    f xs (y+1) s
                else
                    get_value c
    in
    f [] 0 line

let filter_f (line : string) =
    if (illegal line) = 0 then Some line else None

let rec print_list (l : int list) =
    match l with
    | [] -> ()
    | x :: xs -> 
        print_int x;
        print_string ",";
        print_list xs

let rec print_list' (l : string list) =
    match l with
    | [] -> ()
    | x :: xs -> 
        print_string x;
        print_string ",";
        print_list' xs

let greater x y =
    if x > y then 1 
    else if y > x then -1 
    else 0

let naloga1 vsebina_datoteke =
    format vsebina_datoteke |> List.map illegal |> List.fold_left (+) 0 |> string_of_int

let naloga2 vsebina_datoteke =
    let x = format vsebina_datoteke |> List.filter_map filter_f |> List.map autocomplete |> List.sort greater in
    string_of_int (Array.of_list x).((List.length x)/2)
    

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
    let vsebina_datoteke = preberi_datoteko "data/day_10.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day10/answer_1.out" odgovor1;
    izpisi_datoteko "day10/answer_2.out" odgovor2
