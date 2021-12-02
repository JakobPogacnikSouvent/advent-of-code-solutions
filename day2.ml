let f double =
    let dvojcek = String.split_on_char ' ' double in
    match dvojcek with
    | a :: b :: [] -> (a, int_of_string b)
    | _ -> failwith "Error in f"

let format input =
 String.split_on_char '\n' input |> List.map String.trim |> List.map f

let rec solve forward_acc depth_acc = 
    function
    | [] -> forward_acc * depth_acc
    | double :: xs ->
        match double with
        | ("forward", x) -> solve (forward_acc + x) depth_acc xs
        | ("down", x) -> solve forward_acc (depth_acc + x) xs
        | ("up", x) -> solve forward_acc (depth_acc - x) xs

let rec solve' forward_acc depth_acc aim_acc = 
    function
    | [] -> forward_acc * depth_acc
    | double :: xs ->
        match double with
        | ("forward", x) -> solve' (forward_acc + x) (depth_acc + (aim_acc * x)) aim_acc xs
        | ("down", x) -> solve' forward_acc depth_acc (aim_acc + x) xs
        | ("up", x) -> solve' forward_acc depth_acc (aim_acc - x) xs

let naloga1 vsebina_datoteke =
    format vsebina_datoteke |> solve 0 0 |> string_of_int


let naloga2 vsebina_datoteke =
    format vsebina_datoteke |> solve' 0 0 0 |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "data/day_2.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day2/answer_1.out" odgovor1;
    izpisi_datoteko "day2/answer_2.out" odgovor2
