let format input =
    String.split_on_char ' ' input

let count list =
    let rec f cnt =
    function
    | [] -> cnt
    | x :: xs ->
        let l = String.length x in
        if l = 2 || l = 4 || l = 3 || l = 7 then
        f (cnt+1) xs
        else
        f cnt xs
    in
    f 0 list

let naloga1 vsebina_datoteke =
    format vsebina_datoteke |> count |> string_of_int
    
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
    let vsebina_datoteke = preberi_datoteko "data/day_8.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day8/answer_1.out" odgovor1;
    izpisi_datoteko "day8/answer_2.out" odgovor2
