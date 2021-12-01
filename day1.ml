let rec f acc previus =
    function
    | [] -> acc
    | x :: xs when x > previus -> f (acc + 1) x xs
    | x :: xs -> f acc x xs

let rec sum_by_3 acc =
    function
    | x :: y :: z :: tail -> sum_by_3 (acc @ [(x+y+z)]) (y :: z :: tail)
    | _ -> acc

let format input =
 String.split_on_char '\n' input |> List.map String.trim  |> List.map int_of_string

let naloga1 vsebina_datoteke =
    f 0 99999999 (format vsebina_datoteke) |> string_of_int

let naloga2 vsebina_datoteke =
    sum_by_3 [] (format vsebina_datoteke) |> f 0 99999999 |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "data/day_1.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day1/answer_1.out" odgovor1;
    izpisi_datoteko "day1/answer_2.out" odgovor2
