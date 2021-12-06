let format input =
 String.split_on_char '\n' input |> List.map String.trim |> List.hd |> String.split_on_char ',' |> List.map int_of_string

type fish = {
    cnt0 : int;
    cnt1 : int;
    cnt2 : int;
    cnt3 : int;
    cnt4 : int;
    cnt5 : int;
    cnt6 : int;
    cnt7 : int;
    cnt8 : int;
}

let add_fish (i : int) (f : fish) =
    match i with
    | 0 -> {cnt0=(f.cnt0+1); cnt1=f.cnt1; cnt2=f.cnt2; cnt3=f.cnt3; cnt4=f.cnt4; cnt5=f.cnt5; cnt6=f.cnt6; cnt7=f.cnt7; cnt8=f.cnt8}
    | 1 -> {cnt0=f.cnt0; cnt1=(f.cnt1+1); cnt2=f.cnt2; cnt3=f.cnt3; cnt4=f.cnt4; cnt5=f.cnt5; cnt6=f.cnt6; cnt7=f.cnt7; cnt8=f.cnt8}
    | 2 -> {cnt0=f.cnt0; cnt1=f.cnt1; cnt2=(f.cnt2+1); cnt3=f.cnt3; cnt4=f.cnt4; cnt5=f.cnt5; cnt6=f.cnt6; cnt7=f.cnt7; cnt8=f.cnt8}
    | 3 -> {cnt0=f.cnt0; cnt1=f.cnt1; cnt2=f.cnt2; cnt3=(f.cnt3+1); cnt4=f.cnt4; cnt5=f.cnt5; cnt6=f.cnt6; cnt7=f.cnt7; cnt8=f.cnt8}
    | 4 -> {cnt0=f.cnt0; cnt1=f.cnt1; cnt2=f.cnt2; cnt3=f.cnt3; cnt4=(f.cnt4+1); cnt5=f.cnt5; cnt6=f.cnt6; cnt7=f.cnt7; cnt8=f.cnt8}
    | 5 -> {cnt0=f.cnt0; cnt1=f.cnt1; cnt2=f.cnt2; cnt3=f.cnt3; cnt4=f.cnt4; cnt5=(f.cnt5+1); cnt6=f.cnt6; cnt7=f.cnt7; cnt8=f.cnt8}
    | 6 -> {cnt0=f.cnt0; cnt1=f.cnt1; cnt2=f.cnt2; cnt3=f.cnt3; cnt4=f.cnt4; cnt5=f.cnt5; cnt6=(f.cnt6+1); cnt7=f.cnt7; cnt8=f.cnt8}
    | 7 -> {cnt0=f.cnt0; cnt1=f.cnt1; cnt2=f.cnt2; cnt3=f.cnt3; cnt4=f.cnt4; cnt5=f.cnt5; cnt6=f.cnt6; cnt7=(f.cnt7+1); cnt8=f.cnt8}
    | 8 -> {cnt0=f.cnt0; cnt1=f.cnt1; cnt2=f.cnt2; cnt3=f.cnt3; cnt4=f.cnt4; cnt5=f.cnt5; cnt6=f.cnt6; cnt7=f.cnt7; cnt8=(f.cnt8+1)}
    | _ -> failwith "Error in add fish"

let count_fish (list : int list) =
    let rec f acc =
    function
    | [] -> acc
    | x :: xs -> f (add_fish x acc) xs
    in
    f {cnt0=0; cnt1=0; cnt2=0; cnt3=0; cnt4=0;cnt5=0;cnt6=0; cnt7=0; cnt8=0} list

let next_day (f : fish) =
    {cnt0=f.cnt1; cnt1=f.cnt2; cnt2=f.cnt3; cnt3=f.cnt4; cnt4=f.cnt5; cnt5=f.cnt6; cnt6=(f.cnt7+f.cnt0); cnt7=f.cnt8; cnt8=f.cnt0}

let get_number_of_fish (f : fish) =
    f.cnt0 + f.cnt1 + f.cnt2 + f.cnt3 + f.cnt4 + f.cnt5 + f.cnt6 + f.cnt7 + f.cnt8

let rec call_n_times n f x =
    match n with
    | y when n <= 0 -> x
    | y when n > 0 -> call_n_times (n-1) f (f x)

let naloga1 vsebina_datoteke =
    let x = format vsebina_datoteke |> count_fish in
    
    call_n_times 80 next_day x |> get_number_of_fish |> string_of_int
    
let naloga2 vsebina_datoteke =
    let x = format vsebina_datoteke |> count_fish in
    
    call_n_times 256 next_day x |> get_number_of_fish |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "data/day_6.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day6/answer_1.out" odgovor1;
    izpisi_datoteko "day6/answer_2.out" odgovor2
