let format input =
    let rec f (acc : int list) (cnt : int) (s : string) =
    match cnt with
    | x when x < 0 -> acc
    | _ -> f (acc @ [(s.[cnt] |> String.make 1 |> int_of_string)]) (cnt-1) s
    in
    String.split_on_char '\n' input |> List.map (f [] 99) |> List.map Array.of_list |> Array.of_list

let int_of_bool b =
    if b then 1 else 0

let naloga1 vsebina_datoteke =
    let data = format vsebina_datoteke in
    let cnt = ref 0 in
    for i = 0 to 99 do
        for j = 0 to 99 do
            let x = data.(i).(j) in
            if i = 0 then
                if j = 0 then
                    
                    cnt := !cnt + ((x+1) * (int_of_bool (x < data.(i+1).(j) && x < data.(i).(j+1))))
                else if j = 99 then
                    cnt := !cnt + ((x+1) * (int_of_bool (x < data.(i+1).(j) && x < data.(i).(j-1))))
                else
                    cnt := !cnt + ((x+1) * (int_of_bool (x < data.(i+1).(j) && x < data.(i).(j+1) && x < data.(i).(j-1))))

            else if i = 99 then
                if j = 0 then
                    cnt := !cnt + ((x+1) * (int_of_bool (x < data.(i-1).(j) && x < data.(i).(j+1))))
                else if j = 99 then
                    cnt := !cnt + ((x+1) * (int_of_bool (x < data.(i-1).(j) && x < data.(i).(j-1))))
                else
                    cnt := !cnt + ((x+1) * (int_of_bool (x < data.(i-1).(j) && x < data.(i).(j+1) && x < data.(i).(j-1))))
            
            else
                if j = 0 then
                    cnt := !cnt + ((x+1) * (int_of_bool (x < data.(i+1).(j) && x < data.(i-1).(j) && x < data.(i).(j+1))))
                else if j = 99 then
                    cnt := !cnt + ((x+1) * (int_of_bool (x < data.(i+1).(j) && x < data.(i-1).(j) && x < data.(i).(j-1))))
                else
                    cnt := !cnt + ((x+1) * (int_of_bool (x < data.(i+1).(j) && x < data.(i-1).(j) && x < data.(i).(j+1) && x < data.(i).(j-1))))
        done
    done;
    string_of_int !cnt
    


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
    let vsebina_datoteke = preberi_datoteko "data/day_9.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day9/answer_1.out" odgovor1;
    izpisi_datoteko "day9/answer_2.out" odgovor2
