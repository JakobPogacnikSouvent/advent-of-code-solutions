let format input =
 String.split_on_char '\n' input |> List.map String.trim

type bingo_space = {
    number : int;
    drawn : bool;
}

type bingo_board = {
     row1 : bingo_space list;
     row2 : bingo_space list;
     row3 : bingo_space list;
     row4 : bingo_space list;
     row5 : bingo_space list;
}

let list_index_at index =
    let rec f cnt =
    function
    | [] -> failwith "Length exception in list_index_at"
    | x :: xs when cnt = index -> x
    | _ :: xs -> f (cnt+1) xs
    in
    f 0

let check_for_bingo (board : bingo_board) =
    let rec check_rows (rows : bingo_space list list) =
        let rec check_row (acc : int) (row : bingo_space list) =
            match row with
            | [] -> acc (* If all drawn return their sum *)
            | x :: xs when x.drawn -> check_row (acc + x.number) xs
            | x :: xs when not x.drawn -> 0
        in
        match rows with
        | [] -> 0 (* No bingo *)
        | x :: xs ->
            let bingo_value = check_row x in
            match bingo_value with
            | 0 -> check_rows xs
            | y -> y
    in
    let rec get_columns (acc : bingo_space list list) (column_index : int) =
        match column_index with
        | 5 -> acc
        | _ -> get_columns ([list_index_at column_index board.row1; list_index_at column_index board.row2; list_index_at column_index board.row3; list_index_at column_index board.row4; list_index_at column_index board.row5] :: acc) (i+1)
    in
    let get_diagonals =
        [
            [list_index_at 0 board.row5; list_index_at 1 board.row4; list_index_at 2 board.row3; list_index_at 3 board.row2; list_index_at 4 board.row1];
            [list_index_at 0 board.row1; list_index_at 1 board.row2; list_index_at 2 board.row3; list_index_at 3 board.row4; list_index_at 4 board.row5]
        ]
    in
    check_rows ([row1; row2; row3; row4; row5] @ (get_columns [] 0) @ get_diagonals)


let str_list_to_board (list : int list list) =
    {
    row1=(list_index_at 0 list |> List.map int_of_string);
    row2=(list_index_at 1 list |> List.map int_of_string);
    row3=(list_index_at 2 list |> List.map int_of_string);
    row4=(list_index_at 3 list |> List.map int_of_string);
    row5=(list_index_at 4 list |> List.map int_of_string);
    }


let format_boards (rows : str list) =
    let rec f (boards_acc : bingo_board list) (board_row_acc : str list) =
    function
    | [] -> (str_list_to_board board_row_acc) ::  boards_acc
    | x :: xs when x = "" -> f ((str_list_to_board board_row_acc) ::  boards_acc) []
    | x :: xs -> f boards_acc (board_row_acc @ x)
    in
    f [] [] rows


let naloga1 vsebina_datoteke =
    let data = format vsebina_datoteke in
    let numbers = List.hd data |> String.split_on_char ',' in
    let boards = format_boards List.tl data |> format_boards in
    ""
    
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
    let vsebina_datoteke = preberi_datoteko "data/day_4.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day4/answer_1.out" odgovor1;
    izpisi_datoteko "day4/answer_2.out" odgovor2
