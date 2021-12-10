let f a b =
    let rec f acc i =
    match i with
    | 0 -> acc
    | _ -> 
        match acc with
            | [] -> f (1 :: []) i-1
            | x :: xs -> f ((x+1) :: x :: xs)