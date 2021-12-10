let f =
    let x = ref 0 in
    for i = 0 to 9 do
        for j = 0 to 9 do
            x := !x + 1
        done
    done;
    string_of_int !x