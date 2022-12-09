

let print_position (tx, ty) (hx, hy) =
    let matr = Array.make_matrix (y_max+1) (x_max + 1) '.' in
    matr.(y_max - ty).(tx) <- 'T';
    matr.(y_max - hy).(hx) <- 'H';
    print_endline (String.make (x_max+1) '-');
    Array.iter (fun row -> Array.iter print_char row; print_newline()) matr;
    print_endline (String.make (x_max+1) '-');;
