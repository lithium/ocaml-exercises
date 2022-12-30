
let remaining_args = ref []
let invert_pattern = ref false

let usage = "minigrep [-v] <pattern> [file ...]"
let speclist = [
    ("-v", Arg.Set invert_pattern, "Invert the pattern match");
]


let read_lines ic ~pattern:p = 
    let rec build_list l = match input_line ic with
        | line -> 
            let matches = match Str.search_forward p line 0 with
                | exception Not_found -> false
                | _ -> true 
            in
            build_list (if matches <> !invert_pattern then line :: l else l)
        | exception End_of_file -> List.rev l 
    in
    build_list []


let read_files ics ~pattern:p =
    let rec read_file lines = function
        | [] -> lines
        | ic :: t -> read_file ((read_lines ic ~pattern:p) :: lines) t 
    in
    read_file [] ics


let grep ~pattern:p files = 
    let all_ics = match files with
        | [] | ["-"] -> [stdin]
        | l -> List.map open_in l
    in
    let all_lines = List.flatten (read_files (List.rev all_ics) ~pattern:p) in 
    List.map print_endline all_lines
    ;;


Arg.parse 
    speclist
    (fun f -> remaining_args := f :: !remaining_args) 
    usage
    ;;


match List.rev !remaining_args with
    | [] -> begin 
        Arg.usage speclist usage;
        exit 1
    end
    | pattern :: files -> 
        let p = match Str.regexp pattern with
            | exception Failure s -> begin print_endline s; exit 1 end
            | _ as p -> p
        in 
        grep ~pattern:p files
    ;;
