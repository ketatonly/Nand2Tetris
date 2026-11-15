type command = 
| Push of string * int
| Pop of string * int
| Add 
| Sub
| Neg
| EQ
| GT
| LT
| And
| Or
| Not

let clean (line: string) : string option = 
   let rec comment i = 
    if i + 1 >= String.length line then String.length line
    else if line.[i] = '/' && line.[i+1] = '/' then i 
    else comment (i + 1)
in 
let index = comment 0 in
let cleaned = String.sub line 0 index |> String.trim in
if cleaned = "" then None else Some cleaned

let parse (s : string) : command =
    let len = String.length s in
    if len = 0 then failwith "parse: empty"
    else 
        (*split the line by spaces*)
        let parts = String.split_on_char ' ' s in
        match parts with
        | ["push"; segment; str] -> Push (segment, int_of_string str)
        | ["pop"; segment; str] -> Pop (segment, int_of_string str)
        | ["add"] -> Add
        | ["sub"] -> Sub
        | ["neg"] -> Neg
        | ["eq"] -> EQ
        | ["gt"] -> GT
        | ["lt"] -> LT 
        | ["and"] -> And
        | ["or"] -> Or
        | ["not"] -> Not
        | _ -> failwith ("Unknown command: " ^ s)
        

let push_segment base index = 
    Printf.sprintf 
            "@%d\n\
            D=A\n\
            @%s\n\
            A=D+M\n\
            D=M\n\
            @SP\n\
            A=M\n\
            M=D\n\
            @SP\n\
            M=M+1\n" index base

let pop_segment base index = 
    Printf.sprintf 
            "@%d\n\
            D=A\n\
            @%s\n\
            A=D+M\n\
            D=A\n\
            @R13\n\
            M=D\n\
            @SP\n\
            M=M-1\n\
            A=M\n\
            D=M\n\
            @R13\n\
            A=M\n\
            M=D\n" index base


let label_count = ref 0 

let translate (cmd: command) : string = 
    match cmd with
    | Push (segment, index) ->
        (match segment with
        | "constant" -> 
            Printf.sprintf
            "@%d\n\
            D=A\n\
            @SP\n\
            A=M\n\
            M=D\n\
            @SP\n\
            M=M+1\n"
            index
        | "local" -> push_segment "LCL" index
        | "argument" -> push_segment "ARG" index
        | "this" -> push_segment "THIS" index
        | "that" -> push_segment "THAT" index
        | "temp" -> 
            Printf.sprintf
            "@%d\n\
             D=M\n\
             @SP\n\
             A=M\n\
             M=D\n\
             @SP\n\
             M=M+1\n"
            (5 + index)
        | "pointer" -> 
            let base = if index = 0 then "THIS" else "THAT" in
            Printf.sprintf
            "@%s\n\
             D=M\n\
             @SP\n\
             A=M\n\
             M=D\n\
             @SP\n\
             M=M+1\n"
            base
        | "static" ->
            Printf.sprintf
            "@StaticVar.%d\n\
             D=M\n\
             @SP\n\
             A=M\n\
             M=D\n\
             @SP\n\
             M=M+1\n"
            index
        | _ -> failwith "Unknown segment")
    | Pop (segment, index) -> 
        (match segment with
        | "local" -> pop_segment "LCL" index
        | "argument" -> pop_segment "ARG" index
        | "this" -> pop_segment "THIS" index
        | "that" -> pop_segment "THAT" index
        | "temp" ->
            Printf.sprintf
            "@SP\n\
             M=M-1\n\
             A=M\n\
             D=M\n\
             @%d\n\
             M=D\n"
            (5 + index)
        | "pointer" ->
            let base = if index = 0 then "THIS" else "THAT" in
            Printf.sprintf
            "@SP\n\
             M=M-1\n\
             A=M\n\
             D=M\n\
             @%s\n\
             M=D\n"
            base
        | "static" ->
            Printf.sprintf
            "@SP\n\
             M=M-1\n\
             A=M\n\
             D=M\n\
             @StaticVar.%d\n\
             M=D\n"
            index
        | _ -> failwith "Unknown segment")
    | Add -> 
        "@SP\n\
        M=M-1\n\
        A=M\n\
        D=M\n\
        A=A-1\n\
        M=D+M\n"
    | Sub -> 
        "@SP\n\
        M=M-1\n\
        A=M\n\
        D=M\n\
        A=A-1\n\
        M=M-D\n"
    | Neg ->
        "@SP\n\
        A=M-1\n\
        M=-M\n"
        | EQ ->
        incr label_count;
        let true_label = Printf.sprintf "EQ_TRUE_%d" !label_count in
        let end_label = Printf.sprintf "EQ_END_%d" !label_count in
        Printf.sprintf 
        "@SP\n\
         M=M-1\n\
         A=M\n\
         D=M\n\
         A=A-1\n\
         D=M-D\n\
         @%s\n\
         D;JEQ\n\
         @SP\n\
         A=M-1\n\
         M=0\n\
         @%s\n\
         0;JMP\n\
         (%s)\n\
         @SP\n\
         A=M-1\n\
         M=-1\n\
         (%s)\n"
        true_label end_label true_label end_label

    | GT ->
        incr label_count;
        let true_label = Printf.sprintf "GT_TRUE_%d" !label_count in
        let end_label = Printf.sprintf "GT_END_%d" !label_count in
        Printf.sprintf 
        "@SP\n\
         M=M-1\n\
         A=M\n\
         D=M\n\
         A=A-1\n\
         D=M-D\n\
         @%s\n\
         D;JGT\n\
         @SP\n\
         A=M-1\n\
         M=0\n\
         @%s\n\
         0;JMP\n\
         (%s)\n\
         @SP\n\
         A=M-1\n\
         M=-1\n\
         (%s)\n"
        true_label end_label true_label end_label

    | LT ->
        incr label_count;
        let true_label = Printf.sprintf "LT_TRUE_%d" !label_count in
        let end_label = Printf.sprintf "LT_END_%d" !label_count in
        Printf.sprintf 
        "@SP\n\
         M=M-1\n\
         A=M\n\
         D=M\n\
         A=A-1\n\
         D=M-D\n\
         @%s\n\
         D;JLT\n\
         @SP\n\
         A=M-1\n\
         M=0\n\
         @%s\n\
         0;JMP\n\
         (%s)\n\
         @SP\n\
         A=M-1\n\
         M=-1\n\
         (%s)\n"
        true_label end_label true_label end_label
    | And -> 
        "@SP\n\
        M=M-1\n\
        A=M\n\
        D=M\n\
        A=A-1\n\
        M=D&M\n"
    | Or ->
        "@SP\n\
        M=M-1\n\
        A=M\n\
        D=M\n\
        A=A-1\n\
        M=D|M\n"
    | Not ->
        "@SP\n\
        A=M-1\n\
        M=!M\n"


let file (src: string) : unit = 
    let dst =
        if Filename.check_suffix src ".vm"
        then Filename.chop_suffix src ".vm" ^ ".asm"
        else src ^ ".asm"
    in
    let ic = open_in src in
    let buf = Buffer.create 4096 in

    let rec loop () =
        match input_line ic with
        | line -> 
            (match clean line with
            | None -> ()
            | Some s -> 
                let cmd = parse s in
                let asm = translate cmd in
                Buffer.add_string buf asm);
            loop ()
        | exception End_of_file -> ()
    in

    loop ();
    close_in ic;

    let oc = open_out dst in
    output_string oc (Buffer.contents buf);
    close_out oc

let () =
 if Array.length Sys.argv < 2 then begin
    Printf.eprintf "usage: %s Prog.vm\n" Sys.argv.(0);
    exit 1
 end;
 file Sys.argv.(1)
