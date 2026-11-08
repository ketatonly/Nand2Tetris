(* ============================================================
   Hack Assembler in OCaml
   Output: Prog.hack 

   Structure (mirrors the book's four modules):
     - Parser       : clean lines, parse into a [command] variant
     - Code         : mnemonic -> bit-string lookups
     - SymbolTable  : Hashtbl.t  (string -> int)
     - Main driver  : two passes + output
   ============================================================ *)

(* ---------- 1. Types ---------- *)

(* A parsed command. Label is a pseudo-command (no code emitted). *)
type command =
  | A of string                         (* @xxx : xxx is a number or symbol *)
  | C of string option * string * string option
                                        (* dest = comp ; jump  *)
  | Label of string                     (* (xxx) *)


(* ---------- 2. Parser ---------- *)

(* Remove comments (// ... end of line) and surrounding whitespace.
   Return None if the line is now empty (blank or comment-only). *)
let clean_line (line : string) : string option =
  let rec find_comment i =
    if i + 1 >= String.length line then String.length line
    else if line.[i] = '/' && line.[i+1] = '/' then i
    else find_comment (i + 1)
  in
  let end_ = find_comment 0 in
  let stripped = String.sub line 0 end_ |> String.trim in
  if stripped = "" then None else Some stripped

(* Parse one cleaned line into a [command]. *)
let parse (s : string) : command =
  let n = String.length s in
  if n = 0 then failwith "parse: empty"
  else if s.[0] = '@' then
    A (String.sub s 1 (n - 1))
  else if s.[0] = '(' && s.[n-1] = ')' then
    Label (String.sub s 1 (n - 2))
  else begin
    (* C-instruction:  [dest=]comp[;jump] *)
    let jump, before_jump =
      match String.index_opt s ';' with
      | None -> (None, s)
      | Some i ->
          (Some (String.sub s (i+1) (n - i - 1)),
           String.sub s 0 i)
    in
    let dest, comp =
      match String.index_opt before_jump '=' with
      | None -> (None, before_jump)
      | Some i ->
          (Some (String.sub before_jump 0 i),
           String.sub before_jump (i+1)
              (String.length before_jump - i - 1))
    in
    let tr = String.trim in
    let dest = Option.map tr dest in
    let comp = tr comp in
    let jump = Option.map tr jump in
    C (dest, comp, jump)
  end


(* ---------- 3. Code: mnemonic -> bits ---------- *)

(* comp field: returns the 7 bits (a c1 c2 c3 c4 c5 c6). *)
let comp_bits (c : string) : string =
  match c with
  | "0"   -> "0101010"
  | "1"   -> "0111111"
  | "-1"  -> "0111010"
  | "D"   -> "0001100"
  | "A"   -> "0110000"
  | "M"   -> "1110000"
  | "!D"  -> "0001101"
  | "!A"  -> "0110001"
  | "!M"  -> "1110001"
  | "-D"  -> "0001111"
  | "-A"  -> "0110011"
  | "-M"  -> "1110011"
  | "D+1" -> "0011111"
  | "A+1" -> "0110111"
  | "M+1" -> "1110111"
  | "D-1" -> "0001110"
  | "A-1" -> "0110010"
  | "M-1" -> "1110010"
  | "D+A" -> "0000010"
  | "D+M" -> "1000010"
  | "D-A" -> "0010011"
  | "D-M" -> "1010011"
  | "A-D" -> "0000111"
  | "M-D" -> "1000111"
  | "D&A" -> "0000000"
  | "D&M" -> "1000000"
  | "D|A" -> "0010101"
  | "D|M" -> "1010101"
  | _     -> failwith (Printf.sprintf "unknown comp: %s" c)

(* dest field: 3 bits, in order A D M (d1 d2 d3). *)
let dest_bits (d : string option) : string =
  match d with
  | None       -> "000"
  | Some "M"   -> "001"
  | Some "D"   -> "010"
  | Some "MD"  -> "011"
  | Some "DM"  -> "011"       (* accepted alternative *)
  | Some "A"   -> "100"
  | Some "AM"  -> "101"
  | Some "AD"  -> "110"
  | Some "AMD" -> "111"
  | Some "ADM" -> "111"       (* accepted alternative *)
  | Some x     -> failwith (Printf.sprintf "unknown dest: %s" x)

(* jump field: 3 bits. *)
let jump_bits (j : string option) : string =
  match j with
  | None       -> "000"
  | Some "JGT" -> "001"
  | Some "JEQ" -> "010"
  | Some "JGE" -> "011"
  | Some "JLT" -> "100"
  | Some "JNE" -> "101"
  | Some "JLE" -> "110"
  | Some "JMP" -> "111"
  | Some x     -> failwith (Printf.sprintf "unknown jump: %s" x)

(* Assemble a single C-instruction into 16 bits. *)
let assemble_c dest comp jump =
  "111" ^ comp_bits comp ^ dest_bits dest ^ jump_bits jump

(* Convert an integer into a 15-character binary string. *)
let to_bin15 (v : int) : string =
  let buf = Bytes.make 15 '0' in
  let rec go i x =
    if i >= 0 then begin
      Bytes.set buf i (if x land 1 = 1 then '1' else '0');
      go (i - 1) (x lsr 1)
    end
  in
  go 14 v;
  Bytes.to_string buf

(* Assemble an A-instruction from a numeric value. *)
let assemble_a (v : int) : string =
  "0" ^ to_bin15 v


(* ---------- 4. Symbol table ---------- *)

(* Predefined symbols: R0..R15, SP, LCL, ARG, THIS, THAT, SCREEN, KBD. *)
let predefined : (string * int) list =
  let regs = List.init 16 (fun i -> (Printf.sprintf "R%d" i, i)) in
  regs @ [
    ("SP",     0);
    ("LCL",    1);
    ("ARG",    2);
    ("THIS",   3);
    ("THAT",   4);
    ("SCREEN", 16384);
    ("KBD",    24576);
  ]

let new_symbol_table () : (string, int) Hashtbl.t =
  let t = Hashtbl.create 64 in
  List.iter (fun (k, v) -> Hashtbl.replace t k v) predefined;
  t


(* ---------- 5. Main driver: two passes ---------- *)

(* Read all lines, clean them, parse them. *)
let load_program (filename : string) : command list =
  let ic = open_in filename in
  let rec loop acc =
    match input_line ic with
    | line ->
        (match clean_line line with
         | None -> loop acc
         | Some s -> loop (parse s :: acc))
    | exception End_of_file -> List.rev acc
  in
  let cmds = loop [] in
  close_in ic;
  cmds

(* ----- Pass 1: record every label's ROM address. ----- *)
let first_pass (table : (string, int) Hashtbl.t) (cmds : command list) : unit =
  let rom = ref 0 in
  List.iter (fun cmd ->
    match cmd with
    | Label name ->
        (* a label points to the address of the NEXT instruction;
           no code is emitted, so do NOT advance rom. *)
        Hashtbl.replace table name !rom
    | A _ | C _ ->
        incr rom
  ) cmds

(* ----- Pass 2: emit binary. ----- *)
let second_pass (table : (string, int) Hashtbl.t) (cmds : command list) : string =
  let ram = ref 16 in                  (* next free variable slot *)
  let buf = Buffer.create 4096 in
  let emit bits =
    Buffer.add_string buf bits;
    Buffer.add_char buf '\n'
  in
  let resolve_a (tok : string) : string =
    (* if tok is a number, use it directly; else look up / allocate *)
    match int_of_string_opt tok with
    | Some v -> assemble_a v
    | None ->
        (match Hashtbl.find_opt table tok with
         | Some addr -> assemble_a addr
         | None ->
             (* new variable *)
             let addr = !ram in
             incr ram;
             Hashtbl.replace table tok addr;
             assemble_a addr)
  in
  List.iter (fun cmd ->
    match cmd with
    | Label _ -> ()                     (* no code *)
    | A tok   -> emit (resolve_a tok)
    | C (d, c, j) -> emit (assemble_c d c j)
  ) cmds;
  Buffer.contents buf

(* ---------- 6. Entry point ---------- *)

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "usage: %s Prog.asm\n" Sys.argv.(0);
    exit 1
  end;
  let src = Sys.argv.(1) in
  (* Replace .asm with .hack (or append .hack if no extension). *)
  let dst =
    if Filename.check_suffix src ".asm"
    then Filename.chop_suffix src ".asm" ^ ".hack"
    else src ^ ".hack"
  in
  let cmds  = load_program src in
  let table = new_symbol_table () in
  first_pass table cmds;
  let output = second_pass table cmds in
  let oc = open_out dst in
  output_string oc output;
  close_out oc