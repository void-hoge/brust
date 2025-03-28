use std::env;
mod brainfuck;
use crate::brainfuck::Brainfuck;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("usage: {} <filename>", args[0]);
    }
    let code = std::fs::read_to_string(&args[1]).expect("failed to read the file");
    let mut prog = Brainfuck::parse(&code);
    prog = Brainfuck::compress(prog);
    prog = Brainfuck::fold_reset_loops(prog);
    prog = Brainfuck::fold_move_loops(prog);
    prog = Brainfuck::fold_skip_loops(prog);
    let flat = Brainfuck::flatten(prog);
    let mut bfi = Brainfuck::new();
    bfi.run(flat);
}
