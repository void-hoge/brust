use clap::Parser;
mod brainfuck;
use crate::brainfuck::Brainfuck;

#[derive(Parser, Debug)]
#[command(name = "brust")]
#[command(about = "Brainfuck interpreter")]
struct Args {
    #[arg(short, long, default_value_t = 65536)]
    length: usize,

    #[arg(short, long, action = clap::ArgAction::SetTrue)]
    flush: bool,

    #[arg(value_name = "FILE")]
    file: String,
}

fn main() {
    let args = Args::parse();
    let code = std::fs::read_to_string(&args.file).expect("Failed to read the file.");
    let mut prog = Brainfuck::parse(&code);
    prog = Brainfuck::compress(prog);
    prog = Brainfuck::fold_reset_loops(prog);
    prog = Brainfuck::fold_set_idioms(prog);
    prog = Brainfuck::fold_move_loops(prog);
    prog = Brainfuck::fold_skip_loops(prog);
    let flat = Brainfuck::flatten(prog);
    let mut bfi = Brainfuck::new(args.length);

    if args.flush {
        bfi.run::<true>(flat);
    } else {
        bfi.run::<false>(flat);
    }
}
