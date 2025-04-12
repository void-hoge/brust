use std::io::{self, Read, Write};
use std::collections::BTreeMap;
use std::iter::Peekable;

fn gcd(mut a: u64, mut b: u64) -> u64 {
    while b > 0 {
        (a, b) = (b, a % b);
    }
    a
}

#[derive(Debug, PartialEq)]
pub enum InstType {
    ShiftInc,
    Output,
    Input,
    Skip,
    Set,
    Mulzero,
    Mul,
    Open,
    Close,
}

#[derive(Debug)]
pub struct Inst {
    cmd: InstType,
    arg: i32,
    inc: u8,
    delta: i16,
}

#[derive(Debug)]
pub enum IR {
    Inc(i32),
    Shift(i32),
    Output,
    Input,
    Reset,
    Set(i32),
    Move(Vec<(i32, i32)>),
    Block(Vec<IR>),
    Skip(i32),
}

pub struct Brainfuck {
    ip: usize,
    dp: usize,
    memory: Vec<u8>,
}

#[allow(dead_code)]
impl Brainfuck {
    pub fn new(length: usize) -> Self {
        Self {
            ip: 0,
            dp: 0,
            memory: vec![0u8; length],
        }
    }

    pub fn parse(code: &str) -> Vec<IR> {
        fn parse_block<I: Iterator<Item = char>>(iter: &mut I, in_block: bool) -> Result<Vec<IR>, String> {
            let mut prog = Vec::new();
            while let Some(ch) = iter.next() {
                match ch {
                    '+' => prog.push(IR::Inc(1)),
                    '-' => prog.push(IR::Inc(-1)),
                    '>' => prog.push(IR::Shift(1)),
                    '<' => prog.push(IR::Shift(-1)),
                    '.' => prog.push(IR::Output),
                    ',' => prog.push(IR::Input),
                    '[' => {
                        let block = parse_block(iter, true)?;
                        prog.push(IR::Block(block));
                    },
                    ']' => {
                        if in_block {
                            return Ok(prog);
                        } else {
                            return Err("Unmached ]".to_string());
                        }
                    },
                    _ => continue,
                }
            }
            if in_block {
                Err("Unmached [".to_string())
            } else {
                Ok(prog)
            }
        }
        parse_block(&mut code.chars(), false).unwrap()
    }

    pub fn compress(prog: Vec<IR>) -> Vec<IR> {
        let mut compressed = Vec::new();
        let mut iter = prog.into_iter().peekable();
        while let Some(inst) = iter.next() {
            match inst {
                IR::Inc(n) => {
                    let mut count = n;
                    while let Some(IR::Inc(m)) = iter.peek() {
                        count += m;
                        iter.next();
                    }
                    if count != 0 {
                        compressed.push(IR::Inc(count));
                    }
                },
                IR::Shift(n) => {
                    let mut count = n;
                    while let Some(IR::Shift(m)) = iter.peek() {
                        count += m;
                        iter.next();
                    }
                    if count != 0 {
                        compressed.push(IR::Shift(count));
                    }
                },
                IR::Block(block) => {
                    let block = Brainfuck::compress(block);
                    compressed.push(IR::Block(block));
                },
                other => {
                    compressed.push(other);
                },
            }
        }
        compressed
    }

    pub fn fold_reset_loops(prog: Vec<IR>) -> Vec<IR> {
        prog
            .into_iter()
            .map(|inst| match inst {
                IR::Block(block) => {
                    let folded = Brainfuck::fold_reset_loops(block);
                    if folded.len() == 1 {
                        match folded[0] {
                            IR::Inc(inc) => {
                                if gcd(inc as u8 as u64, 256) == 1 {
                                    return IR::Reset;
                                }
                            },
                            _ => {}
                        }
                    }
                    IR::Block(folded)
                },
                other => other,
            })
            .collect()
    }

    pub fn fold_set_idioms(prog: Vec<IR>) -> Vec<IR> {
        let mut folded = Vec::new();
        let mut iter = prog.into_iter().peekable();

        while let Some(inst) = iter.next() {
            match inst {
                IR::Reset => {
                    if let Some(IR::Inc(n)) = iter.peek() {
                        let value = *n;
                        iter.next();
                        folded.push(IR::Set(value));
                    } else {
                        folded.push(IR::Set(0));
                    }
                },
                IR::Block(block) => {
                    folded.push(IR::Block(Brainfuck::fold_set_idioms(block)));
                },
                other => {
                    folded.push(other);
                }
            }
        }

        folded
    }

    pub fn fold_move_loops(prog: Vec<IR>) -> Vec<IR> {
        prog
            .into_iter()
            .map(|inst| {
                match inst {
                    IR::Block(block) => {
                        let folded = Brainfuck::fold_move_loops(block);
                        if folded.iter().all(|x| matches!(x, IR::Inc(_) | IR::Shift(_))) {
                            let mut arg: i32 = 0;
                            let mut changes: BTreeMap<i32, i32> = BTreeMap::new();
                            changes.insert(0, 0);
                            for ins in &folded {
                                match ins {
                                    IR::Inc(n) => {
                                        let entry = changes.entry(arg).or_insert(0);
                                        *entry += *n;
                                    },
                                    IR::Shift(n) => {
                                        arg += *n;
                                    },
                                    _ => {}
                                }
                            }
                            if arg == 0 {
                                if let Some(-1) = changes.get(&0) {
                                    let payload: Vec<(i32, i32)> = changes.into_iter()
                                        .filter(|&(arg, weight)| arg != 0 && weight != 0)
                                        .collect();
                                    return IR::Move(payload);
                                }
                            }
                        }
                        IR::Block(folded)
                    },
                    other => other,
                }
            }).collect()
    }

    pub fn fold_skip_loops(prog: Vec<IR>) -> Vec<IR> {
        prog
            .into_iter()
            .map(|inst| match inst {
                IR::Block(block) => {
                    let folded = Brainfuck::fold_skip_loops(block);
                    if folded.len() == 1 {
                        if let IR::Shift(n) = folded[0] {
                            return IR::Skip(n);
                        }
                    }
                    IR::Block(folded)
                },
                other => other,
            })
            .collect()
    }

    pub fn flatten(prog: Vec<IR>) -> Vec<Inst> {
        fn pick_inc<I: Iterator<Item = IR>>(iter: &mut Peekable<I>) -> u8 {
            if let Some(IR::Inc(value)) = iter.peek() {
                let value = *value;
                iter.next();
                return value as u8;
            }
            0
        }
        fn pick_shift<I: Iterator<Item = IR>>(iter: &mut Peekable<I>) -> i16 {
            if let Some(IR::Shift(delta)) = iter.peek() {
                let delta = *delta;
                if i16::MIN as i32 <= delta && delta <= i16::MAX as i32 {
                    iter.next();
                    return delta as i16;
                }
            }
            0
        }
        fn unmatched_flatten<I: Iterator<Item = IR>>(iter: &mut Peekable<I>) -> Vec<Inst> {
            let mut unmatched = Vec::new();
            while let Some(inst) = iter.next() {
                match inst {
                    IR::Inc(inc) => {
                        let inc = inc as u8;
                        let delta = pick_shift(iter);
                        unmatched.push(Inst{cmd: InstType::ShiftInc, arg: 0, inc: inc, delta: delta});
                    },
                    IR::Shift(amount) => {
                        let amount = amount;
                        let inc = pick_inc(iter);
                        let delta = pick_shift(iter);
                        unmatched.push(Inst{cmd: InstType::ShiftInc, arg: amount, inc: inc, delta: delta});
                    },
                    IR::Output => {
                        let inc = pick_inc(iter);
                        let delta = pick_shift(iter);
                        unmatched.push(Inst{cmd: InstType::Output, arg: 0, inc: inc, delta: delta});
                    },
                    IR::Input => {
                        let inc = pick_inc(iter);
                        let delta = pick_shift(iter);
                        unmatched.push(Inst{cmd: InstType::Input, arg: 0, inc: inc, delta: delta});
                    },
                    IR::Reset => {
                        let inc = pick_inc(iter);
                        let delta = pick_shift(iter);
                        unmatched.push(Inst{cmd: InstType::Set, arg: 0, inc: inc, delta: delta});
                    },
                    IR::Set(value) => {
                        let delta = pick_shift(iter);
                        unmatched.push(Inst{cmd: InstType::Set, arg: 0, inc: value as u8, delta: delta});
                    },
                    IR::Move(targets) => {
                        let delta = pick_shift(iter);
                        if let Some((last, rest)) = targets.split_last() {
                            for &(arg, weight) in rest {
                                unmatched.push(Inst{cmd: InstType::Mul, arg: arg, inc: weight as u8, delta: 0});
                            }
                            unmatched.push(Inst{cmd: InstType::Mulzero, arg: last.0, inc: last.1 as u8, delta: delta});
                        } else {
                            unreachable!("Num of targets of move must at least one.");
                        }
                    },
                    IR::Skip(arg) => {
                        let inc = pick_inc(iter);
                        let delta = pick_shift(iter);
                        unmatched.push(Inst{cmd: InstType::Skip, arg: arg, inc: inc, delta: delta});
                    },
                    IR::Block(block) => {
                        let mut blockiter = block.into_iter().peekable();
                        let inc = pick_inc(&mut blockiter);
                        let delta = pick_shift(&mut blockiter);
                        let flatten = unmatched_flatten(&mut blockiter);
                        unmatched.push(Inst{cmd: InstType::Open, arg: 0, inc: inc, delta: delta});
                        unmatched.extend(flatten);
                        unmatched.push(Inst{cmd: InstType::Close, arg:0, inc: inc, delta: delta});
                    },
                }
            }
            unmatched
        }

        let mut iter = prog.into_iter().peekable();
        let mut flat = unmatched_flatten(&mut iter);

        let mut stack = Vec::new();
        for idx in 0..flat.len() {
            match flat[idx].cmd {
                InstType::Open => {
                    stack.push(idx);
                },
                InstType::Close => {
                    let open = stack.pop().unwrap();
                    flat[open].arg = idx as i32;
                    flat[idx].arg = open as i32;
                },
                _ => {},
            }
        }
        flat
    }

    #[inline(always)]
    pub fn run<const FLUSH: bool>(&mut self, prog: Vec<Inst>) {
        while self.ip < prog.len() {
            let Inst{cmd, arg, inc, delta} = &prog[self.ip];
            if *cmd == InstType::ShiftInc {
                self.dp = (self.dp as isize + *arg as isize) as usize;
                self.memory[self.dp] = self.memory[self.dp].wrapping_add(*inc);
                self.dp = (self.dp as isize + *delta as isize) as usize;
            } else if *cmd == InstType::Output {
                print!("{}", self.memory[self.dp] as char);
                self.memory[self.dp] = self.memory[self.dp].wrapping_add(*inc);
                self.dp = (self.dp as isize + *delta as isize) as usize;
                if FLUSH {
                    io::stdout().flush().unwrap();
                }
            } else if *cmd == InstType::Input {
                let mut buf = [0];
                match io::stdin().read_exact(&mut buf) {
                    Ok(()) => {
                        self.memory[self.dp] = buf[0];
                    },
                    Err(_) => {
                        self.memory[self.dp] = 0u8;
                    },
                }
                self.memory[self.dp] = self.memory[self.dp].wrapping_add(*inc);
                self.dp = (self.dp as isize + *delta as isize) as usize;
            } else if *cmd == InstType::Skip {
                while self.memory[self.dp] != 0 {
                    self.dp = (self.dp as isize + *arg as isize) as usize;
                }
                self.memory[self.dp] = self.memory[self.dp].wrapping_add(*inc);
                self.dp = (self.dp as isize + *delta as isize) as usize;
            } else if *cmd == InstType::Set {
                self.memory[self.dp] = *inc;
                self.dp = (self.dp as isize + *delta as isize) as usize;
            } else if *cmd == InstType::Mulzero {
                if self.memory[self.dp] != 0 {
                    let val = self.memory[self.dp];
                    let pos = (self.dp as isize + *arg as isize) as usize;
                    self.memory[pos] = self.memory[pos].wrapping_add(val.wrapping_mul(*inc));
                    self.memory[self.dp] = 0;
                }
                self.dp = (self.dp as isize + *delta as isize) as usize;
            } else if *cmd == InstType::Mul {
                if self.memory[self.dp] != 0 {
                    let val = self.memory[self.dp];
                    let pos = (self.dp as isize + *arg as isize) as usize;
                    self.memory[pos] = self.memory[pos].wrapping_add(val.wrapping_mul(*inc));
                }
                // Because intended to be used only in the [->+<] idiom, delta is always 0 (so the next line will always ignored).
                self.dp = (self.dp as isize + *delta as isize) as usize;
            } else if *cmd == InstType::Open {
                if self.memory[self.dp] == 0 {
                    self.ip = *arg as usize;
                } else {
                    self.memory[self.dp] = self.memory[self.dp].wrapping_add(*inc);
                    self.dp = (self.dp as isize + *delta as isize) as usize;
                }
            } else /* if *cmd == InstType::Close */ {
                if self.memory[self.dp] != 0 {
                    self.ip = *arg as usize;
                    self.memory[self.dp] = self.memory[self.dp].wrapping_add(*inc);
                    self.dp = (self.dp as isize + *delta as isize) as usize;
                }
            }
            self.ip += 1;
        }
    }
}
