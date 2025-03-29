use std::io::{self, Read};
use std::collections::BTreeMap;
use std::iter::Peekable;

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
    offset: i32,
    inc: u8,
    delta: i16,
}

#[derive(Debug)]
pub enum LongInst {
    Inc(i32),
    Shift(i32),
    Output,
    Input,
    Reset,
    Move(Vec<(i32, i32)>),
    Block(Vec<LongInst>),
    Skip(i32),
}

pub struct Brainfuck {
    ip: usize,
    dp: usize,
    memory: Vec<u8>,
}

#[allow(dead_code)]
impl Brainfuck {
    pub fn new() -> Self {
        Self {
            ip: 0,
            dp: 0,
            memory: vec![0u8; 1<<16],
        }
    }

    pub fn parse(code: &str) -> Vec<LongInst> {
        fn parse_block<I: Iterator<Item = char>>(iter: &mut I, in_block: bool) -> Result<Vec<LongInst>, String> {
            let mut prog = Vec::new();
            while let Some(ch) = iter.next() {
                match ch {
                    '+' => prog.push(LongInst::Inc(1)),
                    '-' => prog.push(LongInst::Inc(-1)),
                    '>' => prog.push(LongInst::Shift(1)),
                    '<' => prog.push(LongInst::Shift(-1)),
                    '.' => prog.push(LongInst::Output),
                    ',' => prog.push(LongInst::Input),
                    '[' => {
                        let block = parse_block(iter, true)?;
                        prog.push(LongInst::Block(block));
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

    pub fn compress(prog: Vec<LongInst>) -> Vec<LongInst> {
        let mut compressed = Vec::new();
        let mut iter = prog.into_iter().peekable();
        while let Some(inst) = iter.next() {
            match inst {
                LongInst::Inc(n) => {
                    let mut count = n;
                    while let Some(LongInst::Inc(m)) = iter.peek() {
                        count += m;
                        iter.next();
                    }
                    if count != 0 {
                        compressed.push(LongInst::Inc(count));
                    }
                },
                LongInst::Shift(n) => {
                    let mut count = n;
                    while let Some(LongInst::Shift(m)) = iter.peek() {
                        count += m;
                        iter.next();
                    }
                    if count != 0 {
                        compressed.push(LongInst::Shift(count));
                    }
                },
                LongInst::Block(block) => {
                    let block = Brainfuck::compress(block);
                    compressed.push(LongInst::Block(block));
                },
                other => {
                    compressed.push(other);
                },
            }
        }
        compressed
    }

    pub fn fold_reset_loops(prog: Vec<LongInst>) -> Vec<LongInst> {
        prog
            .into_iter()
            .map(|inst| match inst {
                LongInst::Block(block) => {
                    let folded = Brainfuck::fold_reset_loops(block);
                    if folded.len() == 1 {
                        match folded[0] {
                            LongInst::Inc(1) | LongInst::Inc(-1) => return LongInst::Reset,
                            _ => {}
                        }
                    }
                    LongInst::Block(folded)
                },
                other => other,
            })
            .collect()
    }

    pub fn fold_move_loops(prog: Vec<LongInst>) -> Vec<LongInst> {
        prog
            .into_iter()
            .map(|inst| {
                match inst {
                    LongInst::Block(block) => {
                        let folded = Brainfuck::fold_move_loops(block);
                        if folded.iter().all(|x| matches!(x, LongInst::Inc(_) | LongInst::Shift(_))) {
                            let mut offset: i32 = 0;
                            let mut changes: BTreeMap<i32, i32> = BTreeMap::new();
                            changes.insert(0, 0);
                            for ins in &folded {
                                match ins {
                                    LongInst::Inc(n) => {
                                        let entry = changes.entry(offset).or_insert(0);
                                        *entry += *n;
                                    },
                                    LongInst::Shift(n) => {
                                        offset += *n;
                                    },
                                    _ => {}
                                }
                            }
                            if offset == 0 {
                                if let Some(-1) = changes.get(&0) {
                                    let payload: Vec<(i32, i32)> = changes.into_iter()
                                        .filter(|&(offset, weight)| offset != 0 && weight != 0)
                                        .collect();
                                    return LongInst::Move(payload);
                                }
                            }
                        }
                        LongInst::Block(folded)
                    },
                    other => other,
                }
            }).collect()
    }

    pub fn fold_skip_loops(prog: Vec<LongInst>) -> Vec<LongInst> {
        prog
            .into_iter()
            .map(|inst| match inst {
                LongInst::Block(block) => {
                    let folded = Brainfuck::fold_skip_loops(block);
                    if folded.len() == 1 {
                        if let LongInst::Shift(n) = folded[0] {
                            return LongInst::Skip(n);
                        }
                    }
                    LongInst::Block(folded)
                },
                other => other,
            })
            .collect()
    }

    pub fn flatten(prog: Vec<LongInst>) -> Vec<Inst> {
        fn pick_inc<I: Iterator<Item = LongInst>>(iter: &mut Peekable<I>) -> u8 {
            if let Some(LongInst::Inc(value)) = iter.peek() {
                let value = *value;
                iter.next();
                return value as u8;
            }
            0
        }
        fn pick_shift<I: Iterator<Item = LongInst>>(iter: &mut Peekable<I>) -> i16 {
            if let Some(LongInst::Shift(delta)) = iter.peek() {
                let delta = *delta;
                if i16::MIN as i32 <= delta && delta <= i16::MAX as i32 {
                    iter.next();
                    return delta as i16;
                }
            }
            0
        }
        fn unmatched_flatten<I: Iterator<Item = LongInst>>(iter: &mut Peekable<I>) -> Vec<Inst> {
            let mut unmatched = Vec::new();
            while let Some(inst) = iter.next() {
                match inst {
                    LongInst::Inc(inc) => {
                        let inc = inc as u8;
                        let delta = pick_shift(iter);
                        unmatched.push(Inst{cmd: InstType::ShiftInc, offset: 0, inc: inc, delta: delta});
                    },
                    LongInst::Shift(amount) => {
                        let amount = amount;
                        let inc = pick_inc(iter);
                        let delta = pick_shift(iter);
                        unmatched.push(Inst{cmd: InstType::ShiftInc, offset: amount, inc: inc, delta: delta});
                    },
                    LongInst::Output => {
                        let inc = pick_inc(iter);
                        let delta = pick_shift(iter);
                        unmatched.push(Inst{cmd: InstType::Output, offset: 0, inc: inc, delta: delta});
                    },
                    LongInst::Input => {
                        let inc = pick_inc(iter);
                        let delta = pick_shift(iter);
                        unmatched.push(Inst{cmd: InstType::Input, offset: 0, inc: inc, delta: delta});
                    },
                    LongInst::Reset => {
                        let inc = pick_inc(iter);
                        let delta = pick_shift(iter);
                        unmatched.push(Inst{cmd: InstType::Set, offset: 0, inc: inc, delta: delta});
                    },
                    LongInst::Move(targets) => {
                        let delta = pick_shift(iter);
                        if let Some((last, rest)) = targets.split_last() {
                            for &(offset, weight) in rest {
                                unmatched.push(Inst{cmd: InstType::Mul, offset: offset, inc: weight as u8, delta: 0});
                            }
                            unmatched.push(Inst{cmd: InstType::Mulzero, offset: last.0, inc: last.1 as u8, delta: delta});
                        } else {
                            unreachable!("Num of targets of move must at least one.");
                        }
                    },
                    LongInst::Skip(offset) => {
                        let inc = pick_inc(iter);
                        let delta = pick_shift(iter);
                        unmatched.push(Inst{cmd: InstType::Skip, offset: offset, inc: inc, delta: delta});
                    },
                    LongInst::Block(block) => {
                        let mut blockiter = block.into_iter().peekable();
                        let flatten = unmatched_flatten(&mut blockiter);
                        unmatched.push(Inst{cmd: InstType::Open, offset: 0, inc: 0, delta: 0});
                        unmatched.extend(flatten);
                        unmatched.push(Inst{cmd: InstType::Close, offset:0, inc: 0, delta: 0});
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
                    flat[open].offset = idx as i32;
                    flat[idx].offset = open as i32;
                },
                _ => {},
            }
        }
        flat
    }

    #[inline(always)]
    pub fn run(&mut self, prog: Vec<Inst>) {
        while self.ip < prog.len() {
            let Inst{cmd, offset, inc, delta} = &prog[self.ip];
            match cmd {
                InstType::ShiftInc => {
                    self.dp = (self.dp as isize + *offset as isize) as usize;
                    self.memory[self.dp] = self.memory[self.dp].wrapping_add(*inc);
                },
                InstType::Output => {
                    print!("{}", self.memory[self.dp] as char);
                    self.memory[self.dp] = self.memory[self.dp].wrapping_add(*inc);
                },
                InstType::Input => {
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
                },
                InstType::Skip => {
                    while self.memory[self.dp] != 0 {
                        self.dp = (self.dp as isize + *offset as isize) as usize;
                    }
                    self.memory[self.dp] = self.memory[self.dp].wrapping_add(*inc);
                },
                InstType::Set => {
                    self.memory[self.dp] = *inc;
                },
                InstType::Mulzero => {
                    if self.memory[self.dp] != 0 {
                        let val = self.memory[self.dp];
                        let pos = (self.dp as isize + *offset as isize) as usize;
                        self.memory[pos] = self.memory[pos].wrapping_add(val.wrapping_mul(*inc));
                        self.memory[self.dp] = 0;
                    }
                },
                InstType::Mul => {
                    if self.memory[self.dp] != 0 {
                        let val = self.memory[self.dp];
                        let pos = (self.dp as isize + *offset as isize) as usize;
                        self.memory[pos] = self.memory[pos].wrapping_add(val.wrapping_mul(*inc));
                    }
                },
                InstType::Open => {
                    if self.memory[self.dp] == 0 {
                        self.ip = *offset as usize;
                    }
                },
                InstType::Close => {
                    if self.memory[self.dp] != 0 {
                        self.ip = *offset as usize;
                    }
                },
            }
            self.dp = (self.dp as isize + *delta as isize) as usize;
            self.ip += 1;
        }
    }
}
