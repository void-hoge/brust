use std::io::{self, Read, Write};
use std::collections::BTreeMap;
use std::iter::Peekable;

fn gcd(mut a: u64, mut b: u64) -> u64{
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
    Move(Vec<(i32, i32)>),
    Mul(i32, u8),
    Block(Vec<IR>),
    Skip(i32),
}

pub struct Brainfuck {
    ip: usize,
    dp: usize,
    memory: Vec<u8>,
}

#[derive(Debug)]
pub enum AccessType {
    Inc(u8),
    Set(u8),
    Untracked,
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
                            IR::Inc(1) | IR::Inc(-1) => return IR::Reset,
                            _ => {}
                        }
                    }
                    IR::Block(folded)
                },
                other => other,
            })
            .collect()
    }

    pub fn fold_move_loops(prog: Vec<IR>) -> Vec<IR> {
        prog
            .into_iter()
            .map(|inst| {
                match inst {
                    IR::Block(block) => {
                        let folded = Brainfuck::fold_move_loops(block);
                        if folded.iter().all(|x| matches!(x, IR::Inc(_) | IR::Shift(_))) {
                            let mut delta: i32 = 0;
                            let mut changes: BTreeMap<i32, i32> = BTreeMap::new();
                            changes.insert(0, 0);
                            for ins in &folded {
                                match ins {
                                    IR::Inc(n) => {
                                        let entry = changes.entry(delta).or_insert(0);
                                        *entry += *n;
                                    },
                                    IR::Shift(n) => {
                                        delta += *n;
                                    },
                                    _ => {}
                                }
                            }
                            if delta == 0 {
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

    pub fn optimize_const_loops_(prog: Vec<IR>) -> (Vec<IR>, Option<BTreeMap<i32, AccessType>>) {
        let mut access = BTreeMap::<i32, AccessType>::new();
        access.insert(0, AccessType::Inc(0));
        let mut delta: i32 = 0;
        let mut optimizable = true;
        let mut analyzed: Vec<_> = prog
            .into_iter()
            .map(|inst| {
                match inst {
                    IR::Block(block) => {
                        let (blk_optimized, blk_result) = Brainfuck::optimize_const_loops_(block);
                        if let Some(blk_access) = blk_result {
                            // delta of the block is 0.
                            for (pos, cell) in blk_access {
                                match cell {
                                    AccessType::Set(value) => {
                                        access.insert(delta + pos, AccessType::Set(value));
                                    },
                                    AccessType::Inc(_) | AccessType::Untracked => {
                                        access.insert(delta + pos, AccessType::Untracked);
                                    },
                                }
                            }
                        } else {
                            optimizable = false;
                        }
                        IR::Block(blk_optimized)
                    }
                    IR::Output | IR::Input => {
                        optimizable = false;
                        inst
                    },
                    IR::Inc(inc) => {
                        let entry = access.entry(delta).or_insert(AccessType::Inc(0));
                        match entry {
                            AccessType::Inc(incr) => {
                                *entry = AccessType::Inc(*incr + inc as u8);
                            },
                            AccessType::Set(value) => {
                                *entry = AccessType::Set(*value + inc as u8);
                            },
                            AccessType::Untracked => {
                                *entry = AccessType::Untracked;
                            },
                        }
                        inst
                    },
                    IR::Shift(n) => {
                        delta += n;
                        inst
                    },
                    IR::Reset => {
                        access.insert(delta, AccessType::Set(0));
                        inst
                    },
                    IR::Skip(_) => {
                        optimizable = false;
                        inst
                    },
                    IR::Move(ref targets) => {
                        access.insert(delta, AccessType::Set(0));
                        for (pos, weight) in targets {
                            assert!(*pos != 0);
                            assert!(*weight != 0);
                            access.insert(delta + pos, AccessType::Untracked);
                        }
                        inst
                    },
                    IR::Mul(..) => {
                        unreachable!();
                    }
                }
            }).collect();
        if optimizable && delta == 0 {
            if let Some(AccessType::Inc(inc)) = access.get(&0) {
                if *inc == 255 {
                    let mut dlt = 0;
                    analyzed = analyzed
                        .into_iter()
                        .map(|inst| {
                            match inst {
                                IR::Inc(n) => {
                                    if dlt == 0 {
                                        IR::Reset
                                    } else {
                                        IR::Mul(-dlt, n as u8)
                                    }
                                },
                                IR::Shift(n) => {
                                    dlt += n;
                                    inst
                                },
                                other => other,
                            }
                        }).collect();
                    analyzed.push(IR::Reset);
                    access.insert(0, AccessType::Set(0));
                } else if gcd(*inc as u8 as u64, 256) == 1 {
                    if access.iter().filter(|(&key, _)| key != 0).all(|(_, value)| matches!(value, AccessType::Set(_))) {
                        analyzed.push(IR::Reset);
                        access.insert(0, AccessType::Set(0));
                    }
                } else {
                    // ループを抜けたということは 0 確定
                    access.insert(0, AccessType::Set(0));
                }
            }
            (analyzed, Some(access))
        } else {
            (analyzed, None)
        }
    }

    pub fn optimize_const_loops(prog: Vec<IR>) -> Vec<IR> {
        fn optimize(prog: Vec<IR>) -> (Vec<IR>, Option<BTreeMap<i32, AccessType>>) {
            let mut access = BTreeMap::<i32, AccessType>::new();
            access.insert(0, AccessType::Inc(0));
            let mut delta: i32 = 0;
            let mut optimizable = true;
            let optimized: Vec<_> = prog.into_iter().map(|inst| {
                match inst {
                    IR::Output | IR::Input => {
                        optimizable = false;
                        inst
                    },
                    IR::Shift(n) => {
                        delta += n;
                        inst
                    },
                    IR::Reset => {
                        access.insert(delta, AccessType::Set(0));
                        inst
                    },
                    IR::Skip(_) => {
                        optimizable = false;
                        inst
                    },
                    IR::Move(ref targets) => {
                        access.insert(delta, AccessType::Set(0));
                        for (pos, _weight) in targets {
                            access.insert(delta + pos, AccessType::Untracked);
                        }
                        inst
                    },
                    IR::Inc(n) => {
                        if let Some(entry) = access.get_mut(&delta) {
                            match entry {
                                AccessType::Inc(incr) => {
                                    *entry = AccessType::Inc(*incr + n as u8);
                                },
                                AccessType::Set(value) => {
                                    *entry = AccessType::Set(*value + n as u8);
                                },
                                AccessType::Untracked => {
                                    *entry = AccessType::Untracked;
                                },
                            }
                        } else {
                            access.insert(delta, AccessType::Inc(n as u8));
                        }
                        inst
                    }
                    IR::Block(block) => {
                        let (blk_optimized, blk_result) = optimize(block);
                        if let Some(blk_access) = blk_result {
                            for (pos, cell) in blk_access {
                                match cell {
                                    AccessType::Set(value) => {
                                        access.insert(delta + pos, AccessType::Set(value));
                                    },
                                    AccessType::Inc(_) | AccessType::Untracked => {
                                        access.insert(delta + pos, AccessType::Untracked);
                                    }
                                }
                            }
                        }
                        IR::Block(blk_optimized)
                    }
                    IR::Mul(..) => unreachable!(),
                }
            }).collect();
            get_optimized(optimized, optimizable, delta, access)
        }

        fn get_optimized(mut prog: Vec<IR>,
                         optimizable: bool,
                         delta: i32,
                         mut access: BTreeMap<i32, AccessType>) -> (Vec<IR>, Option<BTreeMap<i32, AccessType>>) {
            if optimizable && delta == 0 {
                if let Some(AccessType::Inc(update)) = access.get(&0) {
                    if *update == 255 {
                        let mut ptr = 0;
                        let mut optimized = Vec::<_>::new();
                        for inst in prog {
                            match inst {
                                IR::Shift(n) => {
                                    ptr += n;
                                    optimized.push(inst);
                                },
                                IR::Inc(n) => {
                                    if ptr != 0 {
                                        if let Some(AccessType::Inc(_)) = access.get(&ptr) {
                                            optimized.push(IR::Mul(-ptr, n as u8));
                                        }
                                    }
                                },
                                other => optimized.push(other),
                            }
                        }
                        optimized.push(IR::Reset);
                        access.insert(0, AccessType::Set(0));
                        return (optimized, Some(access));
                    } else if gcd(*update as u64, 256) == 1 {
                        if access.iter().filter(|(&key, _)| key != 0).all(|(_, value)| matches!(value, AccessType::Set(_))) {
                            prog.push(IR::Reset);
                            access.insert(0, AccessType::Set(0));
                        }
                        return (prog, Some(access));
                    } else {
                        access.insert(0, AccessType::Set(0));
                        return (prog, Some(access));
                    }
                } else {
                    return (prog, Some(access));
                }
            } else {
                return (prog, None);
            }
        }
        
        prog.into_iter().map(|inst| {
            match inst {
                IR::Block(block) => {
                    let (optimized, _) = optimize(block);
                    IR::Block(optimized)
                },
                other => other,
            }
        }).collect()
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
                    IR::Mul(arg, weight) => {
                        let delta = pick_shift(iter);
                        unmatched.push(Inst{cmd: InstType::Mul, arg: arg, inc: weight, delta: delta});
                    }
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
