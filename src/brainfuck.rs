use std::io::{self, Read, Write};
use std::collections::BTreeMap;

#[derive(Debug)]
pub enum Inst {
    Inc(i32),
    Shift(i32),
    Output,
    Input,
    Reset,
    Move1(i16),
    Move2(i16, i16),
    Move3(i16, i16, i16),
    Move(u32),
    Mul(i32, u8),
    Open(u32),
    Close(u32),
    Skip(i32),
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

impl LongInst {
    pub fn flatten(&self) -> Vec<Inst> {
        match self {
            LongInst::Inc(n) => vec![Inst::Inc(*n)],
            LongInst::Shift(n) => vec![Inst::Shift(*n)],
            LongInst::Output => vec![Inst::Output],
            LongInst::Input => vec![Inst::Input],
            LongInst::Reset => vec![Inst::Reset],
            LongInst::Skip(n) => vec![Inst::Skip(*n)],
            LongInst::Move(targets) => {
                if targets.iter().all(|&(offset, weight)| (weight == 1) && (i16::MIN as i32 <= offset) && (offset <= i16::MAX as i32)) {
                    if targets.len() == 1 {
                        return vec![Inst::Move1(targets[0].0 as i16)];
                    } else if targets.len() == 2 {
                        return vec![Inst::Move2(targets[0].0 as i16, targets[1].0 as i16)];
                    } else if targets.len() == 3 {
                        return vec![Inst::Move3(targets[0].0 as i16, targets[1].0 as i16, targets[2].0 as i16)];
                    }
                } else {
                    if targets.len() == 1 {
                        return vec![Inst::Mul(targets[0].0, targets[0].1 as u8)];
                    }
                }
                let mut flat = Vec::new();
                flat.push(Inst::Move(targets.len() as u32));
                for &(offset, weight) in targets {
                    flat.push(Inst::Mul(offset, weight as u8));
                }
                flat
            },
            LongInst::Block(block) => {
                let mut flat = Vec::new();
                flat.push(Inst::Open(0));
                for inst in block {
                    flat.extend(inst.flatten());
                }
                flat.push(Inst::Close(0));
                flat
            },
        }
    }
}

pub struct Brainfuck {
    dp: usize,
    memory: Vec<u8>,
}

#[allow(dead_code)]
impl Brainfuck {
    pub fn new() -> Self {
        Self { 
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
                match inst  {
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
        let mut flat = Vec::new();
        for inst in prog {
            flat.extend(inst.flatten());
        }

        let mut result = Vec::new();
        let mut stack = Vec::new();
        for inst in flat {
            match inst {
                Inst::Open(_) => {
                    stack.push(result.len());
                    result.push(Inst::Open(0));
                },
                Inst::Close(_) => {
                    let open = stack.pop().unwrap();
                    result[open] = Inst::Open(result.len() as u32 + 1);
                    result.push(Inst::Close(open as u32 + 1));
                },
                other => {
                    result.push(other);
                }
            }
        }
        result
    }

    #[inline(always)]
    pub fn run(&mut self, prog: Vec<Inst>) {
        let mut ip = 0;
        while ip < prog.len() {
            match prog[ip] {
                Inst::Inc(n) => {
                    self.memory[self.dp] = self.memory[self.dp].wrapping_add(n as u8);
                    ip += 1;
                },
                Inst::Shift(n) => {
                    self.dp = (self.dp as i32 + n) as usize;
                    ip += 1;
                },
                Inst::Output => {
                    print!("{}", self.memory[self.dp] as char);
                    std::io::stdout().flush().unwrap();
                    ip += 1;
                },
                Inst::Input => {
                    let mut buf = [0];
                    match io::stdin().read_exact(&mut buf) {
                        Ok(()) => {
                            self.memory[self.dp] = buf[0];
                        },
                        Err(_) => {
                            self.memory[self.dp] = 0;
                        },
                    }
                    ip += 1;
                },
                Inst::Reset => {
                    self.memory[self.dp] = 0;
                    ip += 1;
                },
                Inst::Move1(offset) => {
                    if self.memory[self.dp] != 0 {
                        let val = self.memory[self.dp];
                        let pos = (self.dp as isize + offset as isize) as usize;
                        self.memory[pos] = self.memory[pos].wrapping_add(val);
                        self.memory[self.dp] = 0;
                    }
                    ip += 1;
                },
                Inst::Move2(offset0, offset1) => {
                    if self.memory[self.dp] != 0 {
                        let val = self.memory[self.dp];
                        let pos0 = (self.dp as isize + offset0 as isize) as usize;
                        let pos1 = (self.dp as isize + offset1 as isize) as usize;
                        self.memory[pos0] = self.memory[pos0].wrapping_add(val);
                        self.memory[pos1] = self.memory[pos1].wrapping_add(val);
                        self.memory[self.dp] = 0;
                    }
                    ip += 1;
                },
                Inst::Move3(offset0, offset1, offset2) => {
                    if self.memory[self.dp] != 0 {
                        let val = self.memory[self.dp];
                        let pos0 = (self.dp as isize + offset0 as isize) as usize;
                        let pos1 = (self.dp as isize + offset1 as isize) as usize;
                        let pos2 = (self.dp as isize + offset2 as isize) as usize;
                        self.memory[pos0] = self.memory[pos0].wrapping_add(val);
                        self.memory[pos1] = self.memory[pos1].wrapping_add(val);
                        self.memory[pos2] = self.memory[pos2].wrapping_add(val);
                        self.memory[self.dp] = 0;
                    }
                    ip += 1;
                },
                Inst::Mul(offset, weight) => {
                    if self.memory[self.dp] != 0 {
                        let val = self.memory[self.dp];
                        let pos = (self.dp as isize + offset as isize) as usize;
                        let inc = val.wrapping_mul(weight);
                        self.memory[pos] = self.memory[pos].wrapping_add(inc);
                        self.memory[self.dp] = 0;
                    }
                    ip += 1;
                }
                Inst::Move(len) => {
                    if self.memory[self.dp] != 0 {
                        let val = self.memory[self.dp];
                        for idx in 0..len as usize {
                            if let Inst::Mul(offset, weight) = prog[ip + idx + 1] {
                                let pos = (self.dp as isize + offset as isize) as usize;
                                let inc = val.wrapping_mul(weight);
                                self.memory[pos] = self.memory[pos].wrapping_add(inc);
                            }
                        }
                        self.memory[self.dp] = 0;
                    }
                    ip += len as usize + 1;
                },
                Inst::Open(dst) => {
                    if self.memory[self.dp] == 0 {
                        ip = dst as usize;
                    } else {
                        ip += 1;
                    }
                },
                Inst::Close(dst) => {
                    if self.memory[self.dp] != 0 {
                        ip = dst as usize;
                    } else {
                        ip += 1;
                    }
                },
                Inst::Skip(n) => {
                    while self.memory[self.dp] != 0 {
                        self.dp = (self.dp as i32 + n) as usize;
                    }
                    ip += 1;
                },
            }
        }
    }

    pub fn to_c(prog: &Vec<LongInst>) -> String {
        fn block_to_c(prog: &Vec<LongInst>, level: usize) -> String {
            let mut code = String::new();
            fn indent(level: usize) -> String {
                "    ".repeat(level)
            }
            for inst in prog {
                match inst {
                    LongInst::Inc(n) => {
                        code.push_str(&format!("{}memory[dp] += {};\n", indent(level), n));
                    },
                    LongInst::Shift(n) => {
                        code.push_str(&format!("{}dp += {};\n", indent(level), n));
                    },
                    LongInst::Output => {
                        code.push_str(&format!("{}putchar(memory[dp]);\n", indent(level)));
                    },
                    LongInst::Input => {
                        code.push_str(&format!("{}memory[dp] = getchar();\n", indent(level)));
                    },
                    LongInst::Reset => {
                        code.push_str(&format!("{}memory[dp] = 0;\n", indent(level)));
                    },
                    LongInst::Move(targets) => {
                        code.push_str(&format!("{}if (memory[dp]) {{\n", indent(level)));
                        for &(offset, weight) in targets {
                            if weight == 1 {
                                code.push_str(&format!(
                                    "{}memory[dp + {}] += memory[dp];\n",
                                    indent(level + 1), offset
                                ));
                            } else {
                                code.push_str(&format!(
                                    "{}memory[dp + {}] += memory[dp] * {};\n",
                                    indent(level + 1), offset, weight
                                ));
                            }
                        }
                        code.push_str(&format!("{}memory[dp] = 0;\n", indent(level)));
                        code.push_str(&format!("{}}}\n", indent(level)));
                    },
                    LongInst::Skip(n) => {
                        code.push_str(&format!("{}while (memory[dp]) dp += {};\n", indent(level), n))
                    },
                    LongInst::Block(block) => {
                        code.push_str(&format!("{}while (memory[dp]) {{\n", indent(level)));
                        code.push_str(&block_to_c(block, level + 1));
                        code.push_str(&format!("{}}}\n", indent(level)));
                    },
                }
            }
            code
        }
        
        let mut code = String::new();
        code.push_str("#include <stdio.h>\n");
        code.push_str("#include <stdlib.h>\n");
        code.push_str("int main() {\n");
        code.push_str("    char memory[(1<<16)] = {};\n");
        code.push_str("    int dp = 0;\n");
        code.push_str(&block_to_c(prog, 1));
        code.push_str("    return 0;\n");
        code.push_str("}\n");
        code
    }
}
