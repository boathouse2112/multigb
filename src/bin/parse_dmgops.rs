#![allow(non_snake_case)]

use anyhow::Result;
use regex::Regex;
use serde::Deserialize;
use std::fs;
use std::str::FromStr;

#[derive(Debug, Deserialize)]
struct DmgOpt {
    Name: String,
    TCyclesBranch: u8,
    TCyclesNoBranch: u8,
    Length: u8,
}

#[derive(Debug, Deserialize)]
struct DmgOpts {
    Unprefixed: Vec<DmgOpt>,
    CBPrefixed: Vec<DmgOpt>,
}

#[derive(Debug, PartialOrd, PartialEq)]
enum InstructionArg {
    Direct8(String),
    Direct16(String),
    Indirect16(String),
    Condition(String),
    Vec(String),
    Literal(u8),
    Hli,
    Hld,
    SpPlusI8,
    IndirectFF00PlusC,
    IndirectFF00PlusU8,
    U8,
    I8,
    U16,
    IndirectU16,
}

#[derive(Debug, PartialOrd, PartialEq)]
enum InstructionName {
    // Arithmetic & logic
    Adc(InstructionArg, InstructionArg),
    Add(InstructionArg, InstructionArg),
    And(InstructionArg, InstructionArg),
    Cp(InstructionArg, InstructionArg),
    Dec(InstructionArg),
    Inc(InstructionArg),
    Or(InstructionArg, InstructionArg),
    Sbc(InstructionArg, InstructionArg),
    Sub(InstructionArg, InstructionArg),
    Xor(InstructionArg, InstructionArg),
    // Bitwise operations
    Bit(InstructionArg, InstructionArg),
    Res(InstructionArg, InstructionArg),
    Set(InstructionArg, InstructionArg),
    Swap(InstructionArg),
    // Bit shift
    Rl(InstructionArg),
    Rla(),
    Rlc(InstructionArg),
    Rlca(),
    Rr(InstructionArg),
    Rra(),
    Rrc(InstructionArg),
    Rrca(),
    Sla(InstructionArg),
    Sra(InstructionArg),
    Srl(InstructionArg),
    // Load instructions
    Ld(InstructionArg, InstructionArg),
    Lhd(InstructionArg, InstructionArg),
    // Jumps & subroutines
    Call(InstructionArg),
    Jp(InstructionArg),
    Jr(InstructionArg),
    Ret,
    Reti,
    Rst(InstructionArg),
    // Stack operations
    Pop(InstructionArg),
    Push(InstructionArg),
    // Misc,
    Ccf,
    Cpl,
    Daa,
    Di,
    Ei,
    Halt,
    Nop,
    Scf,
    Stop,
    Unused,
}

#[derive(PartialOrd, PartialEq)]
struct Instruction {
    opcode: u16,
    name: InstructionName,
    length: u8,
    cycles_branch: u8,
    cycles_no_branch: u8,
}

fn parse_instructions(opts: Vec<DmgOpt>) -> Vec<Instruction> {
    opts.into_iter()
        .enumerate()
        .map(|(opcode, opt)| {
            let name = parse_instruction_name(opt.Name);
            let length = opt.Length;
            let cycles_branch = opt.TCyclesBranch;
            let cycles_no_branch = opt.TCyclesNoBranch;
            Instruction {
                opcode: opcode as u16,
                name,
                length,
                cycles_branch,
                cycles_no_branch,
            }
        })
        .collect()
}

fn parse_instruction_arg(instr: &str, arg: &str) -> InstructionArg {
    match (instr, arg) {
        // Conditionals
        ("JP" | "JR" | "RET" | "CALL", "C" | "NC" | "Z" | "NZ") => {
            InstructionArg::Condition(arg.to_string())
        }
        ("RST", _) => InstructionArg::Vec(arg.to_string()),
        ("BIT" | "RES" | "SET", u3) if u8::from_str(u3).is_ok() => {
            InstructionArg::Literal(u8::from_str(u3).unwrap())
        }
        (_, "A" | "B" | "C" | "D" | "E" | "F" | "H" | "L") => {
            InstructionArg::Direct8(arg.to_string())
        }
        (_, "AF" | "BC" | "DE" | "HL" | "PC" | "SP") => InstructionArg::Direct16(arg.to_string()),
        (_, "(BC)" | "(DE)" | "(HL)") => InstructionArg::Indirect16(arg[1..=2].to_string()),
        (_, "(HL+)") => InstructionArg::Hli,
        (_, "(HL-)") => InstructionArg::Hld,
        (_, "SP+i8") => InstructionArg::SpPlusI8,
        (_, "(FF00+C)") => InstructionArg::IndirectFF00PlusC,
        (_, "(FF00+u8)") => InstructionArg::IndirectFF00PlusU8,
        (_, "u8") => InstructionArg::U8,
        (_, "i8") => InstructionArg::I8,
        (_, "u16") => InstructionArg::U16,
        (_, "(u16)") => InstructionArg::IndirectU16,
        _ => panic!("Invalid instruction arg: {}", arg),
    }
}

fn parse_instruction_name(name: String) -> InstructionName {
    let name_regex =
        Regex::new(r"(\w+) ?(\(?\w+-?\+?(?:u8)?(?:i8)?C?\)?)?,?(\(?\w+-?\+?(?:u8)?(?:i8)?C?\)?)?")
            .unwrap();
    let captures = name_regex.captures(&name).unwrap();

    let name_str = captures.get(1).map(|m| m.as_str()).unwrap();
    let arg_1_str = captures.get(2).map(|m| m.as_str());
    let arg_2_str = captures.get(3).map(|m| m.as_str());

    let arg_1 = arg_1_str.map(|arg| parse_instruction_arg(name_str, arg));
    let arg_2 = arg_2_str.map(|arg| parse_instruction_arg(name_str, arg));

    match name_str {
        // Arithmetic & logic
        "ADC" => InstructionName::Adc(arg_1.unwrap(), arg_2.unwrap()),
        "ADD" => InstructionName::Add(arg_1.unwrap(), arg_2.unwrap()),
        "AND" => InstructionName::And(arg_1.unwrap(), arg_2.unwrap()),
        "CP" => InstructionName::Cp(arg_1.unwrap(), arg_2.unwrap()),
        "DEC" => InstructionName::Dec(arg_1.unwrap()),
        "INC" => InstructionName::Inc(arg_1.unwrap()),
        "OR" => InstructionName::Or(arg_1.unwrap(), arg_2.unwrap()),
        "SBC" => InstructionName::Sbc(arg_1.unwrap(), arg_2.unwrap()),
        "SUB" => InstructionName::Sub(arg_1.unwrap(), arg_2.unwrap()),
        "XOR" => InstructionName::Xor(arg_1.unwrap(), arg_2.unwrap()),
        // Bitwise operations
        "BIT" => InstructionName::Bit(arg_1.unwrap(), arg_2.unwrap()),
        "RES" => InstructionName::Res(arg_1.unwrap(), arg_2.unwrap()),
        "SET" => InstructionName::Set(arg_1.unwrap(), arg_2.unwrap()),
        "SWAP" => InstructionName::Swap(arg_1.unwrap()),
        // Bit shift
        "RL" => InstructionName::Rl(arg_1.unwrap()),
        "RLA" => InstructionName::Rla(),
        "RLC" => InstructionName::Rlc(arg_1.unwrap()),
        "RLCA" => InstructionName::Rlca(),
        "RR" => InstructionName::Rr(arg_1.unwrap()),
        "RRA" => InstructionName::Rra(),
        "RRC" => InstructionName::Rrc(arg_1.unwrap()),
        "RRCA" => InstructionName::Rrca(),
        "SLA" => InstructionName::Sla(arg_1.unwrap()),
        "SRA" => InstructionName::Sra(arg_1.unwrap()),
        "SRL" => InstructionName::Srl(arg_1.unwrap()),
        // Load instructions
        "LD" => InstructionName::Ld(arg_1.unwrap(), arg_2.unwrap()),
        "LHD" => InstructionName::Lhd(arg_1.unwrap(), arg_2.unwrap()),
        // Jumps & subroutines
        "CALL" => InstructionName::Call(arg_1.unwrap()),
        "JP" => InstructionName::Jp(arg_1.unwrap()),
        "JR" => InstructionName::Jr(arg_1.unwrap()),
        "RET" => InstructionName::Ret,
        "RETI" => InstructionName::Reti,
        "RST" => InstructionName::Rst(arg_1.unwrap()),
        // Stack operations
        "POP" => InstructionName::Pop(arg_1.unwrap()),
        "PUSH" => InstructionName::Push(arg_1.unwrap()),
        // Misc,
        "CCF" => InstructionName::Ccf,
        "CPL" => InstructionName::Cpl,
        "DAA" => InstructionName::Daa,
        "DI" => InstructionName::Di,
        "EI" => InstructionName::Ei,
        "HALT" => InstructionName::Halt,
        "NOP" => InstructionName::Nop,
        "SCF" => InstructionName::Scf,
        "STOP" => InstructionName::Stop,
        "UNUSED" => InstructionName::Unused,
        _ => panic!("Invalid name string: {}", name_str),
    }
}

fn instr_to_string(instr: &Instruction) -> String {
    format!(
        "Instruction::new(0x{:02X}, InstructionName::{:?}, {}, {}, {}),",
        instr.opcode, instr.name, instr.length, instr.cycles_branch, instr.cycles_no_branch
    )
}

fn main() -> Result<()> {
    let dmg_opts_json = fs::read_to_string("dmgops.json")?;
    let dmg_opts: DmgOpts = serde_json::from_str(&dmg_opts_json)?;

    let unprefixed_instructions = parse_instructions(dmg_opts.Unprefixed);
    let unprefixed_instructions: Vec<_> = unprefixed_instructions
        .into_iter()
        .filter(|instr| instr.name != InstructionName::Unused)
        .collect();

    let prefixed_instructions = parse_instructions(dmg_opts.CBPrefixed);
    let prefixed_instructions: Vec<_> = prefixed_instructions.into_iter().map(|instr|{
        let opcode = 0xCB00 + instr.opcode;
        Instruction {
            opcode,
            ..instr
        }
    }).collect();
    let prefixed_instructions: Vec<_> = prefixed_instructions
        .into_iter()
        .filter(|instr| instr.name != InstructionName::Unused)
        .collect();

    let lines = unprefixed_instructions
        .iter()
        .map(instr_to_string)
        .collect::<Vec<_>>()
        .join("\n");

    let prefixed_lines = prefixed_instructions
        .iter()
        .map(instr_to_string)
        .collect::<Vec<_>>()
        .join("\n");

    let text = format!("
    use crate::instruction::{{Instruction, InstructionName, InstructionArg}};\n\
    #[rustfmt_skip]
    fn instructions() -> Vec<Instruction> {{\nvec![{}{}]\n}}\
    ", lines, prefixed_lines);

    fs::write("codegen/instructions.rs", text)?;

    Ok(())
}
