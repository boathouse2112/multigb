use crate::bus;
use crate::console::Console;
use crate::cpu::flag::Flags;
use crate::instruction::{Instruction, InstructionArg, InstructionName};
use crate::util::Number;

mod flag;

#[derive(Clone, Debug)]
pub struct Cpu {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub f: u8,
    pub h: u8,
    pub l: u8,
    pub pc: u16,
    pub sp: u16,
    pub flags: Flags,
}

impl Cpu {
    pub fn new() -> Self {
        Cpu {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            f: 0,
            h: 0,
            l: 0,
            pc: 0,
            sp: 0,
            flags: Flags::new(),
        }
    }

    pub fn af(&self) -> u16 {
        Cpu::join(self.a, self.flags.bits())
    }

    pub fn bc(&self) -> u16 {
        Cpu::join(self.b, self.c)
    }

    pub fn de(&self) -> u16 {
        Cpu::join(self.d, self.e)
    }

    pub fn hl(&self) -> u16 {
        Cpu::join(self.h, self.l)
    }

    /// Join two u8 registers into a u16 register
    fn join(first: u8, second: u8) -> u16 {
        ((first as u16) << 8) + (second as u16)
    }
}

/// Step the CPU forward 1 instruction
pub fn step(instructions: &Vec<Instruction>, console: &mut Console) {
    // Read opcode
    let opcode_or_prefix = bus::read_u8(console, console.cpu.pc);
    console.cpu.pc += 1;
    // Read rest of 0xCB prefixed opcode
    let opcode = if opcode_or_prefix == 0xCB {
        let suffix = bus::read_u8(console, console.cpu.pc);
        console.cpu.pc += 1;
        0xCB00 + (suffix as u16)
    } else {
        opcode_or_prefix as u16
    };

    // Match opcode to instruction
    let instruction = instructions
        .iter()
        .find(|instr| instr.opcode == opcode)
        .unwrap_or_else(|| panic!("Opcode 0x{:X} has an associated instruction.", opcode));

    // Run instruction
    match instruction.name {
        InstructionName::Adc(lhs, rhs) => {}
        InstructionName::Add(_, _) => {}
        InstructionName::And(_, _) => {}
        InstructionName::Cp(_, _) => {}
        InstructionName::Dec(_) => {}
        InstructionName::Inc(_) => {}
        InstructionName::Or(_, _) => {}
        InstructionName::Sbc(_, _) => {}
        InstructionName::Sub(_, _) => {}
        InstructionName::Xor(_, _) => {}
        InstructionName::Bit(_, _) => {}
        InstructionName::Res(_, _) => {}
        InstructionName::Set(_, _) => {}
        InstructionName::Swap(_) => {}
        InstructionName::Rl(_) => {}
        InstructionName::Rla => {}
        InstructionName::Rlc(_) => {}
        InstructionName::Rlca => {}
        InstructionName::Rr(_) => {}
        InstructionName::Rra => {}
        InstructionName::Rrc(_) => {}
        InstructionName::Rrca => {}
        InstructionName::Sla(_) => {}
        InstructionName::Sra(_) => {}
        InstructionName::Srl(_) => {}
        InstructionName::Ld(_, _) => {}
        InstructionName::Lhd(_, _) => {}
        InstructionName::Call(_) => {}
        InstructionName::Jp(_) => {}
        InstructionName::Jr(_) => {}
        InstructionName::Ret => {}
        InstructionName::Reti => {}
        InstructionName::Rst(_) => {}
        InstructionName::Pop(_) => {}
        InstructionName::Push(_) => {}
        InstructionName::Ccf => {}
        InstructionName::Cpl => {}
        InstructionName::Daa => {}
        InstructionName::Di => {}
        InstructionName::Ei => {}
        InstructionName::Halt => {}
        InstructionName::Nop => {}
        InstructionName::Scf => {}
        InstructionName::Stop => {}
        InstructionName::Unused => {}
    }

    /// A parsed arg value
    enum ArgValue {
        U8(u8),
        I8(i8),
        U16(u16),
        Condition,
    }

    /// Read an arg of the given type from memory.
    /// Increments PC for each byte read
    fn read_arg(console: &mut Console, arg: InstructionArg) -> ArgValue {
        match arg {
            InstructionArg::DirectRegister8(register) => match register.as_str() {
                "A" => ArgValue::U8(console.cpu.a),
                "B" => ArgValue::U8(console.cpu.b),
                "C" => ArgValue::U8(console.cpu.c),
                "D" => ArgValue::U8(console.cpu.d),
                "E" => ArgValue::U8(console.cpu.e),
                "F" => ArgValue::U8(console.cpu.f),
                "H" => ArgValue::U8(console.cpu.h),
                "L" => ArgValue::U8(console.cpu.l),
                _ => panic!(),
            },
            InstructionArg::DirectRegister16(register) => match register.as_str() {
                "AF" => ArgValue::U16(console.cpu.af()),
                "BC" => ArgValue::U16(console.cpu.bc()),
                "DE" => ArgValue::U16(console.cpu.de()),
                "HL" => ArgValue::U16(console.cpu.hl()),
                "PC" => ArgValue::U16(console.cpu.pc),
                "SP" => ArgValue::U16(console.cpu.sp),
                _ => panic!(),
            },
            InstructionArg::IndirectRegister16(_) => {}
            InstructionArg::Condition(_) => {}
            InstructionArg::Vector(_) => {}
            InstructionArg::Literal(_) => {}
            InstructionArg::Hli => {}
            InstructionArg::Hld => {}
            InstructionArg::SpPlusI8 => {}
            InstructionArg::IndirectFF00PlusC => {}
            InstructionArg::IndirectFF00PlusU8 => {}
            InstructionArg::U8 => {}
            InstructionArg::I8 => {}
            InstructionArg::U16 => {}
            InstructionArg::IndirectU16 => {}
        };
        todo!()
    }
}
