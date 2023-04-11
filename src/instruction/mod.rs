pub mod arg;

use crate::cpu;
use crate::instruction::arg::{ArgReader, IntoWriter};
pub use crate::instruction::arg::{ArgReaderIntoWriter, ArgWriter, Condition, RstVector};

// ==== Arg utility types ====

pub type BoxArgWriter<Arg> = Box<dyn ArgWriter<Arg = Arg>>;
pub type BoxArgReader<Arg> = Box<dyn ArgReader<Arg = Arg>>;
pub type BoxIntoWriter<Arg> = Box<dyn IntoWriter<Arg = Arg>>;
pub type BoxArgReaderIntoWriter<Arg> = Box<dyn ArgReaderIntoWriter<Arg = Arg>>;

pub enum InstructionName {
    // Arithmetic & logic
    Adc(BoxArgReaderIntoWriter<u8>, BoxArgReader<u8>),
    Add8(BoxArgReaderIntoWriter<u8>, BoxArgReader<u8>),
    AddI8(BoxArgReaderIntoWriter<u16>, BoxArgReader<i8>),
    Add16(BoxArgReaderIntoWriter<u16>, BoxArgReader<u16>),
    And(BoxArgReaderIntoWriter<u8>, BoxArgReader<u8>),
    Cp(BoxArgReader<u8>, BoxArgReader<u8>),
    Dec8(BoxArgReaderIntoWriter<u8>),
    Dec16(BoxArgReaderIntoWriter<u16>),
    Inc8(BoxArgReaderIntoWriter<u8>),
    Inc16(BoxArgReaderIntoWriter<u16>),
    Or(BoxArgReaderIntoWriter<u8>, BoxArgReader<u8>),
    Sbc(BoxArgReaderIntoWriter<u8>, BoxArgReader<u8>),
    Sub(BoxArgReaderIntoWriter<u8>, BoxArgReader<u8>),
    Xor(BoxArgReaderIntoWriter<u8>, BoxArgReader<u8>),
    // Bitwise operations
    Bit(BoxArgReader<u8>, BoxArgReader<u8>),
    Res(BoxArgReader<u8>, BoxArgReaderIntoWriter<u8>),
    Set(BoxArgReader<u8>, BoxArgReaderIntoWriter<u8>),
    Swap(BoxArgReaderIntoWriter<u8>),
    // Bit shift
    Rl(BoxArgReaderIntoWriter<u8>),
    Rlc(BoxArgReaderIntoWriter<u8>),
    Rr(BoxArgReaderIntoWriter<u8>),
    Rrc(BoxArgReaderIntoWriter<u8>),
    Sla(BoxArgReaderIntoWriter<u8>),
    Sra(BoxArgReaderIntoWriter<u8>),
    Srl(BoxArgReaderIntoWriter<u8>),
    // Load instructions
    Ld8(BoxIntoWriter<u8>, BoxArgReader<u8>),
    Ld16(BoxIntoWriter<u16>, BoxArgReader<u16>),
    // Jumps & subroutines
    Call(BoxArgReader<u16>),
    CallIf(Condition, BoxArgReader<u16>),
    Jp(BoxArgReader<u16>),
    JpIf(Condition, BoxArgReader<u16>),
    Jr(BoxArgReader<i8>),
    JrIf(Condition, BoxArgReader<i8>),
    Ret,
    RetIf(Condition),
    Reti,
    Rst(RstVector),
    // Stack operations
    Pop(cpu::Register16),
    Push(cpu::Register16),
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

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Instruction {
    pub opcode: u16,
    pub name: InstructionName,
    pub length: u8,
    pub cycles_branch: u8,
    pub cycles_no_branch: u8,
}

impl Instruction {
    pub fn new(
        opcode: u16,
        name: InstructionName,
        length: u8,
        cycles_branch: u8,
        cycles_no_branch: u8,
    ) -> Self {
        Instruction {
            opcode,
            name,
            length,
            cycles_branch,
            cycles_no_branch,
        }
    }
}
