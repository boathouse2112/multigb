pub mod arg;

pub use crate::instruction::arg::{Condition, IntoWriteArg, ReadArg, RstVector, WriteArg};

// ==== Arg utility types ====

pub type BoxRead<T> = Box<dyn ReadArg<ReadValue = T>>;
pub type BoxWrite<T> = Box<dyn ReadArg<ReadValue = T>>;
pub trait ReadWrite: ReadArg + WriteArg {}
pub type BoxReadWrite<T> = Box<dyn ReadWrite<ReadValue = T, WriteValue = T>>;
pub enum Writeable<T> {
    IntoWriteArg(Box<dyn IntoWriteArg<ReadValue = T>>),
    WriteArg(Box<dyn WriteArg<WriteValue = T>>),
}

pub enum InstructionName {
    // Arithmetic & logic
    Adc(BoxReadWrite<u8>, BoxRead<u8>),
    Add8(BoxReadWrite<u8>, BoxRead<u8>),
    AddI8(BoxReadWrite<u16>, BoxRead<i8>),
    Add16(BoxReadWrite<u16>, BoxRead<u16>),
    And(BoxReadWrite<u8>, BoxRead<u8>),
    Cp(BoxRead<u8>, BoxRead<u8>),
    Dec8(BoxReadWrite<u8>),
    Dec16(BoxReadWrite<u16>),
    Inc8(BoxReadWrite<u8>),
    Inc16(BoxReadWrite<u16>),
    Or(BoxReadWrite<u8>, BoxRead<u8>),
    Sbc(BoxReadWrite<u8>, BoxRead<u8>),
    Sub(BoxReadWrite<u8>, BoxRead<u8>),
    Xor(BoxReadWrite<u8>, BoxRead<u8>),
    // Bitwise operations
    Bit(BoxRead<u8>, BoxRead<u8>),
    Res(BoxRead<u8>, BoxReadWrite<u8>),
    Set(BoxRead<u8>, BoxReadWrite<u8>),
    Swap(BoxReadWrite<u8>),
    // Bit shift
    Rl(BoxReadWrite<u8>),
    Rla,
    Rlc(BoxReadWrite<u8>),
    Rlca,
    Rr(BoxReadWrite<u8>),
    Rra,
    Rrc(BoxReadWrite<u8>),
    Rrca,
    Sla(BoxReadWrite<u8>),
    Sra(BoxReadWrite<u8>),
    Srl(BoxReadWrite<u8>),
    // Load instructions
    Ld8(Writeable<u8>, BoxRead<u8>),
    Ld16(Writeable<u16>, BoxRead<u16>),
    Ldh(BoxWrite<u8>, BoxRead<u8>),
    // Jumps & subroutines
    Call(BoxRead<u16>),
    CallIf(BoxRead<Condition>, BoxRead<u16>),
    Jp(BoxRead<u16>),
    JpIf(BoxRead<Condition>, BoxRead<u16>),
    Jr(BoxRead<i8>),
    JrIf(BoxRead<Condition>, BoxRead<i8>),
    Ret,
    RetIf(BoxRead<Condition>),
    Reti,
    Rst(BoxRead<RstVector>),
    // Stack operations
    Pop(BoxWrite<u16>),
    Push(BoxRead<u16>),
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
