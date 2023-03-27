
#[derive(Debug, PartialOrd, PartialEq)]
pub enum InstructionArg {
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
pub enum InstructionName {
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
pub struct Instruction {
    opcode: u16,
    name: InstructionName,
    length: u8,
    cycles_branch: u8,
    cycles_no_branch: u8,
}
