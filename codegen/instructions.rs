
    use crate::instruction::{Instruction, InstructionName, InstructionArg};
#[rustfmt_skip]
    fn instructions() -> Vec<Instruction> {
vec![Instruction::new(0x00, InstructionName::Nop, 1, 4, 4),
Instruction::new(0x01, InstructionName::Ld(Direct16("BC"), U16), 3, 12, 12),
Instruction::new(0x02, InstructionName::Ld(Indirect16("BC"), Direct8("A")), 1, 8, 8),
Instruction::new(0x03, InstructionName::Inc(Direct16("BC")), 1, 8, 8),
Instruction::new(0x04, InstructionName::Inc(Direct8("B")), 1, 4, 4),
Instruction::new(0x05, InstructionName::Dec(Direct8("B")), 1, 4, 4),
Instruction::new(0x06, InstructionName::Ld(Direct8("B"), U8), 2, 8, 8),
Instruction::new(0x07, InstructionName::Rlca, 1, 4, 4),
Instruction::new(0x08, InstructionName::Ld(IndirectU16, Direct16("SP")), 3, 20, 20),
Instruction::new(0x09, InstructionName::Add(Direct16("HL"), Direct16("BC")), 1, 8, 8),
Instruction::new(0x0A, InstructionName::Ld(Direct8("A"), Indirect16("BC")), 1, 8, 8),
Instruction::new(0x0B, InstructionName::Dec(Direct16("BC")), 1, 8, 8),
Instruction::new(0x0C, InstructionName::Inc(Direct8("C")), 1, 4, 4),
Instruction::new(0x0D, InstructionName::Dec(Direct8("C")), 1, 4, 4),
Instruction::new(0x0E, InstructionName::Ld(Direct8("C"), U8), 2, 8, 8),
Instruction::new(0x0F, InstructionName::Rrca, 1, 4, 4),
Instruction::new(0x10, InstructionName::Stop, 1, 4, 4),
Instruction::new(0x11, InstructionName::Ld(Direct16("DE"), U16), 3, 12, 12),
Instruction::new(0x12, InstructionName::Ld(Indirect16("DE"), Direct8("A")), 1, 8, 8),
Instruction::new(0x13, InstructionName::Inc(Direct16("DE")), 1, 8, 8),
Instruction::new(0x14, InstructionName::Inc(Direct8("D")), 1, 4, 4),
Instruction::new(0x15, InstructionName::Dec(Direct8("D")), 1, 4, 4),
Instruction::new(0x16, InstructionName::Ld(Direct8("D"), U8), 2, 8, 8),
Instruction::new(0x17, InstructionName::Rla, 1, 4, 4),
Instruction::new(0x18, InstructionName::Jr(I8), 2, 12, 12),
Instruction::new(0x19, InstructionName::Add(Direct16("HL"), Direct16("DE")), 1, 8, 8),
Instruction::new(0x1A, InstructionName::Ld(Direct8("A"), Indirect16("DE")), 1, 8, 8),
Instruction::new(0x1B, InstructionName::Dec(Direct16("DE")), 1, 8, 8),
Instruction::new(0x1C, InstructionName::Inc(Direct8("E")), 1, 4, 4),
Instruction::new(0x1D, InstructionName::Dec(Direct8("E")), 1, 4, 4),
Instruction::new(0x1E, InstructionName::Ld(Direct8("E"), U8), 2, 8, 8),
Instruction::new(0x1F, InstructionName::Rra, 1, 4, 4),
Instruction::new(0x20, InstructionName::Jr(Condition("NZ")), 2, 12, 8),
Instruction::new(0x21, InstructionName::Ld(Direct16("HL"), U16), 3, 12, 12),
Instruction::new(0x22, InstructionName::Ld(Hli, Direct8("A")), 1, 8, 8),
Instruction::new(0x23, InstructionName::Inc(Direct16("HL")), 1, 8, 8),
Instruction::new(0x24, InstructionName::Inc(Direct8("H")), 1, 4, 4),
Instruction::new(0x25, InstructionName::Dec(Direct8("H")), 1, 4, 4),
Instruction::new(0x26, InstructionName::Ld(Direct8("H"), U8), 2, 8, 8),
Instruction::new(0x27, InstructionName::Daa, 1, 4, 4),
Instruction::new(0x28, InstructionName::Jr(Condition("Z")), 2, 12, 8),
Instruction::new(0x29, InstructionName::Add(Direct16("HL"), Direct16("HL")), 1, 8, 8),
Instruction::new(0x2A, InstructionName::Ld(Direct8("A"), Hli), 1, 8, 8),
Instruction::new(0x2B, InstructionName::Dec(Direct16("HL")), 1, 8, 8),
Instruction::new(0x2C, InstructionName::Inc(Direct8("L")), 1, 4, 4),
Instruction::new(0x2D, InstructionName::Dec(Direct8("L")), 1, 4, 4),
Instruction::new(0x2E, InstructionName::Ld(Direct8("L"), U8), 2, 8, 8),
Instruction::new(0x2F, InstructionName::Cpl, 1, 4, 4),
Instruction::new(0x30, InstructionName::Jr(Condition("NC")), 2, 12, 8),
Instruction::new(0x31, InstructionName::Ld(Direct16("SP"), U16), 3, 12, 12),
Instruction::new(0x32, InstructionName::Ld(Hld, Direct8("A")), 1, 8, 8),
Instruction::new(0x33, InstructionName::Inc(Direct16("SP")), 1, 8, 8),
Instruction::new(0x34, InstructionName::Inc(Indirect16("HL")), 1, 12, 12),
Instruction::new(0x35, InstructionName::Dec(Indirect16("HL")), 1, 12, 12),
Instruction::new(0x36, InstructionName::Ld(Indirect16("HL"), U8), 2, 12, 12),
Instruction::new(0x37, InstructionName::Scf, 1, 4, 4),
Instruction::new(0x38, InstructionName::Jr(Condition("C")), 2, 12, 8),
Instruction::new(0x39, InstructionName::Add(Direct16("HL"), Direct16("SP")), 1, 8, 8),
Instruction::new(0x3A, InstructionName::Ld(Direct8("A"), Hld), 1, 8, 8),
Instruction::new(0x3B, InstructionName::Dec(Direct16("SP")), 1, 8, 8),
Instruction::new(0x3C, InstructionName::Inc(Direct8("A")), 1, 4, 4),
Instruction::new(0x3D, InstructionName::Dec(Direct8("A")), 1, 4, 4),
Instruction::new(0x3E, InstructionName::Ld(Direct8("A"), U8), 2, 8, 8),
Instruction::new(0x3F, InstructionName::Ccf, 1, 4, 4),
Instruction::new(0x40, InstructionName::Ld(Direct8("B"), Direct8("B")), 1, 4, 4),
Instruction::new(0x41, InstructionName::Ld(Direct8("B"), Direct8("C")), 1, 4, 4),
Instruction::new(0x42, InstructionName::Ld(Direct8("B"), Direct8("D")), 1, 4, 4),
Instruction::new(0x43, InstructionName::Ld(Direct8("B"), Direct8("E")), 1, 4, 4),
Instruction::new(0x44, InstructionName::Ld(Direct8("B"), Direct8("H")), 1, 4, 4),
Instruction::new(0x45, InstructionName::Ld(Direct8("B"), Direct8("L")), 1, 4, 4),
Instruction::new(0x46, InstructionName::Ld(Direct8("B"), Indirect16("HL")), 1, 8, 8),
Instruction::new(0x47, InstructionName::Ld(Direct8("B"), Direct8("A")), 1, 4, 4),
Instruction::new(0x48, InstructionName::Ld(Direct8("C"), Direct8("B")), 1, 4, 4),
Instruction::new(0x49, InstructionName::Ld(Direct8("C"), Direct8("C")), 1, 4, 4),
Instruction::new(0x4A, InstructionName::Ld(Direct8("C"), Direct8("D")), 1, 4, 4),
Instruction::new(0x4B, InstructionName::Ld(Direct8("C"), Direct8("E")), 1, 4, 4),
Instruction::new(0x4C, InstructionName::Ld(Direct8("C"), Direct8("H")), 1, 4, 4),
Instruction::new(0x4D, InstructionName::Ld(Direct8("C"), Direct8("L")), 1, 4, 4),
Instruction::new(0x4E, InstructionName::Ld(Direct8("C"), Indirect16("HL")), 1, 8, 8),
Instruction::new(0x4F, InstructionName::Ld(Direct8("C"), Direct8("A")), 1, 4, 4),
Instruction::new(0x50, InstructionName::Ld(Direct8("D"), Direct8("B")), 1, 4, 4),
Instruction::new(0x51, InstructionName::Ld(Direct8("D"), Direct8("C")), 1, 4, 4),
Instruction::new(0x52, InstructionName::Ld(Direct8("D"), Direct8("D")), 1, 4, 4),
Instruction::new(0x53, InstructionName::Ld(Direct8("D"), Direct8("E")), 1, 4, 4),
Instruction::new(0x54, InstructionName::Ld(Direct8("D"), Direct8("H")), 1, 4, 4),
Instruction::new(0x55, InstructionName::Ld(Direct8("D"), Direct8("L")), 1, 4, 4),
Instruction::new(0x56, InstructionName::Ld(Direct8("D"), Indirect16("HL")), 1, 8, 8),
Instruction::new(0x57, InstructionName::Ld(Direct8("D"), Direct8("A")), 1, 4, 4),
Instruction::new(0x58, InstructionName::Ld(Direct8("E"), Direct8("B")), 1, 4, 4),
Instruction::new(0x59, InstructionName::Ld(Direct8("E"), Direct8("C")), 1, 4, 4),
Instruction::new(0x5A, InstructionName::Ld(Direct8("E"), Direct8("D")), 1, 4, 4),
Instruction::new(0x5B, InstructionName::Ld(Direct8("E"), Direct8("E")), 1, 4, 4),
Instruction::new(0x5C, InstructionName::Ld(Direct8("E"), Direct8("H")), 1, 4, 4),
Instruction::new(0x5D, InstructionName::Ld(Direct8("E"), Direct8("L")), 1, 4, 4),
Instruction::new(0x5E, InstructionName::Ld(Direct8("E"), Indirect16("HL")), 1, 8, 8),
Instruction::new(0x5F, InstructionName::Ld(Direct8("E"), Direct8("A")), 1, 4, 4),
Instruction::new(0x60, InstructionName::Ld(Direct8("H"), Direct8("B")), 1, 4, 4),
Instruction::new(0x61, InstructionName::Ld(Direct8("H"), Direct8("C")), 1, 4, 4),
Instruction::new(0x62, InstructionName::Ld(Direct8("H"), Direct8("D")), 1, 4, 4),
Instruction::new(0x63, InstructionName::Ld(Direct8("H"), Direct8("E")), 1, 4, 4),
Instruction::new(0x64, InstructionName::Ld(Direct8("H"), Direct8("H")), 1, 4, 4),
Instruction::new(0x65, InstructionName::Ld(Direct8("H"), Direct8("L")), 1, 4, 4),
Instruction::new(0x66, InstructionName::Ld(Direct8("H"), Indirect16("HL")), 1, 8, 8),
Instruction::new(0x67, InstructionName::Ld(Direct8("H"), Direct8("A")), 1, 4, 4),
Instruction::new(0x68, InstructionName::Ld(Direct8("L"), Direct8("B")), 1, 4, 4),
Instruction::new(0x69, InstructionName::Ld(Direct8("L"), Direct8("C")), 1, 4, 4),
Instruction::new(0x6A, InstructionName::Ld(Direct8("L"), Direct8("D")), 1, 4, 4),
Instruction::new(0x6B, InstructionName::Ld(Direct8("L"), Direct8("E")), 1, 4, 4),
Instruction::new(0x6C, InstructionName::Ld(Direct8("L"), Direct8("H")), 1, 4, 4),
Instruction::new(0x6D, InstructionName::Ld(Direct8("L"), Direct8("L")), 1, 4, 4),
Instruction::new(0x6E, InstructionName::Ld(Direct8("L"), Indirect16("HL")), 1, 8, 8),
Instruction::new(0x6F, InstructionName::Ld(Direct8("L"), Direct8("A")), 1, 4, 4),
Instruction::new(0x70, InstructionName::Ld(Indirect16("HL"), Direct8("B")), 1, 8, 8),
Instruction::new(0x71, InstructionName::Ld(Indirect16("HL"), Direct8("C")), 1, 8, 8),
Instruction::new(0x72, InstructionName::Ld(Indirect16("HL"), Direct8("D")), 1, 8, 8),
Instruction::new(0x73, InstructionName::Ld(Indirect16("HL"), Direct8("E")), 1, 8, 8),
Instruction::new(0x74, InstructionName::Ld(Indirect16("HL"), Direct8("H")), 1, 8, 8),
Instruction::new(0x75, InstructionName::Ld(Indirect16("HL"), Direct8("L")), 1, 8, 8),
Instruction::new(0x76, InstructionName::Halt, 1, 4, 4),
Instruction::new(0x77, InstructionName::Ld(Indirect16("HL"), Direct8("A")), 1, 8, 8),
Instruction::new(0x78, InstructionName::Ld(Direct8("A"), Direct8("B")), 1, 4, 4),
Instruction::new(0x79, InstructionName::Ld(Direct8("A"), Direct8("C")), 1, 4, 4),
Instruction::new(0x7A, InstructionName::Ld(Direct8("A"), Direct8("D")), 1, 4, 4),
Instruction::new(0x7B, InstructionName::Ld(Direct8("A"), Direct8("E")), 1, 4, 4),
Instruction::new(0x7C, InstructionName::Ld(Direct8("A"), Direct8("H")), 1, 4, 4),
Instruction::new(0x7D, InstructionName::Ld(Direct8("A"), Direct8("L")), 1, 4, 4),
Instruction::new(0x7E, InstructionName::Ld(Direct8("A"), Indirect16("HL")), 1, 8, 8),
Instruction::new(0x7F, InstructionName::Ld(Direct8("A"), Direct8("A")), 1, 4, 4),
Instruction::new(0x80, InstructionName::Add(Direct8("A"), Direct8("B")), 1, 4, 4),
Instruction::new(0x81, InstructionName::Add(Direct8("A"), Direct8("C")), 1, 4, 4),
Instruction::new(0x82, InstructionName::Add(Direct8("A"), Direct8("D")), 1, 4, 4),
Instruction::new(0x83, InstructionName::Add(Direct8("A"), Direct8("E")), 1, 4, 4),
Instruction::new(0x84, InstructionName::Add(Direct8("A"), Direct8("H")), 1, 4, 4),
Instruction::new(0x85, InstructionName::Add(Direct8("A"), Direct8("L")), 1, 4, 4),
Instruction::new(0x86, InstructionName::Add(Direct8("A"), Indirect16("HL")), 1, 8, 8),
Instruction::new(0x87, InstructionName::Add(Direct8("A"), Direct8("A")), 1, 4, 4),
Instruction::new(0x88, InstructionName::Adc(Direct8("A"), Direct8("B")), 1, 4, 4),
Instruction::new(0x89, InstructionName::Adc(Direct8("A"), Direct8("C")), 1, 4, 4),
Instruction::new(0x8A, InstructionName::Adc(Direct8("A"), Direct8("D")), 1, 4, 4),
Instruction::new(0x8B, InstructionName::Adc(Direct8("A"), Direct8("E")), 1, 4, 4),
Instruction::new(0x8C, InstructionName::Adc(Direct8("A"), Direct8("H")), 1, 4, 4),
Instruction::new(0x8D, InstructionName::Adc(Direct8("A"), Direct8("L")), 1, 4, 4),
Instruction::new(0x8E, InstructionName::Adc(Direct8("A"), Indirect16("HL")), 1, 8, 8),
Instruction::new(0x8F, InstructionName::Adc(Direct8("A"), Direct8("A")), 1, 4, 4),
Instruction::new(0x90, InstructionName::Sub(Direct8("A"), Direct8("B")), 1, 4, 4),
Instruction::new(0x91, InstructionName::Sub(Direct8("A"), Direct8("C")), 1, 4, 4),
Instruction::new(0x92, InstructionName::Sub(Direct8("A"), Direct8("D")), 1, 4, 4),
Instruction::new(0x93, InstructionName::Sub(Direct8("A"), Direct8("E")), 1, 4, 4),
Instruction::new(0x94, InstructionName::Sub(Direct8("A"), Direct8("H")), 1, 4, 4),
Instruction::new(0x95, InstructionName::Sub(Direct8("A"), Direct8("L")), 1, 4, 4),
Instruction::new(0x96, InstructionName::Sub(Direct8("A"), Indirect16("HL")), 1, 8, 8),
Instruction::new(0x97, InstructionName::Sub(Direct8("A"), Direct8("A")), 1, 4, 4),
Instruction::new(0x98, InstructionName::Sbc(Direct8("A"), Direct8("B")), 1, 4, 4),
Instruction::new(0x99, InstructionName::Sbc(Direct8("A"), Direct8("C")), 1, 4, 4),
Instruction::new(0x9A, InstructionName::Sbc(Direct8("A"), Direct8("D")), 1, 4, 4),
Instruction::new(0x9B, InstructionName::Sbc(Direct8("A"), Direct8("E")), 1, 4, 4),
Instruction::new(0x9C, InstructionName::Sbc(Direct8("A"), Direct8("H")), 1, 4, 4),
Instruction::new(0x9D, InstructionName::Sbc(Direct8("A"), Direct8("L")), 1, 4, 4),
Instruction::new(0x9E, InstructionName::Sbc(Direct8("A"), Indirect16("HL")), 1, 8, 8),
Instruction::new(0x9F, InstructionName::Sbc(Direct8("A"), Direct8("A")), 1, 4, 4),
Instruction::new(0xA0, InstructionName::And(Direct8("A"), Direct8("B")), 1, 4, 4),
Instruction::new(0xA1, InstructionName::And(Direct8("A"), Direct8("C")), 1, 4, 4),
Instruction::new(0xA2, InstructionName::And(Direct8("A"), Direct8("D")), 1, 4, 4),
Instruction::new(0xA3, InstructionName::And(Direct8("A"), Direct8("E")), 1, 4, 4),
Instruction::new(0xA4, InstructionName::And(Direct8("A"), Direct8("H")), 1, 4, 4),
Instruction::new(0xA5, InstructionName::And(Direct8("A"), Direct8("L")), 1, 4, 4),
Instruction::new(0xA6, InstructionName::And(Direct8("A"), Indirect16("HL")), 1, 8, 8),
Instruction::new(0xA7, InstructionName::And(Direct8("A"), Direct8("A")), 1, 4, 4),
Instruction::new(0xA8, InstructionName::Xor(Direct8("A"), Direct8("B")), 1, 4, 4),
Instruction::new(0xA9, InstructionName::Xor(Direct8("A"), Direct8("C")), 1, 4, 4),
Instruction::new(0xAA, InstructionName::Xor(Direct8("A"), Direct8("D")), 1, 4, 4),
Instruction::new(0xAB, InstructionName::Xor(Direct8("A"), Direct8("E")), 1, 4, 4),
Instruction::new(0xAC, InstructionName::Xor(Direct8("A"), Direct8("H")), 1, 4, 4),
Instruction::new(0xAD, InstructionName::Xor(Direct8("A"), Direct8("L")), 1, 4, 4),
Instruction::new(0xAE, InstructionName::Xor(Direct8("A"), Indirect16("HL")), 1, 8, 8),
Instruction::new(0xAF, InstructionName::Xor(Direct8("A"), Direct8("A")), 1, 4, 4),
Instruction::new(0xB0, InstructionName::Or(Direct8("A"), Direct8("B")), 1, 4, 4),
Instruction::new(0xB1, InstructionName::Or(Direct8("A"), Direct8("C")), 1, 4, 4),
Instruction::new(0xB2, InstructionName::Or(Direct8("A"), Direct8("D")), 1, 4, 4),
Instruction::new(0xB3, InstructionName::Or(Direct8("A"), Direct8("E")), 1, 4, 4),
Instruction::new(0xB4, InstructionName::Or(Direct8("A"), Direct8("H")), 1, 4, 4),
Instruction::new(0xB5, InstructionName::Or(Direct8("A"), Direct8("L")), 1, 4, 4),
Instruction::new(0xB6, InstructionName::Or(Direct8("A"), Indirect16("HL")), 1, 8, 8),
Instruction::new(0xB7, InstructionName::Or(Direct8("A"), Direct8("A")), 1, 4, 4),
Instruction::new(0xB8, InstructionName::Cp(Direct8("A"), Direct8("B")), 1, 4, 4),
Instruction::new(0xB9, InstructionName::Cp(Direct8("A"), Direct8("C")), 1, 4, 4),
Instruction::new(0xBA, InstructionName::Cp(Direct8("A"), Direct8("D")), 1, 4, 4),
Instruction::new(0xBB, InstructionName::Cp(Direct8("A"), Direct8("E")), 1, 4, 4),
Instruction::new(0xBC, InstructionName::Cp(Direct8("A"), Direct8("H")), 1, 4, 4),
Instruction::new(0xBD, InstructionName::Cp(Direct8("A"), Direct8("L")), 1, 4, 4),
Instruction::new(0xBE, InstructionName::Cp(Direct8("A"), Indirect16("HL")), 1, 8, 8),
Instruction::new(0xBF, InstructionName::Cp(Direct8("A"), Direct8("A")), 1, 4, 4),
Instruction::new(0xC0, InstructionName::Ret, 1, 20, 8),
Instruction::new(0xC1, InstructionName::Pop(Direct16("BC")), 1, 12, 12),
Instruction::new(0xC2, InstructionName::Jp(Condition("NZ")), 3, 16, 12),
Instruction::new(0xC3, InstructionName::Jp(U16), 3, 16, 16),
Instruction::new(0xC4, InstructionName::Call(Condition("NZ")), 3, 24, 12),
Instruction::new(0xC5, InstructionName::Push(Direct16("BC")), 1, 16, 16),
Instruction::new(0xC6, InstructionName::Add(Direct8("A"), U8), 2, 8, 8),
Instruction::new(0xC7, InstructionName::Rst(Vec("00h")), 1, 16, 16),
Instruction::new(0xC8, InstructionName::Ret, 1, 20, 8),
Instruction::new(0xC9, InstructionName::Ret, 1, 16, 16),
Instruction::new(0xCA, InstructionName::Jp(Condition("Z")), 3, 16, 12),
Instruction::new(0xCC, InstructionName::Call(Condition("Z")), 3, 24, 12),
Instruction::new(0xCD, InstructionName::Call(U16), 3, 24, 24),
Instruction::new(0xCE, InstructionName::Adc(Direct8("A"), U8), 2, 8, 8),
Instruction::new(0xCF, InstructionName::Rst(Vec("08h")), 1, 16, 16),
Instruction::new(0xD0, InstructionName::Ret, 1, 20, 8),
Instruction::new(0xD1, InstructionName::Pop(Direct16("DE")), 1, 12, 12),
Instruction::new(0xD2, InstructionName::Jp(Condition("NC")), 3, 16, 12),
Instruction::new(0xD4, InstructionName::Call(Condition("NC")), 3, 24, 12),
Instruction::new(0xD5, InstructionName::Push(Direct16("DE")), 1, 16, 16),
Instruction::new(0xD6, InstructionName::Sub(Direct8("A"), U8), 2, 8, 8),
Instruction::new(0xD7, InstructionName::Rst(Vec("10h")), 1, 16, 16),
Instruction::new(0xD8, InstructionName::Ret, 1, 20, 8),
Instruction::new(0xD9, InstructionName::Reti, 1, 16, 16),
Instruction::new(0xDA, InstructionName::Jp(Condition("C")), 3, 16, 12),
Instruction::new(0xDC, InstructionName::Call(Condition("C")), 3, 24, 12),
Instruction::new(0xDE, InstructionName::Sbc(Direct8("A"), U8), 2, 8, 8),
Instruction::new(0xDF, InstructionName::Rst(Vec("18h")), 1, 16, 16),
Instruction::new(0xE0, InstructionName::Ld(IndirectFF00PlusU8, Direct8("A")), 2, 12, 12),
Instruction::new(0xE1, InstructionName::Pop(Direct16("HL")), 1, 12, 12),
Instruction::new(0xE2, InstructionName::Ld(IndirectFF00PlusC, Direct8("A")), 1, 8, 8),
Instruction::new(0xE5, InstructionName::Push(Direct16("HL")), 1, 16, 16),
Instruction::new(0xE6, InstructionName::And(Direct8("A"), U8), 2, 8, 8),
Instruction::new(0xE7, InstructionName::Rst(Vec("20h")), 1, 16, 16),
Instruction::new(0xE8, InstructionName::Add(Direct16("SP"), I8), 2, 16, 16),
Instruction::new(0xE9, InstructionName::Jp(Direct16("HL")), 1, 4, 4),
Instruction::new(0xEA, InstructionName::Ld(IndirectU16, Direct8("A")), 3, 16, 16),
Instruction::new(0xEE, InstructionName::Xor(Direct8("A"), U8), 2, 8, 8),
Instruction::new(0xEF, InstructionName::Rst(Vec("28h")), 1, 16, 16),
Instruction::new(0xF0, InstructionName::Ld(Direct8("A"), IndirectFF00PlusU8), 2, 12, 12),
Instruction::new(0xF1, InstructionName::Pop(Direct16("AF")), 1, 12, 12),
Instruction::new(0xF2, InstructionName::Ld(Direct8("A"), IndirectFF00PlusC), 1, 8, 8),
Instruction::new(0xF3, InstructionName::Di, 1, 4, 4),
Instruction::new(0xF5, InstructionName::Push(Direct16("AF")), 1, 16, 16),
Instruction::new(0xF6, InstructionName::Or(Direct8("A"), U8), 2, 8, 8),
Instruction::new(0xF7, InstructionName::Rst(Vec("30h")), 1, 16, 16),
Instruction::new(0xF8, InstructionName::Ld(Direct16("HL"), SpPlusI8), 2, 12, 12),
Instruction::new(0xF9, InstructionName::Ld(Direct16("SP"), Direct16("HL")), 1, 8, 8),
Instruction::new(0xFA, InstructionName::Ld(Direct8("A"), IndirectU16), 3, 16, 16),
Instruction::new(0xFB, InstructionName::Ei, 1, 4, 4),
Instruction::new(0xFE, InstructionName::Cp(Direct8("A"), U8), 2, 8, 8),
Instruction::new(0xFF, InstructionName::Rst(Vec("38h")), 1, 16, 16),Instruction::new(0xCB00, InstructionName::Rlc(Direct8("B")), 2, 8, 8),
Instruction::new(0xCB01, InstructionName::Rlc(Direct8("C")), 2, 8, 8),
Instruction::new(0xCB02, InstructionName::Rlc(Direct8("D")), 2, 8, 8),
Instruction::new(0xCB03, InstructionName::Rlc(Direct8("E")), 2, 8, 8),
Instruction::new(0xCB04, InstructionName::Rlc(Direct8("H")), 2, 8, 8),
Instruction::new(0xCB05, InstructionName::Rlc(Direct8("L")), 2, 8, 8),
Instruction::new(0xCB06, InstructionName::Rlc(Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCB07, InstructionName::Rlc(Direct8("A")), 2, 8, 8),
Instruction::new(0xCB08, InstructionName::Rrc(Direct8("B")), 2, 8, 8),
Instruction::new(0xCB09, InstructionName::Rrc(Direct8("C")), 2, 8, 8),
Instruction::new(0xCB0A, InstructionName::Rrc(Direct8("D")), 2, 8, 8),
Instruction::new(0xCB0B, InstructionName::Rrc(Direct8("E")), 2, 8, 8),
Instruction::new(0xCB0C, InstructionName::Rrc(Direct8("H")), 2, 8, 8),
Instruction::new(0xCB0D, InstructionName::Rrc(Direct8("L")), 2, 8, 8),
Instruction::new(0xCB0E, InstructionName::Rrc(Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCB0F, InstructionName::Rrc(Direct8("A")), 2, 8, 8),
Instruction::new(0xCB10, InstructionName::Rl(Direct8("B")), 2, 8, 8),
Instruction::new(0xCB11, InstructionName::Rl(Direct8("C")), 2, 8, 8),
Instruction::new(0xCB12, InstructionName::Rl(Direct8("D")), 2, 8, 8),
Instruction::new(0xCB13, InstructionName::Rl(Direct8("E")), 2, 8, 8),
Instruction::new(0xCB14, InstructionName::Rl(Direct8("H")), 2, 8, 8),
Instruction::new(0xCB15, InstructionName::Rl(Direct8("L")), 2, 8, 8),
Instruction::new(0xCB16, InstructionName::Rl(Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCB17, InstructionName::Rl(Direct8("A")), 2, 8, 8),
Instruction::new(0xCB18, InstructionName::Rr(Direct8("B")), 2, 8, 8),
Instruction::new(0xCB19, InstructionName::Rr(Direct8("C")), 2, 8, 8),
Instruction::new(0xCB1A, InstructionName::Rr(Direct8("D")), 2, 8, 8),
Instruction::new(0xCB1B, InstructionName::Rr(Direct8("E")), 2, 8, 8),
Instruction::new(0xCB1C, InstructionName::Rr(Direct8("H")), 2, 8, 8),
Instruction::new(0xCB1D, InstructionName::Rr(Direct8("L")), 2, 8, 8),
Instruction::new(0xCB1E, InstructionName::Rr(Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCB1F, InstructionName::Rr(Direct8("A")), 2, 8, 8),
Instruction::new(0xCB20, InstructionName::Sla(Direct8("B")), 2, 8, 8),
Instruction::new(0xCB21, InstructionName::Sla(Direct8("C")), 2, 8, 8),
Instruction::new(0xCB22, InstructionName::Sla(Direct8("D")), 2, 8, 8),
Instruction::new(0xCB23, InstructionName::Sla(Direct8("E")), 2, 8, 8),
Instruction::new(0xCB24, InstructionName::Sla(Direct8("H")), 2, 8, 8),
Instruction::new(0xCB25, InstructionName::Sla(Direct8("L")), 2, 8, 8),
Instruction::new(0xCB26, InstructionName::Sla(Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCB27, InstructionName::Sla(Direct8("A")), 2, 8, 8),
Instruction::new(0xCB28, InstructionName::Sra(Direct8("B")), 2, 8, 8),
Instruction::new(0xCB29, InstructionName::Sra(Direct8("C")), 2, 8, 8),
Instruction::new(0xCB2A, InstructionName::Sra(Direct8("D")), 2, 8, 8),
Instruction::new(0xCB2B, InstructionName::Sra(Direct8("E")), 2, 8, 8),
Instruction::new(0xCB2C, InstructionName::Sra(Direct8("H")), 2, 8, 8),
Instruction::new(0xCB2D, InstructionName::Sra(Direct8("L")), 2, 8, 8),
Instruction::new(0xCB2E, InstructionName::Sra(Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCB2F, InstructionName::Sra(Direct8("A")), 2, 8, 8),
Instruction::new(0xCB30, InstructionName::Swap(Direct8("B")), 2, 8, 8),
Instruction::new(0xCB31, InstructionName::Swap(Direct8("C")), 2, 8, 8),
Instruction::new(0xCB32, InstructionName::Swap(Direct8("D")), 2, 8, 8),
Instruction::new(0xCB33, InstructionName::Swap(Direct8("E")), 2, 8, 8),
Instruction::new(0xCB34, InstructionName::Swap(Direct8("H")), 2, 8, 8),
Instruction::new(0xCB35, InstructionName::Swap(Direct8("L")), 2, 8, 8),
Instruction::new(0xCB36, InstructionName::Swap(Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCB37, InstructionName::Swap(Direct8("A")), 2, 8, 8),
Instruction::new(0xCB38, InstructionName::Srl(Direct8("B")), 2, 8, 8),
Instruction::new(0xCB39, InstructionName::Srl(Direct8("C")), 2, 8, 8),
Instruction::new(0xCB3A, InstructionName::Srl(Direct8("D")), 2, 8, 8),
Instruction::new(0xCB3B, InstructionName::Srl(Direct8("E")), 2, 8, 8),
Instruction::new(0xCB3C, InstructionName::Srl(Direct8("H")), 2, 8, 8),
Instruction::new(0xCB3D, InstructionName::Srl(Direct8("L")), 2, 8, 8),
Instruction::new(0xCB3E, InstructionName::Srl(Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCB3F, InstructionName::Srl(Direct8("A")), 2, 8, 8),
Instruction::new(0xCB40, InstructionName::Bit(Literal(0), Direct8("B")), 2, 8, 8),
Instruction::new(0xCB41, InstructionName::Bit(Literal(0), Direct8("C")), 2, 8, 8),
Instruction::new(0xCB42, InstructionName::Bit(Literal(0), Direct8("D")), 2, 8, 8),
Instruction::new(0xCB43, InstructionName::Bit(Literal(0), Direct8("E")), 2, 8, 8),
Instruction::new(0xCB44, InstructionName::Bit(Literal(0), Direct8("H")), 2, 8, 8),
Instruction::new(0xCB45, InstructionName::Bit(Literal(0), Direct8("L")), 2, 8, 8),
Instruction::new(0xCB46, InstructionName::Bit(Literal(0), Indirect16("HL")), 2, 12, 12),
Instruction::new(0xCB47, InstructionName::Bit(Literal(0), Direct8("A")), 2, 8, 8),
Instruction::new(0xCB48, InstructionName::Bit(Literal(1), Direct8("B")), 2, 8, 8),
Instruction::new(0xCB49, InstructionName::Bit(Literal(1), Direct8("C")), 2, 8, 8),
Instruction::new(0xCB4A, InstructionName::Bit(Literal(1), Direct8("D")), 2, 8, 8),
Instruction::new(0xCB4B, InstructionName::Bit(Literal(1), Direct8("E")), 2, 8, 8),
Instruction::new(0xCB4C, InstructionName::Bit(Literal(1), Direct8("H")), 2, 8, 8),
Instruction::new(0xCB4D, InstructionName::Bit(Literal(1), Direct8("L")), 2, 8, 8),
Instruction::new(0xCB4E, InstructionName::Bit(Literal(1), Indirect16("HL")), 2, 12, 12),
Instruction::new(0xCB4F, InstructionName::Bit(Literal(1), Direct8("A")), 2, 8, 8),
Instruction::new(0xCB50, InstructionName::Bit(Literal(2), Direct8("B")), 2, 8, 8),
Instruction::new(0xCB51, InstructionName::Bit(Literal(2), Direct8("C")), 2, 8, 8),
Instruction::new(0xCB52, InstructionName::Bit(Literal(2), Direct8("D")), 2, 8, 8),
Instruction::new(0xCB53, InstructionName::Bit(Literal(2), Direct8("E")), 2, 8, 8),
Instruction::new(0xCB54, InstructionName::Bit(Literal(2), Direct8("H")), 2, 8, 8),
Instruction::new(0xCB55, InstructionName::Bit(Literal(2), Direct8("L")), 2, 8, 8),
Instruction::new(0xCB56, InstructionName::Bit(Literal(2), Indirect16("HL")), 2, 12, 12),
Instruction::new(0xCB57, InstructionName::Bit(Literal(2), Direct8("A")), 2, 8, 8),
Instruction::new(0xCB58, InstructionName::Bit(Literal(3), Direct8("B")), 2, 8, 8),
Instruction::new(0xCB59, InstructionName::Bit(Literal(3), Direct8("C")), 2, 8, 8),
Instruction::new(0xCB5A, InstructionName::Bit(Literal(3), Direct8("D")), 2, 8, 8),
Instruction::new(0xCB5B, InstructionName::Bit(Literal(3), Direct8("E")), 2, 8, 8),
Instruction::new(0xCB5C, InstructionName::Bit(Literal(3), Direct8("H")), 2, 8, 8),
Instruction::new(0xCB5D, InstructionName::Bit(Literal(3), Direct8("L")), 2, 8, 8),
Instruction::new(0xCB5E, InstructionName::Bit(Literal(3), Indirect16("HL")), 2, 12, 12),
Instruction::new(0xCB5F, InstructionName::Bit(Literal(3), Direct8("A")), 2, 8, 8),
Instruction::new(0xCB60, InstructionName::Bit(Literal(4), Direct8("B")), 2, 8, 8),
Instruction::new(0xCB61, InstructionName::Bit(Literal(4), Direct8("C")), 2, 8, 8),
Instruction::new(0xCB62, InstructionName::Bit(Literal(4), Direct8("D")), 2, 8, 8),
Instruction::new(0xCB63, InstructionName::Bit(Literal(4), Direct8("E")), 2, 8, 8),
Instruction::new(0xCB64, InstructionName::Bit(Literal(4), Direct8("H")), 2, 8, 8),
Instruction::new(0xCB65, InstructionName::Bit(Literal(4), Direct8("L")), 2, 8, 8),
Instruction::new(0xCB66, InstructionName::Bit(Literal(4), Indirect16("HL")), 2, 12, 12),
Instruction::new(0xCB67, InstructionName::Bit(Literal(4), Direct8("A")), 2, 8, 8),
Instruction::new(0xCB68, InstructionName::Bit(Literal(5), Direct8("B")), 2, 8, 8),
Instruction::new(0xCB69, InstructionName::Bit(Literal(5), Direct8("C")), 2, 8, 8),
Instruction::new(0xCB6A, InstructionName::Bit(Literal(5), Direct8("D")), 2, 8, 8),
Instruction::new(0xCB6B, InstructionName::Bit(Literal(5), Direct8("E")), 2, 8, 8),
Instruction::new(0xCB6C, InstructionName::Bit(Literal(5), Direct8("H")), 2, 8, 8),
Instruction::new(0xCB6D, InstructionName::Bit(Literal(5), Direct8("L")), 2, 8, 8),
Instruction::new(0xCB6E, InstructionName::Bit(Literal(5), Indirect16("HL")), 2, 12, 12),
Instruction::new(0xCB6F, InstructionName::Bit(Literal(5), Direct8("A")), 2, 8, 8),
Instruction::new(0xCB70, InstructionName::Bit(Literal(6), Direct8("B")), 2, 8, 8),
Instruction::new(0xCB71, InstructionName::Bit(Literal(6), Direct8("C")), 2, 8, 8),
Instruction::new(0xCB72, InstructionName::Bit(Literal(6), Direct8("D")), 2, 8, 8),
Instruction::new(0xCB73, InstructionName::Bit(Literal(6), Direct8("E")), 2, 8, 8),
Instruction::new(0xCB74, InstructionName::Bit(Literal(6), Direct8("H")), 2, 8, 8),
Instruction::new(0xCB75, InstructionName::Bit(Literal(6), Direct8("L")), 2, 8, 8),
Instruction::new(0xCB76, InstructionName::Bit(Literal(6), Indirect16("HL")), 2, 12, 12),
Instruction::new(0xCB77, InstructionName::Bit(Literal(6), Direct8("A")), 2, 8, 8),
Instruction::new(0xCB78, InstructionName::Bit(Literal(7), Direct8("B")), 2, 8, 8),
Instruction::new(0xCB79, InstructionName::Bit(Literal(7), Direct8("C")), 2, 8, 8),
Instruction::new(0xCB7A, InstructionName::Bit(Literal(7), Direct8("D")), 2, 8, 8),
Instruction::new(0xCB7B, InstructionName::Bit(Literal(7), Direct8("E")), 2, 8, 8),
Instruction::new(0xCB7C, InstructionName::Bit(Literal(7), Direct8("H")), 2, 8, 8),
Instruction::new(0xCB7D, InstructionName::Bit(Literal(7), Direct8("L")), 2, 8, 8),
Instruction::new(0xCB7E, InstructionName::Bit(Literal(7), Indirect16("HL")), 2, 12, 12),
Instruction::new(0xCB7F, InstructionName::Bit(Literal(7), Direct8("A")), 2, 8, 8),
Instruction::new(0xCB80, InstructionName::Res(Literal(0), Direct8("B")), 2, 8, 8),
Instruction::new(0xCB81, InstructionName::Res(Literal(0), Direct8("C")), 2, 8, 8),
Instruction::new(0xCB82, InstructionName::Res(Literal(0), Direct8("D")), 2, 8, 8),
Instruction::new(0xCB83, InstructionName::Res(Literal(0), Direct8("E")), 2, 8, 8),
Instruction::new(0xCB84, InstructionName::Res(Literal(0), Direct8("H")), 2, 8, 8),
Instruction::new(0xCB85, InstructionName::Res(Literal(0), Direct8("L")), 2, 8, 8),
Instruction::new(0xCB86, InstructionName::Res(Literal(0), Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCB87, InstructionName::Res(Literal(0), Direct8("A")), 2, 8, 8),
Instruction::new(0xCB88, InstructionName::Res(Literal(1), Direct8("B")), 2, 8, 8),
Instruction::new(0xCB89, InstructionName::Res(Literal(1), Direct8("C")), 2, 8, 8),
Instruction::new(0xCB8A, InstructionName::Res(Literal(1), Direct8("D")), 2, 8, 8),
Instruction::new(0xCB8B, InstructionName::Res(Literal(1), Direct8("E")), 2, 8, 8),
Instruction::new(0xCB8C, InstructionName::Res(Literal(1), Direct8("H")), 2, 8, 8),
Instruction::new(0xCB8D, InstructionName::Res(Literal(1), Direct8("L")), 2, 8, 8),
Instruction::new(0xCB8E, InstructionName::Res(Literal(1), Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCB8F, InstructionName::Res(Literal(1), Direct8("A")), 2, 8, 8),
Instruction::new(0xCB90, InstructionName::Res(Literal(2), Direct8("B")), 2, 8, 8),
Instruction::new(0xCB91, InstructionName::Res(Literal(2), Direct8("C")), 2, 8, 8),
Instruction::new(0xCB92, InstructionName::Res(Literal(2), Direct8("D")), 2, 8, 8),
Instruction::new(0xCB93, InstructionName::Res(Literal(2), Direct8("E")), 2, 8, 8),
Instruction::new(0xCB94, InstructionName::Res(Literal(2), Direct8("H")), 2, 8, 8),
Instruction::new(0xCB95, InstructionName::Res(Literal(2), Direct8("L")), 2, 8, 8),
Instruction::new(0xCB96, InstructionName::Res(Literal(2), Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCB97, InstructionName::Res(Literal(2), Direct8("A")), 2, 8, 8),
Instruction::new(0xCB98, InstructionName::Res(Literal(3), Direct8("B")), 2, 8, 8),
Instruction::new(0xCB99, InstructionName::Res(Literal(3), Direct8("C")), 2, 8, 8),
Instruction::new(0xCB9A, InstructionName::Res(Literal(3), Direct8("D")), 2, 8, 8),
Instruction::new(0xCB9B, InstructionName::Res(Literal(3), Direct8("E")), 2, 8, 8),
Instruction::new(0xCB9C, InstructionName::Res(Literal(3), Direct8("H")), 2, 8, 8),
Instruction::new(0xCB9D, InstructionName::Res(Literal(3), Direct8("L")), 2, 8, 8),
Instruction::new(0xCB9E, InstructionName::Res(Literal(3), Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCB9F, InstructionName::Res(Literal(3), Direct8("A")), 2, 8, 8),
Instruction::new(0xCBA0, InstructionName::Res(Literal(4), Direct8("B")), 2, 8, 8),
Instruction::new(0xCBA1, InstructionName::Res(Literal(4), Direct8("C")), 2, 8, 8),
Instruction::new(0xCBA2, InstructionName::Res(Literal(4), Direct8("D")), 2, 8, 8),
Instruction::new(0xCBA3, InstructionName::Res(Literal(4), Direct8("E")), 2, 8, 8),
Instruction::new(0xCBA4, InstructionName::Res(Literal(4), Direct8("H")), 2, 8, 8),
Instruction::new(0xCBA5, InstructionName::Res(Literal(4), Direct8("L")), 2, 8, 8),
Instruction::new(0xCBA6, InstructionName::Res(Literal(4), Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCBA7, InstructionName::Res(Literal(4), Direct8("A")), 2, 8, 8),
Instruction::new(0xCBA8, InstructionName::Res(Literal(5), Direct8("B")), 2, 8, 8),
Instruction::new(0xCBA9, InstructionName::Res(Literal(5), Direct8("C")), 2, 8, 8),
Instruction::new(0xCBAA, InstructionName::Res(Literal(5), Direct8("D")), 2, 8, 8),
Instruction::new(0xCBAB, InstructionName::Res(Literal(5), Direct8("E")), 2, 8, 8),
Instruction::new(0xCBAC, InstructionName::Res(Literal(5), Direct8("H")), 2, 8, 8),
Instruction::new(0xCBAD, InstructionName::Res(Literal(5), Direct8("L")), 2, 8, 8),
Instruction::new(0xCBAE, InstructionName::Res(Literal(5), Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCBAF, InstructionName::Res(Literal(5), Direct8("A")), 2, 8, 8),
Instruction::new(0xCBB0, InstructionName::Res(Literal(6), Direct8("B")), 2, 8, 8),
Instruction::new(0xCBB1, InstructionName::Res(Literal(6), Direct8("C")), 2, 8, 8),
Instruction::new(0xCBB2, InstructionName::Res(Literal(6), Direct8("D")), 2, 8, 8),
Instruction::new(0xCBB3, InstructionName::Res(Literal(6), Direct8("E")), 2, 8, 8),
Instruction::new(0xCBB4, InstructionName::Res(Literal(6), Direct8("H")), 2, 8, 8),
Instruction::new(0xCBB5, InstructionName::Res(Literal(6), Direct8("L")), 2, 8, 8),
Instruction::new(0xCBB6, InstructionName::Res(Literal(6), Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCBB7, InstructionName::Res(Literal(6), Direct8("A")), 2, 8, 8),
Instruction::new(0xCBB8, InstructionName::Res(Literal(7), Direct8("B")), 2, 8, 8),
Instruction::new(0xCBB9, InstructionName::Res(Literal(7), Direct8("C")), 2, 8, 8),
Instruction::new(0xCBBA, InstructionName::Res(Literal(7), Direct8("D")), 2, 8, 8),
Instruction::new(0xCBBB, InstructionName::Res(Literal(7), Direct8("E")), 2, 8, 8),
Instruction::new(0xCBBC, InstructionName::Res(Literal(7), Direct8("H")), 2, 8, 8),
Instruction::new(0xCBBD, InstructionName::Res(Literal(7), Direct8("L")), 2, 8, 8),
Instruction::new(0xCBBE, InstructionName::Res(Literal(7), Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCBBF, InstructionName::Res(Literal(7), Direct8("A")), 2, 8, 8),
Instruction::new(0xCBC0, InstructionName::Set(Literal(0), Direct8("B")), 2, 8, 8),
Instruction::new(0xCBC1, InstructionName::Set(Literal(0), Direct8("C")), 2, 8, 8),
Instruction::new(0xCBC2, InstructionName::Set(Literal(0), Direct8("D")), 2, 8, 8),
Instruction::new(0xCBC3, InstructionName::Set(Literal(0), Direct8("E")), 2, 8, 8),
Instruction::new(0xCBC4, InstructionName::Set(Literal(0), Direct8("H")), 2, 8, 8),
Instruction::new(0xCBC5, InstructionName::Set(Literal(0), Direct8("L")), 2, 8, 8),
Instruction::new(0xCBC6, InstructionName::Set(Literal(0), Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCBC7, InstructionName::Set(Literal(0), Direct8("A")), 2, 8, 8),
Instruction::new(0xCBC8, InstructionName::Set(Literal(1), Direct8("B")), 2, 8, 8),
Instruction::new(0xCBC9, InstructionName::Set(Literal(1), Direct8("C")), 2, 8, 8),
Instruction::new(0xCBCA, InstructionName::Set(Literal(1), Direct8("D")), 2, 8, 8),
Instruction::new(0xCBCB, InstructionName::Set(Literal(1), Direct8("E")), 2, 8, 8),
Instruction::new(0xCBCC, InstructionName::Set(Literal(1), Direct8("H")), 2, 8, 8),
Instruction::new(0xCBCD, InstructionName::Set(Literal(1), Direct8("L")), 2, 8, 8),
Instruction::new(0xCBCE, InstructionName::Set(Literal(1), Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCBCF, InstructionName::Set(Literal(1), Direct8("A")), 2, 8, 8),
Instruction::new(0xCBD0, InstructionName::Set(Literal(2), Direct8("B")), 2, 8, 8),
Instruction::new(0xCBD1, InstructionName::Set(Literal(2), Direct8("C")), 2, 8, 8),
Instruction::new(0xCBD2, InstructionName::Set(Literal(2), Direct8("D")), 2, 8, 8),
Instruction::new(0xCBD3, InstructionName::Set(Literal(2), Direct8("E")), 2, 8, 8),
Instruction::new(0xCBD4, InstructionName::Set(Literal(2), Direct8("H")), 2, 8, 8),
Instruction::new(0xCBD5, InstructionName::Set(Literal(2), Direct8("L")), 2, 8, 8),
Instruction::new(0xCBD6, InstructionName::Set(Literal(2), Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCBD7, InstructionName::Set(Literal(2), Direct8("A")), 2, 8, 8),
Instruction::new(0xCBD8, InstructionName::Set(Literal(3), Direct8("B")), 2, 8, 8),
Instruction::new(0xCBD9, InstructionName::Set(Literal(3), Direct8("C")), 2, 8, 8),
Instruction::new(0xCBDA, InstructionName::Set(Literal(3), Direct8("D")), 2, 8, 8),
Instruction::new(0xCBDB, InstructionName::Set(Literal(3), Direct8("E")), 2, 8, 8),
Instruction::new(0xCBDC, InstructionName::Set(Literal(3), Direct8("H")), 2, 8, 8),
Instruction::new(0xCBDD, InstructionName::Set(Literal(3), Direct8("L")), 2, 8, 8),
Instruction::new(0xCBDE, InstructionName::Set(Literal(3), Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCBDF, InstructionName::Set(Literal(3), Direct8("A")), 2, 8, 8),
Instruction::new(0xCBE0, InstructionName::Set(Literal(4), Direct8("B")), 2, 8, 8),
Instruction::new(0xCBE1, InstructionName::Set(Literal(4), Direct8("C")), 2, 8, 8),
Instruction::new(0xCBE2, InstructionName::Set(Literal(4), Direct8("D")), 2, 8, 8),
Instruction::new(0xCBE3, InstructionName::Set(Literal(4), Direct8("E")), 2, 8, 8),
Instruction::new(0xCBE4, InstructionName::Set(Literal(4), Direct8("H")), 2, 8, 8),
Instruction::new(0xCBE5, InstructionName::Set(Literal(4), Direct8("L")), 2, 8, 8),
Instruction::new(0xCBE6, InstructionName::Set(Literal(4), Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCBE7, InstructionName::Set(Literal(4), Direct8("A")), 2, 8, 8),
Instruction::new(0xCBE8, InstructionName::Set(Literal(5), Direct8("B")), 2, 8, 8),
Instruction::new(0xCBE9, InstructionName::Set(Literal(5), Direct8("C")), 2, 8, 8),
Instruction::new(0xCBEA, InstructionName::Set(Literal(5), Direct8("D")), 2, 8, 8),
Instruction::new(0xCBEB, InstructionName::Set(Literal(5), Direct8("E")), 2, 8, 8),
Instruction::new(0xCBEC, InstructionName::Set(Literal(5), Direct8("H")), 2, 8, 8),
Instruction::new(0xCBED, InstructionName::Set(Literal(5), Direct8("L")), 2, 8, 8),
Instruction::new(0xCBEE, InstructionName::Set(Literal(5), Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCBEF, InstructionName::Set(Literal(5), Direct8("A")), 2, 8, 8),
Instruction::new(0xCBF0, InstructionName::Set(Literal(6), Direct8("B")), 2, 8, 8),
Instruction::new(0xCBF1, InstructionName::Set(Literal(6), Direct8("C")), 2, 8, 8),
Instruction::new(0xCBF2, InstructionName::Set(Literal(6), Direct8("D")), 2, 8, 8),
Instruction::new(0xCBF3, InstructionName::Set(Literal(6), Direct8("E")), 2, 8, 8),
Instruction::new(0xCBF4, InstructionName::Set(Literal(6), Direct8("H")), 2, 8, 8),
Instruction::new(0xCBF5, InstructionName::Set(Literal(6), Direct8("L")), 2, 8, 8),
Instruction::new(0xCBF6, InstructionName::Set(Literal(6), Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCBF7, InstructionName::Set(Literal(6), Direct8("A")), 2, 8, 8),
Instruction::new(0xCBF8, InstructionName::Set(Literal(7), Direct8("B")), 2, 8, 8),
Instruction::new(0xCBF9, InstructionName::Set(Literal(7), Direct8("C")), 2, 8, 8),
Instruction::new(0xCBFA, InstructionName::Set(Literal(7), Direct8("D")), 2, 8, 8),
Instruction::new(0xCBFB, InstructionName::Set(Literal(7), Direct8("E")), 2, 8, 8),
Instruction::new(0xCBFC, InstructionName::Set(Literal(7), Direct8("H")), 2, 8, 8),
Instruction::new(0xCBFD, InstructionName::Set(Literal(7), Direct8("L")), 2, 8, 8),
Instruction::new(0xCBFE, InstructionName::Set(Literal(7), Indirect16("HL")), 2, 16, 16),
Instruction::new(0xCBFF, InstructionName::Set(Literal(7), Direct8("A")), 2, 8, 8),]
}