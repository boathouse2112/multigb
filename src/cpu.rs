use crate::{
    instruction::{Instruction, InstructionSpecification},
    parser::{self, Memory},
};

#[derive(Clone, Copy)]
pub enum Register8Bit {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
}

#[derive(Clone, Copy)]
pub enum Register16Bit {
    AF,
    BC,
    DE,
    HL,
    PC,
    SP,
}

#[derive(Clone, Copy)]
/// Register flags. u8 is the bit-position of the flag in the F register.
pub enum Flag {
    Z = 7, // Zero flag
    N = 6, // Subtraction flag
    H = 5, // Half carry flag
    C = 4, // Carry flag
}

#[derive(Clone, Copy)]
struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
    pc: u16,
    sp: u16,
}

pub struct Cpu {
    registers: Registers,
}

impl Cpu {
    pub fn get_register_8_bit(&self, register: Register8Bit) -> u8 {
        match register {
            Register8Bit::A => self.registers.a,
            Register8Bit::B => self.registers.b,
            Register8Bit::C => self.registers.c,
            Register8Bit::D => self.registers.d,
            Register8Bit::E => self.registers.e,
            Register8Bit::F => self.registers.f,
            Register8Bit::H => self.registers.h,
            Register8Bit::L => self.registers.l,
        }
    }

    pub fn set_register_8_bit(&mut self, register: Register8Bit, value: u8) {
        match register {
            Register8Bit::A => self.registers.a = value,
            Register8Bit::B => self.registers.b = value,
            Register8Bit::C => self.registers.c = value,
            Register8Bit::D => self.registers.d = value,
            Register8Bit::E => self.registers.e = value,
            Register8Bit::F => self.registers.f = value,
            Register8Bit::H => self.registers.h = value,
            Register8Bit::L => self.registers.l = value,
        }
    }

    pub fn get_register_16_bit(&self, register: Register16Bit) -> u16 {
        match register {
            Register16Bit::AF => self.get_virtual_register(Register8Bit::A, Register8Bit::F),
            Register16Bit::BC => self.get_virtual_register(Register8Bit::B, Register8Bit::C),
            Register16Bit::DE => self.get_virtual_register(Register8Bit::D, Register8Bit::E),
            Register16Bit::HL => self.get_virtual_register(Register8Bit::H, Register8Bit::L),
            Register16Bit::PC => self.registers.pc,
            Register16Bit::SP => self.registers.sp,
        }
    }

    pub fn set_register_16_bit(&mut self, register: Register16Bit, value: u16) {
        match register {
            Register16Bit::AF => self.set_virtual_register(Register8Bit::A, Register8Bit::F, value),
            Register16Bit::BC => self.set_virtual_register(Register8Bit::B, Register8Bit::C, value),
            Register16Bit::DE => self.set_virtual_register(Register8Bit::D, Register8Bit::E, value),
            Register16Bit::HL => self.set_virtual_register(Register8Bit::H, Register8Bit::L, value),
            Register16Bit::PC => self.registers.pc = value,
            Register16Bit::SP => self.registers.sp = value,
        }
    }

    pub fn get_flag(&self, flag: Flag) -> bool {
        let bit_position = flag as u8;
        let flag_val = self.get_register_8_bit(Register8Bit::F);
        let flag_bit = (flag_val >> bit_position) & 1;
        flag_bit != 0
    }

    pub fn set_flag(&mut self, flag: Flag, value: bool) {
        let bit_value: u8 = if value { 1 } else { 0 };
        let bit_position = flag as u8;
        let flag_mask = bit_value << bit_position;
        let f_register_value = self.get_register_8_bit(Register8Bit::F);
        self.set_register_8_bit(Register8Bit::F, f_register_value | flag_mask);
    }

    fn get_virtual_register(&self, first: Register8Bit, second: Register8Bit) -> u16 {
        let first_val = self.get_register_8_bit(first);
        let second_val = self.get_register_8_bit(second);

        ((first_val as u16) << 8) + (second_val as u16)
    }

    fn set_virtual_register(&mut self, first: Register8Bit, second: Register8Bit, value: u16) {
        let first_val = (value & 0x0F) as u8;
        let second_val = (value & 0xF0) as u8;
        self.set_register_8_bit(first, first_val);
        self.set_register_8_bit(second, second_val);
    }
}

/// Reads an opcode from memory, starting at the given index.
pub fn read_instruction<'a>(
    instruction_specs: &'a Vec<InstructionSpecification>,
    memory: Memory<'a>,
    index: usize,
) -> Result<(usize, Instruction<'a>), String> {
    // println!("Read instruction");
    parser::any_instruction(instruction_specs)(memory, index)
}
