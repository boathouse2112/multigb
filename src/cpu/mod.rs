use crate::bus;
use crate::console::Console;
use crate::cpu::flags::Flags;
use crate::instruction::{Condition, Instruction, InstructionName};
use enum_map::EnumMap;
use enum_map::{enum_map, Enum};
use std::mem;

mod flags;

#[derive(Enum, Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Register8 {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
}

#[derive(Enum, Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Register16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

#[derive(Clone, Debug)]
pub struct Cpu {
    registers_8: EnumMap<Register8, u8>, // All 8-bit registers except F
    pub pc: u16,
    pub sp: u16,
    pub flags: Flags,
    pub ime: bool,
}

impl Cpu {
    pub fn new() -> Self {
        Cpu {
            registers_8: enum_map! {
                Register8::A => 0,
                Register8::B => 0,
                Register8::C => 0,
                Register8::D => 0,
                Register8::E => 0,
                Register8::H => 0,
                Register8::L => 0,
                Register8::F => 0, // Unused TODO: Move flags into here somehow? flags::interpret()?
            },
            pc: 0,
            sp: 0,
            flags: Flags::new(),
            ime: false,
        }
    }

    pub fn register_8(&self, register: Register8) -> u8 {
        self.registers_8[register]
    }

    pub fn set_register_8(&mut self, register: Register8, value: u8) {
        match register {
            Register8::F => self.flags = Flags::from(value),
            _ => self.registers_8[register] = value,
        }
    }

    pub fn register_16(&self, register: Register16) -> u16 {
        match register {
            Register16::AF => Cpu::join(self.register_8(Register8::A), self.flags.bits()),
            Register16::BC => {
                Cpu::join(self.register_8(Register8::B), self.register_8(Register8::C))
            }
            Register16::DE => {
                Cpu::join(self.register_8(Register8::D), self.register_8(Register8::E))
            }
            Register16::HL => {
                Cpu::join(self.register_8(Register8::H), self.register_8(Register8::L))
            }
            Register16::SP => self.sp,
            Register16::PC => self.pc,
        }
    }

    pub fn set_register_16(&mut self, register: Register16, value: u16) {
        let high_byte = ((value & 0xFF00) >> 8) as u8;
        let low_byte = (value & 0x00FF) as u8;
        match register {
            Register16::AF => {
                self.set_register_8(Register8::A, high_byte);
                self.set_register_8(Register8::F, low_byte);
            }
            Register16::BC => {
                self.set_register_8(Register8::B, high_byte);
                self.set_register_8(Register8::C, low_byte);
            }
            Register16::DE => {
                self.set_register_8(Register8::D, high_byte);
                self.set_register_8(Register8::E, low_byte);
            }
            Register16::HL => {
                self.set_register_8(Register8::H, high_byte);
                self.set_register_8(Register8::L, low_byte);
            }
            Register16::SP => self.sp = value,
            Register16::PC => self.pc = value,
        }
    }

    /// Join two u8 registers into a u16 register
    fn join(first: u8, second: u8) -> u16 {
        ((first as u16) << 8) + (second as u16)
    }
}

pub fn stack_pop_16(console: &mut Console) -> u16 {
    let value = bus::read_u16(console, console.cpu.sp);
    console.cpu.sp += 2;
    value
}

pub fn stack_push_16(console: &mut Console, value: u16) {
    console.cpu.sp -= 2;
    bus::write_u16(console, console.cpu.sp, value);
}

/// Step the CPU forward 1 instruction
pub fn step(instructions: &mut Vec<Instruction>, console: &mut Console) {
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
        .iter_mut()
        .find(|instr| instr.opcode == opcode)
        .unwrap_or_else(|| panic!("Opcode 0x{:X} has an associated instruction.", opcode));

    let instruction = mem::replace(instruction, instruction.clone());

    /// Test the given condition with the current console
    fn test_condition(console: &mut Console, condition: Condition) -> bool {
        match condition {
            Condition::Carry => console.cpu.flags.carry,
            Condition::NotCarry => !console.cpu.flags.carry,
            Condition::Zero => console.cpu.flags.zero,
            Condition::NotZero => !console.cpu.flags.zero,
        }
    }

    // Run instruction
    match instruction.name {
        InstructionName::Adc(lhs_reader, rhs_reader) => {
            let (lhs_writer, lhs) = lhs_reader.read_into_writer(console);
            let rhs = rhs_reader.read(console);

            let old_carry = if console.cpu.flags.carry { 1 } else { 0 };
            let (result, overflow_1) = lhs.overflowing_add(rhs);
            let (result, overflow_2) = result.overflowing_add(old_carry);
            lhs_writer.write(console, result);

            let half_result = (lhs & 0x0F) + (rhs & 0x0F) + old_carry;

            let zero = result == 0;
            let negative = false;
            let half_carry = (half_result & 0x10) != 0;
            let carry = overflow_1 || overflow_2;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Add8(lhs_reader, rhs_reader) => {
            let (lhs_writer, lhs) = lhs_reader.read_into_writer(console);
            let rhs = rhs_reader.read(console);

            let (result, overflow) = lhs.overflowing_add(rhs);
            lhs_writer.write(console, result);

            let half_result = (lhs & 0x0F) + (rhs & 0x0F);

            let zero = result == 0;
            let negative = false;
            let half_carry = (half_result & 0x10) != 0;
            let carry = overflow;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::AddI8(lhs_reader, rhs_reader) => {
            let (lhs_writer, lhs) = lhs_reader.read_into_writer(console);

            let rhs = rhs_reader.read(console);

            let result = lhs.wrapping_add_signed(rhs.into());
            lhs_writer.write(console, result);

            // Gameboy determines flags for (u16 + i8) by treating it as (u8 + u8)
            let (_, u8_overflow) = ((lhs & 0x00_FF) as u8).overflowing_add(rhs as u8);
            let half_result = ((lhs & 0x0F) as u8) + ((rhs & 0x0F) as u8);

            let zero = false;
            let negative = false;
            let half_carry = (half_result & 0x10) != 0;
            let carry = u8_overflow;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Add16(lhs_reader, rhs_reader) => {
            let (lhs_writer, lhs) = lhs_reader.read_into_writer(console);
            let rhs = rhs_reader.read(console);

            let (result, overflow) = lhs.overflowing_add(rhs);
            lhs_writer.write(console, result);

            let half_result = (lhs & 0x0F_FF) + (rhs & 0x0F_FF);

            let zero = result == 0;
            let negative = false;
            let half_carry = (half_result & 0x10_00) != 0;
            let carry = overflow;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::And(lhs_reader, rhs_reader) => {
            let (lhs_writer, lhs) = lhs_reader.read_into_writer(console);
            let rhs = rhs_reader.read(console);

            let result = lhs & rhs;
            lhs_writer.write(console, result);

            let zero = result == 0;
            let negative = false;
            let half_carry = true;
            let carry = false;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Cp(lhs_reader, rhs_reader) => {
            let lhs = lhs_reader.read(console);
            let rhs = rhs_reader.read(console);

            let result = lhs.wrapping_sub(rhs);

            let zero = result == 0;
            let negative = true;
            let half_carry = (lhs & 0x0F) < (rhs & 0x0F);
            let carry = lhs < rhs;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Dec8(value_reader) => {
            let (value_writer, value) = value_reader.read_into_writer(console);
            let result = value.wrapping_sub(1);
            value_writer.write(console, result);

            let zero = result == 0;
            let negative = true;
            let half_carry = (value & 0x0F) < 1;
            let carry = false;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Dec16(value_reader) => {
            let (value_writer, value) = value_reader.read_into_writer(console);

            let result = value.wrapping_sub(1);
            value_writer.write(console, result);

            let zero = result == 0;
            let negative = true;
            let half_carry = (value & 0x0F) < 1;
            let carry = false;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Inc8(value_reader) => {
            let (value_writer, value) = value_reader.read_into_writer(console);

            let result = value.wrapping_add(1);
            value_writer.write(console, result);

            let half_result = (value & 0x0F) + 1;

            let zero = result == 0;
            let negative = false;
            let half_carry = (half_result & 0x10) != 0;
            let carry = false;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Inc16(value_reader) => {
            let (value_writer, value) = value_reader.read_into_writer(console);

            let result = value.wrapping_add(1);
            value_writer.write(console, result);

            let half_result = (value & 0x0F) + 1;

            let zero = result == 0;
            let negative = false;
            let half_carry = (half_result & 0x10) != 0;
            let carry = false;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Or(lhs_reader, rhs_reader) => {
            let (lhs_writer, lhs) = lhs_reader.read_into_writer(console);
            let rhs = rhs_reader.read(console);

            let result = lhs & rhs;
            lhs_writer.write(console, result);

            let zero = result == 0;
            let negative = false;
            let half_carry = false;
            let carry = false;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Sbc(lhs_reader, rhs_reader) => {
            let (lhs_writer, lhs) = lhs_reader.read_into_writer(console);
            let rhs = rhs_reader.read(console);

            let old_carry = if console.cpu.flags.carry { 1 } else { 0 };
            let result = lhs.wrapping_sub(rhs).wrapping_sub(old_carry);
            lhs_writer.write(console, result);

            let zero = result == 0;
            let negative = true;
            let half_carry = (lhs & 0x0F) < (rhs & 0x0F) + old_carry;
            let carry = lhs < rhs + old_carry;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Sub(lhs_reader, rhs_reader) => {
            let (lhs_writer, lhs) = lhs_reader.read_into_writer(console);
            let rhs = rhs_reader.read(console);

            let result = lhs.wrapping_sub(rhs);
            lhs_writer.write(console, result);

            let zero = result == 0;
            let negative = true;
            let half_carry = (lhs & 0x0F) < (rhs & 0x0F);
            let carry = lhs < rhs;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Xor(lhs_reader, rhs_reader) => {
            let (lhs_writer, lhs) = lhs_reader.read_into_writer(console);
            let rhs = rhs_reader.read(console);

            let result = lhs ^ rhs;
            lhs_writer.write(console, result);

            let zero = result == 0;
            let negative = true;
            let half_carry = false;
            let carry = false;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Bit(bit_idx_reader, value_reader) => {
            let bit_idx = bit_idx_reader.read(console);
            let value = value_reader.read(console);

            let bit_mask = 1 << bit_idx;
            let bit_set = (value & bit_mask) != 0;

            let zero = !bit_set;
            let negative = false;
            let half_carry = false;
            let carry = false;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Res(bit_idx_reader, value_reader) => {
            // Set bit `bit_idx` in n to 0
            let bit_idx = bit_idx_reader.read(console);
            let (value_writer, value) = value_reader.read_into_writer(console);

            let bit_mask = !(1 << bit_idx);
            let result = value & bit_mask;
            value_writer.write(console, result);
        }
        InstructionName::Set(bit_idx_reader, value_reader) => {
            // Set bit `bit_idx` in n to 1
            let bit_idx = bit_idx_reader.read(console);

            let (value_writer, value) = value_reader.read_into_writer(console);

            let bit_mask = 1 << bit_idx;
            let result = value | bit_mask;
            value_writer.write(console, result);
        }
        InstructionName::Swap(value_reader) => {
            // Swap the high 4 bytes and the low 4 bytes in n
            let (value_writer, value) = value_reader.read_into_writer(console);

            let high_4 = (value & 0xF0) >> 4;
            let low_4 = value & 0x0F;
            let result = (low_4 << 4) + high_4;
            value_writer.write(console, result);
        }
        InstructionName::Rl(bits_reader) => {
            // Rotate bits left through carry
            let (bits_writer, bits) = bits_reader.read_into_writer(console);

            let old_carry = if console.cpu.flags.carry { 1 } else { 0 };
            let bit_7 = bits >> 7;
            let result = (bits << 1) | old_carry;
            bits_writer.write(console, result);

            let zero = result == 0;
            let negative = false;
            let half_carry = false;
            let carry = bit_7 != 0;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Rlc(bits_reader) => {
            // Rotate bits left circularly
            let (bits_writer, bits) = bits_reader.read_into_writer(console);

            let bit_7 = bits >> 7;
            let result = (bits << 1) | bit_7;
            bits_writer.write(console, result);

            let zero = result == 0;
            let negative = false;
            let half_carry = false;
            let carry = bit_7 != 0;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Rr(bits_reader) => {
            // Rotate bits right through carry
            let (bits_writer, bits) = bits_reader.read_into_writer(console);

            let old_carry = if console.cpu.flags.carry { 1 } else { 0 };
            let bit_0 = bits & 1;
            let result = (old_carry << 7) | (bits >> 1);
            bits_writer.write(console, result);

            let zero = result == 0;
            let negative = false;
            let half_carry = false;
            let carry = bit_0 != 0;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Rrc(bits_value) => {
            // Rotate bits right circularly
            let (bits_writer, bits) = bits_value.read_into_writer(console);

            let bit_0 = bits & 1;
            let result = (bit_0 << 7) | (bits >> 1);
            bits_writer.write(console, result);

            let zero = result == 0;
            let negative = false;
            let half_carry = false;
            let carry = bit_0 != 0;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Sla(bits_reader) => {
            // Shift bits left arithmetically
            let (bits_writer, bits) = bits_reader.read_into_writer(console);

            let bit_7 = bits >> 7;
            let result = bits << 1;
            bits_writer.write(console, result);

            let zero = result == 0;
            let negative = false;
            let half_carry = false;
            let carry = bit_7 != 0;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Sra(bits_reader) => {
            // Shift bits right arithmetically
            // Set new bit_7 to old bit_7
            let (bits_writer, bits) = bits_reader.read_into_writer(console);

            let bit_0 = bits & 1;
            let bit_7 = bits >> 7;
            let result = (bit_7 << 7) | (bits >> 1);
            bits_writer.write(console, result);

            let zero = result == 0;
            let negative = false;
            let half_carry = false;
            let carry = bit_0 != 0;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Srl(bits_reader) => {
            // Shift bits right logically
            // Set new bit_7 to 0
            let (bits_writer, bits) = bits_reader.read_into_writer(console);

            let bit_0 = bits & 1;
            let result = (bits >> 1);
            bits_writer.write(console, result);

            let zero = result == 0;
            let negative = false;
            let half_carry = false;
            let carry = bit_0 != 0;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Ld8(into_destination_writer, value_reader) => {
            let destination_writer = into_destination_writer.into_writer(console);
            let value = value_reader.read(console);

            destination_writer.write(console, value);
        }
        InstructionName::Ld16(into_destination_writer, value_reader) => {
            let destination_writer = into_destination_writer.into_writer(console);
            let value = value_reader.read(console);

            destination_writer.write(console, value);
        }
        InstructionName::Call(address_reader) => {
            // Push the return_address after the immediate value to the stack
            // Set pc to the immediate value jump_address
            let jump_address = address_reader.read(console);
            let return_address = console.cpu.pc;

            stack_push_16(console, return_address);
            console.cpu.pc = jump_address;
        }
        InstructionName::CallIf(condition, address_reader) => {
            let condition = test_condition(console, condition);
            let address = address_reader.read(console);
            let return_address = console.cpu.pc;

            if condition {
                stack_push_16(console, return_address);
                console.cpu.pc = address;
            }
        }
        InstructionName::Jp(address_reader) => {
            let address = address_reader.read(console);
            console.cpu.pc = address;
        }
        InstructionName::JpIf(condition, address_reader) => {
            let condition = test_condition(console, condition);
            let address = address_reader.read(console);

            if condition {
                console.cpu.pc = address;
            }
        }
        InstructionName::Jr(address_offset_reader) => {
            let address_offset = address_offset_reader.read(console);
            let address = console.cpu.pc.wrapping_add_signed(address_offset.into());

            console.cpu.pc = address;
        }
        InstructionName::JrIf(condition, address_offset_reader) => {
            let condition = test_condition(console, condition);
            let address_offset = address_offset_reader.read(console);
            let address = console.cpu.pc.wrapping_add_signed(address_offset.into());

            if condition {
                console.cpu.pc = address;
            }
        }
        InstructionName::Ret => {
            let return_address = stack_pop_16(console);
            console.cpu.pc = return_address;
        }
        InstructionName::RetIf(condition) => {
            let condition = test_condition(console, condition);
            let return_address = stack_pop_16(console);

            if condition {
                console.cpu.pc = return_address;
            }
        }
        InstructionName::Reti => {
            let return_address = stack_pop_16(console);
            console.cpu.pc = return_address;
            console.cpu.ime = true;
        }
        InstructionName::Rst(rst) => {
            let address = rst.into_u16();
            console.cpu.pc = address;
        }
        InstructionName::Pop(register) => {
            let value = stack_pop_16(console);
            console.cpu.set_register_16(register, value);
        }
        InstructionName::Push(register) => {
            let value = console.cpu.register_16(register);
            stack_push_16(console, value);
        }
        InstructionName::Ccf => {
            // Complement carry flag
            let zero = false;
            let negative = false;
            let half_carry = false;
            let carry = !console.cpu.flags.carry;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Cpl => {
            // Complement accumulator
            let value = console.cpu.register_8(Register8::A);
            let result = !value;

            console.cpu.set_register_8(Register8::A, result);
        }
        InstructionName::Daa => {}
        InstructionName::Di => {
            // Disable interrupts
            console.cpu.ime = false;
        }
        InstructionName::Ei => {
            // Enable interrupts
            console.cpu.ime = true;
        }
        InstructionName::Halt => {}
        InstructionName::Nop => {}
        InstructionName::Scf => {
            // Set carry flag
            let zero = false;
            let negative = false;
            let half_carry = false;
            let carry = true;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Stop => {}
        InstructionName::Unused => {}
    }
}
