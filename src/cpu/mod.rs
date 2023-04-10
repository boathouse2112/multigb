use crate::bus;
use crate::console::Console;
use crate::cpu::flags::Flags;
use crate::instruction::{Instruction, InstructionName};
use enum_map::EnumMap;
use enum_map::{enum_map, Enum};

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

    fn add_offset(console: &mut Console, pc_offset: i16) {
        console.cpu.pc = console.cpu.pc.wrapping_add_signed(pc_offset);
    }

    // Run instruction
    match &instruction.name {
        InstructionName::Adc(lhs, rhs) => {
            let (lhs_value, pc_offset) = lhs.read(console);
            add_offset(console, pc_offset);
            let (rhs_value, pc_offset) = rhs.read(console);
            add_offset(console, pc_offset);

            let old_carry = if console.cpu.flags.carry { 1 } else { 0 };
            let (result, overflow_1) = lhs_value.overflowing_add(rhs_value);
            let (result, overflow_2) = result.overflowing_add(old_carry);
            lhs.write(console, result);

            let half_result = (lhs_value & 0x0F) + (rhs_value & 0x0F) + old_carry;

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
        InstructionName::Add8(lhs, rhs) => {
            let (lhs_value, pc_offset) = lhs.read(console);
            add_offset(console, pc_offset);
            let (rhs_value, pc_offset) = rhs.read(console);
            add_offset(console, pc_offset);

            let (result, overflow) = lhs_value.overflowing_add(rhs_value);
            lhs.write(console, result);

            let half_result = (lhs_value & 0x0F) + (rhs_value & 0x0F);

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
        InstructionName::AddI8(lhs, rhs) => {
            let (lhs_value, pc_offset) = lhs.read(console);
            add_offset(console, pc_offset);
            let (rhs_value, pc_offset) = rhs.read(console);
            add_offset(console, pc_offset);

            let result = lhs_value.wrapping_add_signed(rhs_value.into());
            lhs.write(console, result);

            // Gameboy determines flags for (u16 + i8) by treating it as (u8 + u8)
            let (_, u8_overflow) = ((lhs_value & 0x00_FF) as u8).overflowing_add(rhs_value as u8);
            let half_result = ((lhs_value & 0x0F) as u8) + ((rhs_value & 0x0F) as u8);

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
        InstructionName::Add16(lhs, rhs) => {
            let (lhs_value, pc_offset) = lhs.read(console);
            add_offset(console, pc_offset);
            let (rhs_value, pc_offset) = rhs.read(console);
            add_offset(console, pc_offset);

            let (result, overflow) = lhs_value.overflowing_add(rhs_value);
            lhs.write(console, result);

            let half_result = (lhs_value & 0x0F_FF) + (rhs_value & 0x0F_FF);

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
        InstructionName::And(lhs, rhs) => {
            let (lhs_value, pc_offset) = lhs.read(console);
            add_offset(console, pc_offset);
            let (rhs_value, pc_offset) = rhs.read(console);
            add_offset(console, pc_offset);

            let result = lhs_value & rhs_value;
            lhs.write(console, result);

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
        InstructionName::Cp(lhs, rhs) => {
            let (lhs_value, pc_offset) = lhs.read(console);
            add_offset(console, pc_offset);
            let (rhs_value, pc_offset) = rhs.read(console);
            add_offset(console, pc_offset);

            let result = lhs_value.wrapping_sub(rhs_value);

            let zero = result == 0;
            let negative = true;
            let half_carry = (lhs_value & 0x0F) < (rhs_value & 0x0F);
            let carry = lhs_value < rhs_value;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Dec8(n) => {
            let (n_value, pc_offset) = n.read(console);
            add_offset(console, pc_offset);

            let result = n_value.wrapping_sub(1);
            n.write(console, result);

            let zero = result == 0;
            let negative = true;
            let half_carry = (n_value & 0x0F) < 1;
            let carry = false;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Dec16(n) => {
            let (n_value, pc_offset) = n.read(console);
            add_offset(console, pc_offset);

            let result = n_value.wrapping_sub(1);
            n.write(console, result);

            let zero = result == 0;
            let negative = true;
            let half_carry = (n_value & 0x0F) < 1;
            let carry = false;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Inc8(n) => {
            let (n_value, pc_offset) = n.read(console);
            add_offset(console, pc_offset);

            let result = n_value.wrapping_add(1);
            n.write(console, result);

            let half_result = (n_value & 0x0F) + 1;

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
        InstructionName::Inc16(n) => {
            let (n_value, pc_offset) = n.read(console);
            add_offset(console, pc_offset);

            let result = n_value.wrapping_add(1);
            n.write(console, result);

            let half_result = (n_value & 0x0F) + 1;

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
        InstructionName::Or(lhs, rhs) => {
            let (lhs_value, pc_offset) = lhs.read(console);
            add_offset(console, pc_offset);
            let (rhs_value, pc_offset) = rhs.read(console);
            add_offset(console, pc_offset);

            let result = lhs_value & rhs_value;
            lhs.write(console, result);

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
        InstructionName::Sbc(lhs, rhs) => {
            let (lhs_value, pc_offset) = lhs.read(console);
            add_offset(console, pc_offset);
            let (rhs_value, pc_offset) = rhs.read(console);
            add_offset(console, pc_offset);

            let old_carry = if console.cpu.flags.carry { 1 } else { 0 };
            let result = lhs_value.wrapping_sub(rhs_value).wrapping_sub(old_carry);
            lhs.write(console, result);

            let zero = result == 0;
            let negative = true;
            let half_carry = (lhs_value & 0x0F) < (rhs_value & 0x0F) + old_carry;
            let carry = lhs_value < rhs_value + old_carry;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Sub(lhs, rhs) => {
            let (lhs_value, pc_offset) = lhs.read(console);
            add_offset(console, pc_offset);
            let (rhs_value, pc_offset) = rhs.read(console);
            add_offset(console, pc_offset);

            let result = lhs_value.wrapping_sub(rhs_value);
            lhs.write(console, result);

            let zero = result == 0;
            let negative = true;
            let half_carry = (lhs_value & 0x0F) < (rhs_value & 0x0F);
            let carry = lhs_value < rhs_value;
            console.cpu.flags = Flags {
                zero,
                negative,
                half_carry,
                carry,
            }
        }
        InstructionName::Xor(lhs, rhs) => {
            let (lhs_value, pc_offset) = lhs.read(console);
            add_offset(console, pc_offset);
            let (rhs_value, pc_offset) = rhs.read(console);
            add_offset(console, pc_offset);

            let result = lhs_value ^ rhs_value;
            lhs.write(console, result);

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
        InstructionName::Bit(bit_idx, n) => {
            let (bit_idx_value, pc_offset) = bit_idx.read(console);
            add_offset(console, pc_offset);
            let (n_value, pc_offset) = n.read(console);
            add_offset(console, pc_offset);

            let bit_mask = 1 << bit_idx_value;
            let bit_set = (n_value & bit_mask) != 0;

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
        InstructionName::Res(bit_idx, n) => {
            // Set bit `bit_idx` in n to 0
            let (bit_idx_value, pc_offset) = bit_idx.read(console);
            add_offset(console, pc_offset);
            let (n_value, pc_offset) = n.read(console);
            add_offset(console, pc_offset);

            let bit_mask = !(1 << bit_idx_value);
            let result = n_value & bit_mask;
            n.write(console, result);
        }
        InstructionName::Set(bit_idx, n) => {
            // Set bit `bit_idx` in n to 1
            let (bit_idx_value, pc_offset) = bit_idx.read(console);
            add_offset(console, pc_offset);
            let (n_value, pc_offset) = n.read(console);
            add_offset(console, pc_offset);

            let bit_mask = 1 << bit_idx_value;
            let result = n_value | bit_mask;
            n.write(console, result);
        }
        InstructionName::Swap(n) => {
            // Swap the high 4 bytes and the low 4 bytes in n
            let (n_value, pc_offset) = n.read(console);
            add_offset(console, pc_offset);

            let high_4 = (n_value & 0xF0) >> 4;
            let low_4 = n_value & 0x0F;
            let result = (low_4 << 4) + high_4;
            n.write(console, result);
        }
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
        InstructionName::Ld8(_, _) => {}
        InstructionName::Ld16(_, _) => {}
        InstructionName::Ldh(_, _) => {}
        InstructionName::Call(_) => {}
        InstructionName::CallIf(_, _) => {}
        InstructionName::Jp(_) => {}
        InstructionName::JpIf(_, _) => {}
        InstructionName::Jr(_) => {}
        InstructionName::JrIf(_, _) => {}
        InstructionName::Ret => {}
        InstructionName::RetIf(_) => {}
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
}
