use crate::bus;
use crate::console::Console;
use crate::cpu::flags::Flags;
use crate::instruction::{Instruction, InstructionArg, InstructionName};
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
            registers_8: EnumMap::from(enum_map! {
                A => 0,
                B => 0,
                C => 0,
                D => 0,
                E => 0,
                H => 0,
                L => 0,
            }),
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

    enum SetterValue {
        U8(u8),
        U16(u16),
    }

    /// Creates a function that sets the location of the instruction argument to the given value.
    /// Only works for InstructionArg variants that can be set
    fn arg_setter(arg: InstructionArg) -> impl Fn(&mut Console, SetterValue) {
        match arg {
            InstructionArg::DirectRegister8(register) => |console, value| {
                if let SetterValue::U8(value) = value {
                    console.cpu.set_register_8(register, value);
                } else {
                    panic!("expect value to be u8")
                }
            },
            InstructionArg::DirectRegister16(register) => |console, value| {
                if let SetterValue::U16(value) = value {
                    console.cpu.set_register_16(register, value);
                } else {
                    panic!("expect value to be u16")
                }
            },
            InstructionArg::IndirectRegister16(register) => |console, value| {
                if let SetterValue::U8(value) = value {
                    let address = console.cpu.register_16(Register16::HL);
                    bus::write_u8(console, address, value);
                } else {
                    panic!("expect value to be u8")
                }
            },
            // InstructionArg::Vector(_) => {}
            InstructionArg::Hli => |console, value| {
                if let SetterValue::U16(value) = value {
                    console
                        .cpu
                        .set_register_16(Register16::HL, value.wrapping_add(1));
                } else {
                    panic!("expect value to be u16")
                }
            },
            InstructionArg::Hld => |console, value| {
                if let SetterValue::U16(value) = value {
                    console
                        .cpu
                        .set_register_16(Register16::HL, value.wrapping_sub(1));
                } else {
                    panic!("expect value to be u16")
                }
            },
            InstructionArg::IndirectFF00PlusC => |console, value| {},
            InstructionArg::IndirectFF00PlusU8 => {}
            InstructionArg::IndirectU16 => {}
        }
    }
}
