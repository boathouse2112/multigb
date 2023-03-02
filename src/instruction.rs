use crate::{
    cpu::{Cpu, Flag, Register16Bit, Register8Bit},
    parser::{self, Memory, MutMemory, ParseResult},
};

type NameFn = Box<dyn Fn(&InstructionParams) -> String>;
type TimingFn = Box<dyn Fn(&Cpu) -> u8>;
type ExecuteFn = Box<dyn Fn(&Instruction, MutMemory, &mut Cpu)>;

#[derive(Clone, Copy, Debug)]
pub enum ImmediateValueSpecification {
    U8,
    I8,
    U16,
}

#[derive(Clone, Copy, Debug)]
pub enum ImmediateValue {
    U8(u8),
    I8(i8),
    U16(u16),
}

#[derive(Clone, Copy, Debug)]
pub struct InstructionParamSpecification {
    pub prefix: Option<u8>,
    pub opcode: u8,
    pub immediate_value_type: Option<ImmediateValueSpecification>,
}

#[derive(Debug)]
pub struct InstructionParams {
    pub prefix: Option<u8>,
    pub opcode: u8,
    pub immediate_value: Option<ImmediateValue>,
}

pub struct Instruction<'a> {
    pub name: String,
    pub timing: &'a TimingFn,
    pub params: InstructionParams,
    pub execute: &'a ExecuteFn,
}

pub struct InstructionSpecification {
    name: NameFn,
    timing: TimingFn,
    pub params: InstructionParamSpecification,
    execute: ExecuteFn,
}

impl InstructionSpecification {
    fn build(&self, params: InstructionParams) -> Instruction {
        Instruction {
            name: (self.name)(&params),
            timing: &self.timing,
            params,
            execute: &self.execute,
        }
    }

    /// Parse an instruction, returning its parameters.
    pub fn parse_instruction<'a>(
        &'a self,
    ) -> impl Fn(Memory<'a>, usize) -> ParseResult<Instruction> + '_ {
        let InstructionParamSpecification {
            prefix,
            opcode,
            immediate_value_type,
        } = self.params;

        let prefix_parser = prefix.map(|prefix| parser::byte(prefix));
        let opcode_parser = parser::byte(opcode);
        let immediate_value_parser =
            immediate_value_type.map(|ivt| parser::immediate_value_parser(ivt));

        // println!("Parse instruction");
        // println!(
        //     "   prefix_parser={:}, ivp={:}",
        //     prefix_parser.is_some(),
        //     immediate_value_parser.is_some()
        // );

        move |memory, index| {
            // If there's a prefix, parse it.
            // If it succeeds, update the index. If it fails, return the error.
            // When building the params, we'll just copy any prefix from the specification.
            let index = match &prefix_parser {
                Some(parser) => match parser(memory, index) {
                    Ok((new_index, _)) => new_index,
                    Err(e) => return Err(e),
                },
                None => index,
            };
            // Same for opcode, but it always exists.
            // println!("  Index before opcode_parser={:}", index);
            // println!("  Memory at index={:X?}", memory[index]);
            let index = match opcode_parser(memory, index) {
                Ok((index, _)) => index,
                Err(e) => return Err(e),
            };
            // For immediate_value, we do care about the output.
            let (index, immediate_value) = match &immediate_value_parser {
                Some(parser) => match parser(memory, index) {
                    Ok((index, immediate_value)) => (index, Some(immediate_value)),
                    Err(e) => return Err(e),
                },
                None => (index, None),
            };

            let params = InstructionParams {
                prefix: prefix,
                opcode: opcode,
                immediate_value,
            };

            Ok((index, self.build(params)))
        }
    }
}

// Instruction name helpers -- Require a specific type of immediate value.
// If I was smarter I would just make Instruction generic on ImmediateValue type, but it was too hard.

fn name_none(name_string: String) -> NameFn {
    Box::new(move |_| name_string.clone())
}

/// Takes a format string, and replaces every instance of {} with the Instruction's u8 ImmediateValue
fn name_u8(format_string: String) -> NameFn {
    Box::new(move |params| match &params.immediate_value {
        Some(immediate_value) => match immediate_value {
            ImmediateValue::U8(u8) => format_string.replace("{}", &u8.to_string()),
            _ => String::from("Error: Incorrect immediate value. Should be u16."),
        },
        None => String::from("Error: No immediate value. Should be u16"),
    })
}

/// Takes a format string, and replaces every instance of {} with the Instruction's i8 ImmediateValue
fn name_i8(format_string: String) -> NameFn {
    Box::new(move |params| match &params.immediate_value {
        Some(immediate_value) => match immediate_value {
            ImmediateValue::I8(i8) => format_string.replace("{}", &i8.to_string()),
            _ => String::from("Error: Incorrect immediate value. Should be u16."),
        },
        None => String::from("Error: No immediate value. Should be u16"),
    })
}

/// Takes a format string, and replaces every instance of {} with the Instruction's u16 ImmediateValue
fn name_u16(format_string: String) -> NameFn {
    Box::new(move |params| match &params.immediate_value {
        Some(immediate_value) => match immediate_value {
            ImmediateValue::U16(u16) => format_string.replace("{}", &u16.to_string()),
            _ => String::from("Error: Incorrect immediate value. Should be u16."),
        },
        None => String::from("Error: No immediate value. Should be u16"),
    })
}

// Instruction timing helpers
fn timing_constant(timing: u8) -> TimingFn {
    Box::new(move |cpu| timing)
}

fn timing_branch_if_flag(flag: Flag, branch_timing: u8, no_branch_timing: u8) -> TimingFn {
    Box::new(move |cpu| {
        if cpu.get_flag(flag) {
            branch_timing
        } else {
            no_branch_timing
        }
    })
}

fn timing_branch_if_not_flag(flag: Flag, branch_timing: u8, no_branch_timing: u8) -> TimingFn {
    Box::new(move |cpu| {
        if !cpu.get_flag(flag) {
            branch_timing
        } else {
            no_branch_timing
        }
    })
}

// Instruction functions

fn noop() -> ExecuteFn {
    Box::new(move |_, _, _| {})
}

// Arithmetic

fn add_8_bit(lhs: u8, rhs: u8, destination: Register8Bit) -> ExecuteFn {
    Box::new(move |_, _, cpu| {
        let (result, carry) = lhs.overflowing_add(rhs);
        let is_zero = result == 0;
        let half_carry = ((lhs & 0x0F + rhs & 0x0F) & 0xF0) != 0;
        cpu.set_register_8_bit(destination, result);
        cpu.set_flag(Flag::Z, is_zero);
        cpu.set_flag(Flag::N, false);
        cpu.set_flag(Flag::H, half_carry);
        cpu.set_flag(Flag::C, carry)
    })
}

fn add_carry_8_bit(lhs: u8, rhs: u8, carry: u8, destination: Register8Bit) -> ExecuteFn {
    Box::new(move |_, _, cpu| {
        let result = lhs.wrapping_add(rhs).wrapping_add(carry);
        let is_zero = result == 0;
        let half_carry = ((lhs & 0x0F + rhs & 0x0F + carry) & 0xF0) != 0;
        let result_carry = (((lhs as u32) + (rhs as u32) + (carry as u32)) & !0xF) != 0;
        cpu.set_register_8_bit(destination, result);
        cpu.set_flag(Flag::Z, is_zero);
        cpu.set_flag(Flag::N, false);
        cpu.set_flag(Flag::H, half_carry);
        cpu.set_flag(Flag::C, result_carry);
    })
}

/// Adds the values stored in two u8 registers, and stores the result in the left-hand register.
fn add_8_bit_registers(lhs: Register8Bit, rhs: Register8Bit) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let lhs_value = cpu.get_register_8_bit(lhs);
        let rhs_value = cpu.get_register_8_bit(rhs);
        add_8_bit(lhs_value, rhs_value, lhs)(instruction, memory, cpu)
    })
}

/// Adds the values stored in two u8 registers, plus the value of the carry flag,
/// and stores the result in the left-hand register.
fn add_carry_8_bit_registers(lhs: Register8Bit, rhs: Register8Bit) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let lhs_value = cpu.get_register_8_bit(lhs);
        let rhs_value = cpu.get_register_8_bit(rhs);
        let carry = cpu.get_flag(Flag::C);
        let carry_value = if carry { 1 } else { 0 };
        add_carry_8_bit(lhs_value, rhs_value, carry_value, lhs)(instruction, memory, cpu)
    })
}

fn add_carry_value_to_8_bit_register(destination: Register8Bit) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let rhs_value = instruction
            .params
            .immediate_value
            .expect("has u8 immediate value");
        if let ImmediateValue::U8(rhs_value) = rhs_value {
            let lhs_value = cpu.get_register_8_bit(destination);
            let carry = cpu.get_flag(Flag::C);
            let carry_value = if carry { 1 } else { 0 };
            add_carry_8_bit(lhs_value, rhs_value, carry_value, destination)(
                instruction,
                memory,
                cpu,
            )
        } else {
            panic!("Immediate value must be u8");
        }
    })
}

fn inc_8_bit(register: Register8Bit) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let value = cpu.get_register_8_bit(register);
        add_8_bit(value, 1, register)(instruction, memory, cpu)
    })
}

fn sub_8_bit(lhs: u8, rhs: u8, destination: Register8Bit) -> ExecuteFn {
    Box::new(move |_, _, cpu| {
        let (result, carry) = lhs.overflowing_sub(rhs);
        let is_zero = result == 0;
        let half_carry = (lhs & 0x0F) < (rhs & 0x0F);
        cpu.set_register_8_bit(destination, result);
        cpu.set_flag(Flag::Z, is_zero);
        cpu.set_flag(Flag::N, true);
        cpu.set_flag(Flag::H, half_carry);
        cpu.set_flag(Flag::C, carry)
    })
}

fn sub_8_bit_registers(lhs: Register8Bit, rhs: Register8Bit) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let lhs_value = cpu.get_register_8_bit(lhs);
        let rhs_value = cpu.get_register_8_bit(rhs);
        sub_8_bit(lhs_value, rhs_value, lhs)(instruction, memory, cpu)
    })
}

fn dec_8_bit(register: Register8Bit) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let value = cpu.get_register_8_bit(register);
        sub_8_bit(value, 1, register)(instruction, memory, cpu)
    })
}

fn add_16_bit(lhs: u16, rhs: u16, destination: Register16Bit) -> ExecuteFn {
    Box::new(move |_, _, cpu| {
        let (result, carry) = lhs.overflowing_add(rhs);
        let is_zero = result == 0;
        let half_carry = ((lhs & 0x00FF + rhs & 0x00FF) & 0xFF00) != 0;
        cpu.set_register_16_bit(destination, result);
        cpu.set_flag(Flag::Z, is_zero);
        cpu.set_flag(Flag::H, half_carry);
        cpu.set_flag(Flag::C, carry)
    })
}

/// Adds the values stored in two u16 registers, and stores the result in the left-hand register.
fn add_16_bit_registers(lhs: Register16Bit, rhs: Register16Bit) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let lhs_value = cpu.get_register_16_bit(lhs);
        let rhs_value = cpu.get_register_16_bit(rhs);
        add_16_bit(lhs_value, rhs_value, lhs)(instruction, memory, cpu)
    })
}

fn inc_16_bit(register: Register16Bit) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let value = cpu.get_register_16_bit(register);
        add_16_bit(value, 1, register)(instruction, memory, cpu)
    })
}

fn sub_16_bit(lhs: u16, rhs: u16, destination: Register16Bit) -> ExecuteFn {
    Box::new(move |_, _, cpu| {
        let (result, carry) = lhs.overflowing_sub(rhs);
        let is_zero = result == 0;
        let half_carry = (lhs & 0x00FF) < (rhs & 0x00FF);
        cpu.set_register_16_bit(destination, result);
        cpu.set_flag(Flag::Z, is_zero);
        cpu.set_flag(Flag::N, true);
        cpu.set_flag(Flag::H, half_carry);
        cpu.set_flag(Flag::C, carry)
    })
}

fn dec_16_bit(register: Register16Bit) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let value = cpu.get_register_16_bit(register);
        sub_16_bit(value, 1, register)(instruction, memory, cpu)
    })
}

/// Updates flags as if SUB was called, but doesn't change registers.
fn compare_8_bit_register_value(lhs: Register8Bit) -> ExecuteFn {
    Box::new(move |instruction, _, cpu| {
        let rhs = instruction
            .params
            .immediate_value
            .expect("has U8 immediate value");
        if let ImmediateValue::U8(rhs) = rhs {
            let lhs = cpu.get_register_8_bit(lhs);
            let (result, carry) = lhs.overflowing_sub(rhs);
            let is_zero = result == 0;
            let half_carry = (lhs & 0x0F) < (rhs & 0x0F);
            cpu.set_flag(Flag::Z, is_zero);
            cpu.set_flag(Flag::N, true);
            cpu.set_flag(Flag::H, half_carry);
            cpu.set_flag(Flag::C, carry)
        } else {
            panic!("Immediate value must be U8");
        }
    })
}

fn xor_8_bit_registers(lhs: Register8Bit, rhs: Register8Bit) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let lhs_value = cpu.get_register_8_bit(lhs);
        let rhs_value = cpu.get_register_8_bit(rhs);
        let result = lhs_value ^ rhs_value;
        cpu.set_register_8_bit(lhs, result)
    })
}

fn and_8_bit_register_value(register: Register8Bit) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let value = instruction
            .params
            .immediate_value
            .expect("has u8 immediate value");
        if let ImmediateValue::U8(value) = value {
            let register_value = cpu.get_register_8_bit(register);
            let result = register_value & value;
            cpu.set_register_8_bit(register, result)
        } else {
            panic!("Immediate value must be u8");
        }
    })
}

// Load

fn load_value_to_8_bit_register(register: Register8Bit) -> ExecuteFn {
    Box::new(move |instruction, _, cpu| {
        let value = instruction
            .params
            .immediate_value
            .expect("has u8 immediate value");
        if let ImmediateValue::U8(value) = value {
            cpu.set_register_8_bit(register, value);
        } else {
            panic!("Immediate value must be u8");
        }
    })
}

fn load_value_to_16_bit_register(register: Register16Bit) -> ExecuteFn {
    Box::new(move |instruction, _, cpu| {
        let value = instruction
            .params
            .immediate_value
            .expect("has u16 immediate value");
        if let ImmediateValue::U16(value) = value {
            cpu.set_register_16_bit(register, value)
        } else {
            panic!("Immediate value must be u16");
        }
    })
}

fn load_8_bit_register_to_register(destination: Register8Bit, source: Register8Bit) -> ExecuteFn {
    Box::new(move |_, memory, cpu| {
        let value = cpu.get_register_8_bit(source);
        cpu.set_register_8_bit(destination, value)
    })
}

/// Load data from the source register to the memory location stored in the destination register
fn load_8_bit_register_to_memory_at_register(
    destination: Register16Bit,
    source: Register8Bit,
) -> ExecuteFn {
    Box::new(move |_, memory, cpu| {
        let value = cpu.get_register_8_bit(source);
        let address = cpu.get_register_16_bit(destination);
        memory[address as usize] = value
    })
}

/// Load data from the source register to the memory location stored in the destination register.
/// Then increment the destination register
fn load_8_bit_register_to_memory_at_register_then_inc(
    destination: Register16Bit,
    source: Register8Bit,
) -> ExecuteFn {
    // Yeah, I know...
    Box::new(move |instruction, memory, cpu| {
        load_8_bit_register_to_memory_at_register(destination, source)(instruction, memory, cpu);
        inc_16_bit(destination)(instruction, memory, cpu)
    })
}

/// Load data from the source register to the memory location stored in the destination register.
/// Then decrement the destination register
fn load_8_bit_register_to_memory_at_register_then_dec(
    destination: Register16Bit,
    source: Register8Bit,
) -> ExecuteFn {
    // There's some name for this pattern.
    // I don't think it's a monad, but it feels like one?
    // Or just composition of partial functions?
    // Functor combinators for the functor <T> Fn(Argtype) -> T ?
    Box::new(move |instruction, memory, cpu| {
        load_8_bit_register_to_memory_at_register(destination, source)(instruction, memory, cpu);
        dec_16_bit(destination)(instruction, memory, cpu)
    })
}

fn load_8_bit_register_to_memory_at_value(source: Register8Bit) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let value = instruction
            .params
            .immediate_value
            .expect("has u16 immediate value");
        if let ImmediateValue::U16(value) = value {
            let source_value = cpu.get_register_8_bit(source);
            memory[value as usize] = source_value
        } else {
            panic!("Immediate value must be u16");
        }
    })
}

fn load_8_bit_register_to_memory_at_ff00_plus_register(
    added_to_ff00: Register8Bit,
    source: Register8Bit,
) -> ExecuteFn {
    Box::new(move |_, memory, cpu| {
        let added_to_ff00_value = cpu.get_register_8_bit(added_to_ff00) as u16;
        let source_value = cpu.get_register_8_bit(source);

        let address = 0xFF00 + added_to_ff00_value;
        memory[address as usize] = source_value;
    })
}

fn load_8_bit_register_to_memory_at_ff00_plus_value(source: Register8Bit) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let added_to_ff00_value = instruction
            .params
            .immediate_value
            .expect("has u8 immediate value");
        if let ImmediateValue::U8(added_to_ff00_value) = added_to_ff00_value {
            let source_value = cpu.get_register_8_bit(source);

            let address = 0xFF00 + (added_to_ff00_value as u16);
            memory[address as usize] = source_value;
        } else {
            panic!("Immediate value must be u8");
        }
    })
}

/// Load data from the memory location stored in the source register to the destination register
fn load_memory_at_register_to_8_bit_register(
    destination: Register8Bit,
    source: Register16Bit,
) -> ExecuteFn {
    Box::new(move |_, memory, cpu| {
        let address = cpu.get_register_16_bit(source);
        let value = memory[address as usize];
        cpu.set_register_8_bit(destination, value)
    })
}

fn load_memory_at_ff00_plus_value_to_8_bit_register(destination: Register8Bit) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let added_to_ff00_value = instruction
            .params
            .immediate_value
            .expect("has u8 immediate value");
        if let ImmediateValue::U8(added_to_ff00_value) = added_to_ff00_value {
            let address = 0xFF00 + (added_to_ff00_value as u16);
            let result = memory[address as usize];
            cpu.set_register_8_bit(destination, result)
        } else {
            panic!("Immediate value must be u8");
        }
    })
}

fn load_to_memory_16_bit(address: u16, value: u16) -> ExecuteFn {
    Box::new(move |_, memory, _| {
        let higher_8_bits = (value & 0xFF00 >> 4) as u8;
        let lower_8_bits = value as u8;
        memory[address as usize] = lower_8_bits;
        memory[address as usize + 1] = higher_8_bits
    })
}

/// Load data from the given register to the memory location specified by the u16 immediate value
fn load_16_bit_register_to_memory_at_value(destination: Register16Bit) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let value = instruction
            .params
            .immediate_value
            .expect("has u16 immediate value");
        if let ImmediateValue::U16(value) = value {
            let address = cpu.get_register_16_bit(destination);
            load_to_memory_16_bit(address, value)(instruction, memory, cpu)
        } else {
            panic!("Immediate value must be u16");
        }
    })
}

fn load_memory_at_register_to_16_bit_register(
    destination: Register16Bit,
    source: Register16Bit,
) -> ExecuteFn {
    Box::new(move |_, memory, cpu| {
        let address = cpu.get_register_16_bit(source);
        let lower_bits = memory[address as usize];
        let higher_bits = memory[address as usize + 1];
        let value = ((higher_bits as u16) << 4) + (lower_bits as u16);
        cpu.set_register_16_bit(destination, value)
    })
}

fn push(source: Register16Bit) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let value = cpu.get_register_16_bit(source);
        dec_16_bit(Register16Bit::SP)(instruction, memory, cpu);
        dec_16_bit(Register16Bit::SP)(instruction, memory, cpu);
        let address = cpu.get_register_16_bit(Register16Bit::SP);
        load_to_memory_16_bit(address, value)(instruction, memory, cpu)
    })
}

// Control

// Run given ExecuteFn if the given flag is set to true
fn if_flag(flag: Flag, func: ExecuteFn) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let flag_value = cpu.get_flag(flag);
        if flag_value {
            func(instruction, memory, cpu)
        }
    })
}

// Run given ExecuteFn if the given flag is set to false
fn if_not_flag(flag: Flag, func: ExecuteFn) -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let flag_value = cpu.get_flag(flag);
        if !flag_value {
            func(instruction, memory, cpu)
        }
    })
}

// Add to the PC regiser the i8 immediate value
fn jump_relative() -> ExecuteFn {
    Box::new(move |instruction, _, cpu| {
        let value = instruction
            .params
            .immediate_value
            .expect("has i8 immediate value");
        if let ImmediateValue::I8(value) = value {
            let pc = cpu.get_register_16_bit(Register16Bit::PC);
            let new_pc = ((pc as i32) + (value as i32)) as u16;
            cpu.set_register_16_bit(Register16Bit::PC, new_pc)
        } else {
            panic!("Immediate value must be i8");
        }
    })
}

/// Call a function. Decrement SP by 2, and write the value of PC to the new SP location.
/// Then, set PC to the u16 immediate value
fn call() -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        let value = instruction
            .params
            .immediate_value
            .expect("has u16 immediate value");
        if let ImmediateValue::U16(value) = value {
            dec_16_bit(Register16Bit::SP)(instruction, memory, cpu);
            dec_16_bit(Register16Bit::SP)(instruction, memory, cpu);

            let address = cpu.get_register_16_bit(Register16Bit::SP);
            let pc = cpu.get_register_16_bit(Register16Bit::PC);
            load_to_memory_16_bit(address, pc)(instruction, memory, cpu);

            let new_pc = value;
            cpu.set_register_16_bit(Register16Bit::PC, new_pc)
        } else {
            panic!("Immediate value must be u16");
        }
    })
}

/// Return from a function. Set PC to the value at SP, then increment SP by 2.
fn ret() -> ExecuteFn {
    Box::new(move |instruction, memory, cpu| {
        load_memory_at_register_to_16_bit_register(Register16Bit::PC, Register16Bit::SP)(
            instruction,
            memory,
            cpu,
        );
        inc_16_bit(Register16Bit::SP)(instruction, memory, cpu);
        inc_16_bit(Register16Bit::SP)(instruction, memory, cpu)
    })
}

// Instruction specs vec
pub fn instruction_specs() -> Vec<InstructionSpecification> {
    vec![
        InstructionSpecification {
            name: name_none(String::from("NOP")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x00,
                immediate_value_type: None,
            },
            execute: noop(),
        },
        InstructionSpecification {
            name: name_u16(String::from("LD BC,(${})")),
            timing: timing_constant(12),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x01,
                immediate_value_type: Some(ImmediateValueSpecification::U16),
            },
            execute: load_value_to_16_bit_register(Register16Bit::BC),
        },
        InstructionSpecification {
            name: name_none(String::from("LD (BC),A")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x02,
                immediate_value_type: None,
            },
            execute: load_8_bit_register_to_memory_at_register(Register16Bit::BC, Register8Bit::A),
        },
        InstructionSpecification {
            name: name_none(String::from("INC BC")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x03,
                immediate_value_type: None,
            },
            execute: inc_16_bit(Register16Bit::BC),
        },
        InstructionSpecification {
            name: name_none(String::from("INC B")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x04,
                immediate_value_type: None,
            },
            execute: inc_8_bit(Register8Bit::B),
        },
        InstructionSpecification {
            name: name_none(String::from("DEC B")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x05,
                immediate_value_type: None,
            },
            execute: dec_8_bit(Register8Bit::B),
        },
        InstructionSpecification {
            name: name_u8(String::from("LD B,${}")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x06,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: load_value_to_8_bit_register(Register8Bit::B),
        },
        InstructionSpecification {
            name: name_none(String::from("RLCA")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x07,
                immediate_value_type: None,
            },
            execute: noop(), // TODO
        },
        InstructionSpecification {
            name: name_u16(String::from("LD (${}),SP")),
            timing: timing_constant(20),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x08,
                immediate_value_type: Some(ImmediateValueSpecification::U16),
            },
            execute: load_16_bit_register_to_memory_at_value(Register16Bit::SP),
        },
        InstructionSpecification {
            name: name_none(String::from("ADD HL,BC")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x09,
                immediate_value_type: None,
            },
            execute: add_16_bit_registers(Register16Bit::HL, Register16Bit::BC),
        },
        InstructionSpecification {
            name: name_none(String::from("LD A,(BC)")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x0A,
                immediate_value_type: None,
            },
            execute: load_memory_at_register_to_8_bit_register(Register8Bit::A, Register16Bit::BC),
        },
        InstructionSpecification {
            name: name_none(String::from("DEC BC")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x0B,
                immediate_value_type: None,
            },
            execute: dec_16_bit(Register16Bit::BC),
        },
        InstructionSpecification {
            name: name_none(String::from("INC C")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x0C,
                immediate_value_type: None,
            },
            execute: inc_8_bit(Register8Bit::C),
        },
        InstructionSpecification {
            name: name_none(String::from("DEC C")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x0D,
                immediate_value_type: None,
            },
            execute: dec_8_bit(Register8Bit::C),
        },
        InstructionSpecification {
            name: name_u8(String::from("LD C,${}")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x0E,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: load_value_to_8_bit_register(Register8Bit::C),
        },
        InstructionSpecification {
            name: name_u16(String::from("RRCA")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x0F,
                immediate_value_type: None,
            },
            execute: noop(), // TODO
        },
        InstructionSpecification {
            name: name_u16(String::from("LD DE,${}")),
            timing: timing_constant(12),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x11,
                immediate_value_type: Some(ImmediateValueSpecification::U16),
            },
            execute: load_value_to_16_bit_register(Register16Bit::DE),
        },
        InstructionSpecification {
            name: name_none(String::from("INC DE")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x13,
                immediate_value_type: None,
            },
            execute: inc_16_bit(Register16Bit::DE),
        },
        InstructionSpecification {
            name: name_none(String::from("DEC D")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x15,
                immediate_value_type: None,
            },
            execute: dec_8_bit(Register8Bit::D),
        },
        InstructionSpecification {
            name: name_u8(String::from("LD D,${}")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x16,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: load_value_to_8_bit_register(Register8Bit::D),
        },
        InstructionSpecification {
            name: name_none(String::from("RLA")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x17,
                immediate_value_type: None,
            },
            execute: noop(), // TODO
        },
        InstructionSpecification {
            name: name_i8(String::from("JR Addr_{}")),
            timing: timing_constant(12),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x18,
                immediate_value_type: Some(ImmediateValueSpecification::I8),
            },
            execute: jump_relative(),
        },
        InstructionSpecification {
            name: name_none(String::from("LD A,(DE)")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x1A,
                immediate_value_type: None,
            },
            execute: load_memory_at_register_to_8_bit_register(Register8Bit::A, Register16Bit::DE),
        },
        InstructionSpecification {
            name: name_none(String::from("DEC E")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x1D,
                immediate_value_type: None,
            },
            execute: dec_8_bit(Register8Bit::E),
        },
        InstructionSpecification {
            name: name_u8(String::from("LD E,${}")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x1E,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: load_value_to_8_bit_register(Register8Bit::E),
        },
        InstructionSpecification {
            name: name_i8(String::from("JR NZ, Addr_{}")),
            timing: timing_branch_if_not_flag(Flag::Z, 12, 8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x20,
                immediate_value_type: Some(ImmediateValueSpecification::I8),
            },
            execute: if_not_flag(Flag::Z, jump_relative()),
        },
        InstructionSpecification {
            name: name_u16(String::from("LD HL,${}")),
            timing: timing_constant(12),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x21,
                immediate_value_type: Some(ImmediateValueSpecification::U16),
            },
            execute: load_value_to_16_bit_register(Register16Bit::HL),
        },
        InstructionSpecification {
            name: name_none(String::from("LD (HL+),A")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x22,
                immediate_value_type: None,
            },
            execute: load_8_bit_register_to_memory_at_register_then_inc(
                Register16Bit::HL,
                Register8Bit::A,
            ),
        },
        InstructionSpecification {
            name: name_none(String::from("INC HL")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x23,
                immediate_value_type: None,
            },
            execute: inc_16_bit(Register16Bit::HL),
        },
        InstructionSpecification {
            name: name_none(String::from("INC H")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x24,
                immediate_value_type: None,
            },
            execute: inc_8_bit(Register8Bit::H),
        },
        InstructionSpecification {
            name: name_i8(String::from("JR Z, Addr_{}")),
            timing: timing_branch_if_flag(Flag::Z, 8, 12),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x28,
                immediate_value_type: Some(ImmediateValueSpecification::I8),
            },
            execute: if_flag(Flag::Z, jump_relative()),
        },
        InstructionSpecification {
            name: name_u8(String::from("LD L,${}")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x2E,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: load_value_to_8_bit_register(Register8Bit::L),
        },
        InstructionSpecification {
            name: name_u16(String::from("LD SP,${}")),
            timing: timing_constant(12),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x31,
                immediate_value_type: Some(ImmediateValueSpecification::U16),
            },
            execute: load_value_to_16_bit_register(Register16Bit::SP),
        },
        InstructionSpecification {
            name: name_none(String::from("LD (HL-),A")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x32,
                immediate_value_type: None,
            },
            execute: load_8_bit_register_to_memory_at_register_then_dec(
                Register16Bit::HL,
                Register8Bit::A,
            ),
        },
        InstructionSpecification {
            name: name_none(String::from("DEC A")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x3D,
                immediate_value_type: None,
            },
            execute: dec_8_bit(Register8Bit::A),
        },
        InstructionSpecification {
            name: name_u8(String::from("LD A,{}")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x3E,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: load_value_to_8_bit_register(Register8Bit::A),
        },
        InstructionSpecification {
            name: name_none(String::from("LD C,A")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x4F,
                immediate_value_type: None,
            },
            execute: load_8_bit_register_to_register(Register8Bit::C, Register8Bit::A),
        },
        InstructionSpecification {
            name: name_none(String::from("LD D,A")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x57,
                immediate_value_type: None,
            },
            execute: load_8_bit_register_to_register(Register8Bit::D, Register8Bit::A),
        },
        InstructionSpecification {
            name: name_none(String::from("LD H,(HL)")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x66,
                immediate_value_type: None,
            },
            execute: load_memory_at_register_to_8_bit_register(Register8Bit::H, Register16Bit::HL),
        },
        InstructionSpecification {
            name: name_none(String::from("LD H,A")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x67,
                immediate_value_type: None,
            },
            execute: load_8_bit_register_to_register(Register8Bit::H, Register8Bit::A),
        },
        InstructionSpecification {
            name: name_none(String::from("LD (HL),E")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x73,
                immediate_value_type: None,
            },
            execute: load_8_bit_register_to_memory_at_register(Register16Bit::HL, Register8Bit::E),
        },
        InstructionSpecification {
            name: name_none(String::from("LD (HL),A")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x77,
                immediate_value_type: None,
            },
            execute: load_8_bit_register_to_memory_at_register(Register16Bit::HL, Register8Bit::A),
        },
        InstructionSpecification {
            name: name_none(String::from("LD A,E")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x7B,
                immediate_value_type: None,
            },
            execute: load_8_bit_register_to_register(Register8Bit::A, Register8Bit::E),
        },
        InstructionSpecification {
            name: name_none(String::from("LD A,H")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x7C,
                immediate_value_type: None,
            },
            execute: load_8_bit_register_to_register(Register8Bit::A, Register8Bit::H),
        },
        InstructionSpecification {
            name: name_none(String::from("ADD A,E")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x83,
                immediate_value_type: None,
            },
            execute: add_8_bit_registers(Register8Bit::A, Register8Bit::E),
        },
        InstructionSpecification {
            name: name_none(String::from("ADC A,B")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x88,
                immediate_value_type: None,
            },
            execute: add_carry_8_bit_registers(Register8Bit::A, Register8Bit::B),
        },
        InstructionSpecification {
            name: name_none(String::from("ADC A,C")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x89,
                immediate_value_type: None,
            },
            execute: add_carry_8_bit_registers(Register8Bit::A, Register8Bit::C),
        },
        InstructionSpecification {
            name: name_none(String::from("SUB A,B")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x90,
                immediate_value_type: None,
            },
            execute: sub_8_bit_registers(Register8Bit::A, Register8Bit::B),
        },
        InstructionSpecification {
            name: name_none(String::from("XOR A, A")),
            timing: timing_constant(4),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xAF,
                immediate_value_type: None,
            },
            execute: xor_8_bit_registers(Register8Bit::A, Register8Bit::A),
        },
        InstructionSpecification {
            name: name_none(String::from("RET NZ")),
            timing: timing_branch_if_not_flag(Flag::Z, 8, 20),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xC1,
                immediate_value_type: None,
            },
            execute: if_not_flag(Flag::Z, ret()),
        },
        InstructionSpecification {
            name: name_none(String::from("PUSH BC")),
            timing: timing_constant(16),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xC5,
                immediate_value_type: None,
            },
            execute: push(Register16Bit::BC),
        },
        InstructionSpecification {
            name: name_none(String::from("RET")),
            timing: timing_constant(16),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xC9,
                immediate_value_type: None,
            },
            execute: ret(),
        },
        InstructionSpecification {
            name: name_u16(String::from("CALL Z,${}")),
            timing: timing_constant(12),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xCC,
                immediate_value_type: Some(ImmediateValueSpecification::U16),
            },
            execute: if_flag(Flag::Z, call()),
        },
        InstructionSpecification {
            name: name_u16(String::from("CALL ${}")),
            timing: timing_constant(24),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xCD,
                immediate_value_type: Some(ImmediateValueSpecification::U16),
            },
            execute: call(),
        },
        InstructionSpecification {
            name: name_u8(String::from("ADC A,${}")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xCE,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: add_carry_value_to_8_bit_register(Register8Bit::A),
        },
        InstructionSpecification {
            name: name_u8(String::from("LD (FF00+${}),A")),
            timing: timing_constant(12),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xE0,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: load_8_bit_register_to_memory_at_ff00_plus_value(Register8Bit::A),
        },
        InstructionSpecification {
            name: name_none(String::from("LD (FF00+C),A")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xE2,
                immediate_value_type: None,
            },
            execute: load_8_bit_register_to_memory_at_ff00_plus_register(
                Register8Bit::C,
                Register8Bit::A,
            ),
        },
        InstructionSpecification {
            name: name_u8(String::from("AND A,${}")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xE6,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: and_8_bit_register_value(Register8Bit::A),
        },
        InstructionSpecification {
            name: name_u16(String::from("LD (${}),A")),
            timing: timing_constant(16),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xEA,
                immediate_value_type: Some(ImmediateValueSpecification::U16),
            },
            execute: load_8_bit_register_to_memory_at_value(Register8Bit::A),
        },
        InstructionSpecification {
            name: name_u8(String::from("LD A,(FF00 + ${})")),
            timing: timing_constant(12),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xF0,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: load_memory_at_ff00_plus_value_to_8_bit_register(Register8Bit::A),
        },
        InstructionSpecification {
            name: name_u8(String::from("CP A,${}")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xFE,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: compare_8_bit_register_value(Register8Bit::A),
        },
        // 0xCB prefixed
        InstructionSpecification {
            name: name_none(String::from("RL C")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: Some(0xCB),
                opcode: 0x11,
                immediate_value_type: None,
            },
            execute: noop(),
        },
        InstructionSpecification {
            name: name_none(String::from("BIT 7,H")),
            timing: timing_constant(8),
            params: InstructionParamSpecification {
                prefix: Some(0xCB),
                opcode: 0x7C,
                immediate_value_type: None,
            },
            execute: noop(),
        },
    ]
}
