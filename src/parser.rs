// Todo: Memory type should be replaced with a real memory object, with bounds checking and such.

use crate::instruction::{
    ImmediateValue, ImmediateValueSpecification, Instruction, InstructionSpecification,
};

pub type Memory<'a> = &'a Vec<u8>;
pub type ParseResult<Output> = Result<(usize, Output), String>;

/// Parse a single, predefined byte
pub fn byte(byte: u8) -> impl Fn(Memory<'_>, usize) -> ParseResult<()> {
    move |memory: Memory<'_>, index: usize| {
        if memory[index] == byte {
            Ok((index + 1, ()))
        } else {
            Err(String::from(format!("{:X} not found", byte)))
        }
    }
}

/// Read a single byte from memory, parsing it as u8
pub fn any_u8(memory: Memory<'_>, index: usize) -> ParseResult<u8> {
    Ok((index + 1, memory[index]))
}

/// Read a single byte from memory, parsing it as i8
pub fn any_i8(memory: Memory<'_>, index: usize) -> ParseResult<i8> {
    // TODO: JZ command results in -5 from 0xFB, which seems right, but the test rom expects something different.
    Ok((index + 1, memory[index] as i8))
}

/// Read a single word from memory, parsing it as u16
pub fn any_u16(memory: Memory<'_>, index: usize) -> ParseResult<u16> {
    // GB stores u16 as [lower_byte, higher_byte], but each byte is in normal order.
    // Don't know why.
    let lower_byte: u8 = memory[index];
    let higher_byte: u8 = memory[index + 1];
    let word: u16 = ((higher_byte as u16) << 8) | lower_byte as u16;

    Ok((index + 2, word))
}

/// Returns the first successful parse using the given parsers
pub fn any_instruction(
    specs: &Vec<InstructionSpecification>,
) -> impl Fn(Memory, usize) -> ParseResult<Instruction> + '_ {
    move |memory, index| {
        let mut errors = String::new();
        for spec in specs.iter() {
            // println!("Any -- spec.params={:X?}", spec.params);
            let result = spec.parse_instruction()(memory, index);
            match result {
                Ok(_) => return result,
                Err(error) => errors.push_str(&error),
            }
        }
        Err(errors)
    }
}

pub fn immediate_value_parser(
    immediate_value_type: ImmediateValueSpecification,
) -> impl Fn(Memory, usize) -> ParseResult<ImmediateValue> {
    move |memory, index| {
        match immediate_value_type {
            // Turn the parse results into ImmediateValue's. This is just janky function composition.
            ImmediateValueSpecification::U8 => {
                let (index, u8) = any_u8(memory, index)?;
                Ok((index, ImmediateValue::U8(u8)))
            }
            ImmediateValueSpecification::I8 => {
                let (index, i8) = any_i8(memory, index)?;
                Ok((index, ImmediateValue::I8(i8)))
            }
            ImmediateValueSpecification::U16 => {
                let (index, u16) = any_u16(memory, index)?;
                Ok((index, ImmediateValue::U16(u16)))
            }
        }
    }
}
