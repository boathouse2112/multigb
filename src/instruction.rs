use crate::parser::{self, Memory, ParseResult};

type NameFn = Box<dyn Fn(&InstructionParams) -> String>;

#[derive(Clone, Copy, Debug)]
pub enum ImmediateValueSpecification {
    U8,
    I8,
    U16,
}

#[derive(Debug)]
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

pub struct Instruction {
    pub name: String,
    pub params: InstructionParams,
    pub execute: fn(&Self) -> (),
}

pub struct InstructionSpecification {
    name: Box<dyn Fn(&InstructionParams) -> String>,
    pub params: InstructionParamSpecification,
    execute: fn(&Instruction) -> (),
}

impl InstructionSpecification {
    fn build(&self, params: InstructionParams) -> Instruction {
        Instruction {
            name: (self.name)(&params),
            params,
            execute: self.execute,
        }
    }

    /// Parse an instruction, returning its parameters.
    pub fn parse_instruction<'a>(
        &'a self,
    ) -> impl Fn(Memory<'a>, usize) -> ParseResult<Instruction> + 'a {
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

// Instruction functions

fn noop(_: &Instruction) {}

pub fn instruction_specs() -> Vec<InstructionSpecification> {
    vec![
        InstructionSpecification {
            name: Box::new(|_| String::from("NOP")),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x00,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: Box::new(|_| String::from("XOR A, A")),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xAF,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u16(String::from("LD SP,${}")),
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x31,
                immediate_value_type: Some(ImmediateValueSpecification::U16),
            },
            execute: noop,
        },
    ]
}
