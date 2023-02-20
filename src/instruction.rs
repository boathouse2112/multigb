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
    pub timing: u8,
    pub params: InstructionParams,
    pub execute: fn(&Self) -> (),
}

pub struct InstructionSpecification {
    name: NameFn,
    timing: u8,
    pub params: InstructionParamSpecification,
    execute: fn(&Instruction) -> (),
}

impl InstructionSpecification {
    fn build(&self, params: InstructionParams) -> Instruction {
        Instruction {
            name: (self.name)(&params),
            timing: self.timing,
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

// Instruction functions

fn noop(_: &Instruction) {}

pub fn instruction_specs() -> Vec<InstructionSpecification> {
    vec![
        InstructionSpecification {
            name: name_none(String::from("NOP")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x00,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("INC BC")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x03,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("INC B")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x04,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("DEC B")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x05,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u8(String::from("LD B,${}")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x06,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u16(String::from("LD (${}),SP")),
            timing: 20,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x08,
                immediate_value_type: Some(ImmediateValueSpecification::U16),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("DEC BC")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x0B,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("INC C")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x0C,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("DEC C")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x0D,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u8(String::from("LD C,${}")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x0E,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u16(String::from("LD DE,${}")),
            timing: 12,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x11,
                immediate_value_type: Some(ImmediateValueSpecification::U16),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("INC DE")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x13,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("DEC D")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x15,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u8(String::from("LD D,${}")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x16,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("RLA")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x17,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_i8(String::from("JR Addr_{}")),
            timing: 12,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x18,
                immediate_value_type: Some(ImmediateValueSpecification::I8),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("LD A,(DE)")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x1A,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("DEC E")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x1D,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u8(String::from("LD E,${}")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x1E,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_i8(String::from("JR NZ, Addr_{}")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x20,
                immediate_value_type: Some(ImmediateValueSpecification::I8),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u16(String::from("LD HL,${}")),
            timing: 12,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x21,
                immediate_value_type: Some(ImmediateValueSpecification::U16),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("LD (HL+),A")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x22,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("INC HL")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x23,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("INC H")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x24,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_i8(String::from("JR Z, Addr_{}")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x28,
                immediate_value_type: Some(ImmediateValueSpecification::I8),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u8(String::from("LD L,${}")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x2E,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u16(String::from("LD SP,${}")),
            timing: 12,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x31,
                immediate_value_type: Some(ImmediateValueSpecification::U16),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("LD (HL-),A")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x32,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("DEC A")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x3D,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u8(String::from("LD A,{}")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x3E,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("LD C,A")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x4F,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("LD D,A")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x57,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("LD H,(HL)")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x66,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("LD H,A")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x67,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("LD (HL),E")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x73,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("LD (HL),A")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x77,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("LD A,E")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x7B,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("LD A,H")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x7C,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("ADD A,E")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x83,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("ADC A,B")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x88,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("ADC A,C")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x89,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("SUB A,B")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0x90,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("XOR A, A")),
            timing: 4,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xAF,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("RET NZ")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xC1,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("PUSH BC")),
            timing: 16,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xC5,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("RET")),
            timing: 16,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xC9,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u16(String::from("CALL Z,${}")),
            timing: 12,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xCC,
                immediate_value_type: Some(ImmediateValueSpecification::U16),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u16(String::from("CALL ${}")),
            timing: 24,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xCD,
                immediate_value_type: Some(ImmediateValueSpecification::U16),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u8(String::from("ADC A,${}")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xCE,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u8(String::from("LD (FF00+${}),A")),
            timing: 12,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xE0,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("LD (FF00+C),A")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xE2,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u8(String::from("AND A,${}")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xE6,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u16(String::from("LD (${}),A")),
            timing: 16,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xEA,
                immediate_value_type: Some(ImmediateValueSpecification::U16),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u8(String::from("LD A,(FF00 + ${})")),
            timing: 12,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xF0,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_u8(String::from("CP A,${}")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: None,
                opcode: 0xFE,
                immediate_value_type: Some(ImmediateValueSpecification::U8),
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("RL C")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: Some(0xCB),
                opcode: 0x11,
                immediate_value_type: None,
            },
            execute: noop,
        },
        InstructionSpecification {
            name: name_none(String::from("BIT 7,H")),
            timing: 8,
            params: InstructionParamSpecification {
                prefix: Some(0xCB),
                opcode: 0x7C,
                immediate_value_type: None,
            },
            execute: noop,
        },
    ]
}
