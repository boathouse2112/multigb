use std::io::Write;

// We start with an INSTRUCTION variant mapped to a BEHAVIOR, and n ARGUMENTS
enum Instruction {
    Inc(Box<dyn WriteArg<Value = u8>>),
    Add8(Arg8, Arg8),
    Add16(Arg16, Arg16),
}

fn exec(instruction: Instruction) {
    match instruction {
        Instruction::Inc(arg) => {
            let value = arg.read("console".to_string());
            let result = value + 1;
            arg.write()
        }
        Instruction::Add8(_, _) => {}
        Instruction::Add16(_, _) => {}
    }
}

// ARGUMENT variants can be broken up based on the final value they return when read from memory
// ARGUMENT variants can also be broken up based on whether they are ever written to.
trait Arg {
    type Value;
    fn read(&self, console: String) -> Self::Value;
}

trait IntoWriteArg: Arg {
    fn into_write_arg(self, pc: u16) -> Box<dyn WriteArg<Value = Self::Value>>;
}

// Some ARGUMENT variants can be written to

trait WriteArg {
    type Value;
    fn write(&self, console: String, value: Self::Value);
}

struct LiteralU8 {
    value: u8,
}

impl Arg for LiteralU8 {
    type Value = u8;

    fn read(&self, _: String) -> Self::Value {
        self.value
    }
}

enum Arg8 {
    Literal(u8),
    Value,
    FromRegister(String),
}

enum Arg16 {
    FromRegister(String),
}
//
// // Each ARGUMENT variant has associated read-from-memory behavior.
// // Assume pc is set to the location of the argument
// fn read_arg8(arg: Arg8, memory: [u8; 100], pc: u16) -> u8 {
//     match arg {
//         Arg8::Literal(literal_value) => literal_value,
//         Arg8::FromRegister(register) => todo!(),
//         Arg8::Value => memory[pc as usize],
//     }
// }
//
// // Some ARGUMENT variants also have associated write-to-memory behavior.
//
fn main() {}
