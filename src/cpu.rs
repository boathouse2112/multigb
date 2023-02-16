use crate::{
    instruction::{Instruction, InstructionSpecification},
    parser::{self, Memory},
};

/// Reads an opcode from memory, starting at the given index.
/// Returns the ASM representation.
pub fn read_instruction(
    instruction_specs: &Vec<InstructionSpecification>,
    memory: Memory,
    index: usize,
) -> Result<(usize, Instruction), String> {
    println!("Read instruction");
    parser::any_instruction(instruction_specs)(memory, index)
}
