use std::{error, fs};

mod cpu;
mod instruction;
mod parser;

fn main() -> Result<(), Box<dyn error::Error>> {
    let memory = fs::read("roms/bootstrap.rom")?;
    let instruction_specs = instruction::instruction_specs();

    let (index, instruction) = cpu::read_instruction(&instruction_specs, &memory, 0)?;
    println!("{:?}, {:?}", index, instruction.name);

    Ok(())
}
