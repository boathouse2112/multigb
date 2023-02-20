use std::{error, fs, i8, num::Wrapping};

mod cpu;
mod instruction;
mod parser;

fn main() -> Result<(), Box<dyn error::Error>> {
    let memory = fs::read("roms/bootstrap.rom")?;
    let instruction_specs = instruction::instruction_specs();

    Ok(())
}
