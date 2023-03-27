use std::{error, fs, i8, num::Wrapping, result};

mod cpu;
// mod old_instruction;
mod instruction;
mod instructions;

fn main() -> Result<(), Box<dyn error::Error>> {
    // let mut memory = fs::read("roms/bootstrap.rom")?;
    // let instruction_specs = old_instruction::instruction_specs();
    //
    // let step_rom = || {
    //     let mut cpu = Cpu::new();
    //     let mut index = 0;
    //
    //     println!("{:?}", cpu);
    //     for _ in 0..500 {
    //         let (result_idx, result_instruction) =
    //             parser::any_instruction(&instruction_specs)(&memory, index)?;
    //         (instruction.execute)(&instruction, &mut memory, &mut cpu);
    //
    //         println!("{:?}", instruction.name);
    //         println!("{:?}", cpu);
    //     }
    //
    //     Ok::<(), Box<dyn error::Error>>(())
    // };
    //
    // step_rom()?;
    //
    // Ok(())
}
