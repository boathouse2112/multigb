use crate::bus::Bus;
use crate::console::Console;
use crate::cpu::Cpu;
use std::{error, fs, i8, num::Wrapping, result};

mod cpu;
// mod old_instruction;
mod bus;
mod console;
mod instruction;
mod instructions;
mod util;

fn main() -> Result<(), Box<dyn error::Error>> {
    let rom = fs::read("roms/bootstrap.rom")?;

    let instructions = instructions::new();

    let cpu = Cpu::new();
    let bus = Bus::new_with_rom(rom);
    let mut console = Console::new(cpu, bus);

    console::run(&instructions, &mut console);

    Ok(())
}
