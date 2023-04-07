use crate::bus::Bus;
use crate::console::Console;
use crate::cpu::Cpu;
use std::{error, fs};

mod bus;
mod console;
mod cpu;
mod instruction;
mod instructions;
mod util;

// TODO: Can bus::read functions take a &Console instead of a &mut Console?

fn main() -> Result<(), Box<dyn error::Error>> {
    let rom = fs::read("roms/bootstrap.rom")?;

    let instructions = instructions::new();

    let cpu = Cpu::new();
    let bus = Bus::new_with_rom(rom);
    let mut console = Console::new(cpu, bus);

    console::run(&instructions, &mut console);

    Ok(())
}
