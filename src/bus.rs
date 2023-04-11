use crate::console::Console;
use bytesize::KIB;

const MEMORY_SIZE: u32 = (64 * KIB) as u32;

pub struct Bus {
    memory: [u8; MEMORY_SIZE as usize],
}

impl Bus {
    pub fn new() -> Self {
        Bus {
            memory: [0; MEMORY_SIZE as usize],
        }
    }

    pub fn new_with_rom(rom: Vec<u8>) -> Self {
        let mut memory = [0; MEMORY_SIZE as usize];
        memory[0..rom.len()].copy_from_slice(&rom[0..rom.len()]);
        Bus { memory }
    }
}

pub fn read_u8(console: &mut Console, addr: u16) -> u8 {
    console.bus.memory[addr as usize]
}

pub fn read_i8(console: &mut Console, addr: u16) -> i8 {
    read_u8(console, addr) as i8
}

pub fn read_u16(console: &mut Console, addr: u16) -> u16 {
    todo!()
}

pub fn write_u8(console: &mut Console, addr: u16, value: u8) {
    todo!("write_u8")
}

pub fn write_u16(console: &mut Console, addr: u16, value: u16) {
    todo!("write_u16")
}
