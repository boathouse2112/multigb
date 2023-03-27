use crate::bus::Bus;
use crate::cpu;
use crate::cpu::Cpu;
use crate::instruction::Instruction;

pub struct Console {
    pub cpu: Cpu,
    pub bus: Bus,
}

impl Console {
    pub fn new(cpu: Cpu, bus: Bus) -> Self {
        Console { cpu, bus }
    }
}

pub fn run(instructions: &Vec<Instruction>, console: &mut Console) {
    loop {
        cpu::step(instructions, console);
    }
}
