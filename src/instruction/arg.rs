use crate::console::Console;
use crate::{bus, cpu};

// ==== Traits ====

/// An instruction argument of a certain type.
pub trait Arg {
    type Value;

    /// Get the value of this argument from the console.
    ///     Might read from a cpu register
    ///     Might read from memory based on cpu.pc
    ///
    /// # Arguments
    ///
    /// * `console`: The gameboy console object
    ///
    /// returns: (Self::Value, i32)
    ///     Self::Value is the read value
    ///     i32 is the offset to update cpu.pc by.
    fn read(&self, console: &mut Console) -> (Self::Value, i32);
}

pub trait IntoWriteArg: Arg {
    /// Given the argument's location in memory, converts this Arg into a WriteArg
    fn into_write_arg(self, self_address: u16) -> Box<dyn WriteArg<Value = Self::Value>>;
}

pub trait WriteArg {
    type Value;

    /// Write a new value for this argument to the console.
    /// Might write to a cpu register
    /// Might write to memory
    fn write(&self, console: &mut Console, value: Self::Value);
}

// ==== Arg literal enums ====

/// Some instructions change execution depending on the value of a CPU flag
/// These are the possible flags
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum ConditionValue {
    Carry,
    NotCarry,
    Zero,
    NotZero,
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
/// Some instructions jump to specific memory locations
/// These are the possible locations
pub enum RstVectorValue {
    V0x00,
    V0x08,
    V0x10,
    V0x18,
    V0x20,
    V0x28,
    V0x30,
    V0x38,
}

// ==== Arg types ====

pub struct Literal8 {
    value: u8,
}

impl Arg for Literal8 {
    type Value = u8;

    fn read(&self, _: &mut Console) -> (Self::Value, i32) {
        (self.value, 0)
    }
}

pub struct Condition {
    value: ConditionValue,
}

impl Arg for Condition {
    type Value = ConditionValue;

    fn read(&self, _: &mut Console) -> (Self::Value, i32) {
        (self.value, 0)
    }
}

pub struct RstVector {
    value: RstVectorValue,
}

impl Arg for RstVector {
    type Value = RstVectorValue;

    fn read(&self, _: &mut Console) -> (Self::Value, i32) {
        (self.value, 0)
    }
}

/// A u8 value read from the given cpu register
pub struct Register8 {
    register: cpu::Register8,
}

impl Arg for Register8 {
    type Value = u8;

    fn read(&self, console: &mut Console) -> (Self::Value, i32) {
        let value = console.cpu.register_8(self.register);
        (value, 0)
    }
}

impl WriteArg for Register8 {
    type Value = u8;

    fn write(&self, console: &mut Console, value: Self::Value) {
        console.cpu.set_register_8(self.register, value);
    }
}

/// A u16 value read from the given cpu register
pub struct Register16 {
    register: cpu::Register16,
}

impl Arg for Register16 {
    type Value = u16;

    fn read(&self, console: &mut Console) -> (Self::Value, i32) {
        let value = console.cpu.register_16(self.register);
        (value, 0)
    }
}

impl WriteArg for Register16 {
    type Value = u16;

    fn write(&self, console: &mut Console, value: Self::Value) {
        console.cpu.set_register_16(self.register, value);
    }
}

/// A u8 value read from $FF00 + cpu.c
pub struct U8FromFF00PlusC;

impl Arg for U8FromFF00PlusC {
    type Value = u8;

    fn read(&self, console: &mut Console) -> (Self::Value, i32) {
        let address_base: u16 = 0xFF00;
        let address_offset = console.cpu.register_8(cpu::Register8::C);
        let address = address_base.checked_add(address_offset.into()).unwrap();

        let value = bus::read_u8(console, address);
        (value, 0)
    }
}

impl WriteArg for U8FromFF00PlusC {
    type Value = u8;

    fn write(&self, console: &mut Console, value: Self::Value) {
        let address_base: u16 = 0xFF00;
        let address_offset = console.cpu.register_8(cpu::Register8::C);
        let address = address_base.checked_add(address_offset.into()).unwrap();

        bus::write_u8(console, address, value);
    }
}

/// A u8 value read from $FF00 + (the u8 value stored at the instruction's immediate location)
pub struct U8FromFF00PlusU8;

impl Arg for U8FromFF00PlusU8 {
    type Value = u8;

    fn read(&self, console: &mut Console) -> (Self::Value, i32) {
        let address_base: u16 = 0xFF00;

        let pc = console.cpu.pc;
        let address_offset = bus::read_u8(console, pc);
        let address = address_base.checked_add(address_offset.into()).unwrap();

        let value = bus::read_u8(console, address);
        (value, 1)
    }
}

impl IntoWriteArg for U8FromFF00PlusU8 {
    fn into_write_arg(self, arg_address: u16) -> Box<dyn WriteArg<Value = Self::Value>> {
        Box::new(WriteU8FromFF00PlusU8 {
            indirect_address: arg_address,
        })
    }
}

/// A writer for a u8 value written to memory at the address stored in the given memory location
pub struct WriteU8FromFF00PlusU8 {
    indirect_address: u16,
}

impl WriteArg for WriteU8FromFF00PlusU8 {
    type Value = u8;

    fn write(&self, console: &mut Console, value: Self::Value) {
        let address_base: u16 = 0xFF00;
        let address_offset = bus::read_u8(console, self.indirect_address);
        let address = address_base.checked_add(address_offset.into()).unwrap();

        bus::write_u8(console, address, value);
    }
}

/// A u8 value read from the memory address stored in the given register
pub struct U8FromRegister16 {
    register: cpu::Register16,
}

/// A u8 value read from the memory address stored in cpu.hl
/// cpu.hl is incremented after reading and after writing
pub struct U8FromHlInc;

impl Arg for U8FromHlInc {
    type Value = u8;

    /// Read the value of this argument from memory at the address stored in cpu.hl
    /// Increments cpu.hl
    ///
    /// # Arguments
    ///
    /// * `console`: The gameboy console object
    ///
    /// returns: (u8, i32)
    ///
    fn read(&self, console: &mut Console) -> (Self::Value, i32) {
        let address = console.cpu.register_16(cpu::Register16::HL);
        let value = bus::read_u8(console, address);

        let hl_inc = address.wrapping_add(1);
        console.cpu.set_register_16(cpu::Register16::HL, hl_inc);

        (value, 0)
    }
}

impl WriteArg for U8FromHlInc {
    type Value = u8;

    /// Write the value of this argument to memory at the address stored in cpu.hl
    /// Increments cpu.hl
    ///
    /// # Arguments
    ///
    /// * `console`: The gameboy console object
    /// * `value`: The value to write
    ///
    fn write(&self, console: &mut Console, value: Self::Value) {
        let address = console.cpu.register_16(cpu::Register16::HL);
        bus::write_u8(console, address, value);

        let hl_inc = address.wrapping_add(1);
        console.cpu.set_register_16(cpu::Register16::HL, hl_inc);
    }
}

/// A u8 value read from the memory address stored in cpu.hl
/// cpu.hl is decremented after reading and after writing
pub struct U8FromHlDec;

impl Arg for U8FromHlDec {
    type Value = u8;

    /// Read the value of this argument from memory at the address stored in cpu.hl
    /// Decrements cpu.hl
    ///
    /// # Arguments
    ///
    /// * `console`: The gameboy console object
    ///
    /// returns: (u8, i32)
    ///
    fn read(&self, console: &mut Console) -> (Self::Value, i32) {
        let address = console.cpu.register_16(cpu::Register16::HL);
        let value = bus::read_u8(console, address);

        let hl_inc = address.wrapping_sub(1);
        console.cpu.set_register_16(cpu::Register16::HL, hl_inc);

        (value, 0)
    }
}

impl WriteArg for U8FromHlDec {
    type Value = u8;

    /// Write the value of this argument to memory at the address stored in cpu.hl
    /// Decrements cpu.hl
    ///
    /// # Arguments
    ///
    /// * `console`: The gameboy console object
    /// * `value`: The value to write
    ///
    fn write(&self, console: &mut Console, value: Self::Value) {
        let address = console.cpu.register_16(cpu::Register16::HL);
        bus::write_u8(console, address, value);

        let hl_inc = address.wrapping_sub(1);
        console.cpu.set_register_16(cpu::Register16::HL, hl_inc);
    }
}

/// A u16 value equal to cpu.sp plus an i8 value read from the instruction's immediate location
struct SpPlusI8;

impl Arg for SpPlusI8 {
    type Value = u16;

    fn read(&self, console: &mut Console) -> (Self::Value, i32) {
        let pc = console.cpu.pc;
        let sp = console.cpu.sp;
        let offset = bus::read_i8(console, pc);

        let value = sp.wrapping_add_signed(offset.into());
        (value, 1)
    }
}

/// A u8 value read from the instruction's immediate location
pub struct U8;

impl Arg for U8 {
    type Value = u8;

    fn read(&self, console: &mut Console) -> (Self::Value, i32) {
        let pc = console.cpu.pc;
        let value = bus::read_u8(console, pc);
        (value, 1)
    }
}

/// An i8 value read from the instruction's immediate location
pub struct I8;

impl Arg for I8 {
    type Value = i8;

    fn read(&self, console: &mut Console) -> (Self::Value, i32) {
        let pc = console.cpu.pc;
        let value = bus::read_i8(console, pc);
        (value, 1)
    }
}

/// A u16 value read from the instruction's immediate location
pub struct U16;

impl Arg for U16 {
    type Value = u16;

    fn read(&self, console: &mut Console) -> (Self::Value, i32) {
        let pc = console.cpu.pc;
        let value = bus::read_u16(console, pc);
        (value, 2)
    }
}

/// A u16 value read from memory at the address stored in the instruction's immediate location
pub struct U8FromU16;

impl Arg for U8FromU16 {
    type Value = u8;

    fn read(&self, console: &mut Console) -> (Self::Value, i32) {
        let pc = console.cpu.pc;
        let address = bus::read_u16(console, pc);

        let value = bus::read_u8(console, address);
        (value, 2)
    }
}

impl IntoWriteArg for U8FromU16 {
    fn into_write_arg(self, arg_address: u16) -> Box<dyn WriteArg<Value = Self::Value>> {
        Box::new(WriteU8FromU16 {
            indirect_address: arg_address,
        })
    }
}

/// A writer for a u8 value written to memory at the address stored in the given memory location
pub struct WriteU8FromU16 {
    indirect_address: u16,
}

impl WriteArg for WriteU8FromU16 {
    type Value = u8;

    fn write(&self, console: &mut Console, value: Self::Value) {
        let address = bus::read_u16(console, self.indirect_address);
        bus::write_u8(console, address, value);
    }
}
