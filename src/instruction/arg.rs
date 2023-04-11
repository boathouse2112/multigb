use crate::console::Console;
use crate::{bus, cpu};
use derive_new::new;
use num::PrimInt;

// ==== Traits ====

pub trait Unit {}

pub trait ArgReaderIntoWriter {
    type Arg;

    /// Reads an argument of type `Self::Arg` from memory.
    /// Increments `console.cpu.pc` based on the length of the argument.
    /// Converts `self` into an arg writer.
    ///
    /// # Arguments
    ///
    /// * `console`: The gameboy console object
    ///
    /// returns: `(Self::Into, Self::Arg)`
    ///     `Self::Into` is the converted value of self
    ///     `Self::Arg` is the value of the argument
    fn read_into_writer(
        self,
        console: &mut Console,
    ) -> (Box<dyn ArgWriter<Arg = Self::Arg>>, Self::Arg);
}

pub trait ArgReader {
    type Arg;

    /// Reads an argument of type `Self::Arg` from memory.
    /// Increments `console.cpu.pc` based on the length of the argument.
    ///
    /// # Arguments
    ///
    /// * `console`: The gameboy console object
    fn read(self, console: &mut Console) -> Self::Arg;
}

pub trait IntoWriter {
    type Arg;

    /// Converts `self` into an arg writer.
    /// Increments `console.cpu.pc` based on the length of the argument.
    ///
    /// # Arguments
    ///
    /// * `console`: The gameboy console object
    fn into_writer(self, console: &mut Console) -> Box<dyn ArgWriter<Arg = Self::Arg>>;
}

// Generic ArgReader impl for any ArgReaderIntoWriter
impl<Arg, AnyArgReaderIntoWriter> ArgReader for AnyArgReaderIntoWriter
where
    AnyArgReaderIntoWriter: ArgReaderIntoWriter<Arg = Arg>,
{
    type Arg = Arg;

    fn read(self, console: &mut Console) -> Self::Arg {
        self.read_into_writer(console).1
    }
}

// Generic IntoWriter impl for any ArgReaderIntoWriter
impl<Arg, AnyArgReaderIntoWriter> IntoWriter for AnyArgReaderIntoWriter
where
    AnyArgReaderIntoWriter: ArgReaderIntoWriter<Arg = Arg>,
{
    type Arg = Arg;

    fn into_writer(self, console: &mut Console) -> Box<dyn ArgWriter<Arg = Self::Arg>> {
        self.read_into_writer(console).0
    }
}

pub trait ArgWriter {
    type Arg;

    /// Write a new value for this argument to the console.
    /// Might write to a cpu register
    /// Might write to memory
    fn write(self, console: &mut Console, value: Self::Arg);
}

// ==== Arg literal enums ====

/// Some instructions change execution depending on the value of a CPU flag
/// These are the possible flags
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Condition {
    Carry,
    NotCarry,
    Zero,
    NotZero,
}

impl ArgReader for Condition {
    type Arg = Condition;

    fn read(self, _: &mut Console) -> Self::Arg {
        self
    }
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
/// Some instructions jump to specific memory locations
/// These are the possible locations
pub enum RstVector {
    V0x00,
    V0x08,
    V0x10,
    V0x18,
    V0x20,
    V0x28,
    V0x30,
    V0x38,
}

impl RstVector {
    pub fn into_u16(self) -> u16 {
        match self {
            RstVector::V0x00 => 0x00,
            RstVector::V0x08 => 0x08,
            RstVector::V0x10 => 0x10,
            RstVector::V0x18 => 0x18,
            RstVector::V0x20 => 0x20,
            RstVector::V0x28 => 0x28,
            RstVector::V0x30 => 0x30,
            RstVector::V0x38 => 0x38,
        }
    }
}

impl ArgReader for RstVector {
    type Arg = RstVector;

    fn read(self, _: &mut Console) -> Self::Arg {
        self
    }
}

// ==== Arg types ====

// State machine modes for ArgReaderIntoWriter's
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
struct Reader;
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
struct Writer;

#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq, new)]
pub struct Literal8 {
    value: u8,
}

impl ArgReader for Literal8 {
    type Arg = u8;

    fn read(self, _: &mut Console) -> Self::Arg {
        self.value
    }
}

/// A u8 value read from the given cpu register
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Register8<MODE> {
    register: cpu::Register8,
    mode: MODE,
}

impl Register8<Reader> {
    pub fn new(register: cpu::Register8) -> Self {
        Register8 {
            register,
            mode: Reader,
        }
    }
}

impl ArgReaderIntoWriter for Register8<Reader> {
    type Arg = u8;

    fn read_into_writer(
        self,
        console: &mut Console,
    ) -> (Box<dyn ArgWriter<Arg = Self::Arg>>, Self::Arg) {
        let writer = Register8 {
            register: self.register,
            mode: Writer,
        };
        let value = console.cpu.register_8(self.register);
        (Box::new(writer), value)
    }
}

impl ArgWriter for Register8<Writer> {
    type Arg = u8;

    fn write(self, console: &mut Console, value: Self::Arg) {
        console.cpu.set_register_8(self.register, value);
    }
}

/// A u16 value read from the given cpu register
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Register16<MODE> {
    register: cpu::Register16,
    mode: MODE,
}

impl Register16<Reader> {
    pub fn new(register: cpu::Register16) -> Self {
        Register16 {
            register,
            mode: Reader,
        }
    }
}

impl ArgReaderIntoWriter for Register16<Reader> {
    type Arg = u16;

    fn read_into_writer(
        self,
        console: &mut Console,
    ) -> (Box<dyn ArgWriter<Arg = Self::Arg>>, Self::Arg) {
        let write_register_16 = Register16 {
            register: self.register,
            mode: Writer,
        };
        let value = console.cpu.register_16(self.register);
        (Box::new(write_register_16), value)
    }
}

impl ArgWriter for Register16<Writer> {
    type Arg = u16;

    fn write(self, console: &mut Console, value: Self::Arg) {
        console.cpu.set_register_16(self.register, value);
    }
}

/// A u8 value read from $FF00 + cpu.c
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct U8FromFF00PlusC<MODE> {
    mode: MODE,
}

impl ArgReaderIntoWriter for U8FromFF00PlusC<Reader> {
    type Arg = u8;

    fn read_into_writer(
        self,
        console: &mut Console,
    ) -> (Box<dyn ArgWriter<Arg = Self::Arg>>, Self::Arg) {
        let writer = U8FromFF00PlusC { mode: Writer };

        let address_base: u16 = 0xFF00;
        let address_offset = console.cpu.register_8(cpu::Register8::C);
        let address = address_base.checked_add(address_offset.into()).unwrap();

        let value = bus::read_u8(console, address);
        (Box::new(writer), value)
    }
}

impl ArgWriter for U8FromFF00PlusC<Writer> {
    type Arg = u8;

    fn write(self, console: &mut Console, value: Self::Arg) {
        let address_base: u16 = 0xFF00;
        let address_offset = console.cpu.register_8(cpu::Register8::C);
        let address = address_base.checked_add(address_offset.into()).unwrap();

        bus::write_u8(console, address, value);
    }
}

/// A u8 value read from $FF00 + (the u8 value stored at the instruction's immediate location)
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct U8FromFF00PlusU8;

impl ArgReaderIntoWriter for U8FromFF00PlusU8 {
    type Arg = u8;

    fn read_into_writer(
        self,
        console: &mut Console,
    ) -> (Box<dyn ArgWriter<Arg = Self::Arg>>, Self::Arg) {
        let address_base: u16 = 0xFF00;

        let pc = console.cpu.pc;
        let address_offset = bus::read_u8(console, pc);
        let address = address_base.checked_add(address_offset.into()).unwrap();

        let writer = WriteU8 { address };
        let value = bus::read_u8(console, address);
        console.cpu.pc += 1;

        (Box::new(writer), value)
    }
}

/// A writer for a u8 value
pub struct WriteU8 {
    address: u16,
}

impl ArgWriter for WriteU8 {
    type Arg = u8;

    fn write(self, console: &mut Console, value: Self::Arg) {
        bus::write_u8(console, self.address, value);
    }
}

/// A u8 value read from the memory address stored in the given register
pub struct U8FromRegister16 {
    register: cpu::Register16,
}

/// A u8 value read from the memory address stored in cpu.hl
/// cpu.hl is incremented after reading and after writing
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct U8FromHlInc;

impl ArgReader for U8FromHlInc {
    type Arg = u8;

    /// Read the value of this argument from memory at the address stored in cpu.hl
    /// Increments cpu.hl
    ///
    /// # Arguments
    ///
    /// * `console`: The gameboy console object
    fn read(self, console: &mut Console) -> Self::Arg {
        let address = console.cpu.register_16(cpu::Register16::HL);
        let value = bus::read_u8(console, address);

        let hl_inc = address.wrapping_add(1);
        console.cpu.set_register_16(cpu::Register16::HL, hl_inc);

        value
    }
}

impl ArgWriter for U8FromHlInc {
    type Arg = u8;

    /// Write the value of this argument to memory at the address stored in cpu.hl
    /// Increments cpu.hl
    ///
    /// # Arguments
    ///
    /// * `console`: The gameboy console object
    /// * `value`: The value to write
    ///
    fn write(self, console: &mut Console, value: Self::Arg) {
        let address = console.cpu.register_16(cpu::Register16::HL);
        bus::write_u8(console, address, value);

        let hl_inc = address.wrapping_add(1);
        console.cpu.set_register_16(cpu::Register16::HL, hl_inc);
    }
}

/// A u8 value read from the memory address stored in cpu.hl
/// cpu.hl is decremented after reading and after writing
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct U8FromHlDec;

impl ArgReader for U8FromHlDec {
    type Arg = u8;

    /// Read the value of this argument from memory at the address stored in cpu.hl
    /// Decrements cpu.hl
    ///
    /// # Arguments
    ///
    /// * `console`: The gameboy console object
    fn read(self, console: &mut Console) -> Self::Arg {
        let address = console.cpu.register_16(cpu::Register16::HL);
        let value = bus::read_u8(console, address);

        let hl_inc = address.wrapping_sub(1);
        console.cpu.set_register_16(cpu::Register16::HL, hl_inc);

        value
    }
}

impl ArgWriter for U8FromHlDec {
    type Arg = u8;

    /// Write the value of this argument to memory at the address stored in cpu.hl
    /// Decrements cpu.hl
    ///
    /// # Arguments
    ///
    /// * `console`: The gameboy console object
    /// * `value`: The value to write
    fn write(self, console: &mut Console, value: Self::Arg) {
        let address = console.cpu.register_16(cpu::Register16::HL);
        bus::write_u8(console, address, value);

        let hl_inc = address.wrapping_sub(1);
        console.cpu.set_register_16(cpu::Register16::HL, hl_inc);
    }
}

/// A u16 value equal to cpu.sp plus an i8 value read from the instruction's immediate location
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
struct SpPlusI8;

impl ArgReader for SpPlusI8 {
    type Arg = u16;

    fn read(self, console: &mut Console) -> Self::Arg {
        let pc = console.cpu.pc;
        let sp = console.cpu.sp;
        let offset = bus::read_i8(console, pc);
        console.cpu.pc += 1;

        let value = sp.wrapping_add_signed(offset.into());
        value
    }
}

/// A u8 value read from the instruction's immediate location
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct U8;

impl ArgReader for U8 {
    type Arg = u8;

    fn read(self, console: &mut Console) -> Self::Arg {
        let pc = console.cpu.pc;
        let value = bus::read_u8(console, pc);
        console.cpu.pc += 1;
        value
    }
}

/// An i8 value read from the instruction's immediate location
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct I8;

impl ArgReader for I8 {
    type Arg = i8;

    fn read(self, console: &mut Console) -> Self::Arg {
        let pc = console.cpu.pc;
        let value = bus::read_i8(console, pc);
        console.cpu.pc += 1;
        value
    }
}

/// A u16 value read from the instruction's immediate location
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct U16;

impl ArgReader for U16 {
    type Arg = u16;

    fn read(self, console: &mut Console) -> Self::Arg {
        let pc = console.cpu.pc;
        let value = bus::read_u16(console, pc);
        console.cpu.pc += 2;
        value
    }
}

/// A u16 value read from memory at the address stored in the instruction's immediate location
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct U8FromU16;

impl ArgReaderIntoWriter for U8FromU16 {
    type Arg = u8;

    fn read_into_writer(
        self,
        console: &mut Console,
    ) -> (Box<dyn ArgWriter<Arg = Self::Arg>>, Self::Arg) {
        let pc = console.cpu.pc;
        let address = bus::read_u16(console, pc);

        let writer = WriteU8 { address };
        let value = bus::read_u8(console, address);
        console.cpu.pc += 2;
        (Box::new(writer), value)
    }
}
