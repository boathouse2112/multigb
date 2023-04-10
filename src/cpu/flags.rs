#[derive(Clone, Debug)]
pub struct Flags {
    pub zero: bool,
    pub negative: bool,
    pub half_carry: bool,
    pub carry: bool,
}

impl Flags {
    pub fn new() -> Self {
        Flags {
            zero: false,
            negative: false,
            half_carry: false,
            carry: false,
        }
    }

    pub fn bits(&self) -> u8 {
        let z = if self.zero { 1 } else { 0 }; // bit 7
        let n = if self.negative { 1 } else { 0 }; // bit 6
        let h = if self.half_carry { 1 } else { 0 }; // bit 5
        let c = if self.carry { 1 } else { 0 }; // bit 4

        return ((z << 7) + (n << 6) + (h << 5) + (c << 4)) as u8;
    }
}

impl From<u8> for Flags {
    fn from(value: u8) -> Self {
        let z = (value & 0b1000_0000) != 0; // bit 7
        let n = (value & 0b0100_0000) != 0; // bit 6
        let h = (value & 0b0010_0000) != 0; // bit 5
        let c = (value & 0b0001_0000) != 0; // bit 5

        return Flags {
            zero: z,
            negative: n,
            half_carry: h,
            carry: c,
        };
    }
}
