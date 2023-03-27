#[derive(Clone, Debug)]
pub struct Flags {
    zero: bool,
    negative: bool,
    half_carry: bool,
    carry: bool,
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
        let z = if self.zero { 1 } else { 0 };
        let n = if self.negative { 1 } else { 0 };
        let h = if self.half_carry { 1 } else { 0 };
        let c = if self.carry { 1 } else { 0 };

        return ((z << 7) + (n << 6) + (h << 5) + (c << 4)) as u8;
    }
}
