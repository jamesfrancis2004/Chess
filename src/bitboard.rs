use std::fmt;

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub struct BitBoard {
    pub bitboard: i64
}


impl BitBoard {
    pub fn new() -> Self {
        Self {
            bitboard: 0,
        }

    }
    pub fn from_pos(x: i64, y: i64) -> Self {
        let shifted = 1 << ((8 * y) + x);
        Self {
            bitboard: shifted
        }
    }

    pub fn from_num(x: i64) -> BitBoard {
        Self {
            bitboard: x,
        }

    }
    
    pub fn from_square(square: usize) -> BitBoard {
        Self {
            bitboard: 1 << square,
        }

    }

    pub fn extract_position(&self) -> (usize, usize) {
        let square = self.extract_tile();
        let y_val = square / 8;
        let x_val = square % 8;
        (x_val, y_val)
    }

    pub fn extract_tile(&self) -> usize {
        self.bitboard.trailing_zeros() as usize
    }


    
    pub fn combine(&mut self,  other: BitBoard) {
        self.bitboard |= other.bitboard;
    }

    pub fn combine_all(boards: &[BitBoard]) -> Self {
        let mut board = BitBoard::new();
        boards.iter().for_each(|x| board.combine(*x));
        board
    }

    pub fn extract_all(&self) -> Vec<BitBoard> {
        let mut to_return = vec![];
        let mut board_repr = self.bitboard;
        while board_repr != 0 {
            let lsb = board_repr & -board_repr;
            board_repr &= !lsb;
            to_return.push(BitBoard::from_num(lsb));

        }
        to_return
    }

}


impl fmt::Display for BitBoard {
    // Format is for debug reasons. Shows a pieces pos as a layout of 1's and 0's 
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for y in 0..8 {
            for x in 0..8 {
                let current = (self.bitboard >> ((8 * (7-y)) + x)) & 1;
                write!(f, "{}", current).unwrap();
            }
            write!(f, "\n").unwrap();

        }
        write!(f, "")



    }
}





