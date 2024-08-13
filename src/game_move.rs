use crate::bitboard::BitBoard;
use crate::engine::MoveType;


const OLD_POS_SHIFT: u64 = 9;
const NEW_POS_SHIFT: u64 = 3;
const POS_MASK: u64 = 0b111111;
const MOVE_TYPE_MASK: u64 = 0b111;

#[derive(PartialEq, Eq, Copy, Clone)]
pub struct Move(u64); 


// In format old_pos, new_pos, move_type in binary
impl Move {
    pub fn new(old_pos: BitBoard, new_pos: BitBoard, move_type: MoveType) -> Self {
        let old_pos_num = (old_pos.extract_tile() << OLD_POS_SHIFT) as u64;
        let new_pos_num = (new_pos.extract_tile() << NEW_POS_SHIFT) as u64;
        let move_type_num = move_type as u64;
        //let result = old_pos_num | new_pos_num | move_type_num;
        //println!("old pos");
        //println!("{:#b}", old_pos_num);
        //println!("new_pos");
        //println!("{:#b}", new_pos_num);
        //println!("move type");
        //println!("{:#b}", move_type_num);
        //jprintln!("result");
        //println!("{:#b}", result);



        Self(old_pos_num | new_pos_num | move_type_num)
    }

    pub fn get(&self) -> (BitBoard, BitBoard, MoveType) {
        let move_type = match self.0 & MOVE_TYPE_MASK  {
            0 => MoveType::Normal,
            1 => MoveType::PawnStart,
            2 => MoveType::Castle,
            3 => MoveType::PromotionQueen,
            4 => MoveType::PromotionBishop,
            5 => MoveType::PromotionKnight,
            6 => MoveType::PromotionRook,
            7 => MoveType::Empassant,
            _ => panic!("Shouldn't go here"),

        };
        
        (BitBoard::from_square(((self.0 >> OLD_POS_SHIFT) & POS_MASK) as usize), 
         BitBoard::from_square(((self.0 >> NEW_POS_SHIFT) & POS_MASK) as usize),
         move_type)

    }


}



