use rand::{Rng, SeedableRng};
use rand::rngs::StdRng;
use crate::engine;
use crate::engine::Piece;
use crate::bitboard::BitBoard;


const SIDE_TABLE_SIZE: usize = 2;
const PIECE_TABLE_SIZE: usize = 64;
const CASTLING_TABLE_SIZE: usize = 16;
const EMPASSANT_TABLE_SIZE: usize = 8;

pub struct ZobristTable {
    side_key: Vec<u64>,
    white_pawn_key: Vec<u64>,
    black_pawn_key: Vec<u64>,
    white_knight_key: Vec<u64>,
    black_knight_key: Vec<u64>,
    white_bishop_key: Vec<u64>,
    black_bishop_key: Vec<u64>,
    white_rook_key: Vec<u64>,
    black_rook_key: Vec<u64>,
    white_queen_key: Vec<u64>,
    black_queen_key: Vec<u64>,
    white_king_key: Vec<u64>,
    black_king_key: Vec<u64>,
    castling_rights: Vec<u64>,
    empassant_key: Vec<u64>,
}



impl ZobristTable {
    pub fn new() -> Self {
        let mut rng = StdRng::seed_from_u64(12345);
        Self {
            side_key: Self::get_random_table(&mut rng, SIDE_TABLE_SIZE),
            white_pawn_key: Self::get_random_table(&mut rng, PIECE_TABLE_SIZE),
            black_pawn_key: Self::get_random_table(&mut rng, PIECE_TABLE_SIZE),
            white_knight_key: Self::get_random_table(&mut rng, PIECE_TABLE_SIZE),
            black_knight_key: Self::get_random_table(&mut rng, PIECE_TABLE_SIZE),
            white_bishop_key: Self::get_random_table(&mut rng, PIECE_TABLE_SIZE),
            black_bishop_key: Self::get_random_table(&mut rng, PIECE_TABLE_SIZE),
            white_rook_key: Self::get_random_table(&mut rng, PIECE_TABLE_SIZE),
            black_rook_key: Self::get_random_table(&mut rng, PIECE_TABLE_SIZE),
            white_queen_key: Self::get_random_table(&mut rng, PIECE_TABLE_SIZE),
            black_queen_key: Self::get_random_table(&mut rng, PIECE_TABLE_SIZE),
            white_king_key: Self::get_random_table(&mut rng, PIECE_TABLE_SIZE),
            black_king_key: Self::get_random_table(&mut rng, PIECE_TABLE_SIZE),
            castling_rights: Self::get_random_table(&mut rng, CASTLING_TABLE_SIZE),
            empassant_key: Self::get_random_table(&mut rng, EMPASSANT_TABLE_SIZE),
        }
    }
    
    fn get_key_for_white_piece(&self, piece: &engine::Piece) -> u64 {
        let tile = piece.pos.extract_tile();
        match piece.piece_type {
            engine::PieceType::Pawn => self.white_pawn_key[tile],
            engine::PieceType::Knight => self.white_knight_key[tile],
            engine::PieceType::Bishop => self.white_bishop_key[tile],
            engine::PieceType::Rook => self.white_rook_key[tile],
            engine::PieceType::Queen => self.white_queen_key[tile],
            engine::PieceType::King => self.white_king_key[tile]
        }
    }

    fn get_key_for_black_piece(&self, piece: &engine::Piece) -> u64 {
        let tile = piece.pos.extract_tile();
        match piece.piece_type {
            engine::PieceType::Pawn => self.black_pawn_key[tile],
            engine::PieceType::Knight => self.black_knight_key[tile],
            engine::PieceType::Bishop => self.black_bishop_key[tile],
            engine::PieceType::Rook => self.black_rook_key[tile],
            engine::PieceType::Queen => self.black_queen_key[tile],
            engine::PieceType::King => self.black_king_key[tile]
        }
    }

    fn get_castling_rights(&self, white_left_castle: bool, white_right_castle: bool,
                                  black_left_castle: bool, black_right_castle: bool) -> usize {
        let white_right_key = white_right_castle as usize;
        let white_left_key = (white_left_castle as usize) << 1;
        let black_left_key = (black_left_castle as usize) << 2;
        let black_right_key = (black_right_castle as usize) << 3;
        white_right_key | white_left_key | black_left_key | black_right_key
    }

    pub fn calculate_initial_key(&self, boardstate: &engine::BoardState) -> u64 {
        let mut zobrist = self.side_key[boardstate.active_player as usize];
        boardstate.white_pieces.values().for_each(|piece| {
            zobrist ^= self.get_key_for_white_piece(&piece)
        });
        boardstate.black_pieces.values().for_each(|piece| {
            zobrist ^= self.get_key_for_black_piece(&piece) 
        });
        zobrist ^= self.castling_rights[self.get_castling_rights(
        boardstate.white_can_left_castle,
        boardstate.white_can_right_castle,
        boardstate.black_can_left_castle,
        boardstate.black_can_right_castle)];
        if let Some(empassant_pos) = boardstate.empassant_pos {
            let (x, _) = empassant_pos.extract_position();
            zobrist ^= self.empassant_key[x as usize];
        }
        zobrist
    }

    pub fn calculate_subsequent_key(&self,
        old_zobrist: u64,
        white_old_pieces: &Vec<Piece>,
        white_new_pieces: &Vec<Piece>,
        black_old_pieces: &Vec<Piece>,
        black_new_pieces: &Vec<Piece>,
        old_white_can_left_castle: bool,
        old_white_can_right_castle: bool,
        old_black_can_left_castle: bool,
        old_black_can_right_castle: bool,
        old_empassant_pos: &Option<BitBoard>,
        new_white_can_left_castle: bool,
        new_white_can_right_castle: bool,
        new_black_can_left_castle: bool,
        new_black_can_right_castle: bool,
        new_empassant_pos: &Option<BitBoard>) -> u64 {
        let mut new_zobrist = old_zobrist;
        new_zobrist ^= self.side_key[engine::Player::Black as usize];
        new_zobrist ^= self.side_key[engine::Player::White as usize];
        white_old_pieces.iter().for_each(|piece| {
            new_zobrist ^= self.get_key_for_white_piece(&piece);
        });
        white_new_pieces.iter().for_each(|piece| {
            new_zobrist ^= self.get_key_for_white_piece(&piece);
        });
        black_old_pieces.iter().for_each(|piece| {
            new_zobrist ^= self.get_key_for_black_piece(&piece);
        });
        black_new_pieces.iter().for_each(|piece| {
            new_zobrist ^= self.get_key_for_black_piece(&piece);
        });
        new_zobrist ^= self.castling_rights[self.get_castling_rights(old_white_can_left_castle,
            old_white_can_right_castle, old_black_can_left_castle,
            old_black_can_right_castle)];
        new_zobrist ^= self.castling_rights[self.get_castling_rights(new_white_can_left_castle,
            new_white_can_right_castle, new_black_can_left_castle,
            new_black_can_right_castle)];
        if let Some(pos) = old_empassant_pos {
            new_zobrist ^= self.empassant_key[pos.extract_position().0 as usize];
        }
        if let Some(pos) = new_empassant_pos {
            new_zobrist ^= self.empassant_key[pos.extract_position().0 as usize];
        }
        new_zobrist

    }


    fn get_random_table<R: Rng>(rng: &mut R, table_size: usize) -> Vec<u64> {
        let mut random_values = Vec::with_capacity(table_size);
        for _ in 0..table_size {
            random_values.push(Self::get_random_value(rng));
        }
        random_values
    }



    fn get_random_value<R: Rng>(rng: &mut R) -> u64 {
        rng.gen()
    }




}
