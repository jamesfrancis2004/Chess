use indexmap::IndexMap;
use crate::bitboard::BitBoard;
use crate::constants;
use crate::magic_bitboard::MagicTable;
use crate::game_move::Move;
use crate::zobrist_table::ZobristTable;


const WH_LEFT_CASTLE: i64 = 31;

const WH_RIGHT_CASTLE: i64 = 240;

const BL_LEFT_CASTLE: i64 = 2233785415175766016;

const BL_RIGHT_CASTLE: i64 = -1152921504606846976;

const WH_LEFT_CASTLE_BLOCK: i64 = 14;

const WH_RIGHT_CASTLE_BLOCK: i64 = 96;

const BL_LEFT_CASTLE_BLOCK: i64 = 1008806316530991104;

const BL_RIGHT_CASTLE_BLOCK: i64 = 6917529027641081856;



#[derive(Debug)]
pub struct Sniper<'a> {
    sniper_piece: &'a Piece,
    sniper_bitboard: BitBoard,
}



impl<'a> Sniper<'a> {
    fn new(sniper_piece: &'a Piece, sniper_bitboard: BitBoard) -> Self {
        Self {
            sniper_piece,
            sniper_bitboard
        }
    }

}


#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PieceType {
    Pawn = 5,
    Knight = 4,
    Bishop = 3,
    Rook = 2,
    Queen = 1,
    King = 0,
}


#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub struct Piece {
    pub piece_type: PieceType,
    pub pos: BitBoard,
}

#[derive(PartialEq, Eq, Clone)]
struct PastMove {
    pub old_white_pieces: Vec<Piece>,
    pub new_white_pieces: Vec<Piece>,
    pub old_black_pieces: Vec<Piece>,
    pub new_black_pieces: Vec<Piece>,
    pub white_can_left_castle: bool,
    pub white_can_right_castle: bool,
    pub black_can_left_castle: bool,
    pub black_can_right_castle: bool,
    pub empassant_pos: Option<BitBoard>

}


impl PastMove {
    fn new(active_old_pieces: Vec<Piece>, 
           active_new_pieces: Vec<Piece>,
           enemy_old_pieces: Vec<Piece>,
           white_can_left_castle: bool,
           white_can_right_castle: bool,
           black_can_left_castle: bool,
           black_can_right_castle: bool,
           empassant_pos: Option<BitBoard>,
           active_player: Player) -> Self {
        let (old_white_pieces, new_white_pieces,
             old_black_pieces, new_black_pieces) = if active_player == Player::White {
            (active_old_pieces,
            active_new_pieces,
            enemy_old_pieces,
            Vec::new())
        } else {
            (enemy_old_pieces,
            Vec::new(),
            active_old_pieces,
            active_new_pieces)
        };
        PastMove {
            old_white_pieces,
            new_white_pieces,
            old_black_pieces,
            new_black_pieces,
            white_can_left_castle,
            white_can_right_castle,
            black_can_left_castle,
            black_can_right_castle,
            empassant_pos
        }


        }

}

impl Piece {
    fn new(piece_type: PieceType, pos: BitBoard) -> Self {
        Self { 
            piece_type,
            pos
        }
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Player {
    White = 0,
    Black = 1
}


#[derive(Debug, PartialEq, Clone, Copy, Eq)]
pub enum MoveType {
    Normal = 0, 
    PawnStart = 1,
    Castle = 2, 
    PromotionQueen = 3,
    PromotionBishop = 4,
    PromotionKnight = 5,
    PromotionRook = 6,
    Empassant = 7,
}



pub struct BoardState {
    pub active_player: Player,
    pub white_pieces: IndexMap<BitBoard, Piece>,
    pub black_pieces: IndexMap<BitBoard, Piece>,
    white_layout: BitBoard,
    black_layout: BitBoard,
    rook_magic_table: MagicTable,
    bishop_magic_table: MagicTable,
    hori_between_tiles: Vec<Vec<u64>>,
    vert_between_tiles: Vec<Vec<u64>>,
    diag_between_tiles: Vec<Vec<u64>>,
    pub empassant_pos: Option<BitBoard>,
    pub white_can_left_castle: bool,
    pub white_can_right_castle: bool,
    pub black_can_left_castle: bool,
    pub black_can_right_castle: bool,
    zobrist_table: ZobristTable,
    pub zobrist_key: u64,
    past_moves: Vec<PastMove>
}

impl BoardState {
    fn calculate_layout(piece_list: &IndexMap<BitBoard, Piece>) -> BitBoard {
        BitBoard::combine_all(&piece_list.keys().map(|x| *x).collect::<Vec<_>>())

    }

    pub fn new() -> Self {
        let mut black_pieces = (0..8)
            .into_iter()
            .map(|x| Piece::new(PieceType::Pawn, BitBoard::from_pos(x, 6))).collect::<Vec<_>>();
        let mut white_pieces = (0..8)
            .into_iter()
            .map(|x| Piece::new(PieceType::Pawn, BitBoard::from_pos(x, 1))).collect::<Vec<_>>();
        // Starting with PAWNS first. Collects them into arrays and attaches their bitboards
        black_pieces.extend([0, 7]
            .into_iter()
            .map(|x| Piece::new(PieceType::Rook, BitBoard::from_pos(x, 7))).collect::<Vec<_>>());
        white_pieces.extend([0, 7]
            .into_iter()
            .map(|x| Piece::new(PieceType::Rook, BitBoard::from_pos(x, 0))).collect::<Vec<_>>());
        // Next we assemble the Rooks. 
        black_pieces.extend([1, 6]
            .into_iter()
            .map(|x| Piece::new(PieceType::Knight, BitBoard::from_pos(x, 7))).collect::<Vec<_>>());
        white_pieces.extend([1, 6]
            .into_iter()
            .map(|x| Piece::new(PieceType::Knight, BitBoard::from_pos(x, 0))).collect::<Vec<_>>());
        // Next the knights
        black_pieces.extend([2, 5]
            .into_iter()
            .map(|x| Piece::new(PieceType::Bishop, BitBoard::from_pos(x, 7))).collect::<Vec<_>>());
        white_pieces.extend([2, 5]
            .into_iter()
            .map(|x| Piece::new(PieceType::Bishop, BitBoard::from_pos(x, 0))).collect::<Vec<_>>());
        // Now the Bishops 
        black_pieces.push(Piece::new(PieceType::Queen, BitBoard::from_pos(3, 7)));
        white_pieces.push(Piece::new(PieceType::Queen, BitBoard::from_pos(3, 0)));
        // Both Queens
        black_pieces.push(Piece::new(PieceType::King, BitBoard::from_pos(4, 7)));
        white_pieces.push(Piece::new(PieceType::King, BitBoard::from_pos(4, 0)));
        let white_hashed = white_pieces.into_iter().map(|x| {
            (x.pos, x)
        }).collect::<IndexMap<BitBoard, Piece>>();
        let black_hashed = black_pieces.into_iter().map(|x| {
            (x.pos, x)
        }).collect::<IndexMap<BitBoard, Piece>>();

        // Both Kings
        let white_layout = Self::calculate_layout(&white_hashed);
        let black_layout = Self::calculate_layout(&black_hashed);
        let mut boardstate = Self {
            active_player: Player::White,
            white_pieces: white_hashed,
            black_pieces: black_hashed,
            white_layout,
            black_layout,
            rook_magic_table: MagicTable::new(
                &constants::MAGIC_MOVES_R_MAGICS,
                &constants::MAGIC_MOVES_R_MASK,
                &constants::MAGIC_MOVES_R_SHIFT, 
                true),
            bishop_magic_table: MagicTable::new(
                &constants::MAGIC_MOVES_B_MAGICS,
                &constants::MAGIC_MOVES_B_MASK,
                &constants::MAGIC_MOVES_B_SHIFT,
                false,

            ),
            hori_between_tiles: Self::get_all_horis(),
            vert_between_tiles: Self::get_all_verts(),
            diag_between_tiles: Self::get_all_diags(),
            empassant_pos: None,
            white_can_left_castle: true,
            white_can_right_castle: true,
            black_can_left_castle: true,
            black_can_right_castle: true,
            zobrist_table: ZobristTable::new(),
            zobrist_key: 0,
            past_moves: Vec::new(),
        };
        boardstate.zobrist_key = boardstate.zobrist_table.calculate_initial_key(&boardstate);
        boardstate
    }

    fn get_diag_between_squares(pos1: i64, pos2: i64) -> u64 {
        if pos1 == pos2 {
            return 0
        }
        let (x1, x2) = (pos1 % 8, pos2 % 8);
        let (y1, y2) = (pos1 / 8, pos2 / 8);
        if (x1 - x2).abs() != (y1 - y2).abs() {
            return 0
        }
        let x_range = if x1 < x2 {
            (x1..x2).into_iter().collect::<Vec<_>>()

        } else {
            ((x2+1)..=x1).rev().into_iter().collect::<Vec<_>>()
        };
        let y_range = if y1 < y2 {
            (y1..y2).into_iter().collect::<Vec<_>>()

        } else {
            ((y2+1)..=y1).rev().into_iter().collect::<Vec<_>>()
        };
        BitBoard::combine_all(&x_range
            .into_iter()
            .zip(y_range.into_iter())
            .map(|(new_x, new_y)| BitBoard::from_pos(new_x, new_y)).collect::<Vec<_>>()).bitboard as u64

    }

    fn get_all_diags() -> Vec<Vec<u64>> {
        let mut boards = vec![];
        for tile1 in 0..64 {
            let mut temp_board = vec![];
            for tile2 in 0..64 {
                temp_board.push(Self::get_diag_between_squares(tile1, tile2));
            }
            boards.push(temp_board);

        }
        boards
    }



    fn get_vert_between_squares(pos1: u64, pos2: u64) -> u64 {
        if pos1 == pos2 {
            return 0
        }
        let (x1, x2) = (pos1 % 8, pos2 % 8);
        if x1 != x2 {
            return 0
        }
        let (y1, y2) = (pos1 / 8, pos2 / 8);
        let iterator = if y1 < y2 {
            y1..y2
        } else {
            (y2+1)..(y1+1)
        };
        //let (low, high) = ([y1, y2].into_iter().min().unwrap(), [y1, y2].into_iter().max().unwrap());
        let mut bitboard = BitBoard::combine_all(&iterator.into_iter().map(|new_y| {
            BitBoard::from_pos(x1 as i64, new_y as i64)
        }).collect::<Vec<_>>());
        bitboard.combine(BitBoard::from_pos((pos1 % 8) as i64, (pos1 / 8) as i64));
        bitboard.bitboard as u64
    }


    fn get_all_verts() -> Vec<Vec<u64>> {
        let mut boards = vec![];
        for tile1 in 0..64 {
            let mut temp_board = vec![];
            for tile2 in 0..64 {
                temp_board.push(Self::get_vert_between_squares(tile1, tile2));
            }
            boards.push(temp_board);

        }
        boards
    }



    fn get_hori_between_squares(pos1: u64, pos2: u64) -> u64 {
        if pos1 == pos2 {
            return 0
        }

        let (y1, y2) = (pos1 / 8, pos2 / 8);
        if y1 != y2 {
            return 0
        }
        let (x1, x2) = (pos1 % 8, pos2 % 8);
        let iterator = if x1 < x2 {
            x1..x2
        } else {
            (x2+1)..(x1+1)
        };

        //let (low, high) = ([x1, x2].into_iter().min().unwrap(), [x1, x2].into_iter().max().unwrap());
        let mut bitboard = BitBoard::combine_all(&iterator.into_iter().map(|new_x| {
            BitBoard::from_pos(new_x as i64, y1 as i64)
        }).collect::<Vec<_>>());
        bitboard.combine(BitBoard::from_pos((pos1 % 8) as i64, (pos1 / 8) as i64));
        bitboard.bitboard as u64
    }

    fn get_all_horis() -> Vec<Vec<u64>> {
        let mut boards = vec![];
        for tile1 in 0..64 {
            let mut temp_board = vec![];
            for tile2 in 0..64 {
                temp_board.push(Self::get_hori_between_squares(tile1, tile2));
            }
            boards.push(temp_board);

        }
        boards
    }

    fn change_castle_condition(&mut self, piece: &Piece) {
        if piece.piece_type == PieceType::King {
            if self.active_player == Player::White {
                self.white_can_left_castle = false;
                self.white_can_right_castle = false;
            } else {
                self.black_can_left_castle = false;
                self.black_can_right_castle = false;
            }
        }
        if piece.piece_type == PieceType::Rook {
            let (x, y) = piece.pos.extract_position();
            if self.active_player == Player::White && x == 0 && y == 0 {
                self.white_can_left_castle = false;
            } else if self.active_player == Player::White && x == 7 && y == 0 {
                self.white_can_right_castle = false;
            } else if self.active_player == Player::Black && x == 0 && y == 7 {
                self.black_can_left_castle = false;
            } else if self.active_player == Player::Black && x == 7 && y == 7 {
                self.black_can_right_castle = false;

            }
        }
    }


    fn change_enemy_castle_cond(&mut self, piece: &Piece) {
        if piece.piece_type == PieceType::Rook {
            let (x, y) = piece.pos.extract_position();
            if self.active_player == Player::White && x == 0 && y == 7 {
                self.black_can_left_castle = false;
            } else if self.active_player == Player::White && x == 7 && y == 7 {
                self.black_can_right_castle = false;
            } else if self.active_player == Player::Black && x == 0 && y == 0 {
                self.white_can_left_castle = false;
            } else if self.active_player == Player::Black && x == 7 && y == 0 {
                self.white_can_right_castle = false;
            }
        }

    }

    fn normal_move_piece(&mut self, new_move: Move) -> (Vec<Piece>, Vec<Piece>, Vec<Piece>) {
        self.empassant_pos = None;
        let mut active_old_pieces = Vec::new();
        let mut active_new_pieces = Vec::new();
        let mut enemy_old_pieces = Vec::new();
        let (old_pos, new_pos, _) = new_move.get();
        let mut active_piece = {
            let (active_list, _) = self.get_active_enemy_list_mut(self.active_player);
            Self::remove_piece_at_pos(old_pos, active_list).unwrap()
        };
        self.change_castle_condition(&active_piece);
        active_old_pieces.push(active_piece);
        let enemy_piece = {
            let (_, enemy_list) = self.get_active_enemy_list_mut(self.active_player);
            Self::remove_piece_at_pos(new_pos, enemy_list)
        };
        if let Some(piece) = enemy_piece {
            self.change_enemy_castle_cond(&piece);
            enemy_old_pieces.push(piece);
        };
        active_piece.pos = new_pos;
        active_new_pieces.push(active_piece);
        let (active_list, _) = self.get_active_enemy_list_mut(self.active_player);
        Self::add_piece_at_pos(active_piece, active_list);
        (active_old_pieces, active_new_pieces, enemy_old_pieces)
    }

    fn promotion_move(&mut self, new_move: Move) -> (Vec<Piece>, Vec<Piece>, Vec<Piece>) {
        self.empassant_pos = None;
        let mut active_old_pieces = Vec::new();
        let mut active_new_pieces = Vec::new();
        let mut enemy_old_pieces = Vec::new();
        let (old_pos, new_pos, move_type) = new_move.get();
        let mut active_piece = {
            let (active_list, _) = self.get_active_enemy_list_mut(self.active_player);
            Self::remove_piece_at_pos(old_pos, active_list).unwrap()
        };
        active_old_pieces.push(active_piece);
        let enemy_piece = {
            let (_, enemy_list) = self.get_active_enemy_list_mut(self.active_player);
            Self::remove_piece_at_pos(new_pos, enemy_list)
        };
        if let Some(piece) = enemy_piece {
            self.change_enemy_castle_cond(&piece);
            enemy_old_pieces.push(piece);
        };
        active_piece.pos = new_pos;
        active_piece.piece_type = match move_type {
            MoveType::PromotionKnight => PieceType::Knight,
            MoveType::PromotionBishop => PieceType::Bishop,
            MoveType::PromotionRook => PieceType::Rook,
            MoveType::PromotionQueen => PieceType::Queen,
            _ => panic!("Shouldn't hit this branch"),
        };
        active_new_pieces.push(active_piece);
        let (active_list, _) = self.get_active_enemy_list_mut(self.active_player);
        Self::add_piece_at_pos(active_piece, active_list);
        (active_old_pieces, active_new_pieces, enemy_old_pieces)

    }

    fn empassant_move(&mut self, new_move: Move) -> (Vec<Piece>, Vec<Piece>, Vec<Piece>) {
        self.empassant_pos = None;
        let y_ch = if self.active_player == Player::White {
            -1
        } else {
            1
        };
        let mut active_old_pieces = Vec::new();
        let mut active_new_pieces = Vec::new();
        let mut enemy_old_pieces = Vec::new();
        let (old_pos, new_pos, _) = new_move.get();
        let (active_list, enemy_list) = self.get_active_enemy_list_mut(self.active_player);
        let mut active_piece = Self::remove_piece_at_pos(old_pos, active_list).unwrap();
        active_old_pieces.push(active_piece);
        if let Some(piece) = Self::remove_piece_at_pos(new_pos, enemy_list) {
            enemy_old_pieces.push(piece);
        };
        active_piece.pos = new_pos;
        active_new_pieces.push(active_piece);
        Self::add_piece_at_pos(active_piece, active_list);
        let (x_pos, y_pos) = new_pos.extract_position();
        let enemy_piece_pos = BitBoard::from_pos(x_pos as i64, y_pos as i64 + y_ch);
        if let Some(piece) = Self::remove_piece_at_pos(enemy_piece_pos, enemy_list) {
            enemy_old_pieces.push(piece);
        }
        (active_old_pieces, active_new_pieces, enemy_old_pieces)

    }

    fn pawn_start_move(&mut self, new_move: Move) -> (Vec<Piece>, Vec<Piece>, Vec<Piece>) {
        let (x, y) = new_move.get().1.extract_position() ;
        let y_ch = if self.active_player == Player::White {
            -1
        } else {
            1
        };
        let to_return = self.normal_move_piece(new_move);
        self.empassant_pos = Some(BitBoard::from_pos(x as i64, y as i64 + y_ch));
        to_return

    }

    fn castle_move(&mut self, new_move: Move) -> (Vec<Piece>, Vec<Piece>, Vec<Piece>) {
        self.empassant_pos = None;
        let (active_list, _) = self.get_active_enemy_list_mut(self.active_player);
        let mut active_old_pieces = Vec::new();
        let mut active_new_pieces = Vec::new();
        let (old_pos, new_pos, _) = new_move.get();
        let (old_x, old_y) = old_pos.extract_position();
        let (new_x, _new_y) = new_pos.extract_position();
        let (old_rook_pos, new_rook_pos) = if (new_x as i64) - (old_x as i64) > 0 {
            (BitBoard::from_pos(7, old_y as i64), 
            BitBoard::from_pos((new_x - 1) as i64, old_y as i64))
        } else {
            (BitBoard::from_pos(0, old_y as i64), 
            BitBoard::from_pos((new_x + 1) as i64, old_y as i64))
        };
        let mut king_piece = Self::remove_piece_at_pos(old_pos, active_list).unwrap();
        let mut rook_piece = Self::remove_piece_at_pos(old_rook_pos, active_list).unwrap();
        active_old_pieces.extend([king_piece, rook_piece]);
        king_piece.pos = new_pos;
        rook_piece.pos = new_rook_pos;
        active_new_pieces.extend([king_piece, rook_piece]);
        Self::add_piece_at_pos(king_piece, active_list);
        Self::add_piece_at_pos(rook_piece, active_list);
        if self.active_player == Player::White {
            self.white_can_left_castle = false;
            self.white_can_right_castle = false;
        } else {
            self.black_can_left_castle = false;
            self.black_can_right_castle = false;
        }
        (active_old_pieces, active_new_pieces, Vec::new())
        


    }



    pub fn move_piece(&mut self, new_move: Move) {
        // Have to store states before move is made
        let (old_white_can_left_castle,
             old_white_can_right_castle,
             old_black_can_left_castle,
             old_black_can_right_castle,
             old_empassant_pos) = (self.white_can_left_castle,
                               self.white_can_right_castle,
                               self.black_can_left_castle,
                               self.black_can_right_castle,
                               self.empassant_pos);
        let (_old_pos, _new_pos, move_type) = new_move.get();
        let (active_old, active_new, enemy_old) = match move_type {
            MoveType::Normal => self.normal_move_piece(new_move),
            MoveType::PawnStart => self.pawn_start_move(new_move),
            MoveType::Empassant => self.empassant_move(new_move),
            MoveType::PromotionKnight => self.promotion_move(new_move),
            MoveType::PromotionBishop => self.promotion_move(new_move),
            MoveType::PromotionRook => self.promotion_move(new_move),
            MoveType::PromotionQueen => self.promotion_move(new_move),
            MoveType::Castle => self.castle_move(new_move),
        };
        self.past_moves.push(PastMove::new(
            active_old,
            active_new,
            enemy_old,
            old_white_can_left_castle,
            old_white_can_right_castle,
            old_black_can_left_castle,
            old_black_can_right_castle,
            old_empassant_pos,
            self.active_player));

        self.white_layout = Self::calculate_layout(&self.white_pieces);
        self.black_layout = Self::calculate_layout(&self.black_pieces);
        self.switch_active_player();
        let past_move = self.past_moves.last().unwrap();
        self.zobrist_key = self.zobrist_table.calculate_subsequent_key(
            self.zobrist_key,
            &past_move.old_white_pieces,
            &past_move.new_white_pieces,
            &past_move.old_black_pieces,
            &past_move.new_black_pieces,
            past_move.white_can_left_castle,
            past_move.white_can_right_castle,
            past_move.black_can_left_castle,
            past_move.black_can_right_castle,
            &past_move.empassant_pos,
            self.white_can_left_castle,
            self.white_can_right_castle,
            self.black_can_left_castle,
            self.black_can_right_castle,
            &self.empassant_pos);

    }



    fn remove_piece_at_pos(pos: BitBoard, piece_list: &mut IndexMap<BitBoard, Piece>) 
    -> Option<Piece> {
        piece_list.shift_remove(&pos)
    }

    fn add_piece_at_pos(piece: Piece, piece_list: &mut IndexMap<BitBoard, Piece>) {
        piece_list.insert(piece.pos, piece);
    }

    fn switch_active_player(&mut self) {
        self.active_player = match self.active_player {
            Player::White => Player::Black,
            Player::Black => Player::White,
        }
    }


    fn get_active_enemy_layout(&self, active_player: Player) -> (BitBoard, BitBoard) {
        if active_player == Player::White {
            (self.white_layout, self.black_layout)
        } else {
            (self.black_layout, self.white_layout)
        }
    }

    fn get_active_enemy_list_mut(&mut self, active_player: Player) -> (&mut IndexMap<BitBoard, Piece>, &mut IndexMap<BitBoard, Piece>) {
        if active_player == Player::White {
            (&mut self.white_pieces, &mut self.black_pieces)
        } else {
            (&mut self.black_pieces, &mut self.white_pieces)
        }


    }

    fn get_active_enemy_list(&self, active_player: Player) -> (&IndexMap<BitBoard, Piece>, &IndexMap<BitBoard, Piece>) {
        if active_player == Player::White {
            (&self.white_pieces, &self.black_pieces)
        } else {
            (&self.black_pieces, &self.white_pieces)
        }
    }



    fn get_pawn_snipes(&self, piece: &Piece, piece_pos: BitBoard, player: Player) -> Option<BitBoard> {
        let (x_pos, y_pos) = piece.pos.extract_position();
        let attack_moves = if player == Player::White {
            constants::BLACK_PAWN_DIAG_MOVES
        } else {
            constants::WHITE_PAWN_DIAG_MOVES
        };
        if attack_moves[x_pos][y_pos] & piece_pos.bitboard != 0 {
            Some(BitBoard::from_num(piece.pos.bitboard | piece_pos.bitboard))
        } else {
            None
        }
    }



    fn get_knight_snipes(piece: &Piece, piece_pos: BitBoard) -> Option<BitBoard> {
        let (x_pos, y_pos) = piece.pos.extract_position();
        if constants::KNIGHT_MOVES_AT_POS[x_pos][y_pos] & piece_pos.bitboard != 0 {
            Some(BitBoard::from_num(piece.pos.bitboard | piece_pos.bitboard))
        } else {
            None

        }
    }

    fn get_rook_snipes(&self, piece: & Piece, piece_pos: BitBoard, max_blocker: u32, 
        combined_layout: i64) -> Option<BitBoard> {
        let (piece_x, piece_y) = piece.pos.extract_position();
        let (other_x, other_y) = piece_pos.extract_position();
        if piece_x == other_x {
            let mask = self.vert_between_tiles[piece.pos.extract_tile()][piece_pos.extract_tile()] as i64;
            let blockers = mask & combined_layout;
            if blockers.count_ones() < max_blocker {
                Some(BitBoard::from_num(mask))
            } else {
                None
            }
        } else if piece_y == other_y {
            let mask = self.hori_between_tiles[piece.pos.extract_tile()][piece_pos.extract_tile()] as i64;
            let blockers = mask & combined_layout;
            if blockers.count_ones() < max_blocker {
                Some(BitBoard::from_num(mask))
            } else {
                None
            }
        } else {
            None
        }
    }

    fn get_bishop_snipers(&self, piece: &Piece, piece_pos: BitBoard, max_blocker: u32, 
        combined_layout: i64) -> Option<BitBoard> {
        let mask = self.diag_between_tiles[piece.pos.extract_tile()][piece_pos.extract_tile()] as i64;
        if mask == 0 {
            return None;
        }
        let blockers = mask & combined_layout;
        if blockers.count_ones() < max_blocker {
            Some(BitBoard::from_num(mask))
        } else {
            None
        }

        //todo!()
    }

    fn get_queen_snipes(&self, piece: &Piece, piece_pos: BitBoard, max_blocker: u32,
                        combined_layout: i64) -> Option<BitBoard> {
        if let Some(board) = self.get_rook_snipes(piece, piece_pos, max_blocker, combined_layout) {
            Some(board)
        } else {
            self.get_bishop_snipers(piece, piece_pos, max_blocker, combined_layout)
        }

    }

    fn get_king_snipes(&self, piece: &Piece, piece_pos: BitBoard) -> Option<BitBoard> {
        let (x_pos, y_pos) = piece.pos.extract_position();
        if constants::KING_MOVES[x_pos][y_pos] & piece_pos.bitboard != 0 {
            Some(BitBoard::from_num(piece.pos.bitboard | piece_pos.bitboard))
        } else {
            None
        }
    }

    

    fn get_piece_snipers<'a>(&'a self,
        piece: &'a Piece,
        piece_pos: BitBoard,
        active_player: Player,
        max_blocker: u32,
        combined_layout: i64) -> Option<BitBoard> {
        match piece.piece_type {
            PieceType::Pawn => self.get_pawn_snipes(piece, piece_pos, active_player),
            PieceType::Knight => Self::get_knight_snipes(piece, piece_pos),
            PieceType::Bishop => self.get_bishop_snipers(piece, piece_pos, max_blocker, combined_layout),
            PieceType::Rook => self.get_rook_snipes(piece, piece_pos, max_blocker, combined_layout),
            PieceType::Queen => self.get_queen_snipes(piece, piece_pos, max_blocker, combined_layout),
            PieceType::King => self.get_king_snipes(piece, piece_pos),
        }
    }

    fn get_snipers_from_king(&self, active_player: Player) -> Vec<Sniper> {
        let (active_list, enemy_list)  = self.get_active_enemy_list(active_player);
        let king = active_list.values().find(|x| x.piece_type == PieceType::King).expect("No king");
        let combined_layout = self.white_layout.bitboard | self.black_layout.bitboard;
        enemy_list.values().filter_map(|cur_piece| {
            if let Some(bitboard) = self.get_piece_snipers(cur_piece, king.pos, active_player, 3, combined_layout) {
                let cleaned_bitboard = BitBoard::from_num(bitboard.bitboard & !king.pos.bitboard);
                Some(Sniper::new(cur_piece, cleaned_bitboard))
            } else {
                None
            }

        }).collect::<Vec<_>>()
    }

    fn get_snipers_to_pos(&self, piece_pos: BitBoard, active_player: Player) -> BitBoard {
        let (active_list, _enemy_list)  = self.get_active_enemy_list(active_player);
        let king = active_list.values().find(|x| x.piece_type == PieceType::King).expect("No king");
        let combined_layout = (self.white_layout.bitboard | self.black_layout.bitboard) & !king.pos.bitboard;
        let (_active_list, enemy_list) = self.get_active_enemy_list(active_player);
        let bitboard = BitBoard::combine_all(&enemy_list.values().filter_map(|cur_piece| {
            if let Some(bitboard) = self.get_piece_snipers(cur_piece, piece_pos, active_player, 2, combined_layout) {
                Some(BitBoard::from_num((bitboard.bitboard | piece_pos.bitboard) & !cur_piece.pos.bitboard))
            } else {
                None
            }
        }).collect::<Vec<_>>());
        if bitboard.bitboard != 0 {
            BitBoard::from_num(bitboard.bitboard | piece_pos.bitboard)
        } else {
            bitboard
        }
    }

    pub fn undo_move(&mut self) {
        if let Some(past_move) = self.past_moves.pop() {
            self.zobrist_key = self.zobrist_table.calculate_subsequent_key(
                self.zobrist_key,
                &past_move.new_white_pieces,
                &past_move.old_white_pieces,
                &past_move.new_black_pieces,
                &past_move.old_black_pieces,
                self.white_can_left_castle,
                self.white_can_right_castle,
                self.black_can_left_castle,
                self.black_can_right_castle,
                &self.empassant_pos,
                past_move.white_can_left_castle,
                past_move.white_can_right_castle,
                past_move.black_can_left_castle,
                past_move.black_can_right_castle,
                &past_move.empassant_pos);
            past_move.new_black_pieces.into_iter().for_each(|x| 
                { Self::remove_piece_at_pos(x.pos, &mut self.black_pieces); });
            past_move.old_black_pieces.into_iter().for_each(|x| 
                { Self::add_piece_at_pos(x, &mut self.black_pieces); });
            past_move.new_white_pieces.into_iter().for_each(|x| 
                { Self::remove_piece_at_pos(x.pos, &mut self.white_pieces); });
            past_move.old_white_pieces.into_iter().for_each(|x| 
                { Self::add_piece_at_pos(x, &mut self.white_pieces); });
            self.white_can_left_castle = past_move.white_can_left_castle;
            self.white_can_right_castle = past_move.white_can_right_castle;
            self.black_can_left_castle = past_move.black_can_left_castle;
            self.black_can_right_castle = past_move.black_can_right_castle;
            self.empassant_pos = past_move.empassant_pos;
            self.white_layout = Self::calculate_layout(&self.white_pieces);
            self.black_layout = Self::calculate_layout(&self.black_pieces);
            self.switch_active_player();



        }
    }


    fn calculate_pawn_moves(&self, piece: &Piece, player: Player, piece_snipers: &Vec<Sniper>) -> Vec<Move> {
        let (active_layout, enemy_layout) = self.get_active_enemy_layout(player);
        let (x_pos, y_pos) = piece.pos.extract_position();
        let (vert_moves, mut diag_moves) = if player == Player::White {
            (constants::WHITE_PAWN_VERT_MOVES[x_pos][y_pos],
             constants::WHITE_PAWN_DIAG_MOVES[x_pos][y_pos])
        } else {
            (constants::BLACK_PAWN_VERT_MOVES[x_pos][y_pos],
             constants::BLACK_PAWN_DIAG_MOVES[x_pos][y_pos])
        };
        let vert_moves = vert_moves & !(active_layout.bitboard | enemy_layout.bitboard);
        let vert_moves = if BitBoard::from_num(vert_moves).extract_all().len() == 1 
        && (vert_moves.leading_zeros() as i64 - piece.pos.bitboard.leading_zeros() as i64).abs() != 8 {
            0
        } else {
            vert_moves

        };
        /*let vert_moves = if vert_moves != vert_moves & !(active_layout.bitboard | enemy_layout.bitboard) {
            0
        } else {
            vert_moves

        };*/
        diag_moves &= enemy_layout.bitboard 
        | &self.empassant_pos.unwrap_or(BitBoard::new()).bitboard;
        let possible_moves = self.clean_for_check(piece_snipers, 
            piece.pos, 
            BitBoard::from_num(vert_moves | diag_moves));
        possible_moves 
            .extract_all()
            .iter()
            .map(|x| {
                let (_new_x, new_y) = x.extract_position();
                let move_type = if new_y == 0 || new_y == 7 {
                    vec![MoveType::PromotionQueen, MoveType::PromotionRook,
                     MoveType::PromotionBishop, MoveType::PromotionKnight]
                } else if (new_y as i64 - y_pos as i64).abs() == 2 {
                    vec![MoveType::PawnStart]
                } else if Some(x) == self.empassant_pos.as_ref() {
                    vec![MoveType::Empassant]
                } else {
                    vec![MoveType::Normal]
                };
                move_type.into_iter().map(|move_type| Move::new(piece.pos, *x, move_type))

            }).flatten().collect::<Vec<_>>()

    }


    fn calculate_knight_moves(&self, piece: &Piece, player: Player, piece_snipers: &Vec<Sniper>) -> Vec<Move> {
        let (active_layout, _) = self.get_active_enemy_layout(player);
        let (x_pos, y_pos) = piece.pos.extract_position();
        let possible_moves = self.clean_for_check(piece_snipers, 
            piece.pos, 
            BitBoard::from_num(constants::KNIGHT_MOVES_AT_POS[x_pos][y_pos] 
                             & !active_layout.bitboard));
        possible_moves
            .extract_all()
            .iter()
            .map(|x| Move::new(piece.pos, *x, MoveType::Normal)).collect::<Vec<_>>()

    }

    fn calculate_rook_moves(&self, piece: &Piece, player: Player, piece_snipers: &Vec<Sniper>) -> Vec<Move> {
        // Uses Magic Bitboard for generating moves O(1) step
        // Must AND attack set with NOT of active layout as it may be blocked at square
        let (active_layout, enemy_layout) = self.get_active_enemy_layout(player);
        let square = piece.pos.extract_tile(); 
        let blockers = active_layout.bitboard | enemy_layout.bitboard; 
        let attack_set = self.rook_magic_table.get(blockers as u64, square) & !active_layout.bitboard as u64;
        let possible_moves = self.clean_for_check(piece_snipers, 
            piece.pos, 
            BitBoard::from_num(attack_set as i64));
        possible_moves 
            .extract_all()
            .iter()
            .map(|x| Move::new(piece.pos, *x, MoveType::Normal)).collect::<Vec<_>>()
    }



    fn calculate_bishop_moves(&self, piece: &Piece, player: Player, piece_snipers: &Vec<Sniper>) -> Vec<Move> {
        // Uses Magic Bitboard for generating moves O(1) step
        // Must AND attack set with active layout as it may be blocked at square
        let (active_layout, enemy_layout) = self.get_active_enemy_layout(player);
        let square = piece.pos.extract_tile(); 
        let blockers = active_layout.bitboard | enemy_layout.bitboard; 
        let attack_set = self.bishop_magic_table.get(blockers as u64, square) & !active_layout.bitboard as u64;
        let possible_moves = self.clean_for_check(piece_snipers, 
            piece.pos, 
            BitBoard::from_num(attack_set as i64));
        possible_moves 
            .extract_all()
            .iter()
            .map(|x| Move::new(piece.pos, *x, MoveType::Normal))
            .collect::<Vec<_>>()
        // TODO check stuff
    }

    fn calculate_queen_moves(&self, piece: &Piece, player: Player, piece_snipers: &Vec<Sniper>) -> Vec<Move> {
        let diag_moves = self.calculate_bishop_moves(piece, player, piece_snipers);
        let vert_moves = self.calculate_rook_moves(piece, player, piece_snipers);
        let mut total_moves = diag_moves;
        total_moves.extend(vert_moves);
        total_moves
    }


    fn can_castle(&self, block_board: BitBoard,
        castle_check_bitboard: 
        BitBoard, player: Player) -> bool {
        // Castle has different block board to check board because Rook and King
        // Don't block
        if block_board.bitboard
        & !(self.white_layout.bitboard | self.black_layout.bitboard) 
        != block_board.bitboard {
            false
        } else {
            let snipe_board = BitBoard::combine_all(&castle_check_bitboard
                .extract_all()
                .into_iter()
                .map(|x| {
                self.get_snipers_to_pos(x, player)

            }).collect::<Vec<_>>());
            if !snipe_board.bitboard & castle_check_bitboard.bitboard 
            != castle_check_bitboard.bitboard {
                false
            } else {
                true
            }
        }

    }

    fn calculate_king_castle_moves(&self, piece: &Piece, player: Player) -> Vec<Move> {
        let mut castle_moves = vec![];
        if player == Player::White {
            if self.white_can_left_castle {
                if self.can_castle(BitBoard::from_num(WH_LEFT_CASTLE_BLOCK), 
                    BitBoard::from_num(WH_LEFT_CASTLE), 
                    player) {
                    castle_moves.push(Move::new(piece.pos, BitBoard::from_pos(2, 0), MoveType::Castle));
                }
            }
            if self.white_can_right_castle {
                if self.can_castle(BitBoard::from_num(WH_RIGHT_CASTLE_BLOCK),
                    BitBoard::from_num(WH_RIGHT_CASTLE), player) {
                    castle_moves.push(Move::new(piece.pos, BitBoard::from_pos(6, 0), MoveType::Castle));
                }
            }
        } else {
            if self.black_can_left_castle {
                if self.can_castle(BitBoard::from_num(BL_LEFT_CASTLE_BLOCK),
                    BitBoard::from_num(BL_LEFT_CASTLE), player) {
                    castle_moves.push(Move::new(piece.pos, BitBoard::from_pos(2, 7), MoveType::Castle));
                }
            }
            if self.black_can_right_castle {
                if self.can_castle(BitBoard::from_num(BL_RIGHT_CASTLE_BLOCK),
                    BitBoard::from_num(BL_RIGHT_CASTLE), player) {
                    castle_moves.push(Move::new(piece.pos, BitBoard::from_pos(6, 7), MoveType::Castle));
                }
            }
        }
        castle_moves
    }




    fn calculate_king_moves(&self, piece: &Piece, player: Player) -> Vec<Move> {
        let (active_layout, _enemy_layout) = self.get_active_enemy_layout(player);
        let (x_pos, y_pos) = piece.pos.extract_position();
        let possible_moves = BitBoard::from_num(constants::KING_MOVES[x_pos][y_pos] 
                             & !active_layout.bitboard);
        let snipe_board = BitBoard::combine_all(&possible_moves.extract_all().into_iter().map(|x| {
            self.get_snipers_to_pos(x, player)
        }).collect::<Vec<_>>());
        let legal_moves = BitBoard::from_num(!snipe_board.bitboard & possible_moves.bitboard);

        let mut legal_moves = legal_moves 
            .extract_all()
            .iter()
            .map(|x| Move::new(piece.pos, *x, MoveType::Normal))
            .collect::<Vec<_>>();
        legal_moves.extend(self.calculate_king_castle_moves(piece, player));
        legal_moves
    }

    pub fn clean_for_check(&self, snipers: &Vec<Sniper>,
        piece_pos: BitBoard, 
        possible_moves: BitBoard) -> BitBoard {
        let mut possible_moves_bitboard = possible_moves.bitboard;
        let combined_layout = (self.white_layout.bitboard | self.black_layout.bitboard) 
        & !piece_pos.bitboard;
        snipers.iter().for_each(|sniper| {
            let combined_layout = combined_layout & !sniper.sniper_piece.pos.bitboard;
            if !combined_layout & sniper.sniper_bitboard.bitboard == sniper.sniper_bitboard.bitboard {
                possible_moves_bitboard &= sniper.sniper_bitboard.bitboard;
            }
            //let check_mask  = combined_layout & sniper.sniper_piece.pos.bitboard;
            //possible_moves_bitboard &= check_mask;
        });
        BitBoard::from_num(possible_moves_bitboard)

    }

    pub fn calculate_active_player_legal_moves(&self) -> Vec<Move> {
        let (piece_list, _) = self.get_active_enemy_list(self.active_player);
        let piece_snipers = self.get_snipers_from_king(self.active_player);
        let legal_moves = piece_list.values().map(|piece| {
            match piece.piece_type {
                PieceType::Pawn => self.calculate_pawn_moves(&piece, 
                    self.active_player, 
                    &piece_snipers),
                PieceType::Knight => self.calculate_knight_moves(&piece, 
                    self.active_player,
                    &piece_snipers),
                PieceType::Bishop => self.calculate_bishop_moves(&piece, 
                    self.active_player, 
                    &piece_snipers),
                PieceType::Rook => self.calculate_rook_moves(&piece, 
                    self.active_player,
                    &piece_snipers),
                PieceType::Queen => self.calculate_queen_moves(&piece, 
                    self.active_player,
                    &piece_snipers),
                PieceType::King => self.calculate_king_moves(&piece, self.active_player),
            }
        }).flatten().collect::<Vec<_>>();
        legal_moves
    }

}
