use indexmap::IndexMap;
use crate::bitboard::BitBoard;
use crate::constants;
use crate::magic_bitboard::MagicTable;
use crate::game_move::Move;

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
    pos: BitBoard,
}

#[derive(PartialEq, Eq, Clone)]
struct PastMove {
    old_white_pieces: Vec<Piece>,
    new_white_pieces: Vec<Piece>,
    old_black_pieces: Vec<Piece>,
    new_black_pieces: Vec<Piece>,
    white_can_left_castle: bool,
    white_can_right_castle: bool,
    black_can_left_castle: bool,
    black_can_right_castle: bool,
    empassant_pos: Option<BitBoard>

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

#[derive(PartialEq, Eq, Copy, Clone)]
pub enum Player {
    White,
    Black
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



#[derive(Clone)]
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
    empassant_pos: Option<BitBoard>,
    white_can_left_castle: bool,
    white_can_right_castle: bool,
    black_can_left_castle: bool,
    black_can_right_castle: bool,
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
        black_pieces.push(Piece::new(PieceType::Queen, BitBoard::from_pos(4, 7)));
        white_pieces.push(Piece::new(PieceType::Queen, BitBoard::from_pos(4, 0)));
        // Both Queens
        black_pieces.push(Piece::new(PieceType::King, BitBoard::from_pos(3, 7)));
        white_pieces.push(Piece::new(PieceType::King, BitBoard::from_pos(3, 0)));
        let white_hashed = white_pieces.into_iter().map(|x| {
            (x.pos, x)
        }).collect::<IndexMap<BitBoard, Piece>>();
        let black_hashed = black_pieces.into_iter().map(|x| {
            (x.pos, x)
        }).collect::<IndexMap<BitBoard, Piece>>();

        // Both Kings
        let white_layout = Self::calculate_layout(&white_hashed);
        let black_layout = Self::calculate_layout(&black_hashed);
        Self {
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
            past_moves: Vec::new(),
        }
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
            (x1..=x2).into_iter().collect::<Vec<_>>()

        } else {
            (x2..=x1).rev().into_iter().collect::<Vec<_>>()
        };
        let y_range = if y1 < y2 {
            (y1..=y2).into_iter().collect::<Vec<_>>()

        } else {
            (y2..=y1).rev().into_iter().collect::<Vec<_>>()
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
        let (low, high) = ([y1, y2].into_iter().min().unwrap(), [y1, y2].into_iter().max().unwrap());
        let mut bitboard = BitBoard::combine_all(&((low)..(high+1)).into_iter().map(|new_y| {
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
        let (low, high) = ([x1, x2].into_iter().min().unwrap(), [x1, x2].into_iter().max().unwrap());
        let mut bitboard = BitBoard::combine_all(&((low)..(high+1)).into_iter().map(|new_x| {
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

    fn normal_move_piece(&mut self, new_move: Move) -> (Vec<Piece>, Vec<Piece>, Vec<Piece>) {
        self.empassant_pos = None;
        let mut active_old_pieces = Vec::new();
        let mut active_new_pieces = Vec::new();
        let mut enemy_old_pieces = Vec::new();
        let (old_pos, new_pos, _) = new_move.get();
        let (active_list, enemy_list) = self.get_active_enemy_list_mut(self.active_player);
        let mut active_piece = Self::remove_piece_at_pos(old_pos, active_list).unwrap_or_else(||{
            println!("{}", old_pos);
            println!("{}", new_pos);
            panic!("no piece here");
            Piece::new(PieceType::Pawn, BitBoard::new())
        });
        active_old_pieces.push(active_piece);
        if let Some(piece) = Self::remove_piece_at_pos(new_pos, enemy_list) {
            if piece.piece_type == PieceType::King {
                println!("{:?}", active_piece.piece_type);
            }
            enemy_old_pieces.push(piece);
        };
        active_piece.pos = new_pos;
        active_new_pieces.push(active_piece);
        Self::add_piece_at_pos(active_piece, active_list);
        (active_old_pieces, active_new_pieces, enemy_old_pieces)
    }

    fn promotion_move(&mut self, new_move: Move) -> (Vec<Piece>, Vec<Piece>, Vec<Piece>) {
        self.empassant_pos = None;
        let mut active_old_pieces = Vec::new();
        let mut active_new_pieces = Vec::new();
        let mut enemy_old_pieces = Vec::new();
        let (old_pos, new_pos, move_type) = new_move.get();
        let (active_list, enemy_list) = self.get_active_enemy_list_mut(self.active_player);
        let mut active_piece = Self::remove_piece_at_pos(old_pos, active_list).unwrap();
        active_old_pieces.push(active_piece);
        if let Some(piece) = Self::remove_piece_at_pos(new_pos, enemy_list) {
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



    pub fn move_piece(&mut self, new_move: Move) {
        // Have to store states before move is made
        let (white_can_left_castle,
             white_can_right_castle,
             black_can_left_castle,
             black_can_right_castle,
             empassant_pos) = (self.white_can_left_castle,
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
            _ => panic!("Shouldn't hit here"),
        };
        self.past_moves.push(PastMove::new(
            active_old,
            active_new,
            enemy_old,
            white_can_left_castle,
            white_can_right_castle,
            black_can_left_castle,
            black_can_right_castle,
            empassant_pos,
            self.active_player));

        self.white_layout = Self::calculate_layout(&self.white_pieces);
        self.black_layout = Self::calculate_layout(&self.black_pieces);
        self.switch_active_player();

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

    fn get_rook_snipes(&self, piece: & Piece, piece_pos: BitBoard, max_blocker: u32) -> Option<BitBoard> {
        let (piece_x, piece_y) = piece.pos.extract_position();
        let (other_x, other_y) = piece_pos.extract_position();
        if piece_x == other_x {
            let mask = self.vert_between_tiles[piece.pos.extract_tile()][piece_pos.extract_tile()] as i64;
            let combined_layout = self.white_layout.bitboard | self.black_layout.bitboard;
            let blockers = mask & combined_layout;
            if blockers.count_ones() < max_blocker {
                Some(BitBoard::from_num(mask))
            } else {
                None
            }
        } else if piece_y == other_y {
            let mask = self.hori_between_tiles[piece.pos.extract_tile()][piece_pos.extract_tile()] as i64;
            let combined_layout = self.white_layout.bitboard | self.black_layout.bitboard;
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

    fn get_bishop_snipers(&self, piece: &Piece, piece_pos: BitBoard, max_blocker: u32) -> Option<BitBoard> {
        let mask = self.diag_between_tiles[piece.pos.extract_tile()][piece_pos.extract_tile()] as i64;
        if mask == 0 {
            return None;
        }
        let combined_layout = self.white_layout.bitboard | self.black_layout.bitboard;
        let blockers = mask & combined_layout;
        if blockers.count_ones() < max_blocker {
            Some(BitBoard::from_num(mask))
        } else {
            None
        }

        //todo!()
    }

    fn get_queen_snipes(&self, piece: &Piece, piece_pos: BitBoard, max_blocker: u32) -> Option<BitBoard> {
        if let Some(board) = self.get_rook_snipes(piece, piece_pos, max_blocker) {
            Some(board)
        } else {
            self.get_bishop_snipers(piece, piece_pos, max_blocker)
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
        max_blocker: u32) -> Option<BitBoard> {
        match piece.piece_type {
            PieceType::Pawn => self.get_pawn_snipes(piece, piece_pos, active_player),
            PieceType::Knight => Self::get_knight_snipes(piece, piece_pos),
            PieceType::Bishop => self.get_bishop_snipers(piece, piece_pos, max_blocker),
            PieceType::Rook => self.get_rook_snipes(piece, piece_pos, max_blocker),
            PieceType::Queen => self.get_queen_snipes(piece, piece_pos, max_blocker),
            PieceType::King => self.get_king_snipes(piece, piece_pos),
        }
    }

    fn get_snipers_from_king(&self, active_player: Player) -> Vec<Sniper> {
        let (active_list, enemy_list)  = self.get_active_enemy_list(active_player);
        let king = active_list.values().find(|x| x.piece_type == PieceType::King).expect("No king");
        enemy_list.values().filter_map(|cur_piece| {
            if let Some(bitboard) = self.get_piece_snipers(cur_piece, king.pos, active_player, 4) {
                let cleaned_bitboard = BitBoard::from_num(bitboard.bitboard & !king.pos.bitboard);
                Some(Sniper::new(cur_piece, cleaned_bitboard))
            } else {
                None
            }

        }).collect::<Vec<_>>()
    }

    fn get_snipers_to_pos(&self, piece_pos: BitBoard, active_player: Player) -> BitBoard {
        let (_active_list, enemy_list) = self.get_active_enemy_list(active_player);
        let bitboard = BitBoard::combine_all(&enemy_list.values().filter_map(|cur_piece| {
            if let Some(bitboard) = self.get_piece_snipers(cur_piece, piece_pos, active_player, 3) {
                Some(BitBoard::from_num(bitboard.bitboard & !cur_piece.pos.bitboard))
            } else {
                None
            }
        }).collect::<Vec<_>>());
        if bitboard.bitboard != 0 {
            BitBoard::from_num(bitboard.bitboard | piece_pos.bitboard)
        } else {
            bitboard
        }
        // TODO modify this method to fix things
    }

    pub fn undo_move(&mut self) {
        if let Some(past_move) = self.past_moves.pop() {
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
            self.black_can_right_castle = past_move.white_can_right_castle;
            self.empassant_pos = past_move.empassant_pos;
            self.switch_active_player();

        }
    }


    fn calculate_pawn_moves(&self, piece: &Piece, player: Player, piece_snipers: &Vec<Sniper>) -> Vec<Move> {
        let (active_layout, enemy_layout) = self.get_active_enemy_layout(player);
        let (x_pos, y_pos) = piece.pos.extract_position();
        let (mut vert_moves, mut diag_moves) = if player == Player::White {
            (constants::WHITE_PAWN_VERT_MOVES[x_pos][y_pos],
             constants::WHITE_PAWN_DIAG_MOVES[x_pos][y_pos])
        } else {
            (constants::BLACK_PAWN_VERT_MOVES[x_pos][y_pos],
             constants::BLACK_PAWN_DIAG_MOVES[x_pos][y_pos])
        };
        vert_moves &= !(active_layout.bitboard | enemy_layout.bitboard);
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

        // TODO check stuff 
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
        // TODO check stuff
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

    fn calculate_king_moves(&self, piece: &Piece, player: Player) -> Vec<Move> {
        let (active_layout, _enemy_layout) = self.get_active_enemy_layout(player);
        let (x_pos, y_pos) = piece.pos.extract_position();
        let possible_moves = BitBoard::from_num(constants::KING_MOVES[x_pos][y_pos] 
                             & !active_layout.bitboard);
        let snipe_board = BitBoard::combine_all(&possible_moves.extract_all().into_iter().map(|x| {
            self.get_snipers_to_pos(x, player)
        }).collect::<Vec<_>>());
        let legal_moves = BitBoard::from_num(!snipe_board.bitboard & possible_moves.bitboard);

        legal_moves 
            .extract_all()
            .iter()
            .map(|x| Move::new(piece.pos, *x, MoveType::Normal))
            .collect::<Vec<_>>()

        // TODO check stuff
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
