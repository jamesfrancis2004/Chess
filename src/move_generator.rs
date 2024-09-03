use crate::engine::{BoardState, Player, PieceType, Piece};
use crate::game_move::Move;
use crate::constants::MVV_LVA;
use crate::eval_constants;
use crate::eval_constants::FLIP;
use crate::transposition_table::{TranspositionTable, NodeBound};



pub struct MoveGenerator {
    pub node_count: i64,
    pub transposition_table: TranspositionTable,
}




impl MoveGenerator {
    pub fn new() -> Self {
        Self {
            node_count: 0,
            transposition_table: TranspositionTable::new(),
        }

    }
    fn return_piece_eval_white(piece: &Piece) -> i64 {
        match piece.piece_type {
            PieceType::Pawn => eval_constants::PAWN_MG[FLIP[piece.pos.extract_tile()]],
            PieceType::Knight => eval_constants::KNIGHT_MG[FLIP[piece.pos.extract_tile()]],
            PieceType::Bishop => eval_constants::BISHOP_MG[FLIP[piece.pos.extract_tile()]],
            PieceType::Rook => eval_constants::ROOK_MG[FLIP[piece.pos.extract_tile()]],
            PieceType::Queen => eval_constants::QUEEN_MG[FLIP[piece.pos.extract_tile()]],
            PieceType::King => eval_constants::KING_MG[FLIP[piece.pos.extract_tile()]],


        }
    }


    fn return_piece_eval_black(piece: &Piece) -> i64 {
        match piece.piece_type {
            PieceType::Pawn => eval_constants::PAWN_MG[piece.pos.extract_tile()],
            PieceType::Knight => eval_constants::KNIGHT_MG[piece.pos.extract_tile()],
            PieceType::Bishop => eval_constants::BISHOP_MG[piece.pos.extract_tile()],
            PieceType::Rook => eval_constants::ROOK_MG[piece.pos.extract_tile()],
            PieceType::Queen => eval_constants::QUEEN_MG[piece.pos.extract_tile()],
            PieceType::King => eval_constants::KING_MG[piece.pos.extract_tile()],


        }


    }


    fn evaluate_state(boardstate: &BoardState) -> i64 {
        let (active_sum, enemy_sum) = if boardstate.active_player == Player::White {
             (&boardstate.white_pieces.values().map(|x| Self::return_piece_eval_white(x)).sum::<i64>(),
              &boardstate.black_pieces.values().map(|x| Self::return_piece_eval_black(x)).sum::<i64>())
        } else {
             (&boardstate.black_pieces.values().map(|x| Self::return_piece_eval_black(x)).sum::<i64>(),
              &boardstate.white_pieces.values().map(|x| Self::return_piece_eval_white(x)).sum::<i64>())
        };
        active_sum - enemy_sum
    }


    fn sort_legal_moves(legal_moves: &mut Vec<Move>, boardstate: &BoardState) {
        let (active_list, enemy_list) = if boardstate.active_player == Player::White {
                (&boardstate.white_pieces, &boardstate.black_pieces)
        } else {
                (&boardstate.black_pieces, &boardstate.white_pieces)
        };

        legal_moves.sort_by_cached_key(|x| {
            let (old_pos, new_pos, _) = x.get();
            let attacker_idx = active_list.get(&old_pos).unwrap().piece_type as usize;
            let victim = enemy_list.get(&new_pos);
            let victim_idx = if let Some(victim) = victim {
                victim.piece_type as usize
            } else {
                6
            };
            MVV_LVA[victim_idx][attacker_idx]
        });
        legal_moves.reverse();
    }


    fn nega_max(&mut self, boardstate: &mut BoardState,
        depth: i64,
        alpha: i64,
        beta: i64) -> (i64, Option<Move>) {
        let mut alpha = alpha;
        let mut beta = beta;
        self.node_count += 1;
        if depth == 0 {
            return (Self::evaluate_state(&boardstate), None)
        } else {
            
            if let Some(entry) = self.transposition_table.get(boardstate, depth) {
                match entry.node_bound {
                    NodeBound::Exact => { return (entry.score, Some(entry.entry_move)) },
                    NodeBound::LowerBound => { alpha = *[alpha, entry.score].iter().max().unwrap(); }
                    NodeBound::UpperBound => { beta = *[beta, entry.score].iter().min().unwrap(); }
                }
                if alpha >= beta {
                    return (entry.score, Some(entry.entry_move));
                }
                Some(entry.entry_move)
            } else {
                None

            };
            let mut legal_moves = boardstate.calculate_active_player_legal_moves();
            if legal_moves.len() == 0 {
                return (-1_000_000 + depth, None);
            }
            Self::sort_legal_moves(&mut legal_moves, &boardstate);
            let mut best_score = std::i64::MIN;
            let mut best_move = None;

            for new_move in legal_moves.into_iter() {
                boardstate.move_piece(new_move);
                let (score, _) = self.nega_max(boardstate, depth-1,
                    -beta,
                    -alpha);
                let score = -score;
                boardstate.undo_move();
                if score > best_score {
                    best_score = score;
                    best_move = Some(new_move);
                }
                alpha = *[alpha, best_score].iter().max().unwrap();
                if alpha >= beta {
                    break;
                }


            }
            
            self.transposition_table.insert(
                    boardstate, 
                    best_move.clone().unwrap(), 
                    best_score, 
                    depth,
                    alpha,
                    beta);
            return (best_score, best_move);

        }


    }


    pub fn alpha_beta(&mut self, boardstate: &mut BoardState, depth: i64) -> (i64, Option<Move>) {
        self.node_count = 0;
        let to_return = self.nega_max(boardstate, depth, -1_000_000, 1_000_000);
        println!("Used entries {}", self.transposition_table.used_entries);
        to_return

    }

}
