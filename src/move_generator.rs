use crate::engine::{BoardState, Player, PieceType, Piece};
use crate::game_move::Move;
use crate::constants::MVV_LVA;



pub struct MoveGenerator {
    pub node_count: i64,
}




impl MoveGenerator {
    pub fn new() -> Self {
        Self {
            node_count: 0,
        }

    }
    fn return_piece_eval(piece: &Piece) -> i64 {
        match piece.piece_type {
            PieceType::Pawn => 100,
            PieceType::Knight => 300,
            PieceType::Bishop => 350,
            PieceType::Rook => 500,
            PieceType::Queen => 900,
            PieceType::King => 200
        }


    }


    fn evaluate_state(boardstate: &BoardState) -> i64 {
        let (active_sum, enemy_sum) = if boardstate.active_player == Player::White {
             (&boardstate.white_pieces.values().map(|x| Self::return_piece_eval(x)).sum::<i64>(),
              &boardstate.black_pieces.values().map(|x| Self::return_piece_eval(x)).sum::<i64>())
        } else {
             (&boardstate.black_pieces.values().map(|x| Self::return_piece_eval(x)).sum::<i64>(),
              &boardstate.white_pieces.values().map(|x| Self::return_piece_eval(x)).sum::<i64>())
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
        self.node_count += 1;
        if depth == 0 {
            return (Self::evaluate_state(&boardstate), None)
        } else {
            let mut legal_moves = boardstate.calculate_active_player_legal_moves();
            if legal_moves.len() == 0 {
                return (-10_000 + depth, None);
            }
            Self::sort_legal_moves(&mut legal_moves, &boardstate);
            let mut best_score = std::i64::MIN;
            let mut best_move = None;
            let mut alpha = alpha;
            for new_move in legal_moves.into_iter() {
                boardstate.move_piece(new_move);
                let value = self.nega_max(boardstate, depth-1,
                    -beta,
                    -alpha);
                let score = -value.0;
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
            return (best_score, best_move);

        }


    }


    pub fn alpha_beta(&mut self, boardstate: &mut BoardState, depth: i64) -> (i64, Option<Move>) {
        self.nega_max(boardstate, depth, std::i64::MIN, std::i64::MAX)

    }

}
