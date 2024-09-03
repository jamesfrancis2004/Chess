use crate::game_move::Move;
use crate::engine::BoardState;

const TABLE_SIZE: usize = 10_000_000;

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum NodeBound {
    Exact,
    LowerBound,
    UpperBound,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct TableEntry {
    zobrist_signature: u64,
    pub entry_move: Move,
    pub score: i64,
    depth: i8,
    pub node_bound: NodeBound,
}

impl TableEntry {
    fn new(zobrist_signature: u64,
           entry_move: Move,
           score: i64,
           depth: i8,
           node_bound: NodeBound) -> Self {
        Self {
            zobrist_signature,
            entry_move,
            score,
            depth,
            node_bound,
        }
    }

}

pub struct TranspositionTable {
    entries: Vec<Option<TableEntry>>,
    pub used_entries: usize,
}



impl TranspositionTable {
    pub fn new() -> Self {
        Self {
            entries: vec![None; TABLE_SIZE],
            used_entries: 0,
        }
    }

    pub fn get(&self, boardstate: &BoardState, depth: i64) -> Option<TableEntry> {
        let idx = boardstate.zobrist_key % (TABLE_SIZE as u64);
        if let Some(ref entry) = self.entries[idx as usize] {
            if entry.depth as i64 >= depth && entry.zobrist_signature == boardstate.zobrist_key {
                Some(entry.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn insert(&mut self, 
        boardstate: &BoardState, 
        game_move: Move,
        move_score: i64,
        depth: i64,
        alpha: i64,
        beta: i64) {
        let idx = boardstate.zobrist_key % (TABLE_SIZE as u64);
        let node_type = if move_score <= alpha {
            NodeBound::UpperBound
        } else if move_score >= beta {
            NodeBound::LowerBound
        } else {
            NodeBound::Exact
        };
        let entry = TableEntry::new(
            boardstate.zobrist_key,
            game_move,
            move_score,
            depth as i8,
            node_type
        );
        if self.entries[idx as usize] == None {
            self.used_entries += 1;
        }
        self.entries[idx as usize] = Some(entry);
    }



}
