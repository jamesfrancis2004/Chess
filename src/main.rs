mod engine;
mod bitboard;
mod constants;
mod magic_bitboard;
mod game_move;
mod graphical_interface;
mod move_generator;
mod eval_constants;
mod transposition_table;
mod zobrist_table;

use std::time::Instant;





fn main() {
    let mut engine = engine::BoardState::new();
    let mut move_generator = move_generator::MoveGenerator::new();

    let start = Instant::now();
    //let mut move_generator = move_generator::MoveGenerator::new();
    let (score, new_move) = move_generator.alpha_beta(&mut engine, 7);
    let (old_pos, new_pos, move_type) = new_move.unwrap().get();
    println!("{}", move_generator.node_count);
    //println!("old pos");
    //println!("{}", old_pos);
    //println!("new pos");
    //println!("{}", new_pos);
    //println!("move type");
    //println!("{:?}", move_type);
    //let start = Instant::now();
    /*for _ in 0..1_000_000 {
        engine.calculate_active_player_legal_moves();
    }*/
    println!("{:?}", start.elapsed());
    let native_options = eframe::NativeOptions::default();
    let _ = eframe::run_native("Chess",
        native_options,
        Box::new(|cc| Box::new(graphical_interface::GraphicalBoard::new(cc))),
    );
    //let blockers = generate_all_blockers(generate_blocker_mask(0));
    //let mut rook_attack_tables = vec![vec![]; 64];
    //let mut rook_magics = vec![0; 64];
    //println!("{}", bitboard::BitBoard::from_num(constants::MAGIC_MOVES_R_MASK[0] as i64));

    /*for square in 0..64 {
        rook_attack_tables[square] = precompute_attack_table(square, true);
    }*/
    //let magic_idx = magic_index(blockers[4], constants::MAGIC_MOVES_R_MAGICS[0], constants::MAGIC_MOVES_R_SHIFT[0]);
    //println!("{}", magic_idx);
    //let value = bitboard::BitBoard::from_num(generate_blocker_mask(63) as i64);
    //println!("{}", value);
    //println!("{}", bitboard::BitBoard::from_num(blockers[4] as i64));
    //println!("{}", bitboard::BitBoard::from_num(rook_attack_tables[0][magic_idx] as i64));
    //println!("{}", bitboard::BitBoard::from_num(blockers[4] as i64));
    //println!("{}", bitboard::BitBoard::from_num(generate_attack_set(0, blockers[4]) as i64));
    //println!("{}", bitboard::BitBoard::from_num(blockers[30000] as i64));

}
