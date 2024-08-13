
#[derive(Clone)]
pub struct MagicTable {
    magic_table: Vec<Vec<u64>>,
    magic_numbers: &'static [u64],
    magic_masks: &'static [u64],
    magic_shifts: &'static [u64]

}


impl MagicTable {
    pub fn new(magic_numbers: &'static [u64], 
        magic_masks: &'static [u64], 
        magic_shifts: &'static [u64], 
        is_rook: bool) -> Self {
        let magic_table = Self::precompute_magic_tables(magic_numbers,
            magic_masks,
            magic_shifts,
            is_rook
        );
        Self {
            magic_table,
            magic_numbers,
            magic_masks,
            magic_shifts
        }


    }

    pub fn get(&self, blockers: u64, square: usize) -> u64 {
        let blockers = blockers & self.magic_masks[square];
        let magic_idx = Self::magic_index(blockers, 
            self.magic_numbers[square], 
            self.magic_shifts[square]);
        self.magic_table[square][magic_idx]


    }

    fn magic_index(blockers: u64, magic: u64, shift: u64) -> usize {
        (blockers.wrapping_mul(magic) >> shift) as usize 
    }


    fn precompute_magic_tables(magic_numbers: &[u64], blocker_mask: &[u64], shift_arr: &[u64], is_rook: bool) -> Vec<Vec<u64>> {
        let mut attack_tables = vec![vec![]; 64];
        for square in 0..64 {
            let blocker_mask = blocker_mask[square];
            let blockers = Self::generate_all_blockers(blocker_mask);
            let mut attack_sets = Vec::new();
            for &blocker in blockers.iter() {
                let attack_set = if is_rook {
                    Self::generate_rook_attack_set(square, blocker)
                } else {
                    Self::generate_bishop_attack_set(square, blocker)
                };
                attack_sets.push(attack_set);
            }
            let magic = magic_numbers[square];


            let mut attack_table = vec![0u64; 1 << (64 - shift_arr[square])];
            for (i, &blocker) in blockers.iter().enumerate() {
                let index = Self::magic_index(blocker, magic, shift_arr[square]);
                attack_table[index] = attack_sets[i];
            }
            attack_tables[square] = attack_table;


        }
        attack_tables
    }


    fn generate_all_blockers(blocker_mask: u64) -> Vec<u64> {
        let mut blockers = Vec::new();
        let num_combinations = 1 << blocker_mask.count_ones();  // 2^n combinations

        for i in 0..num_combinations {
            let mut blocker_config = 0u64;
            let mut bit_index = 0;

            for bit_pos in 0..64 {
                if (blocker_mask & (1u64 << bit_pos)) != 0 {
                    if (i & (1 << bit_index)) != 0 {
                        blocker_config |= 1u64 << bit_pos;
                    }
                    bit_index += 1;
                }
            }

            blockers.push(blocker_config);
        }

        blockers
    }

    fn generate_rook_attack_set(square: usize, blockers: u64) -> u64 {
        let mut attacks = 0u64;
        for dir in [8, -8, 1, -1].into_iter() {
            let mut s = square as i32;
            while s >= 0 && s < 64 {
                s += dir;
                if s < 0 || s >= 64 {
                    break;
                }
                if dir == 1 && s % 8 == 0 {
                    break;
                }
                if dir == -1 && s % 8 == 7 {
                    break;
                }
                attacks |= 1u64 << s;
                if blockers & (1u64 << s) != 0 {
                    break;
                }

            }
        }
        attacks
    }

    pub fn generate_bishop_attack_set(square: usize, blockers: u64) -> u64 {
        let mut attacks = 0u64;
        let directions = [9, -9, 7, -7]; // Diagonals: top-right, bottom-left, top-left, bottom-right
        for &dir in directions.iter() {
            let mut s = square as i32;
            while s >= 0 && s < 64 {
                let current_row = s / 8;
                let current_col = s % 8;

                // Move to the next square in the direction
                s += dir;

                // Ensure we stay within the board's boundaries
                if s < 0 || s >= 64 {
                    break;
                }

                let next_row = s / 8;
                let next_col = s % 8;

                // Check if we're still on the same diagonal
                if (next_row - current_row).abs() != 1 || (next_col - current_col).abs() != 1 {
                    break;
                }

                attacks |= 1u64 << s;

                // Stop if we encounter a blocker
                if blockers & (1u64 << s) != 0 {
                    break;
                }
            }
        }

        attacks
    }





}
