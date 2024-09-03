use eframe::egui;
use crate::engine;
use std::collections::HashMap;
use crate::bitboard;
use crate::game_move;
use crate::move_generator::MoveGenerator;

use image;

#[derive(Hash, Eq, PartialEq, Debug)]
struct Position {
    x: usize,
    y: usize
}

impl Position {
    fn new(x: usize, y: usize) -> Self {
        Self {
            x,
            y
        }

    }

}



pub struct GraphicalBoard {
    boardstate: engine::BoardState,
    white_pawn: egui::TextureHandle,
    black_pawn: egui::TextureHandle,
    white_knight: egui::TextureHandle,
    black_knight: egui::TextureHandle,
    white_bishop: egui::TextureHandle,
    black_bishop: egui::TextureHandle,
    white_queen: egui::TextureHandle,
    black_queen: egui::TextureHandle,
    white_king: egui::TextureHandle,
    black_king: egui::TextureHandle,
    white_rook: egui::TextureHandle,
    black_rook: egui::TextureHandle,
    possible_moves: Option<HashMap<Position, Vec<(Position, game_move::Move)>>>,
    selected_piece: Option<Position>,
    move_generator: MoveGenerator,
}


impl GraphicalBoard {

    fn load_image_texture(ctx: &egui::Context, path: &str) -> egui::TextureHandle {
            let image = image::open(path).expect("Failed to load image");
            let size = [image.width() as _, image.height() as _];
            let pixels = image.to_rgba8().into_raw();
            let color_image = egui::ColorImage::from_rgba_unmultiplied(size, &pixels);
            ctx.load_texture("my_image", color_image, Default::default())
    }


    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        Self {
            boardstate: engine::BoardState::new(),
            white_pawn: Self::load_image_texture(&cc.egui_ctx, "../assets/white-pawn.png"),
            black_pawn: Self::load_image_texture(&cc.egui_ctx, "../assets/black-pawn.png"),
            white_knight: Self::load_image_texture(&cc.egui_ctx, "../assets/white-knight.png"),
            black_knight: Self::load_image_texture(&cc.egui_ctx, "../assets/black-knight.png"),
            white_bishop: Self::load_image_texture(&cc.egui_ctx, "../assets/white-bishop.png"),
            black_bishop: Self::load_image_texture(&cc.egui_ctx, "../assets/black-bishop.png"),
            white_queen: Self::load_image_texture(&cc.egui_ctx, "../assets/white-queen.png"),
            black_queen: Self::load_image_texture(&cc.egui_ctx, "../assets/black-queen.png"),
            white_king: Self::load_image_texture(&cc.egui_ctx, "../assets/white-king.png"),
            black_king: Self::load_image_texture(&cc.egui_ctx, "../assets/black-king.png"),
            white_rook: Self::load_image_texture(&cc.egui_ctx, "../assets/white-rook.png"),
            black_rook: Self::load_image_texture(&cc.egui_ctx, "../assets/black-rook.png"),
            possible_moves: None,
            selected_piece: None,
            move_generator: MoveGenerator::new(),
        }

    }

    fn convert_move_list_to_hashmap(to_convert: Vec<game_move::Move>) 
        -> HashMap<Position, Vec<(Position, game_move::Move)>> {
        let mut mapped_values: HashMap<Position, Vec<(Position, game_move::Move)>> = HashMap::new();
        for move_value in to_convert.into_iter() {
            let (old_pos, new_pos, _) = move_value.get();
            let (old_pos_x, old_pos_y) = old_pos.extract_position();
            let old_pos = Position::new(old_pos_x, old_pos_y);
            let (new_pos_x, new_pos_y) = new_pos.extract_position();
            let new_pos = Position::new(new_pos_x, new_pos_y);
            if let Some(val) = mapped_values.get_mut(&old_pos) {
                val.push((new_pos, move_value));
            } else {
                mapped_values.insert(old_pos, vec![(new_pos, move_value)]);
            }

        }
        //println!("{:?}", mapped_values);
        mapped_values


    }


    fn render_possible_moves(&mut self, ui: &mut egui::Ui, tile_size: f32, h_offset: f32) {
        let radius_divisor = 0.65;
        let radius = (tile_size / 2.0) * radius_divisor;
        let color = egui::Color32::GRAY;

        if let Some(selected_piece) = &self.selected_piece {
            if let Some(possible_moves) = &self.possible_moves {
                for move_val in possible_moves.get(selected_piece).unwrap_or(&Vec::new()) {
                    let y_pos = if self.boardstate.active_player == engine::Player::Black {
                        move_val.0.y
                    } else {
                        7 - move_val.0.y
                    };
                    //let y_pos = 7 - move_val.0.y;
                    let center = egui::pos2(
                            radius + tile_size*move_val.0.x as f32 + h_offset + tile_size * ((1.0 - radius_divisor) / 2.0), 
                            tile_size*(y_pos+1) as f32 - radius - tile_size * ((1.0 - radius_divisor) / 2.0),
                    );
                    ui.painter().circle(center, radius, color, (1.0, color));

                    let rect_position = egui::pos2(
                        tile_size*(move_val.0.x) as f32 + h_offset, 
                        tile_size*(y_pos) as f32);
                    let rect = egui::Rect::from_min_size(
                        rect_position,
                        egui::vec2(tile_size, tile_size)
                    );
                    let response = ui.allocate_rect(rect, egui::Sense::click());
                    if response.clicked() {
                        //let new_move = engine::Move::new(selected_piece, move_val.0, move_val.1);
                        self.boardstate.move_piece(move_val.1);
                        self.possible_moves = None;
                        let (_score, new_move) = self.move_generator.alpha_beta(&mut self.boardstate, 7);
                        self.boardstate.move_piece(new_move.unwrap());
                        println!("{}", self.move_generator.node_count);
                        //self.ai_move = true;
                        return;
                    }

                }


            }


        }



    }


    fn get_white_texture_for_piece_at_pos(&self,  
        piece: &engine::Piece) -> &egui::TextureHandle {

        match piece.piece_type {
            engine::PieceType::Pawn => &self.white_pawn,
            engine::PieceType::Knight => &self.white_knight,
            engine::PieceType::Bishop => &self.white_bishop,
            engine::PieceType::Rook => &self.white_rook,
            engine::PieceType::Queen => &self.white_queen,
            engine::PieceType::King => &self.white_king,
        }

    }

    fn get_black_texture_for_piece_at_pos(&self,  
        piece: &engine::Piece) -> &egui::TextureHandle {
        match piece.piece_type {
            engine::PieceType::Pawn => &self.black_pawn,
            engine::PieceType::Knight => &self.black_knight,
            engine::PieceType::Bishop => &self.black_bishop,
            engine::PieceType::Rook => &self.black_rook,
            engine::PieceType::Queen=> &self.black_queen,
            engine::PieceType::King => &self.black_king,
        }

    }




    fn render_possible_pieces(&mut self, ui: &mut egui::Ui,
                             tile: egui::Rect, pos: bitboard::BitBoard) {
        let white_piece = self.boardstate.white_pieces.get(&pos);
        let black_piece = self.boardstate.black_pieces.get(&pos);
        let texture_ref = if let Some(piece) = white_piece {
            Some(self.get_white_texture_for_piece_at_pos(piece))
        } else if let Some(piece) = black_piece {
            Some(self.get_black_texture_for_piece_at_pos(piece))
        } else {
            None
        };
        let response = ui.allocate_rect(tile, egui::Sense::click());
        if let Some(texture) = texture_ref {
            egui::Image::new(texture).paint_at(ui, tile);
            if response.clicked() {
                let (x, y) = pos.extract_position();
                self.selected_piece = Some(Position::new(x, y));
            }
        } else {
            if response.clicked() {
                self.selected_piece = None;
            }

        }

    }


    fn render_board_state(&mut self, ui: & mut egui::Ui,
                          screen_width: f32, screen_height: f32) {
        let tile_size = if screen_width / 10.0 > screen_height / 10.0 {
                screen_height / 8.0
            } else {
                screen_width / 8.0
        };
        let h_offset = screen_width / 2.0 - (4.0 * tile_size);
        let tile_dims = egui::vec2(tile_size, tile_size);
        let green = egui::Color32::from_rgb(118, 150, 86);
        let white = egui::Color32::from_rgb(255, 255, 255);
        let stroke = egui::Stroke::new(2.0, egui::Color32::BLACK);
        
        for x in 0..8 {
            for y in 0..8 {
                let y_pos = 7 - y;
                let y_pos = if self.boardstate.active_player == engine::Player::Black {
                    y
                } else {
                    7 - y
                };
                let rect_position = egui::pos2(
                        tile_size* (x) as f32 + h_offset, 
                        tile_size*y_pos as f32);
                let rect = egui::Rect::from_min_size(
                        rect_position,
                        tile_dims,
                );
                let colour = if (y % 2 == 0 && x % 2 == 0) || (y % 2 == 1 && x % 2 == 1) {
                    green
                } else {
                    white
                };
                ui.painter().rect(rect, 0.0, colour, stroke);
                self.render_possible_pieces(ui, rect, bitboard::BitBoard::from_pos(x, y)); 
                //self.render_piece_at_pos(ui, rect, gamestate::Position{x, y});
            }
        }
        self.render_possible_moves(ui, tile_size, h_offset); 
    }
}



impl eframe::App for GraphicalBoard {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if self.possible_moves == None {
            let move_vec = self.boardstate.calculate_active_player_legal_moves();
            self.possible_moves = Some(Self::convert_move_list_to_hashmap(move_vec));

        }
        egui::CentralPanel::default().show(ctx, |ui| {
                self.render_board_state(ui, 
                    ctx.screen_rect().width(),
                    ctx.screen_rect().height());
                let undo = ui.button("Undo");
                if undo.clicked() {
                    self.boardstate.undo_move();
                    self.possible_moves = None;
                    self.selected_piece = None;
                }

        });
        ctx.request_repaint();
    }

}
