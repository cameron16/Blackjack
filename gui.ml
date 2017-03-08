open Graphics
open Types
open Processor


let window_game_size = 800
let coordinate_size = 20
let () = open_graph " 1000x800"


let () =Graphics.set_window_title "Blackjack"

let hot_pink = 16722885
let pink = 16630538

let p1_stats_x = 200
let p2_stats_x = 500
let bet_stats_y = 225
let card_header_y = bet_stats_y - 50
let card_stats_y_pos = card_header_y - 35
let cards_stats_y = ref card_stats_y_pos



let ellipse_center_x = 400
let ellipse_center_y = 450 
let ellipse_hor_rad = 350
let ellipse_vert_rad = 150

let dealer_card_x = 360
let dealer_card_y_header = ellipse_center_y+50
let dealer_card_y_pos = dealer_card_y_header-25
let dealer_card_y = ref dealer_card_y_pos

let directions_x = 100
let directions_y = 750

let draw_background (state : state) : unit = 
	Graphics.clear_graph ();
	Graphics.set_color black;
	Graphics.fill_rect 0 0 window_game_size window_game_size;
	Graphics.set_color green;
	Graphics.fill_ellipse ellipse_center_x ellipse_center_y ellipse_hor_rad ellipse_vert_rad;
	let x_pos = window_game_size - 200 in
	Graphics.moveto x_pos 20;
	Graphics.draw_string "Press 'q' to quit";
	let y_pos = bet_stats_y + 50 in
	let p1_earnings_string = string_of_int state.p1_earnings in
	let p1_string = "Player 1 Earnings: " ^ p1_earnings_string in
	Graphics.moveto p1_stats_x y_pos; 
	Graphics.draw_string p1_string;
	let p2_earnings_string = string_of_int state.p2_earnings in
	let p2_string = "Player 2 Earnings: " ^ p2_earnings_string in
	Graphics.moveto p2_stats_x y_pos; 
	Graphics.draw_string p2_string;
	()

let help_pregame (state: state) : unit = 
	draw_background state;
	let y_coord = ellipse_center_y + 200 in
	Graphics.moveto 360 y_coord; 
	Graphics.set_color green;
	Graphics.draw_string "Press 1 to play";
	()

let write_p1_bet (state: state) : unit = 
	let p1_bet = Pervasives.string_of_int state.p1_bet in 
	Graphics.moveto p1_stats_x bet_stats_y;
	let my_string = "Player 1 Bet: "^p1_bet in
	Graphics.set_color green;
	Graphics.draw_string my_string;
	()

let write_p2_bet (state: state) : unit = 

	let p2_bet = Pervasives.string_of_int state.p2_bet in 
	Graphics.moveto p2_stats_x bet_stats_y;
	let my_string = "Player 2 Bet: "^p2_bet in
	Graphics.set_color green;
	Graphics.draw_string my_string;
	()


let write_betting_directions (k: unit) : unit = 
	Graphics.set_color green;
	let bet_level = directions_y-50 in 
	let number_level = directions_y-100 in
	Graphics.moveto directions_x bet_level;
	Graphics.draw_string "Choose your bet amount";
	Graphics.moveto directions_x number_level;
	Graphics.draw_string "1 - $1  :::  2 - $5  :::  3 - $25  :::  4 - $100  :::  5 - $500";
	()



let help_turn_bet (state: state) : unit =
	draw_background state;
	write_betting_directions();
	Graphics.moveto directions_x directions_y;
	if (state.round = Player1_turn_bet) then
		let () = Graphics.draw_string "Player 1 Turn" in
		()
	else
		let () = Graphics.draw_string "Player 2 Turn" in
		let () = write_p1_bet state in
		()
	
let get_suit (suit : suit) : string = 
	match suit with
	|Hearts -> "Hearts"
	|Spades -> "Spades"
	|Diamonds -> "Diamonds"
	|Clubs -> "Clubs"


let get_number (number: int) : string = 
	if (number = 11) then "Ace"
	else if (number = 12) then "Queen"
	else if (number = 13) then "King"
	else if (number = 14) then "Jack"
	else Pervasives.string_of_int number


let rec write_p1_cards (p1_hand: hand) : unit = 
	Graphics.moveto p1_stats_x card_header_y;
	Graphics.draw_string "Player 1 Cards:";
	match p1_hand with
	|[] -> cards_stats_y := card_stats_y_pos;
			()
	|h::t -> let my_suit = get_suit h.suit in
			let number = get_number h.rank in
			let card_string = number ^ " of " ^ my_suit in
			Graphics.moveto p1_stats_x (!cards_stats_y); 
			Graphics.draw_string card_string;
			cards_stats_y := (!cards_stats_y-20);
			write_p1_cards t

let rec write_p2_cards (p2_hand: hand) : unit = 
	Graphics.moveto p2_stats_x card_header_y;
	Graphics.draw_string "Player 2 Cards:";
	match p2_hand with
	|[] -> cards_stats_y := card_stats_y_pos;
			()
	|h::t -> let my_suit = get_suit h.suit in
			let number = get_number h.rank in
			let card_string = number ^ " of " ^ my_suit in
			Graphics.moveto p2_stats_x (!cards_stats_y); 
			Graphics.draw_string card_string;
			cards_stats_y := (!cards_stats_y-20);
			write_p2_cards t

let rec write_dealer_cards (dealer_hand: hand) : unit = 
	Graphics.set_color black;
	Graphics.moveto dealer_card_x dealer_card_y_header;
	Graphics.draw_string "Dealer Cards:";
	match dealer_hand with
	|[] -> dealer_card_y := dealer_card_y_pos;
			()
	|h::t -> let my_suit = get_suit h.suit in
			let number = get_number h.rank in
			let card_string = number ^ " of " ^ my_suit in
			Graphics.moveto dealer_card_x (!dealer_card_y); 
			Graphics.draw_string card_string;
			dealer_card_y := (!dealer_card_y-20);
			write_dealer_cards t


let write_one_dealer_card (dealer_hand: hand) : unit = 
	Graphics.set_color black;
	Graphics.moveto dealer_card_x dealer_card_y_header;
	Graphics.draw_string "Dealer Cards:";
	match dealer_hand with
	|[] -> Graphics.moveto dealer_card_x (!dealer_card_y);
			Graphics.draw_string "P1 and P2 Bust"
	|h::t -> let my_suit = get_suit h.suit in
			let number = get_number h.rank in
			let card_string = number ^ " of " ^ my_suit in
			Graphics.moveto dealer_card_x (!dealer_card_y); 
			Graphics.draw_string card_string;
			()


let help_deal_time (state: state) : unit =
	draw_background state;
	Graphics.set_color green;
	write_p1_bet state; 
	write_p2_bet state;
	write_p1_cards state.p1_hand;
 	write_p2_cards state.p2_hand;
 	write_one_dealer_card state.dealer_hand;
 	()

 let do_you_double (state: state) : unit = 
 	let hand_number = 
 		if state.round = Player1_turn_hit 
 		then List.length state.p1_hand
 		else if state.round = Player2_turn_hit
 		then List.length state.p2_hand
 		else 3
 	in
 	let y_pos = directions_y - 25 in 
 	Graphics.moveto directions_x y_pos; 
 	if hand_number = 2 
 	then Graphics.draw_string "1 - Hit ::: 2 - Check ::: 3 - Double ::: 4 - Surrender"
 	else Graphics.draw_string "1 - Hit ::: 2 - Check"






let help_p1_turn_hit (state:state) : unit = 
	draw_background state;
	Graphics.set_color green;
	write_p1_bet state; 
	write_p2_bet state;
	write_p1_cards state.p1_hand;
 	write_p2_cards state.p2_hand;
 	write_one_dealer_card state.dealer_hand;
 	Graphics.moveto directions_x directions_y;
 	Graphics.set_color white;

 	Graphics.draw_string "Player 1 Turn";
 	do_you_double state;
 	()
 	

let check_if_p1_bust (hand: hand) : unit =
	let x_pos = directions_x + 225 in
	let y_pos = directions_y in
	Graphics.set_color green;
	Graphics.moveto x_pos y_pos; 
	if check_bust hand 
	then Graphics.draw_string "Player 1 Busted" 
	else Graphics.draw_string ""

let check_if_p2_bust (hand : hand) : unit =
	let x_pos = directions_x + 225 in
	let y_pos = directions_y - 25 in
	Graphics.set_color green;
	Graphics.moveto x_pos y_pos; 
	if check_bust hand 
	then Graphics.draw_string "Player 2 Busted" 
	else Graphics.draw_string ""

let check_if_dealer_bust (hand: hand) : unit = 
	let y_pos = dealer_card_y_header + 50 in
	Graphics.set_color green;
	Graphics.moveto dealer_card_x y_pos; 
	Graphics.set_color black;
	if check_bust hand 
	then Graphics.draw_string "Dealer Busted" 
	else Graphics.draw_string ""



let help_p2_turn_hit (state:state) : unit = 
	draw_background state;
	Graphics.set_color green;
	write_p1_bet state; 
	write_p2_bet state;
	write_p1_cards state.p1_hand;
 	write_p2_cards state.p2_hand;
 	write_one_dealer_card state.dealer_hand;
 	check_if_p1_bust state.p1_hand; 
 	Graphics.moveto directions_x directions_y;
 	Graphics.set_color white;
 	Graphics.draw_string "Player 2 Turn";
 	do_you_double state;
 	()

 let help_dealer_turn_hit (state:state) : unit = 
	draw_background state;
	Graphics.set_color green;
	write_p1_bet state; 
	write_p2_bet state;
	write_p1_cards state.p1_hand;
 	write_p2_cards state.p2_hand;
 	write_dealer_cards state.dealer_hand;
 	check_if_p2_bust state.p2_hand;
 	Graphics.moveto directions_x directions_y;
 	Graphics.draw_string "Dealer Turn";
 	let y_pos = directions_y-25 in
 	Graphics.moveto directions_x y_pos;
 	Graphics.draw_string "1 - Hit ::: 2 - Check";
 	()

let check_p1 (state: state) : unit =
	draw_background state;
	write_p1_bet state; 
	write_p2_bet state;
	write_p1_cards state.p1_hand;
 	write_p2_cards state.p2_hand;
 	write_dealer_cards state.dealer_hand;
 	Graphics.moveto directions_x directions_y;
 	Graphics.set_color green;
	match state.p1_winner with
	|P1 -> Graphics.draw_string "Player 1 Wins"; ()
	|Dealer -> Graphics.draw_string "Player 1 Loses"; ()
	|Tie -> Graphics.draw_string "Player 1 Ties"; ()
	|_ -> ()
	


let check_p2 (state: state) : unit =
 	let y_pos = directions_y - 25 in
 	Graphics.moveto directions_x y_pos;
 	Graphics.set_color green;
	match state.p2_winner with
	|P2 ->  Graphics.draw_string "Player 2 Wins"; () 
	|Dealer ->  Graphics.draw_string "Player 2 Loses"; () 
	|Tie -> Graphics.draw_string "Player 2 Ties"; () 
	|_ -> ()

let give_directions unit : unit = 
	Graphics.set_color green;
	let x_pos = directions_x + 225 in
	let y_pos = directions_y - 75 in
	Graphics.moveto x_pos y_pos;
	Graphics.draw_string "Press '1' to continue to next round";
	let y_pos = directions_y -100 in
	Graphics.moveto x_pos y_pos;
	Graphics.draw_string "Press '2' to restart";
	()

let give_totals (state: state) : unit = 
	Graphics.set_color green;
	let y_pos = directions_y - 75 in
	Graphics.moveto directions_x y_pos;
	Graphics.draw_string "Totals";
	let y_pos = directions_y - 100 in
	let dealer_sum = string_of_int (get_player_sum state.dealer_hand) in
	let dealer_string = "Dealer: "^dealer_sum in
	Graphics.moveto directions_x y_pos;
	Graphics.draw_string dealer_string;
	let y_pos = directions_y - 125 in
	let p1_sum = string_of_int (get_player_sum state.p1_hand) in 
	let p1_string = "Player 1: " ^ p1_sum in 
	Graphics.moveto directions_x y_pos;
	Graphics.draw_string p1_string;
	let y_pos = directions_y-150 in 
	let p2_sum = string_of_int (get_player_sum state.p2_hand) in 
	let p2_string = "Player 2: "^p2_sum in
	Graphics.moveto directions_x y_pos;
	Graphics.draw_string p2_string;
	()

let help_wait (state: state) : unit = 
	check_p1 state;
	check_p2 state;
	check_if_p1_bust state.p1_hand;
	check_if_p2_bust state.p2_hand;
	check_if_dealer_bust state.dealer_hand;
	give_totals state;
	give_directions ();
	()

let update_gui (state:state) : unit =
	match state.round with
	|Pregame -> help_pregame state
	|Player1_turn_bet -> help_turn_bet state
	|Player2_turn_bet -> help_turn_bet state
	|Time_to_deal -> help_deal_time state
	|Player1_turn_hit -> help_p1_turn_hit state
	|Player2_turn_hit -> help_p2_turn_hit state
	|Wait -> help_wait state
	|_ -> Graphics.set_color blue; Graphics.fill_rect 0 0 window_game_size window_game_size; ()






