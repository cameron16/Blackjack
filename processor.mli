open Types

val update_p1_bets: char -> state -> state

val update_p2_bets: char -> state -> state

val update_p1_hit: char -> state -> state

val update_p2_hit: char -> state -> state

val deal_cards: state -> string -> string -> state

val dealer_hit: state -> state

val cleanup_round: state -> state

val clear_cards: unit -> unit

val renew_num_gen: unit -> unit

val get_player_sum: hand -> int

val check_for_natural: hand -> bool

val move_to_p1_hit: state -> state

val move_to_p2_hit: state -> state

val move_to_dealer_hit: state -> state

val check_bust: hand -> bool