open Graphics
open Types
open Processor
open Gui

let pre_game_state =  {
    round = Pregame;
    dealer_hand = [];
    p1_hand = [];
    p2_hand = [];
    p1_bet = 0;
    p2_bet = 0;
    p1_earnings = 0;
    p2_earnings = 0;
    p1_winner = Undetermined;
    p2_winner = Undetermined
  }

let make_card (rank:int) (suit:suit) : card =
   {rank = rank; suit = suit}

(*1,5,25,100,500*)
let bet_commands = ['1';'2';'3';'4'; '5']


let check_quit mode_command: unit = 
  if (mode_command = 'q') then ( (*quit game*)
    let () = clear_cards () in
    exit 0
  )

let rec get_bets (state :state) : state = 
  let mode_status = wait_next_event [Key_pressed] in
  let mode_command = mode_status.key in 
  check_quit mode_command;
  if (List.mem mode_command bet_commands = false) then get_bets state
  else if (state.round = Player1_turn_bet) then
    update_p1_bets mode_command state
  else
    update_p2_bets mode_command state
    
    (*return true iff player hit '3' and is allowed to double
    i.e. player only has 2 cards*)
let can_you_double (state: state) (mode_command: char) : bool = 
  if state.round = Player1_turn_hit then 
    (mode_command = '3' && List.length state.p1_hand = 2)
  else
    (mode_command = '3' && List.length state.p2_hand = 2)

(*return true iff player hit '4' and is allowed to surrender
i.e. player has only 2 cards*)
let can_you_surrender (state: state) (mode_command: char) : bool = 
  if state.round = Player1_turn_hit then
    (mode_command = '4' && List.length state.p1_hand = 2)
  else
    (mode_command = '4' && List.length state.p2_hand = 2)

(*1 - hit. 2 - check. 3 - double*)
let rec get_hit_check (state :state) : state = 
  (*if player has a natural (ace and 10), then don't let them hit*)
  if state.round = Player1_turn_hit && state.p1_winner = P1 then state
  else if state.round = Player2_turn_hit && state.p2_winner = P2 then state
  else
    let mode_status = wait_next_event [Key_pressed] in
    let mode_command = mode_status.key in 
    check_quit mode_command;
    if (mode_command <> '1' && mode_command <> '2' 
      &&  not (can_you_double state mode_command)
      && not (can_you_surrender state mode_command)) 
    then get_hit_check state
    else if (state.round = Player1_turn_hit) then
      update_p1_hit mode_command state
    else
      update_p2_hit mode_command state 


let rec wait_for_click (state: state) : state = 
  let mode_status = wait_next_event [Key_pressed] in
  let mode_command = mode_status.key in 
  if (mode_command = 'q') then ( (*quit game*)
  exit 0
  )
  else if (mode_command = '1') then (*continue*)
    {
    round = Player1_turn_bet;
    dealer_hand = [];
    p1_hand = [];
    p2_hand = [];
    p1_bet = 0;
    p2_bet= 0;
    p1_earnings = state.p1_earnings;
    p2_earnings = state.p2_earnings;
    p1_winner = Undetermined;
    p2_winner= Undetermined;
  }
else if (mode_command = '2') then (*reset*)
    {
    round = Player1_turn_bet;
    dealer_hand = [];
    p1_hand = [];
    p2_hand = [];
    p1_bet = 0;
    p2_bet= 0;
    p1_earnings = 0;
    p2_earnings = 0;
    p1_winner = Undetermined;
    p2_winner= Undetermined;
  }
  else wait_for_click state



let rec controller (state :state) : unit =
  match state.round with
  |Pregame -> controller state
  |Player1_turn_bet ->
    let new_state = get_bets state in
    update_gui new_state;
    print_endline "p1 just bet";
    controller new_state
  |Player2_turn_bet ->
    print_endline "p2 turn to bet";
    let new_state = get_bets state in
    update_gui new_state;
    print_endline "p2 just bet";
    controller new_state
  |Time_to_deal ->
    let new_state = deal_cards state "player1" "1st" in
    let newer_state = deal_cards new_state "player2" "1st" in
    let newest_state = deal_cards newer_state "dealer" "1st" in
    update_gui newest_state;
    let next_state = move_to_p1_hit newest_state in
    controller next_state
  |Player1_turn_hit ->
    update_gui state;
    let new_state = 
      if check_for_natural state.p1_hand 
      then move_to_p2_hit state
      else get_hit_check state 
    in 
    update_gui new_state;
    controller new_state 
  |Player2_turn_hit ->
    let new_state = 
      if check_for_natural state.p2_hand 
      then move_to_dealer_hit state
      else get_hit_check state
    in 
    update_gui new_state;
    controller new_state; 
  |Dealer_turn_hit ->
    let new_state = dealer_hit state in
    update_gui new_state;
    controller new_state
  |Find_winner -> 
    let new_state = cleanup_round state in
    update_gui new_state;
    controller new_state
  |Wait ->
    let new_state = wait_for_click state in
    update_gui new_state;
    controller new_state


let rec play () =
  let game_state = {
    round = Player1_turn_bet;
    dealer_hand = [];
    p1_hand = [];
    p2_hand = [];
    p1_bet = 0;
    p2_bet = 0;
    p1_earnings = 0;
    p2_earnings = 0;
    p1_winner = Undetermined;
    p2_winner = Undetermined
  } in
  update_gui pre_game_state;
  let mode_status = wait_next_event [Key_pressed] in
  let mode_command = mode_status.key in 
  check_quit mode_command; (*check if quit button was hit*)
  if (mode_command = '1') then (*start game*)
    let () = update_gui game_state in
    controller game_state
  else play ()

  

