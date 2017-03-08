open Types


(*--------------------------------*)
(*----------INITIALIZERS----------*)
(*--------------------------------*)

(*jack is 14, ace is 11
queen is 12, king is 13*)

let cards_used = ref []

(*count ace as 11, only*)
let rec get_dealer_sum (hand: hand) : int = 
  match hand with
  |[] -> 0
  |h::[] -> if (h.rank = 12 || h.rank = 13 || h.rank = 14) then 10 
            else h.rank
  |h::t -> if (h.rank = 12 || h.rank = 13 || h.rank = 14) then 10 + get_dealer_sum t
            else h.rank + get_dealer_sum t

(*count ace as 1, for checking bust or not bust of players*)
let rec get_sum (hand : hand) : int = 
  match hand with
  |[] -> 0
  |h::[] -> if (h.rank = 11) then 1
           else if (h.rank = 12 || h.rank = 13 || h.rank = 14) then 10
          else h.rank
  |h::t -> if (h.rank=11) then (1 + get_sum t) 
          else if (h.rank = 12 || h.rank = 13 || h.rank = 14) then (10 + get_sum t)
          else (h.rank + get_sum t)

let clear_cards () : unit = 
  cards_used := [];
  ()

let renew_num_gen () : unit = 
  Random.self_init ();
  ()

(*true iff hand is a bust (if sum exceeds 21)*)
let check_bust hand : bool =
  let sum = get_sum hand in 
  sum > 21 

let rec get_card () : card = 
  Random.self_init ();
  let ran_num = Random.int 15 in 
  let ran_num_suit = Random.int 3 in 
  let ran_suit = 
    if ran_num_suit = 0 then Hearts
    else if ran_num_suit = 1 then Spades
    else if ran_num_suit = 2 then Diamonds
    else Clubs
  in let possible_hand = (ran_num, ran_suit) in 
  if ((List.mem possible_hand !cards_used) || ran_num<2)then get_card ()
  else 
    let () = cards_used := (!cards_used @ [possible_hand]) in
    let ran_num =
      if (ran_num = 11) then 14 (*switch jack to 14*)
      (* if (ran_num = 11) then 12 || ran_num = 12 || ran_num = 13) then 10 *)
      else if (ran_num = 14) then 11 (*switch ace to 11*)
      else ran_num
    in 
    {rank = ran_num; suit = ran_suit}

let rec deal_cards (state:state) (player:string) (round:string) : state =
  let card_dealt = get_card () in 
  if (player = "player1" && round = "1st") then 
    let new_state = 
    {
      round = state.round;
      dealer_hand = state.dealer_hand;
      p1_hand = state.p1_hand@[card_dealt];
      p2_hand = state.p2_hand;
      p1_bet = state.p1_bet;
      p2_bet=state.p2_bet;
      p1_earnings = state.p1_earnings;
      p2_earnings = state.p2_earnings;
      p1_winner = state.p1_winner;
      p2_winner=state.p2_winner;
    } in 
    print_endline "player 1 dealing";
    deal_cards new_state "player1" "2nd"
  else if (player = "player1" && round = "2nd") then
    let new_state = 
    {
      round = state.round;
      dealer_hand = state.dealer_hand;
      p1_hand = state.p1_hand@[card_dealt]; 
      p2_hand = state.p2_hand;
      p1_bet = state.p1_bet;
      p2_bet=state.p2_bet;
      p1_earnings = state.p1_earnings;
      p2_earnings = state.p2_earnings;
      p1_winner = state.p1_winner;
      p2_winner=state.p2_winner;
    } in 
    print_endline "player 1 done dealing";
    new_state
  else if (player = "player2" && round = "1st") then
    let new_state = 
    {
      round = state.round;
      dealer_hand = state.dealer_hand;
      p1_hand = state.p1_hand;
      p2_hand = state.p2_hand@[card_dealt];
      p1_bet = state.p1_bet;
      p2_bet=state.p2_bet;
      p1_earnings = state.p1_earnings;
      p2_earnings = state.p2_earnings;
      p1_winner = state.p1_winner;
      p2_winner=state.p2_winner;
    } in deal_cards new_state "player2" "2nd"
  else if (player = "player2" && round = "2nd") then
    let new_state = 
    {
      round = state.round;
      dealer_hand = state.dealer_hand;
      p1_hand = state.p1_hand;
      p2_hand = state.p2_hand@[card_dealt];
      p1_bet = state.p1_bet;
      p2_bet=state.p2_bet;
      p1_earnings = state.p1_earnings;
      p2_earnings = state.p2_earnings;
      p1_winner = state.p1_winner;
      p2_winner=state.p2_winner;
    } in new_state
  else if (player = "dealer" && round = "1st") then
    let new_state = 
    {
      round = state.round;
      dealer_hand = state.dealer_hand@[card_dealt];
      p1_hand = state.p1_hand;
      p2_hand = state.p2_hand;
      p1_bet = state.p1_bet;
      p2_bet=state.p2_bet;
      p1_earnings = state.p1_earnings;
      p2_earnings = state.p2_earnings;
      p1_winner = state.p1_winner;
      p2_winner=state.p2_winner;
    } in deal_cards new_state "dealer" "2nd"
  else (* if (player = "dealer" && round = "2nd") then *)
    let new_state = 
    {
      round = state.round;
      dealer_hand = state.dealer_hand@[card_dealt];
      p1_hand = state.p1_hand;
      p2_hand = state.p2_hand;
      p1_bet = state.p1_bet;
      p2_bet=state.p2_bet;
      p1_earnings = state.p1_earnings;
      p2_earnings = state.p2_earnings;
      p1_winner = state.p1_winner;
      p2_winner=state.p2_winner;
    } in new_state


let update_p1_bets mode_command state : state = 
  let new_p1_bet = 
    if mode_command = '1' then 
      state.p1_bet + 1 
    else if mode_command = '2' then
      state.p1_bet +5
    else if mode_command = '3' then
      state.p1_bet + 25
    else if mode_command = '4' then
      state.p1_bet + 100
    else
      state.p1_bet+500
  in 
    {
      round = Player2_turn_bet;
      dealer_hand = state.dealer_hand;
      p1_hand = state.p1_hand;
      p2_hand = state.p2_hand;
      p1_bet = new_p1_bet;
      p2_bet=state.p2_bet;
      p1_earnings = state.p1_earnings;
      p2_earnings = state.p2_earnings;
      p1_winner = state.p1_winner;
      p2_winner=state.p2_winner;
    }
  

let update_p2_bets mode_command state : state = 
  let new_p2_bet = 
    if mode_command = '1' then 
      state.p2_bet + 1 
    else if mode_command = '2' then
      state.p2_bet +5
    else if mode_command = '3' then
      state.p2_bet + 25
    else if mode_command = '4' then
      state.p2_bet + 100
    else
      state.p2_bet+500
  in
    {
      round = Time_to_deal;
      dealer_hand = state.dealer_hand;
      p1_hand = state.p1_hand;
      p2_hand = state.p2_hand;
      p1_bet = state.p1_bet;
      p2_bet= new_p2_bet;
      p1_earnings = state.p1_earnings;
      p2_earnings = state.p2_earnings;
      p1_winner = state.p1_winner;
      p2_winner=state.p2_winner;
    }


let update_p1_hit (mode_command : char) (state:state) : state = 
  if (mode_command = '2') then (*p1 checked, move onto p2 hit round*)
  {
    round = Player2_turn_hit;
    dealer_hand = state.dealer_hand;
    p1_hand = state.p1_hand;
    p2_hand = state.p2_hand;
    p1_bet = state.p1_bet;
    p2_bet=state.p2_bet;
    p1_earnings = state.p1_earnings;
    p2_earnings = state.p2_earnings;
    p1_winner = state.p1_winner;
    p2_winner=state.p2_winner;
  } 
  else if (mode_command = '1') then (*p1 hit*)
    let new_state = deal_cards state "player1" "2nd" in
    if (check_bust new_state.p1_hand) then (*player 1 busted*)
    {
      round = Player2_turn_hit;
      dealer_hand = new_state.dealer_hand;
      p1_hand = new_state.p1_hand;
      p2_hand = new_state.p2_hand;
      p1_bet = new_state.p1_bet;
      p2_bet= new_state.p2_bet;
      p1_earnings = new_state.p1_earnings - new_state.p1_bet;
      p2_earnings = new_state.p2_earnings;
      p1_winner = Dealer;
      p2_winner=new_state.p2_winner;
    }
    else new_state
  else if (mode_command = '4') then (*p1 surrendered*) 
   { 
      round = Player2_turn_hit;
      dealer_hand = state.dealer_hand;
      p1_hand = state.p1_hand;
      p2_hand = state.p2_hand;
      p1_bet = state.p1_bet - state.p1_bet/2;
      p2_bet= state.p2_bet;
      p1_earnings = state.p1_earnings - state.p1_bet + state.p1_bet/2;
      p2_earnings = state.p2_earnings;
      p1_winner = Dealer;
      p2_winner=state.p2_winner;
    }
  else (* if (mode_command = '3') *)(*p1 doubled*)
    let new_state = deal_cards state "player1" "2nd" in
    if (check_bust new_state.p1_hand) then 
    { (*player 1 doubled and busted*)
      round = Player2_turn_hit;
      dealer_hand = new_state.dealer_hand;
      p1_hand = new_state.p1_hand;
      p2_hand = new_state.p2_hand;
      p1_bet = new_state.p1_bet*2;
      p2_bet= new_state.p2_bet;
      p1_earnings = new_state.p1_earnings - 2*new_state.p1_bet;
      p2_earnings = new_state.p2_earnings;
      p1_winner = Dealer;
      p2_winner=new_state.p2_winner;
    }
    else 
    { (*player 1 doubled and did not bust*)
      round = new_state.round;
      dealer_hand = new_state.dealer_hand;
      p1_hand = new_state.p1_hand;
      p2_hand = new_state.p2_hand;
      p1_bet = new_state.p1_bet*2;
      p2_bet= new_state.p2_bet;
      p1_earnings = new_state.p1_earnings;
      p2_earnings = new_state.p2_earnings;
      p1_winner = new_state.p1_winner;
      p2_winner=new_state.p2_winner;
    }
  

let update_p2_hit (mode_command: char) (state:state) : state =
  if (mode_command = '2') then (*p2 checked, move onto Dealer hit round*)
  {
    round = Dealer_turn_hit;
    dealer_hand = state.dealer_hand;
    p1_hand = state.p1_hand;
    p2_hand = state.p2_hand;
    p1_bet = state.p1_bet;
    p2_bet=state.p2_bet;
    p1_earnings = state.p1_earnings;
    p2_earnings = state.p2_earnings;
    p1_winner = state.p1_winner;
    p2_winner=state.p2_winner;
  }  
  else if (mode_command = '1') then (*p2 hit*)
    let new_state = deal_cards state "player2" "2nd" in
    if (check_bust new_state.p2_hand) then (*player 2 busted*)
    {
      round = Dealer_turn_hit;
      dealer_hand = new_state.dealer_hand;
      p1_hand = new_state.p1_hand;
      p2_hand = new_state.p2_hand;
      p1_bet = new_state.p1_bet;
      p2_bet= new_state.p2_bet;
      p1_earnings = new_state.p1_earnings;
      p2_earnings = new_state.p2_earnings - new_state.p2_bet;
      p1_winner = new_state.p1_winner;
      p2_winner=Dealer;
    }
    else new_state
  else if (mode_command = '4') then (*p2 surrendered*) 
   { 
      round = Dealer_turn_hit;
      dealer_hand = state.dealer_hand;
      p1_hand = state.p1_hand;
      p2_hand = state.p2_hand;
      p1_bet = state.p1_bet;
      p2_bet= state.p2_bet - state.p2_bet/2;
      p1_earnings = state.p1_earnings;
      p2_earnings = state.p2_earnings- state.p2_bet + state.p2_bet/2;
      p1_winner = Dealer;
      p2_winner=state.p2_winner;
    }
  else (*p2 doubled*)
    let new_state = deal_cards state "player2" "2nd" in
    if (check_bust new_state.p2_hand) then 
    { (*player 2 doubled and busted, change p2 bet amount and earnings
    and p2 winner*)
      round = Dealer_turn_hit;
      dealer_hand = new_state.dealer_hand;
      p1_hand = new_state.p1_hand;
      p2_hand = new_state.p2_hand;
      p1_bet = new_state.p1_bet;
      p2_bet= new_state.p2_bet*2;
      p1_earnings = new_state.p1_earnings;
      p2_earnings = new_state.p2_earnings-  2*new_state.p2_bet;
      p1_winner = new_state.p1_winner;
      p2_winner= Dealer;
    }
    else 
      { (*p2 doubled and did not bust, only change bet amount*)
      round = new_state.round;
      dealer_hand = new_state.dealer_hand;
      p1_hand = new_state.p1_hand;
      p2_hand = new_state.p2_hand;
      p1_bet = new_state.p1_bet;
      p2_bet= new_state.p2_bet*2;
      p1_earnings = new_state.p1_earnings;
      p2_earnings = new_state.p2_earnings;
      p1_winner = new_state.p1_winner;
      p2_winner= new_state.p2_winner;
    }

let check_for_ace x : bool = 
  let pair= (Pervasives.fst x) in 
  pair.rank = 11

(*dealer hits if sum is less than 17. this sum counts an Ace as 11, so long as it does not 
cause dealer to bust
hence, we go by the hard 17 rule, which favors the player, not the house
If the dealer has an Ace and two other cards that would cause them to go over 21 
the Ace is then counted as 1. *)
let rec dealer_hit (state: state) : state = 
  (*if both players busted, then dealer shouldn't hit*)
  if (state.p1_winner = Dealer && state.p2_winner = Dealer) then 
  {
    round = Find_winner;
    dealer_hand = state.dealer_hand;
    p1_hand = state.p1_hand;
    p2_hand = state.p2_hand;
    p1_bet = state.p1_bet;
    p2_bet= state.p2_bet;
    p1_earnings = state.p1_earnings;
    p2_earnings = state.p2_earnings;
    p1_winner = state.p1_winner;
    p2_winner=state.p2_winner;
  }
  else
    let sum = get_dealer_sum state.dealer_hand in 
    (*sum counts ace as an 11*)
    if (sum > 16 && sum < 22) then (*stand*)
    {
      round = Find_winner;
      dealer_hand = state.dealer_hand;
      p1_hand = state.p1_hand;
      p2_hand = state.p2_hand;
      p1_bet = state.p1_bet;
      p2_bet= state.p2_bet;
      p1_earnings = state.p1_earnings;
      p2_earnings = state.p2_earnings;
      p1_winner = state.p1_winner;
      p2_winner=state.p2_winner;
    }
    (*at this point, dealer has not busted. however, the 'sum' variable may exceed 21
    this is b/c the 'sum' variable counts ace as an 11. if the 'sum' variable
    exceeds 21, then the dealer must have an ace b/c there is no other way for the dealer to
    have not busted but have a hadn that exceeds 21. in this case, the dealer should hit again. 
    this particular situation is accounted for here*)

    else let new_state = deal_cards state "dealer" "2nd" in
      if (check_bust new_state.dealer_hand) then (*dealer busted*) 
      {
        round = Find_winner;
        dealer_hand = new_state.dealer_hand;
        p1_hand = state.p1_hand;
        p2_hand = state.p2_hand;
        p1_bet = state.p1_bet;
        p2_bet= state.p2_bet;
        p1_earnings =  if (state.p1_winner = Undetermined) then state.p1_bet+state.p1_earnings else state.p1_earnings;
        p2_earnings = if (state.p2_winner = Undetermined) then state.p2_bet+state.p2_earnings else state.p2_earnings;
        p1_winner = if (state.p1_winner = Undetermined) then P1 else state.p1_winner;
        p2_winner= if (state.p2_winner = Undetermined) then P2 else state.p2_winner;
      }
      else dealer_hit new_state

let get_player_sum (hand: hand) : int = 
  if get_dealer_sum hand > 21 
  then get_sum hand 
  else get_dealer_sum hand

let find_p1_winner (state: state) : state = 
  if (state.p1_winner <> Undetermined) then state
  else (*neither p1 nor dealer busted*)
    let p1_sum = get_player_sum state.p1_hand in
    let dealer_sum = get_player_sum state.dealer_hand in
    if (p1_sum > dealer_sum) then 
    {
      round = state.round;
      dealer_hand = state.dealer_hand;
      p1_hand = state.p1_hand;
      p2_hand = state.p2_hand;
      p1_bet = state.p1_bet;
      p2_bet= state.p2_bet;
      p1_earnings = state.p1_earnings + state.p1_bet;
      p2_earnings = state.p2_earnings;
      p1_winner = P1;
      p2_winner=state.p2_winner;
    }
    else if (p1_sum < dealer_sum) then
    {
      round = state.round;
      dealer_hand = state.dealer_hand;
      p1_hand = state.p1_hand;
      p2_hand = state.p2_hand;
      p1_bet = state.p1_bet;
      p2_bet= state.p2_bet;
      p1_earnings = state.p1_earnings - state.p1_bet;
      p2_earnings = state.p2_earnings;
      p1_winner = Dealer;
      p2_winner=state.p2_winner;
    }
    else
    {
      round = state.round;
      dealer_hand = state.dealer_hand;
      p1_hand = state.p1_hand;
      p2_hand = state.p2_hand;
      p1_bet = state.p1_bet;
      p2_bet= state.p2_bet;
      p1_earnings = state.p1_earnings;
      p2_earnings = state.p2_earnings;
      p1_winner = Tie;
      p2_winner=state.p2_winner;
    }

let find_p2_winner (state: state) : state = 
  if (state.p2_winner <> Undetermined) then state
  else (*neither p2 nor dealer busted*)
    let p2_sum = get_player_sum state.p2_hand in
    let dealer_sum = get_player_sum state.dealer_hand in
    if (p2_sum > dealer_sum) then 
    {
      round = state.round;
      dealer_hand = state.dealer_hand;
      p1_hand = state.p1_hand;
      p2_hand = state.p2_hand;
      p1_bet = state.p1_bet;
      p2_bet= state.p2_bet;
      p1_earnings = state.p1_earnings;
      p2_earnings = state.p2_earnings + state.p2_bet;
      p1_winner = state.p1_winner;
      p2_winner=P2;
    }
    else if (p2_sum < dealer_sum) then
    {
      round = state.round;
      dealer_hand = state.dealer_hand;
      p1_hand = state.p1_hand;
      p2_hand = state.p2_hand;
      p1_bet = state.p1_bet;
      p2_bet= state.p2_bet;
      p1_earnings = state.p1_earnings;
      p2_earnings = state.p2_earnings - state.p2_bet;
      p1_winner = state.p1_winner;
      p2_winner= Dealer;
    }
    else
    {
      round = state.round;
      dealer_hand = state.dealer_hand;
      p1_hand = state.p1_hand;
      p2_hand = state.p2_hand;
      p1_bet = state.p1_bet;
      p2_bet= state.p2_bet;
      p1_earnings = state.p1_earnings;
      p2_earnings = state.p2_earnings;
      p1_winner = state.p1_winner;
      p2_winner= Tie;
    }

let cleanup_round (state: state) : state = 
  let new_state = find_p1_winner state in
  let new_state = find_p2_winner new_state in 
  (*at this point, all winners have been determined*)
  {
    round = Wait;
    dealer_hand = new_state.dealer_hand;
    p1_hand = new_state.p1_hand;
    p2_hand = new_state.p2_hand;
    p1_bet = new_state.p1_bet;
    p2_bet= new_state.p2_bet;
    p1_earnings = new_state.p1_earnings;
    p2_earnings = new_state.p2_earnings;
    p1_winner = new_state.p1_winner;
    p2_winner= new_state.p2_winner;
  }



  let check_for_natural (hand : hand) : bool = 
    let first_card = List.hd hand in 
    let second_card = List.nth hand 1 in 
    (first_card.rank = 11 || second_card.rank = 11) &&
    (first_card.rank = 10 || second_card.rank = 10)

(*after time to deal state is over, call this to move
to next state, Player1_turn_hit*)
let move_to_p1_hit (state : state) : state = 
  {
    round = Player1_turn_hit;
    dealer_hand = state.dealer_hand;
    p1_hand = state.p1_hand;
    p2_hand = state.p2_hand;
    p1_bet = state.p1_bet;
    p2_bet=state.p2_bet;
    p1_earnings = state.p1_earnings;
    p2_earnings = state.p2_earnings;
    p1_winner = state.p1_winner;
    p2_winner=state.p2_winner;
  }

  (*p1 has a natural, so just move onto p2 hit state
  also set p1 to be a winner*)
  let move_to_p2_hit (state : state) : state = 
  {
    round = Player2_turn_hit;
    dealer_hand = state.dealer_hand;
    p1_hand = state.p1_hand;
    p2_hand = state.p2_hand;
    p1_bet = state.p1_bet;
    p2_bet=state.p2_bet;
    p1_earnings = state.p1_earnings + state.p1_bet;
    p2_earnings = state.p2_earnings;
    p1_winner = P1;
    p2_winner=state.p2_winner;
  }
  (*p2 has a natural, so just move onto dealer hit state
  also set p1 to be a winner*)
  let move_to_dealer_hit (state: state) : state = 
  {
    round = Player2_turn_hit;
    dealer_hand = state.dealer_hand;
    p1_hand = state.p1_hand;
    p2_hand = state.p2_hand;
    p1_bet = state.p1_bet;
    p2_bet=state.p2_bet;
    p1_earnings = state.p1_earnings;
    p2_earnings = state.p2_earnings + state.p2_bet;
    p1_winner = state.p1_winner;
    p2_winner=P2;
  }

