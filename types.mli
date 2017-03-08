type suit =
  |Hearts
  |Spades
  |Diamonds
  |Clubs

type card = {rank: int; suit: suit}
type hand = card list

type p1_or_dealer = 
  |P1
  |Dealer
  |Undetermined
  |Tie

type p2_or_dealer = 
  |P2
  |Dealer
  |Undetermined
  |Tie


type round = 
  |Pregame
  |Player1_turn_bet
  |Player2_turn_bet
  |Time_to_deal
  |Player1_turn_hit
  |Player2_turn_hit
  |Dealer_turn_hit
  |Find_winner
  |Wait


type state = {
  round: round;
  dealer_hand: hand;
  p1_hand: hand;
  p2_hand: hand;
  p1_bet: int;
  p2_bet: int;
  p1_earnings: int;
  p2_earnings: int;
  p1_winner: p1_or_dealer;
  p2_winner: p2_or_dealer
}