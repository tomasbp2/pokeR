suits <- c("H", "D", "C", "S")
ranks <- c(2:10, "J", "Q", "K", "A")

# expand,grid pega todas as combinações possíveis entre os dois vetores
# depois disso, transformamos cada combinação em um objeto da classe Card
deck <- apply(expand.grid(rank = ranks, suit = suits), 1, function(card) {
  card_obj <- list(rank = card["rank"], suit = card["suit"])
  class(card_obj) <- "Card"
  card_obj
})

print.Card <- function(obj) {
  suit_to_ascii <- list(H = "♥", D = "♦", C = "♣", S = "♠")
  cat(obj$rank, suit_to_ascii[[obj$suit]], " ", sep = "")
}

print.CardList <- function(obj) {
  for (card in obj) {
    print(card)
  }
  cat("\n")
}

possible_cards <- deck

# pair
set.seed(124)
# 2 pair
set.seed(210)

community_cards <- sample(possible_cards, 5)
#community_cards = possible_cards[]

class(community_cards) <- "CardList"

possible_cards <- setdiff(possible_cards, community_cards)

player_cards <- sample(possible_cards, 2)
class(player_cards) <- "CardList"
possible_cards <- setdiff(possible_cards, player_cards)

print(community_cards)
print(player_cards)

all_cards = c(player_cards,community_cards)
class(all_cards) = "CardList"

print(all_cards)

has_pair_2_pair_triosla <- function(cards) {

  ranks <- sapply(cards, function(card) card$rank)
  rank_counts <- table(ranks)
  if(sum(rank_counts == 2) == 1){
    return("pair")
  }
  if(sum(rank_counts == 2) == 2){
    return("2pair")
  }
  if(sum(rank_counts == 3) == 1){
    return("trinca")
  }
  if(sum(rank_counts == 4) == 1){
    return("farofakind")
  }

  naipes = sapply(cards, function(card) card$suit)
  if(length(unique(naipes)) == 1){
    return("flush")
  }

}

has_pair_2_pair_triosla(all_cards)
