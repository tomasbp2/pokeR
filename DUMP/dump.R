suits <- c("H", "D", "C", "S")
ranks = c(1:13)

# expand,grid pega todas as combinações possíveis entre os dois vetores
# depois disso, transformamos cada combinação em um objeto da classe Card

deck <- apply(expand.grid(rank = ranks, suit = suits), 1, function(card) {
  card_obj <- list(rank = as.numeric(card["rank"]), suit = card["suit"])
  class(card_obj) <- "Card"
  card_obj
})

print.Card <- function(obj) {
  suit_to_ascii <- list(H = "♥", D = "♦", C = "♣", S = "♠")
    if(obj$rank == 1) {
    obj$rank <- "A"
  } else if(obj$rank == 11) {
    obj$rank <- "J"
  } else if(obj$rank == 12) {
    obj$rank <- "Q"
  } else if(obj$rank == 13) {
    obj$rank <- "K"
  }
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
# trinca
set.seed(210)
# 2pair
set.seed(216)

community_cards <- sample(possible_cards, 5)
class(community_cards) <- "CardList"

possible_cards <- setdiff(possible_cards, community_cards)

player_cards <- sample(possible_cards, 2)
class(player_cards) <- "CardList"

class(player_cards) <- "CardList"

possible_cards <- setdiff(possible_cards, player_cards)

print(community_cards)
print(player_cards)

all_cards = c(player_cards,community_cards)
class(all_cards) = "CardList"

print(all_cards)
