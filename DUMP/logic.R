create_card <- function(rank, suit) {
  card <- list(rank = (rank), suit = suit)
  class(card) <- "Card"
  return(card)
}

create_cards <- function(cards) {
  card_list <- lapply(cards, function(card) {
    rank <- substr(card, 1, nchar(card) - 1)
    class(rank)
    suit <- substr(card, nchar(card), nchar(card))
    if(rank == "A") {
      rank = 1
    }
    if(rank == "J") {
      rank = 11
    }
    if(rank == "Q") {
      rank = 12
    }
    if(rank == "K") {
      rank = 13
    }
    create_card(as.numeric(rank), suit)
  })

  class(card_list) <- "CardList"
  return(card_list)
}

player = create_cards(c("2H", "KS"))
table = create_cards(c("1H", "3H","4H","QS","5H"))

comb = c(player,table)
class(comb) = "CardList"

comb

ranks = sapply(comb, function(card) card$rank)
suits = sapply(comb, function(card) card$suit)
rank_counts = table(ranks)
suits_counts = table(suits)
mat_counts = table(ranks,suits)


check_combination <- function(cards) {

  ranks = sapply(cards, function(card) card$rank)
  suits = sapply(cards, function(card) card$suit)
  rank_counts = table(ranks)
  suits_counts = table(suits)
  mat_counts = table(ranks,suits)

  print(rank_counts)
  print(suits_counts)
  print(mat_counts)

  sorted = sort(ranks)

  for(i in 1:3){
    if( all.equal(seq(sorted[i]:sorted[i+4]), sorted[i:(i+4)]) ){
      if(any(colSums(mat_counts[i:(i+4),]) == 5)){
        return("straight flush")
      } else {
        return("straight")
      }
    }
  }

  if(any(suits_counts == 5)){
    return("flush")
  }
  if(sum(rank_counts == 4) == 1){
    return("farofakind")
  }
  if(sum(rank_counts == 3) == 1){
    return("trinca")
  }
  if(sum(rank_counts == 2) == 2){
    return("2pair")
  }
  if(sum(rank_counts == 2) == 1){
    return("pair")
  }
  return("high card")
}

comb
print(check_combination(comb))


# if((sort(ranks)[i+4] - sort(ranks)[i] == 4) && sort(ranks)[i+1] == (sort(ranks)[i]+1)){
#   if(any(colSums(mat_counts[i:(i+4),])) == 5){
#     return("straight flush")
#   } else
#     return("straight")
# }
