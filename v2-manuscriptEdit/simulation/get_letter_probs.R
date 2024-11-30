#' @return Returns a matrix of letter x position x word length probabilities 
#'

get_letter_probs <- function(){
  # run natural language entropy sim
  letter_freqs <- fread("data/ngrams1.csv",integer64="numeric")
  letter_freqs[letter_freqs==0]<-1
  letter_probabilities <- apply(letter_freqs[,12:(12+44)],2,function(x){x/sum(x)})
  
  ## Create other indices
  position <-c(1,1:2,1:3,1:4,1:5,1:6,1:7,1:8,1:9)
  word_length <-c(1,rep(2,2),
                  rep(3,3),
                  rep(4,4),
                  rep(5,5),
                  rep(6,6),
                  rep(7,7),
                  rep(8,8),
                  rep(9,9))
  
  
  word_probs <-c(.02998,
                 rep(.17651,2),
                 rep(.20511,3),
                 rep(.14787,4),
                 rep(.107,5),
                 rep(.08388,6),
                 rep(.07939,7),
                 rep(.05943,8),
                 rep(.04437,9))
  
  word_ratio <-c(1, 
                 rep(floor(17.651/2.998),2),
                 rep(floor(20.511/2.998),3),
                 rep(floor(14.787/2.998),4),
                 rep(floor(10.700/2.998),5),
                 rep(floor(8.388/2.998),6),
                 rep(floor(7.939/2.998),7),
                 rep(floor(5.943/2.998),8),
                 rep(floor(4.437/2.998),9))
  
  
  output<-list()
  output$letter_probs <- letter_probabilities
  output$position <- position
  output$word_length <- word_length
  output$word_probs <- word_probs
  output$word_ratio <- word_ratio
  
  
  
  return(output)
}

