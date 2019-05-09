#' @return Returns the letter x position x word length probabilities conditionalized by N-1
#' Does not include letter position 1
#' 


# GET Letter probabilities for n-1
get_N1_letter_probs<-function(){
  get_prob<- function(df) {apply(df,2,function(x){x/sum(x)})}
  get_entropies <- function(df){apply(df,2,function(x){-1*sum(x*log2(x))})}
  
  # GET LETTER POSITION > 1
  # read in n-gram tsv and clean up
  gram_2 <- read.table('data/2-gram.txt',header=TRUE,sep="\t")
  colnames(gram_2)<- scan(file="data/2-gram.txt",what="text",nlines=1,sep="\t")
  
  # fix NA level
  levels(gram_2$`2-gram`)<-c(levels(gram_2$`2-gram`),as.character("NA"))
  gram_2[is.na(gram_2$`2-gram`),]$`2-gram` = as.character("NA")
  
  
  # find and replace missing combos with 0 
  allLet<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")
  allCombos<-c()
  for (i in 1:length(allLet)){
    for(j in 1:length(allLet)){
      allCombos<-c(allCombos,paste(allLet[i],allLet[j],sep=""))
    }
  }
  
  missing<-allCombos[!allCombos%in%gram_2$`2-gram`]
  missing<-cbind(missing,matrix(0,nrow = length(missing), ncol = ncol(gram_2)-1))
  colnames(missing)<-colnames(gram_2)
  gram_2<-rbind(gram_2,missing)
  
  # change 0s to 1s
  gram_2[gram_2 == 0] <- 1
  
  #split bigrams into letter 1 & 2
  letters <- data.frame(do.call('rbind', strsplit(as.character(gram_2$`2-gram`),'',fixed=TRUE)))
  colnames(letters)<-c('n-1','n')
  names(gram_2)[names(gram_2) == '2-gram'] <- 'bigram'
  gram_2<-cbind(letters,gram_2)
  
  #remove unnecessary columns
  gram_2<-gram_2[,-4:-12]
  gram_2<-gram_2[,-40:-56]
  gram_2[,4:39]<-apply(gram_2[,4:39],2,function(x){as.numeric(x)})
  
  # GET ENTROPIES
  get_prob<- function(df) {apply(df,2,function(x){x/sum(x)})}
  get_entropies <- function(df){apply(df,2,function(x){-1*sum(x*log2(x))})}
  
  letter_probabilities<-(with(gram_2,
                              by(gram_2[,4:39],gram_2[,'n-1'], get_prob,simplify= TRUE)
  ))
  
  #reshape list into matrix  
  for (i in 1:26){
    if(i == 1){
      df = as.data.frame(letter_probabilities[[i]])
    } else {
      temp = as.data.frame(letter_probabilities[[i]])
      df<-rbind(df,temp)
    }
    
  }
  
  ## Create other indices
  position <-c(2:2,2:3,2:4,2:5,2:6,2:7,2:8,2:9)
  word_length <-c(rep(2,1),
                  rep(3,2),
                  rep(4,3),
                  rep(5,4),
                  rep(6,5),
                  rep(7,6),
                  rep(8,7),
                  rep(9,8))
  
  
  word_probs <-c(rep(.17651,1),
                 rep(.20511,2),
                 rep(.14787,3),
                 rep(.107,4),
                 rep(.08388,5),
                 rep(.07939,6),
                 rep(.05943,7),
                 rep(.04437,8))
  
  word_ratio <-c(rep(floor(17.651/2.998),1),
                 rep(floor(20.511/2.998),2),
                 rep(floor(14.787/2.998),3),
                 rep(floor(10.700/2.998),4),
                 rep(floor(8.388/2.998),5),
                 rep(floor(7.939/2.998),6),
                 rep(floor(5.943/2.998),7),
                 rep(floor(4.437/2.998),8))
  
  
  output<-list()
  output$letter_probs <- df
  output$position <- position
  output$word_length <- word_length
  output$word_probs <- word_probs
  output$word_ratio <- word_ratio
  
  
  return(output)
}