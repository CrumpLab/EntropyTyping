letter_freqs <- fread("data/ngrams1.csv",integer64="numeric")
letter_freqs[letter_freqs==0]<-1

get_prob<- function(df) {apply(df,2,function(x){x/sum(x)})}
get_entropies <- function(df){apply(df,2,function(x){-1*sum(x*log2(x))})}

letter_probabilities<-get_prob(letter_freqs[,2:74])
letter_entropies<-get_entropies(letter_probabilities)

let_pos<-c(1,1:2,1:3,1:4,1:5,1:6,1:7,1:8,1:9)
word_lengths<-c(1,rep(2,2),
                rep(3,3),
                rep(4,4),
                rep(5,5),
                rep(6,6),
                rep(7,7),
                rep(8,8),
                rep(9,9))

uncertainty_df<-data.frame(H=letter_entropies[11:(11+44)],let_pos,word_lengths)
uncertainty_df_pos1<-uncertainty_df %>%
  filter(
    let_pos == 1
  )


## add n-1 letters

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

gram_2[gram_2 == 0] <- 1

letters <- data.frame(do.call('rbind', strsplit(as.character(gram_2$`2-gram`),'',fixed=TRUE)))
colnames(letters)<-c('n-1','n')
names(gram_2)[names(gram_2) == '2-gram'] <- 'bigram'
gram_2<-cbind(letters,gram_2)

#remove unnecessary columns
gram_2<-gram_2[,-4:-12]
gram_2<-gram_2[,-40:-56]
gram_2[,4:39]<-apply(gram_2[,4:39],2,function(x){as.numeric(x)})

get_prob<- function(df) {apply(df,2,function(x){x/sum(x)})}
get_entropies <- function(df){apply(df,2,function(x){-1*sum(x*log2(x))})}

letter_probabilities<-(with(gram_2,
                            by(gram_2[,4:39],gram_2[,'n-1'], get_prob,simplify= TRUE)
))

get_counts <- function(df) {apply(df,2,function(x){sum(x)})}
n1_counts <- (with(gram_2,
                   by(gram_2[,4:39],gram_2[,'n-1'], get_counts,simplify= TRUE)
))
n1_counts<-list.rbind(n1_counts)
n1_probs <- t(t(n1_counts)/colSums(n1_counts))

letter_entropies<-lapply(letter_probabilities,get_entropies)
letter_entropies<-list.rbind(letter_entropies)

letter_entropies_weighted <- letter_entropies *n1_probs
colSums(letter_entropies_weighted)

# create data frame
let_pos<-c(2:2,2:3,2:4,2:5,2:6,2:7,2:8,2:9)
word_lengths<-c(rep(2,1),
                rep(3,2),
                rep(4,3),
                rep(5,4),
                rep(6,5),
                rep(7,6),
                rep(8,7),
                rep(9,8))

uncertainty_df<-data.frame(H=colSums(letter_entropies_weighted),let_pos,word_lengths)
uncertainty_df<-rbind(uncertainty_df,uncertainty_df_pos1)

library(ggplot2)

ggplot(uncertainty_df, aes(x=let_pos,y=H))+
  geom_point()+
  geom_line()+
  facet_wrap(~word_lengths)
