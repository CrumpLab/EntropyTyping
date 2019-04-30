# run natural language entropy sim
library(matrixStats)
library(data.table)
library(Rfast)
library(data.table)
library(dplyr)
library(tibble)

#The objective is to create a 26 by 45 matrix that contains the probability
# of each letter at a given letter position/word length
#  calculated based on bigram frequencies

# The 26 by 45 matrix is bigramLetPercents

#bigramLetPercents will be used to calculate the meanLetterRetriervalTime
#at each letter position/ word length in the instance theory model

    #ngrams2 is the frequency of each bigram in words of lengths 2-9
    # bigramLenthTitles gives column names for the final 26 by 45 matrix
    # 4/2:3 means the second and third letter of a four letter word



ngrams2 <- fread("ngrams2.csv")
ngrams2=as.data.frame(ngrams2)

alphabet=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N",
           "O","P","Q","R","S","T","U","V","W","X","Y","Z")

bigramLengthsTitles=colnames(ngrams2[,11:46])

# letterPercentages stores percents for each letter in various letterPositions
# checkPercents sums the 26 letter probabilities for each letter position, should = ~1

letterPercentages=c()
checkPercentages=c()

bigramLetPercents=data.frame()

#outer loop goes through the 26 letters


for(lets in 1:26)
{
  letterPercentages=c()
  for(colz in 11:46)
  {
    #colz is each column in ngrams2.csv which represents letter position
    #bigram2ndLetter finds all the bigrams whose second letter matches 'lets'
    # for example at lets = 1, bigram2ndLetter will find the frequencies
    #of all bigrams whose second letter is 'A' in the given colz
    bigram2ndLetter = (ngrams2[substr(ngrams2$`2-gram`,2,2)==alphabet[lets],colz])
    bigram2ndLetter[which(bigram2ndLetter=="9218868437227407266")]=NA
    bigram2ndLetter=na.omit(bigram2ndLetter)
    # find the percentage of 'A' being in the 2nd letter of bigram ending at given colz
    letterPercentage = sum(bigram2ndLetter)/sum(ngrams2[,colz])
    #current letter and let position percentage
    letterPercentages=c(letterPercentages,letterPercentage)
    
    
  }
  
  bigramLetPercents=rbind(bigramLetPercents,letterPercentages)
  
}
# HOW TO INTERPRET bigramLetPercents
#The 0.00141 at Row 4, Column 3/1:2 means the percentage of bigrams that occupy the
#1st and 2nd letter of 3 letter word that have a 'D' as the 2nd letter


# each column (letter position) should sum to around 1 
checkPercentages=colSums(bigramLetPercents)
colnames(bigramLetPercents)=bigramLengthsTitles
# Now insert the probabilities of the first letter position in all word lengths of 1-9
letter_freqs <- fread("ngrams1.csv",integer64="numeric")
letter_probabilities <- apply(letter_freqs[,2:46],2,function(x){x/sum(x)})
# contains the probabilities for first letter of 1 let word, first letter of 2 let word...
firstLetProb=letter_probabilities[,grep("1:1",colnames(letter_probabilities))] 
# manually insert the first letter posn probabilities to the bigramPercentGraph
bigramLetPercents=add_column(bigramLetPercents, One=firstLetProb[,1], .after = 0)
bigramLetPercents=add_column(bigramLetPercents, Two=firstLetProb[,2], .after = 1)
bigramLetPercents=add_column(bigramLetPercents, Three=firstLetProb[,3], .after = 3)
bigramLetPercents=add_column(bigramLetPercents, Four=firstLetProb[,4], .after = 6)
bigramLetPercents=add_column(bigramLetPercents, Five=firstLetProb[,5], .after = 10)
bigramLetPercents=add_column(bigramLetPercents, Six=firstLetProb[,6], .after = 15)
bigramLetPercents=add_column(bigramLetPercents, Seven=firstLetProb[,7], .after = 21)
bigramLetPercents=add_column(bigramLetPercents, Eight=firstLetProb[,8], .after = 28)
bigramLetPercents=add_column(bigramLetPercents, Nine=firstLetProb[,9], .after = 36)

colSums(bigramLetPercents)
##########################################################################################
# NOW WE USE BIGRAM LET PERCENTS TO GET MEAN LETTER RETRIEVAL TIMES

#num_traces is number of numbers drawn from the normal distribution based on
# letter probabilities * # of letters in simulation text
# montesimnumber is 

get_retrieval_time <- function(num_traces,monte_sim_number,rnorm_mean,rnorm_sd) {
  sampled_retrieval_times <- matrix(rnorm(num_traces*monte_sim_number,rnorm_mean,rnorm_sd),
                                    ncol=num_traces,
                                    nrow=monte_sim_number)
  min_retrieval_times <- rowMins(sampled_retrieval_times)
  return(mean(min_retrieval_times))
}

all_sims_df <- data.frame()

position <-c(1,1:2,1:3,1:4,1:5,1:6,1:7,1:8,1:9)
#1 1 2 1 2 3 
word_length <-c(1,rep(2,2),
                rep(3,3),
                rep(4,4),
                rep(5,5),
                rep(6,6),
                rep(7,7),
                rep(8,8),
                rep(9,9))
# 1 2 2 3 3 3 
for (l in 1:45){
  letter_probs <- bigramLetPercents[,l]
  # l represents each letter position
  amount_of_practice <- c(50,100,200,500)
  # amount of practice is # of letters in simulation text
  mean_letter_retrieval_time <- length(length(amount_of_practice))
  for (i in 1:length(amount_of_practice)){
    #if l is 'a' and a has prob of 0.8 and we got 50 amtPract then LTFs = 40
    letter_trace_frequencies <- floor(letter_probs*amount_of_practice[i])
    letter_trace_frequencies[letter_trace_frequencies==0] <- 1 # for convenience, we always assume there is 1 trace
    # Table of 100 (simulation #) x 40, each row is a simulation 
    letter_retrieval_times     <- unlist(lapply(letter_trace_frequencies,
                                                function(x) {get_retrieval_time(x,100,500,100)}))
    
    # reset letter_trace_frequencies to include zeros for computing grand_mean
    # this would be 40 'a' in our example
    letter_trace_frequencies <- floor(letter_probs*amount_of_practice[i])
    # 40*400ms/40
    mean_letter_retrieval_time[i] <- sum(letter_retrieval_times*letter_trace_frequencies)/sum(letter_trace_frequencies)
  }
  
  sim_df_natural <- data.frame(amount_of_practice,
                               position = position[l],
                               word_length = word_length[l],
                               mean_letter_retrieval_time)
  all_sims_df <- rbind(all_sims_df,sim_df_natural)
}

all_sims_df$position<-as.factor(all_sims_df$position)
all_sims_df$word_length<-as.factor(all_sims_df$word_length)

library(ggplot2)

ggplot(all_sims_df,aes(x=position,y=mean_letter_retrieval_time,group=word_length,color=word_length))+
  geom_point()+
  geom_line()+
  theme_classic()+
  facet_wrap(~amount_of_practice)

