letter_probabilities[[1]][,1]
letter_probabilities[[2]][,1]

save_mat<-matrix(0,ncol=26,nrow=26)
save_h<-c()
for(i in 1:26){
  a<-letter_probabilities[[i]][,1]
  save_h[i]<-sum(a*log2(a))
}

a<-rowMeans(save_mat)
sum(a)

sum(a*log2(a))





library(matrixStats)

get_retrieval_time <- function(num_traces,monte_sim_number,rnorm_mean,rnorm_sd) {
  sampled_retrieval_times <- matrix(rnorm(num_traces*monte_sim_number,rnorm_mean,rnorm_sd),
                                    ncol=num_traces,
                                    nrow=monte_sim_number)
  min_retrieval_times <- rowMins(sampled_retrieval_times)
  return(mean(min_retrieval_times))
}

# run natural language entropy sim
letter_freqs <- fread("ngrams1.csv",integer64="numeric")
letter_freqs[letter_freqs==0]<-1
letter_probabilities <- apply(letter_freqs[,12:(12+44)],2,function(x){x/sum(x)})


all_sims_df <- data.frame()

position <-c(1,1:2,1:3,1:4,1:5,1:6,1:7,1:8,1:9)
word_length <-c(1,rep(2,2),
                rep(3,3),
                rep(4,4),
                rep(5,5),
                rep(6,6),
                rep(7,7),
                rep(8,8),
                rep(9,9))

for (l in 1:45){
  letter_probs <- letter_probabilities[,l]
  H<- -1*sum(letter_probs*log2(letter_probs))
  amount_of_practice <- c(50,100,200,500)
  
  mean_letter_retrieval_time <- length(length(amount_of_practice))
  for (i in 1:length(amount_of_practice)){
    letter_trace_frequencies <- floor(letter_probs*amount_of_practice[i])
    letter_trace_frequencies[letter_trace_frequencies==0] <- 1 # for convenience, we always assume there is 1 trace
    letter_retrieval_times     <- unlist(lapply(letter_trace_frequencies,
                                                function(x) {get_retrieval_time(x,100,500,100)}))
    # reset letter_trace_frequencies to include zeros for computing grand_mean
    letter_trace_frequencies <- floor(letter_probs*amount_of_practice[i])
    mean_letter_retrieval_time[i] <- sum(letter_retrieval_times*letter_trace_frequencies)/sum(letter_trace_frequencies)
  }
  
  sim_df_natural <- data.frame(amount_of_practice,
                               position = position[l],
                               word_length = word_length[l],
                               mean_letter_retrieval_time,
                               H)
  all_sims_df <- rbind(all_sims_df,sim_df_natural)
}

all_sims_df$position<-as.factor(all_sims_df$position)
all_sims_df$word_length<-as.factor(all_sims_df$word_length)

ggplot(all_sims_df,aes(x=position,y=mean_letter_retrieval_time,group=word_length,color=word_length))+
  geom_point()+
  geom_line()+
  scale_colour_grey()+
  labs(color='Word Length') +
  ylab("Simulated Mean Retrieval Time")+
  xlab("Letter Position")+
  theme_classic()+
  facet_wrap(~amount_of_practice)

ggplot(all_sims_df,aes(x=H,y=mean_letter_retrieval_time))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_colour_grey()+
  labs(color='Word Length') +
  ylab("Simulated Mean Retrieval Time")+
  xlab("Entropy (H)")+
  theme_classic(base_size=15)+
  facet_wrap(~amount_of_practice)
