# added simulation/ to the source on line 28 59
packages <- c("dplyr","ggplot2","cowplot","data.table","matrixStats","retimes")
lapply(packages, require, character.only = TRUE)


source("analysis/vj_outlier.R")


get_retrieval_time <- function(num_traces,monte_sim_number,rnorm_mean,rnorm_sd) {
  sampled_retrieval_times <- matrix(rnorm(num_traces*monte_sim_number,rnorm_mean,rnorm_sd),
                                    ncol=num_traces,
                                    nrow=monte_sim_number)
  
  min_retrieval_times <- rowMins(sampled_retrieval_times)
  return(mean(min_retrieval_times))
}



## Simulation settings
amount_of_practice<-c(50,100,500,10000)
n_monte    <- 1000
rnorm_mean <- 500
rnorm_sd   <- 100

#source("analysis/get_N1_letter_probs.R")
## 1. run simulation for letter positions 2:9 conditionalized by n-1
source("simulation/get_N1_letter_probs.R")
letter_info_N1<-get_N1_letter_probs()

all_sims_df <- data.frame()

for(l in 1:ncol(letter_info_N1$letter_probs)){
  letter_probs<-letter_info_N1$letter_probs[,l]
  mean_letter_retrieval_time <- (length(amount_of_practice))
  for (i in 1:length(amount_of_practice)){
    #letter_trace_frequencies<-unlist(table(factor(sample(x = c(seq(1:676)), amount_of_practice[i], replace = T, prob = letter_probs), levels=1:676)), use.names = FALSE)
    
    letter_trace_frequencies <- floor(letter_probs*amount_of_practice[i])
    letter_trace_frequencies[letter_trace_frequencies==0] <- 1 # for convenience, we always assume there is 1 trace
    letter_retrieval_times     <- unlist(lapply(letter_trace_frequencies,
                                                function(x) {get_retrieval_time(x,n_monte,rnorm_mean,rnorm_sd)}))
    # reset letter_trace_frequencies to include zeros for computing grand_mean
    letter_trace_frequencies <- floor(letter_probs*amount_of_practice[i])
    mean_letter_retrieval_time[i] <- sum(letter_retrieval_times*letter_trace_frequencies)/sum(letter_trace_frequencies)
  }
  
  
  sim_df_natural <- data.frame(amount_of_practice,
                               position = letter_info_N1$position[l],
                               word_length = letter_info_N1$word_length[l],
                               mean_letter_retrieval_time)
  all_sims_df <- rbind(all_sims_df,sim_df_natural)
  
}

######## get first letter position RTs ###########

source("simulation/get_letter_probs.R")
letter_info<-get_letter_probs()

#restrict to first position
letter_info$letter_probs  <-letter_info$letter_probs[,c(1,2,4,7,11,16,22,29,37)]
letter_info$position      <-letter_info$position[c(1,2,4,7,11,16,22,29,37)]
letter_info$word_length   <-letter_info$word_length[c(1,2,4,7,11,16,22,29,37)]
letter_info$word_ratio    <-letter_info$word_ratio[c(1,2,4,7,11,16,22,29,37)]
letter_info$word_probs    <-letter_info$word_probs[c(1,2,4,7,11,16,22,29,37)]

first_pos_df<-data.frame()

## run for each letter position in each word
for (l in 1:ncol(letter_info$letter_probs)){
  letter_probs <- letter_info$letter_probs[,l]
  mean_letter_retrieval_time <- (length(amount_of_practice))
  
  ## fun for each practice amount
  for (i in 1:length(amount_of_practice)){
    letter_trace_frequencies <- floor(letter_probs*amount_of_practice[i])
    letter_trace_frequencies[letter_trace_frequencies==0] <- 1 # for convenience, we always assume there is 1 trace
    letter_retrieval_times     <- unlist(lapply(letter_trace_frequencies,
                                                function(x) {get_retrieval_time(x,n_monte,rnorm_mean,rnorm_sd)}))
    # reset letter_trace_frequencies to include zeros for computing grand_mean
    letter_trace_frequencies <- floor(letter_probs*amount_of_practice[i])
    mean_letter_retrieval_time[i] <- sum(letter_retrieval_times*letter_trace_frequencies)/sum(letter_trace_frequencies)
  }
  
  first_df <- data.frame(amount_of_practice,
                         position = letter_info$position[l],
                         word_length = letter_info$word_length[l],
                         mean_letter_retrieval_time)
  first_pos_df <- rbind(first_pos_df,first_df)
}



##################################################
all_sims_df<-rbind(first_pos_df,all_sims_df)

all_sims_df$position<-as.factor(all_sims_df$position)
all_sims_df$word_length<-as.factor(all_sims_df$word_length)
all_sims_df$amount_of_practice<-as.factor(all_sims_df$amount_of_practice)


save(all_sims_df,file="simulation-data.Rda")