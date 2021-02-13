source("R_install_load.R")
packages <- c("Crump","dplyr","ggplot2","cowplot","data.table","matrixStats")
install_load(packages)



# function to compute expected retrieval time given number of traces
# num_traces is the number of memory traces 
# monte_sim_number is the number of monte_carlo simulations to run
# rnorm_mean is mean of normal distribution of retrieval times
# rnorm_sd is standard deviation of normal distribution of retrieval times



get_retrieval_time <- function(num_traces,monte_sim_number,rnorm_mean,rnorm_sd) {
  sampled_retrieval_times <- matrix(rnorm(num_traces*monte_sim_number,rnorm_mean,rnorm_sd),
                                    ncol=num_traces,
                                    nrow=monte_sim_number)
  min_retrieval_times <- rowMins(sampled_retrieval_times)
  return(mean(min_retrieval_times))
}




if(!exists("the_data")){load("the_data.Rdata")}

IKSI_Dist<-mexgauss(the_data$IKSIs)
rm(the_data)
#rexgauss(10,mu = IKSI_Dist[1], sigma = IKSI_Dist[2], tau = IKSI_Dist[3], positive = TRUE)


get_retrieval_time <- function(num_traces,monte_sim_number,rnorm_mean,rnorm_sd) {
  #sampled_retrieval_times <- matrix(rexgauss(num_traces*monte_sim_number,mu = IKSI_Dist[1], sigma = IKSI_Dist[2], tau = IKSI_Dist[3], positive = TRUE),
  #                                  ncol=num_traces,
  #                                  nrow=monte_sim_number)
  
  
  #sampled_retrieval_times <- matrix(rnorm(num_traces*monte_sim_number,rnorm_mean,rnorm_sd),
  #                                  ncol=num_traces,
  #                                  nrow=monte_sim_number)
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
  amount_of_practice <- c(50)
  
  mean_letter_retrieval_time <- (length(amount_of_practice))
  
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
                               mean_letter_retrieval_time)
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

toPlot<-all_sims_df %>%
 # filter(amount_of_practice== 50 | amount_of_practice == 500) %>%
  mutate(amount_of_practice = factor(amount_of_practice))


names(toPlot)[names(toPlot)=="amount_of_practice"]  <- "Amount of Practice (Keystrokes)"

figure5<-ggplot(toPlot,aes(x=position, y=mean_letter_retrieval_time, group = `Amount of Practice (Keystrokes)`))+
  #geom_errorbar(data = summary, aes(x = let_pos, ymin = mean-ci, ymax = mean+ci), colour = "gray20", width = .8, size = 0.3)+
  geom_line(size = .5)+
  #scale_linetype_manual(values=c("twodash","dotted","dashed","solid"))+
  geom_point(size = 1.3, aes(shape =`Amount of Practice (Keystrokes)`))+
  theme_classic()+
  xlab("Letter Position")+
  ylab("Simulated IKSI (ms)")+
  scale_shape_manual(values=c(15,18,17,16))+
  #scale_y_continuous(limits=c(120,282),breaks=c(120,160,200,240,280), expand = c(0.01,0.01)) + 
  theme(axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold"),
        axis.line=element_blank(),
        axis.ticks=element_line(size = .4),
        panel.spacing = unit(.1,"lines"),
        panel.grid.minor = element_line(colour="gray90", size=0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.major = element_line(colour="gray90", size=0.5),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(fill = NA, colour="black")) +
  theme(legend.position="bottom",
        legend.background = element_rect(colour = "black", fill = "white"),
        legend.box.background = element_rect(colour = "black", size=.75),
        legend.margin = margin(t = 0.1, r = .25, b = 0, l = .25, unit = "cm"),
        legend.text = element_text(size = 8, margin =c(0, unit = "cm")),
        legend.title = element_text(size = 8))+
  facet_grid(.~word_length)+
  theme(strip.text.x = element_text(margin = margin(.1,0,.1,0, "cm"), angle =  0, size = 10),
        strip.placement = "inside",
        strip.background = element_blank())

figure5
ggsave("figure5.pdf", device = "pdf", dpi = 600,
      width = 6.75, height =3.25, units = "in") 
