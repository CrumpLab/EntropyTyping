# Analysis file for Entropy_typing project

# source code for the Crump functions
non_recursive_moving<-function(rts){
  xsize <- c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 
             25, 30, 35, 50, 100)
  stds <- c(1.458, 1.68, 1.841, 1.961, 2.05, 2.12, 2.173, 
            2.22, 2.246, 2.274, 2.31, 2.326, 2.391, 2.41, 2.4305, 
            2.45, 2.48, 2.5)
  if(length(rts>=100)){
    sdc=2.5
  }else{
    sdc<-approx(xsize,stds,xout=length(rts))$y
  }
  mean_rts<-mean(rts)
  restricted_rts<-rts[rts > mean_rts - (sd(rts)*sdc) &
                        rts < mean_rts + (sd(rts)*sdc)]
  
  list(original_rts=rts,restricted=restricted_rts,prop_removed=(1-(length(restricted_rts)/length(rts))))
}

stde <-
  function(x) sd(x)/sqrt(length(x))

## @knitr load_functions  -----



library(data.table)

library(dplyr)

library(ggplot2)


library(ggpubr)





## @knitr load_pre_process  -----



# mturk.txt is the unzipped mturk.txt.zip file

the_data <- fread("C:/Users/Walt/Desktop/mturk.txt")



# Data-Exclusion



the_data[grepl("[[:punct:]]",substr(the_data$whole_word,nchar(the_data$whole_word),nchar(the_data$whole_word))),]$word_lengths=the_data[grepl("[[:punct:]]",substr(the_data$whole_word,nchar(the_data$whole_word),nchar(the_data$whole_word))),]$word_lengths-1



the_data <- the_data %>%
  
  filter (
    
    Letters != " ",                 #removes spaces (just in case they were assigned a letter position)
    
    !grepl("[[:punct:]]",Letters),  #removes punctuation
    
    !grepl("[0-9]",Letters),        #removes numbers
    
    !grepl("[[A-Z]]*",Letters),   #removes Letters that have a capital letter
    
    ParagraphType == "N",
    
    PredBigramCorrect == "11",
    
    IKSIs < 2000
    
  )



save(the_data,file='the_data.Rdata')



# Analysis

# Get the means by word length and letter position for each subject

# Use Van Selst and Jolicouer non-recursive_moving procedure from Crump



## @knitr typing_mean_iksis_aov  ------



load("the_data.Rdata")



# get subject means for each letter position and word length



subject_means <- the_data %>%
  
  group_by(Subject,word_lengths,let_pos) %>%
  
  summarize(mean_IKSI = mean(non_recursive_moving(IKSIs)$restricted))



#restrict to 1-9 positions and word lengths

subject_means <- subject_means[subject_means$let_pos < 10, ]

subject_means <- subject_means[subject_means$word_lengths < 10 &
                                 
                                 subject_means$word_lengths > 0, ]

subject_means2=mutate(subject_means, midword = ((word_lengths/2)== let_pos)| (as.integer(word_lengths/2)+1) == let_pos) 

subject_means2 <- subject_means2[subject_means2$word_lengths < 10 &
                                 
                                 subject_means2$word_lengths > 2, ]
subject_means2 <- subject_means2[subject_means2$let_pos >1, ]
subject_means2=mutate(subject_means2,midLetPos=word_lengths/2+0.5)
subject_means2=mutate(subject_means2,DistFromMid=abs(let_pos-midLetPos))
subject_meansAgg=aggregate(mean_IKSI~DistFromMid*word_lengths,subject_means2,mean)
midWordPlot <- ggplot(subject_meansAgg,aes(x=DistFromMid,y=mean_IKSI/word_lengths,color=word_lengths))+geom_point()
# get the correlation and the midWord Contribution                                    
  
cor.test(subject_meansAgg$DistFromMid,subject_meansAgg$mean_IKSI/subject_meansAgg$word_lengths)
 
  
  

subject_means2$Subject <- as.factor(subject_means2$Subject)

subject_means2$let_pos <- as.factor(subject_means2$let_pos)

subject_means2$word_lengths <- as.factor(subject_means2$word_lengths)

subject_means2$midword <- as.factor(subject_means2$midword)


aov(mean_IKSI ~  midword * word_lengths + Error(Subject/(midword * word_lengths)), subject_means2) 
# make sure numbers are factors

subject_means$Subject <- as.factor(subject_means$Subject)

subject_means$let_pos <- as.factor(subject_means$let_pos)

subject_means$word_lengths <- as.factor(subject_means$word_lengths)



#subject_means<-cbind(subject_means,H=rep(uncertainty_df$H,346))



# design is unbalanced so we create a single factor for a one-way ANOVA

position_length <- as.factor(paste0(subject_means$let_pos,subject_means$word_lengths))

subject_means <- cbind(subject_means, Pos_len =position_length)



# Run the ANOVA



#note very slow with aov and > than 50 subjects

#aov.out<-summary(aov(mean_IKSI ~ Pos_len + Error(Subject/Pos_len), subject_means[1:(45*10),]))



library(Rfast)

iksi_matrix <- matrix(subject_means$mean_IKSI,ncol=45,nrow=346,byrow=T)



rm.anova2<-function (y, logged = FALSE) 
  
{
  
  dm <- dim(y)
  
  d <- dim(y)[2]
  
  n <- dim(y)[1]
  
  ina <- rep(1:n, each = d)
  
  xi <- rep(1:d, n)
  
  yi <- rowmeans(y)
  
  yj <- colmeans(y)
  
  yt <- mean(yi)
  
  sst <- n * sum((yj - yt)^2)
  
  yi <- rep(yi, each = d)
  
  yj <- rep(yj, n)
  
  ssr <- sum((as.vector(t(y)) - yi - yj + yt)^2)
  
  dft <- d - 1
  
  dfs <- n - 1
  
  dfr <- dft * dfs
  
  mst <- sst/dft
  
  msr <- ssr/dfr
  
  stat <- mst/msr
  
  pvalue <- pf(stat, dft, dfr, lower.tail = FALSE, log.p = logged)
  
  list(f=stat, p=pvalue, mse=msr, df1=dft, df2=dfr)
  
}



Exp1_ANOVA <- rm.anova2(iksi_matrix)





## @knitr typing_mean_iksis_plot  -----

# Get the grand means by averaging over subject means

subject_means <- the_data %>%
  
  group_by(Subject,word_lengths,let_pos) %>%
  
  summarize(mean_IKSI = mean(non_recursive_moving(IKSIs)$restricted))



sum_data <- subject_means %>%
  
  group_by(word_lengths,let_pos) %>%
  
  summarize(mean_IKSIs = mean(mean_IKSI, na.rm = TRUE),
            
            SE = stde(mean_IKSI))



# plot the data



sum_data <- sum_data[sum_data$let_pos < 10, ]

sum_data <- sum_data[sum_data$word_lengths < 10 &
                       
                       sum_data$word_lengths > 0, ]



sum_data$let_pos<-as.factor(sum_data$let_pos)

sum_data$word_lengths<-as.factor(sum_data$word_lengths)



limits <- aes(ymax = mean_IKSIs + SE, ymin = mean_IKSIs - SE)



typing_plot1 <- ggplot(sum_data,aes(x=let_pos,
                                    
                                    y=mean_IKSIs,
                                    
                                    group=word_lengths,
                                    
                                    color=word_lengths
                                    
))+
  
  geom_line()+
  
  geom_point()+
  
  geom_errorbar(limits,width=.2)+
  
  theme_classic()+
  
  theme(legend.position="bottom")+
  
  xlab("Letter Position")+
  
  ylab("Mean Interkeystroke Interval (ms)")+
  
  scale_colour_grey()+
  
  labs(color='Word \n Length') +
  
  ggtitle("Mean IKSI by Position and Length")



## @knitr typing-mean-iksis-comparisons ------



# compute all t-tests

all_ts_mat <- matrix(0,ncol=45,nrow=45)

all_ps_mat <- matrix(0,ncol=45,nrow=45)

all_mdiffs <- matrix(0,ncol=45,nrow=45)



for( i in 1:45){
  
  for( j in 1:45){
    
    temp_t <- t.test(iksi_matrix[,i],iksi_matrix[,j],paired = T, var.equal = T)
    
    all_ts_mat[i,j] <- temp_t$statistic
    
    all_ps_mat[i,j] <- temp_t$p.value
    
    all_mdiffs[i,j] <- temp_t$estimate
    
  }
  
}



# 990 total comparisons



bonferonni_alpha <- .05/990

sig_tests <- all_ps_mat< bonferonni_alpha





all_mdiffs[lower.tri(all_mdiffs)] <- NA

diag(all_mdiffs)<-NA

all_mdiffs <- as.data.frame(all_mdiffs)

all_mdiffs$condition <- seq(1,45)

all_mdiffs <- na.omit(melt(all_mdiffs, 'condition', variable_name='means'))



sig_tests[lower.tri(sig_tests)] <- NA

sig_tests <- as.data.frame(sig_tests)

sig_tests$condition <- seq(1,45)

sig_tests <- na.omit(melt(sig_tests, 'condition', variable_name='means'))



all_mdiffs<-cbind(all_mdiffs,sig=as.numeric(sig_tests$value))



position<-c(1,1:2,1:3,1:4,1:5,1:6,1:7,1:8,1:9)

word_length<-c(1,rep(2,2),
               
               rep(3,3),
               
               rep(4,4),
               
               rep(5,5),
               
               rep(6,6),
               
               rep(7,7),
               
               rep(8,8),
               
               rep(9,9))



the_labels <- paste(position,word_length,sep="|")

levels(all_mdiffs$variable) <- the_labels

all_mdiffs$condition <- as.factor(all_mdiffs$condition)

levels(all_mdiffs$condition) <- the_labels





typing_plot2 <- ggplot(all_mdiffs, aes(condition, variable)) +
  
  ggtitle('Paired comparisons (light grey = significant)') +
  
  theme_classic(base_size = 6) +
  
  xlab('Condition') +
  
  ylab('Condition') +
  
  geom_tile(aes(fill = sig), color='white') +
  
  scale_fill_gradient(low = 'darkgrey', high = 'lightgrey', space = 'Lab') +
  
  theme(axis.text.x=element_text(angle=90),
        
        axis.ticks=element_blank(),
        
        axis.line=element_blank(),
        
        panel.border=element_blank(),
        
        panel.grid.major=element_line(color='#eeeeee'))+ 
  
  theme(legend.position="none")

#geom_text(aes(label=abs(round(value))),size=1.5)



ggarrange(typing_plot1, typing_plot2, 
          
          labels = c("A", "B"),
          
          ncol = 2, nrow = 1)







## @knitr letter_uncertainty  ------



letter_freqs <- fread("ngrams1.csv",integer64="numeric")

letter_freqs[letter_freqs==0]<-1



letter_probabilities <- apply(letter_freqs[,2:74],2,function(x){x/sum(x)})



letter_entropies <- apply(letter_probabilities,2,function(x){-1*sum(x*log2(x))})



position<-as.factor(c(1,1:2,1:3,1:4,1:5,1:6,1:7,1:8,1:9))

word_length<-as.factor(c(1,rep(2,2),
                         
                         rep(3,3),
                         
                         rep(4,4),
                         
                         rep(5,5),
                         
                         rep(6,6),
                         
                         rep(7,7),
                         
                         rep(8,8),
                         
                         rep(9,9)))



uncertainty_df<-data.frame(H=letter_entropies[11:(11+44)],position,word_length)



#plot



letter_uncertainty_plot1 <- ggplot(uncertainty_df,
                                   
                                   aes(x=position,
                                       
                                       y=H,
                                       
                                       group=word_length,
                                       
                                       color=word_length))+
  
  geom_line()+
  
  geom_point()+
  
  theme_classic(base_size = 10)+
  
  theme(plot.title = element_text(size = rel(1)))+
  
  theme(legend.position="bottom")+
  
  xlab("Letter Position")+
  
  ylab("Letter Uncertainty (H)")+
  
  labs(color='Word Length') +
  
  scale_colour_grey()+
  
  ggtitle("Letter Uncertainty by Position and Length")



## @knitr letter-uncertainty-by-IKSI  ------



sum_data<-cbind(sum_data,H=uncertainty_df$H)



letter_uncertainty_plot2 <- ggplot(sum_data,aes(x=H,
                                                
                                                y=mean_IKSIs))+
  
  geom_smooth(method="lm", color="black", size=.5, alpha=.2)+
  
  geom_text(aes(label=word_length), nudge_y=-8, size=2.5)+
  
  geom_point(aes(color=let_pos))+
  
  #geom_text(aes(x = 2.5, y = 240, label = lm_eqn(lm(mean_IKSIs ~ H, sum_data))), parse = TRUE)+
  
  theme_classic(base_size = 10)+
  
  theme(plot.title = element_text(size = rel(1)))+
  
  theme(legend.position="bottom")+
  
  xlab("Letter Uncertainty (H)")+
  
  ylab("Mean Interksytroke Interval (ms)")+
  
  labs(color='Letter \n Position') +
  
  scale_colour_grey()+
  
  ggtitle("Mean IKSI by Letter Uncertainty")







ggarrange(letter_uncertainty_plot1, letter_uncertainty_plot2, 
          
          labels = c("A", "B"),
          
          ncol = 2, nrow = 1)



lr_results<-summary(lm(mean_IKSIs~H, sum_data))





subject_means <- the_data %>%
  
  group_by(Subject,word_lengths,let_pos) %>%
  
  summarize(mean_IKSI = mean(non_recursive_moving(IKSIs)$restricted))



#restrict to 1-9 positions and word lengths

subject_means <- subject_means[subject_means$let_pos < 10, ]

subject_means <- subject_means[subject_means$word_lengths < 10 &
                                 
                                 subject_means$word_lengths > 0, ]



subject_means <- cbind(subject_means,H=rep(uncertainty_df$H,length(unique(subject_means$Subject))))



correlation_data <- subject_means %>%
  
  group_by(Subject) %>%
  
  summarize(pearson_r = cor(mean_IKSI,H),
            
            r_squared = cor(mean_IKSI,H)^2,
            
            p_value = cor.test(mean_IKSI,H)$p.value)



library(skimr)



skim_with(numeric=list(n=length,mean=mean,sd=sd,SE=stde),append=FALSE)

skim_out<-skim_to_list(correlation_data)



#Means

#p = skim_out$numeric$mean[1]

#r = skim_out$numeric$mean[2]

#r^2 = skim_out$numeric$mean[3]



#SE

#p = skim_out$numeric$SE[1]

#r = skim_out$numeric$SE[2]

#r^2 = skim_out$numeric$SE[3]



## @knitr letter_uncertainty_by_IKSI_dual  ------



categorical_position<-as.character(sum_data$let_pos)

categorical_position[categorical_position=="1"]<-"first"

categorical_position[categorical_position!="first"]<-"other"

categorical_position<-as.factor(categorical_position)

sum_data<-cbind(sum_data,cp=categorical_position)



lr_results_dual<-summary(lm(mean_IKSIs~cp+H, sum_data))



## @knitr letter-uncertainty-bigram ----



library(dplyr)

library(rlist)

library(ggplot2)

library(bit64)



# GET LETTER POSITION 1 H

# load in the excel file from Norvig:

letter_freqs <- fread("ngrams1.csv",integer64="numeric")

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



# GET LETTER POSITION > 1 H

# read in n-gram tsv and clean up

gram_2 <- read.table('2-gram.txt',header=TRUE,sep="\t")

colnames(gram_2)<- scan(file="2-gram.txt",what="text",nlines=1,sep="\t")



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



letter_entropies<-lapply(letter_probabilities,get_entropies)

letter_entropies<-list.rbind(letter_entropies)



# column means

means<-colMeans(letter_entropies)



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



uncertainty_df<-data.frame(H=means,let_pos,word_lengths)

uncertainty_df<-rbind(uncertainty_df,uncertainty_df_pos1)

#gram_2_test<-merge.data.frame(gram_2,letter_entropies,by.x=('n-1'),by.y=('n-1'))



uncertainty_df$let_pos<-as.factor(uncertainty_df$let_pos)

uncertainty_df$word_lengths<-as.factor(uncertainty_df$word_lengths)



uncertainty_df<-uncertainty_df[order(uncertainty_df$let_pos),]

uncertainty_df<-uncertainty_df[order(uncertainty_df$word_lengths),]



sum_data <- cbind(sum_data,H_bigram=uncertainty_df$H)



# plot



uncertainty_bigram_plot1 <- ggplot(sum_data,aes(x=position,
                                                
                                                y=H_bigram,
                                                
                                                group=word_length,
                                                
                                                color=word_length))+
  
  geom_line()+
  
  geom_point()+
  
  theme_classic(base_size = 10)+
  
  theme(plot.title = element_text(size = rel(1)))+
  
  theme(legend.position="bottom")+
  
  xlab("Letter Position")+
  
  ylab("Bigram Uncertainty (H)")+
  
  labs(color='Word Length') +
  
  scale_colour_grey()+
  
  ggtitle("Bigram Uncertainty by Position and Length")



# analysis



lr_results_bigram<-summary(lm(mean_IKSIs~H_bigram, sum_data))



uncertainty_bigram_plot2 <-ggplot(sum_data,aes(x=H_bigram,
                                               
                                               y=mean_IKSIs))+
  
  #geom_point(aes(color=let_pos))+
  
  geom_smooth(method="lm", color="black", size=.5, alpha=.2)+
  
  geom_text(aes(label=word_length), nudge_y=-8, size=2.5)+
  
  geom_point(aes(color=let_pos))+
  
  #geom_text(aes(x = 2.5, y = 240, label = lm_eqn(lm(mean_IKSIs ~ H, sum_data))), parse = TRUE)+
  
  theme_classic(base_size = 10)+
  
  theme(plot.title = element_text(size = rel(1)))+
  
  theme(legend.position="bottom")+
  
  xlab("Bigram Uncertainty (H)")+
  
  ylab("Mean Interkeystroke Interval (ms)")+
  
  labs(color='Letter \n Position') +
  
  scale_colour_grey()+
  
  ggtitle("Mean IKSIs by Bigram Uncertainty")



ggarrange(uncertainty_bigram_plot1, uncertainty_bigram_plot2, 
          
          labels = c("A", "B"),
          
          ncol = 2, nrow = 1)



## @knitr instance-model   ----



# Declare functions



# function to compute expected retrieval time given number of traces

# num_traces is the number of memory traces 

# monte_sim_number is the number of monte_carlo simulations to run

# rnorm_mean is mean of normal distribution of retrieval times

# rnorm_sd is standard deviation of normal distribution of retrieval times



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





## @knitr first_letter_slowing   ----



the_data <- fread("~/Desktop/mturk.txt")



first_letter_df <- the_data %>%
  
  filter(as.numeric(word_lengths)>1,
         
         as.numeric(word_lengths)<10,
         
         as.numeric(let_pos)<10)

first_vs_rest <- as.character(first_letter_df$let_pos)

first_vs_rest[first_vs_rest=="1"] <- "first"

first_vs_rest[first_vs_rest!="first"] <- "rest"



first_letter_df <- first_letter_df %>%
  
  mutate(first_letter=as.factor(first_vs_rest))



first_letter_means <- first_letter_df %>%
  
  group_by(Subject,word_lengths,first_letter) %>%
  
  summarize(mean_iksi=mean(non_recursive_moving(IKSIs)$restricted))



first_letter_means$Subject<-as.factor(first_letter_means$Subject)

first_letter_means$word_lengths<-as.factor(first_letter_means$word_lengths)





aov.out <- summary(aov(mean_iksi ~ first_letter + Error(Subject/first_letter),first_letter_means))

aov.out



aov.out <- summary(aov(mean_iksi ~ word_lengths*first_letter + Error(Subject/(word_lengths*first_letter)),first_letter_means))

aov.out