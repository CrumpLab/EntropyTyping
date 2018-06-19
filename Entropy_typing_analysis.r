# Analysis file for Entropy_typing project

## @knitr load_functions

library(data.table)
library(dplyr)
library(ggplot2)
library(Crump)  #for standard error function and Van Selst and Jolicouer outlier elimination


## @knitr load_pre_process

# mturk.txt is the unzipped mturk.txt.zip file
the_data <- fread("~/Desktop/mturk.txt")

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

#save(the_data,file='the_data.Rdata')

# Analysis
# Get the means by word length and letter position for each subject
# Use Van Selst and Jolicouer non-recursive_moving procedure from Crump

## @knitr typing_mean_iksis_aov

load("the_data.Rdata")

# get subject means for each letter position and word length

subject_means <- the_data %>%
  group_by(Subject,word_lengths,let_pos) %>%
  summarize(mean_IKSI = mean(non_recursive_moving(IKSIs)$restricted))

#restrict to 1-9 positions and word lengths
subject_means <- subject_means[subject_means$let_pos < 10, ]
subject_means <- subject_means[subject_means$word_lengths < 10 &
                       subject_means$word_lengths > 0, ]

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


## @knitr typing_mean_iksis_plot
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

ggplot(sum_data,aes(x=let_pos,y=mean_IKSIs,group=word_lengths,color=word_lengths))+
  geom_line()+
  geom_point()+
  geom_errorbar(limits,width=.2)+
  theme_classic()+
  ggtitle("Mean IKSI as a Function of Letter Position and Word Length")

## @knitr typing_mean_iksis_comparisons

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


ggplot(all_mdiffs, aes(condition, variable)) +
  ggtitle('Mean Absolute Differences') +
  theme_classic(base_size = 7) +
  xlab('Condition') +
  ylab('Condition') +
  geom_tile(aes(fill = sig), color='white') +
  scale_fill_gradient(low = 'darkgrey', high = 'lightgrey', space = 'Lab') +
  theme(axis.text.x=element_text(angle=90),
        axis.ticks=element_blank(),
        axis.line=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_line(color='#eeeeee'))+
  geom_text(aes(label=abs(round(value))),size=1.5)



## @knitr plot