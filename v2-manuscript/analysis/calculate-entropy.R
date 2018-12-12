get_entropy <- function(){
  packages <- c("dplyr","skimr","rlist","bit64","data.table")
  lapply(packages, require, character.only = TRUE)
  
  if(!exists("the_data")){load("data/the_data.Rdata")}
  
  
  ########### GET UNIGRAM ENTROPY #############################
  ###summarize typing
  # Get the grand means by averaging over subject means
  subject_means <- the_data %>%
    group_by(Subject,word_lengths,let_pos) %>%
    summarize(mean_IKSI = mean(non_recursive_moving(IKSIs)$restricted))
  
  
  sum_data <- subject_means %>%
    group_by(word_lengths,let_pos) %>%
    summarize(mean_IKSIs = mean(mean_IKSI, na.rm = TRUE),
              SE = stde(mean_IKSI))
  
  sum_data <- sum_data[sum_data$let_pos < 10, ]
  sum_data <- sum_data[sum_data$word_lengths < 10 &
                         sum_data$word_lengths > 0, ]
  
  sum_data$let_pos<-as.factor(sum_data$let_pos)
  sum_data$word_lengths<-as.factor(sum_data$word_lengths)
  
  letter_freqs <- fread("data/ngrams1.csv",integer64="numeric")
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
  
  unc_DF<-uncertainty_df
  unc_DF$Condition<-"N"
  names(unc_DF)[names(unc_DF)=="position"]  <- "let_pos"
  names(unc_DF)[names(unc_DF)=="word_length"]  <- "word_lengths"
  
  sum_data<-cbind(sum_data,H=uncertainty_df$H)
  ###############################
  
  ### Linear regression iksi x unigram H
  lr_results<-summary(lm(mean_IKSIs~H, sum_data))
  
  ##################################
  
  #### SAVE UNIGRAM DATA ####
  summary_N <- sum_data
  
  
  
  ################# GET BIGRAM ENTROPY ##############
  # GET LETTER POSITION 1 ENTROPY
  # load in the excel file from Norvig:
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
  
  
  
  # GET LETTER POSITION > 1 ENTROPY
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
  
  
  uncertainty_df$Condition <- "N-1"
  uncertaintyDF<-rbind(unc_DF,uncertainty_df)
  
  sum_data <- cbind(sum_data,H_bigram=uncertainty_df$H)
  
  
  lr_results_bigram<-summary(lm(mean_IKSIs~H_bigram, sum_data))
  
  ### STORE BIGRAM DATA #########
  summary_N1<-sum_data
  
  
  ##### GET INDIVIDUAL SUBJECT CORRELATIONS ######
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
  
  skim_with(numeric=list(n=length,mean=mean,sd=sd,SE=stde),append=FALSE)
  lr_bySub<-skim_to_list(correlation_data)
  
  ##### GET LINEAR REGRESSION WITH CATEGORICAL FIRST POSITION #######
  categorical_position<-as.character(sum_data$let_pos)
  categorical_position[categorical_position=="1"]<-"first"
  categorical_position[categorical_position!="first"]<-"other"
  categorical_position<-as.factor(categorical_position)
  sum_data<-cbind(sum_data,cp=categorical_position)
  
  lr_results_dual<-summary(lm(mean_IKSIs~cp+H, sum_data))
  
  
  ############# STORE AND SAVE ALL RESULTS ############
  
  uncertainty_output<-list()
  uncertainty_output[[1]]<-uncertaintyDF
  uncertainty_output[[2]]<-lr_results
  uncertainty_output[[3]]<-lr_results_bigram
  uncertainty_output[[4]]<-summary_N
  uncertainty_output[[5]]<-summary_N1
  uncertainty_output[[6]]<-lr_bySub
  uncertainty_output[[7]]<-lr_results_dual
  
  names(uncertainty_output)<-c("uncertaintyDF","lr_results","lr_results_bigram","summary_N","summary_N1","lr_bySub","lr_results_dual")
  ## detach loaded packages
  
  return(uncertainty_output)
}    

entropy<-get_entropy()
  
save(entropy,file="data/entropy-data.Rda")
  