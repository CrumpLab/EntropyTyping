library(dplyr)
source('analysis/vj_outlier.R')
library(data.table)
library(ggplot2)
library(cowplot)

packages <- c("dplyr","skimr","rlist","bit64","data.table")
lapply(packages, require, character.only = TRUE)

if(!exists("the_data")){load("data/the_data.Rdata")}

########### GET SUMDATA TO MATCH IKSI TO LETPOS & LENGTH #####################
###summarize typing
# Get the grand means by averaging over subject means
subject_means <- the_data %>%
  group_by(Subject,word_lengths,let_pos) %>%
  summarize(mean_IKSI = mean(non_recursive_moving(IKSIs)$restricted))
#group_by also selects columns/variables

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
# H value for each of the letter positions
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
# sum_data holds the let pos, length & IKSIs from the data
# we will add different H values to each IKSI with each different H calculation method
sum_data<-cbind(sum_data,H=uncertainty_df$H)

##################### GET ONLY POSITION 1 PROBABILITY AND ENTROPY###########
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
###################### GET BIGRAM ENTROPY
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
#binding the vectors of missing bigrams to matrix with bigram positions
#-1 because the column for bigram names
missing<-cbind(missing,matrix(0,nrow = length(missing), ncol = ncol(gram_2)-1))
#colnames is bigram positions
#2/1:2, 3/1:2, ...
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
# n-1 Letter Probabilities calculated here
letter_probabilities<-(with(gram_2,
                            by(gram_2[,4:39],gram_2[,'n-1'], get_prob,simplify= TRUE)
))
library(rlist)
letter_entropies<-lapply(letter_probabilities,get_entropies)
letter_entropies<-list.rbind(letter_entropies)

# rename columns by word length and First Letter Position of Bigram
# in order to weight H values by n-1 Letter's Probability
lengthPosn <- data.frame(do.call('rbind', strsplit(
  as.character(colnames(letter_entropies)),
  '/',fixed=TRUE)))

bigramPosn=do.call('rbind', strsplit(as.character(lengthPosn$X2),':',fixed=TRUE))
lengthPosn=lengthPosn %>%
  mutate(X2=bigramPosn[,1])

lengthPosn=lengthPosn %>%
  mutate(X3=paste(X1,X2,sep='/'))
# make column names for letter entropies match letter probabilities chart (unigram)
colnames(letter_entropies)=lengthPosn$X3

#alphabetize so when we multiply probabilities by H, 
letter_freqs=arrange(letter_freqs,gram)

#Get each letter's probability at each letter position, to weight the Hs at each position
uni_letter_probs<-get_prob(letter_freqs[,2:74])

weighted_entropies=data.frame(allLet)

for(i in 1:ncol(letter_entropies))
{#match each position's H in entropy df with letter 1's probability being n-1 position 
  positionMatch=which(grepl(colnames(letter_entropies)[i],colnames(uni_letter_probs)))
  if(length(positionMatch)==0)
    next 
  
  weighted_entropies=cbind(weighted_entropies,letter_entropies[,i]*uni_letter_probs[,positionMatch])
}

colnames(weighted_entropies)[2:37]=lengthPosn$X3
#contains the weighted Entropies
weighted_means=colSums(weighted_entropies[,2:37])


# match H values with Bigram's 2nd Letter's Letter Position & Word length
let_pos<-c(2:2,2:3,2:4,2:5,2:6,2:7,2:8,2:9)
word_lengths<-c(rep(2,1),
                rep(3,2),
                rep(4,3),
                rep(5,4),
                rep(6,5),
                rep(7,6),
                rep(8,7),
                rep(9,8))

uncertainty_df<-data.frame(H=weighted_means,let_pos,word_lengths)
uncertainty_df<-rbind(uncertainty_df,uncertainty_df_pos1)
#gram_2_test<-merge.data.frame(gram_2,letter_entropies,by.x=('n-1'),by.y=('n-1'))

uncertainty_df$let_pos<-as.factor(uncertainty_df$let_pos)
uncertainty_df$word_lengths<-as.factor(uncertainty_df$word_lengths)
# sort the let pos and lengths so that they correspond to the sum_data & can be cbinded
uncertainty_df<-uncertainty_df[order(uncertainty_df$let_pos),]
uncertainty_df<-uncertainty_df[order(uncertainty_df$word_lengths),]


uncertainty_df$Condition <- "WeightN-1"
uncertaintyDF<-rbind(unc_DF,uncertainty_df)

sum_data2 <- cbind(sum_data,H_bigram_weight=uncertainty_df$H)


lr_results_bigram_wt<-summary(lm(mean_IKSIs~H_bigram_weight, sum_data2))

### STORE BIGRAM DATA #########
summary_N1_weight<-sum_data2

####GRAPH IT


eq1W <- substitute(italic(target) == a + b %.% italic(input)*","~italic(r)^2~"="~r2*","~italic(p)~"="~pvalue, 
                   list(target = "IKSI",
                        input = "H",
                        a = format(as.vector(coef(lr_results_bigram_wt)[1]), digits = 2), 
                        b = format(as.vector(coef(lr_results_bigram_wt)[2]), digits = 2), 
                        r2 = format(lr_results_bigram_wt$r.squared, digits = 3),
                        # getting the pvalue is painful
                        pvalue = format(lr_results_bigram_wt$coefficients[2,'Pr(>|t|)'], digits=2)
                   )
)

figure4C <-ggplot(summary_N1_weight,aes(x=H_bigram_weight,y=mean_IKSIs))+
  geom_point(size = 1.5, color = "gray40", stroke = .9, aes(shape = let_pos))+
  scale_shape_manual(values=c(0,1,2,5,4,15,16,17,18))+
  geom_smooth(method="lm", color = "black", size = 1.1)+
  labs(x = "Wtd Bigram (H)", y = "Mean IKSI (ms)")+
  scale_y_continuous(limits=c(80,300),breaks=c(80,100,120,140,160,180,200,220,240,260,280,300), expand = c(0.05,0.05)) + 
  theme(panel.grid.major = element_line(colour="gray80", size=0.2),
        axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold"),
        axis.line=element_blank()) +
  theme(strip.text.x = element_text(margin = margin(.1,0,.1,0, "cm")))+
  theme(legend.position="none")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = .75)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = .75)+
  draw_label(as.expression(eq1W),1.65,290, size =7, hjust = 0)

figure4C