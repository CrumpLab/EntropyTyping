
if(!exists("the_data")){load("data/the_data.Rdata")}

packages <- c("dplyr","ggplot2","cowplot")
lapply(packages, require, character.only = TRUE)

source("analysis/vj_outlier.R")

########### First-letter and Mid-word Slowing #################

create_letter_bins <- function(the_data, type){
  new <- the_data %>%
    select(Subject,IKSIs,let_pos,word_lengths) %>%
    filter(let_pos%in%c(seq(2:9)),
           word_lengths%in%c(seq(from = 5, to = 9, by = 1))
    ) 
  
  new$new_pos <- NA
  if(type == "avg"){
    ## create bins (average all mid-letters)
    
    new[new$let_pos == 1, ]$new_pos <- "first"
    new[new$let_pos == 2, ]$new_pos <- "second"
    new[new$word_lengths == new$let_pos, ]$new_pos <- "last"
    
    for (i in 4:max(new$word_lengths)){
      new[new$word_lengths == i & new$let_pos < i  & new$let_pos > 2, ]$new_pos <- "middle"
    }
    
    new$new_pos <- as.factor(new$new_pos)
    new$word_lengths <-as.factor(new$word_lengths)
    
    #new$bin_type <- "averaged"
    ######
  }
  
  
  if(type == "peak"){
    ##### Second bins (peak mid)
    temp<-new
    temp$new_pos <- NA
    temp[temp$let_pos == 2, ]$new_pos <- "second"
    temp[temp$word_lengths == temp$let_pos, ]$new_pos <- "last"
    
    temp[temp$word_lengths == 5 & temp$let_pos == 4,]$new_pos <- "middle"
    temp[temp$word_lengths == 6 & temp$let_pos == 4,]$new_pos <- "middle"
    temp[temp$word_lengths == 7 & temp$let_pos == 4,]$new_pos <- "middle"
    temp[temp$word_lengths == 8 & temp$let_pos == 5,]$new_pos <- "middle"
    temp[temp$word_lengths == 9 & temp$let_pos == 5,]$new_pos <- "middle"
    
    #replace w/ alternate
    new<-temp
    new<-new %>% filter (!is.na(new_pos))
  }
  
  
  
  new$new_pos <- factor(new$new_pos, levels = c("second", "middle","last"))
  new <- new %>%
    mutate(
      Subject = factor(Subject),
      new_pos = factor(new_pos),
      let_pos = factor(let_pos),
      word_lengths = factor(word_lengths)
    )
  
  return(new)
}



############ MID WORD SLOWING ###################
data<-create_letter_bins(the_data,type="peak")

###### find summary table including word lengths as factor
subject_means <- data  %>%
  group_by(Subject,word_lengths,new_pos) %>%
  summarize(mean_IKSI = mean(non_recursive_moving(IKSIs)$restricted)) 

## summarize with word groups
summary_wWords <- subject_means %>%
  group_by(word_lengths,new_pos) %>%
  summarize(
    N = n(),
    mean = mean(mean_IKSI),
    median = median(mean_IKSI),
    sd = sd(mean_IKSI),
    se = sd/sqrt(N),
    ci = se*qt(.95/2 + .5,N-1)
  )
######



###### Summarize without word groups
subject_means <- new %>%
  group_by(Subject, word_lengths, new_pos) %>%
  summarize(temp_IKSI = mean(non_recursive_moving(IKSIs)$restricted)) %>%
  group_by(Subject, new_pos) %>%
  summarize(mean_IKSI = mean(temp_IKSI))

# make sure numbers are factors
subject_means$Subject <- as.factor(subject_means$Subject)
subject_means$new_pos <- as.factor(subject_means$new_pos)

summary_noWords <- subject_means %>%
  group_by(new_pos) %>%
  summarize(
    N = n(),
    mean = mean(mean_IKSI),
    median = median(mean_IKSI),
    sd = sd(mean_IKSI),
    se = sd/sqrt(N),
    ci = se*qt(.95/2 + .5,N-1)
  )



### GRAPH

## fix names and level titles
levels(summary_noWords$new_pos) <- c("Second","Middle","Last")
levels(summary_wWords$new_pos) <- c("Second","Middle","Last")



set.seed(52134) ## reproducible jitter
midWordSlowing_peak <- ggplot(summary_noWords,aes(x=new_pos,y=mean, group = 1))+
  geom_line(linetype = 2, size = 0.9)+
  geom_jitter(data = summary_wWords, aes(group = word_lengths, shape = word_lengths), size = 1.8, color = "gray70", position =   position_jitter(width = 0.2), stroke = .75)+
  geom_point( colour = "white", size = 3)+
  geom_errorbar(data = summary_noWords, aes(x = new_pos, ymin = mean-ci, ymax = mean+ci), colour = "BLACK", width = 0.3, size = 0.9)+
  geom_point( colour = "black", size = 2.5)+
  #scale_shape_manual(values=c(0,1,2,5,4,15,16,17,18))+  theme_classic()+
  scale_shape_manual(values=c(4,15,16,17,18))+  
  theme_classic()+
  labs(title="Mid-Word Slowing",x="Letter Position", y="Mean IKSI (ms)", shape="Word Lengths") +
  scale_y_continuous(limits=c(120,280),breaks=c(120,140,160,180,200,220,240,260,280), expand = c(0.05,0.05)) + 
  theme(panel.grid.major = element_line(colour="gray80", size=0.2),
        axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold"),
        axis.line=element_blank()) +
  theme(legend.position=c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.direction = "horizontal",
        legend.background = element_rect(colour = "black", fill = "white"),
        legend.box.background = element_rect(colour = "black"),
        legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)
  )+ 
  guides(shape = guide_legend(keywidth=.8,
                              nrow =2,
                              byrow = TRUE, 
                              title.position = "top",   
                              override.aes = list(size = 2))
         ) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = .75)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = .75)


midWordSlowing_peak
#ggsave("iksi_midWord.pdf", device = "pdf", dpi = 600,
#       width = 3.3, height = 3.3, units = "in") 




########### First-letter and Mid-word Slowing #################

new <- the_data %>%
  select(Subject,IKSIs,let_pos,word_lengths) %>%
  filter(let_pos%in%c(seq(1:2)),
         word_lengths%in%c(seq(from = 2, to = 9, by = 1))
  ) 


levels(new$let_pos)<-c("First", "Second")
new <- new %>%
  mutate(
    Subject = factor(Subject),
    let_pos = factor(let_pos),
    word_lengths = factor(word_lengths)
  )

###### find summary table including word lengths as factor
subject_means <- new  %>%
  group_by(Subject,word_lengths,let_pos) %>%
  summarize(mean_IKSI = mean(non_recursive_moving(IKSIs)$restricted)) 

## summarize with word groups
summary_wWords <- subject_means %>%
  group_by(word_lengths,let_pos) %>%
  summarize(
    N = n(),
    mean = mean(mean_IKSI),
    median = median(mean_IKSI),
    sd = sd(mean_IKSI),
    se = sd/sqrt(N),
    ci = se*qt(.95/2 + .5,N-1)
  )
######



###### Summarize without word groups
subject_means <- new %>%
  group_by(Subject, word_lengths, let_pos) %>%
  summarize(temp_IKSI = mean(non_recursive_moving(IKSIs)$restricted)) %>%
  group_by(Subject, let_pos) %>%
  summarize(mean_IKSI = mean(temp_IKSI))

# make sure numbers are factors
subject_means$Subject <- as.factor(subject_means$Subject)
subject_means$let_pos <- as.factor(subject_means$let_pos)

summary_noWords <- subject_means %>%
  group_by(let_pos) %>%
  summarize(
    N = n(),
    mean = mean(mean_IKSI),
    median = median(mean_IKSI),
    sd = sd(mean_IKSI),
    se = sd/sqrt(N),
    ci = se*qt(.95/2 + .5,N-1)
  )



### GRAPH

## fix names and level titles
#names(summary_wWords)[names(summary_wWords)=="word_lengths"]  <- "Word Lengths"
levels(summary_noWords$let_pos) <- c("First", "Second")
levels(summary_wWords$let_pos) <- c("First", "Second")



set.seed(52134) ## reproducible jitter
firstLetSlowing <- ggplot(summary_noWords,aes(x=let_pos,y=mean, group = 1))+
  geom_line(linetype = 2, size = 0.9)+
  geom_jitter(data = summary_wWords, aes(group = word_lengths, shape = word_lengths), size = 1.8, color = "gray70", position =   position_jitter(width = 0.2), stroke = .75)+
  geom_point(colour = "white", size = 3)+
  geom_errorbar(data = summary_noWords, aes(x = let_pos, ymin = mean-ci, ymax = mean+ci), colour = "BLACK", width = 0.3, size = 0.9)+
  geom_point(colour = "black", size = 2.5)+
  #scale_shape_manual(values=c(0,1,2,5,4,15,16,17,18))+  theme_classic()+
  scale_shape_manual(values=c(1,2,5,4,15,16,17,18))+  
  theme_classic()+
  labs(title="First Letter Slowing",x="Letter Position", y="Mean IKSI (ms)", shape="Word Lengths") +
  scale_y_continuous(limits=c(120,280),breaks=c(120,140,160,180,200,220,240,260,280), expand = c(0.05,0.05)) + 
  theme(panel.grid.major = element_line(colour="gray80", size=0.2),
        axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold"),
        axis.line=element_blank()) +
  theme(legend.position=c(1, 1),
        legend.justification = c("right", "top"),
        legend.direction = "horizontal",
        legend.background = element_rect(colour = "black", fill = "white"),
        legend.box.background = element_rect(colour = "black"),
        legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)
  )+ 
  guides(shape = guide_legend(keywidth=.8,
                              ncol = 2,
                              bycol = TRUE, 
                              title.position = "top",   
                              override.aes = list(size = 2))
         )+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = .75)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = .75)


firstLetSlowing

########## ARRANGE #########
figure2<-plot_grid(firstLetSlowing,NULL,midWordSlowing_peak, 
          nrow = 1, 
          rel_widths = c(.9, 0.05, 1),
          labels = c("A", "", "B"))

figure2
ggsave("figures/2_Figure_collapsed.pdf", device = "pdf", dpi = 600,
      width = 5, height = 3.5, units = "in") 


