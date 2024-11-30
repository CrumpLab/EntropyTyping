if(!exists("the_data")){load("data/the_data.Rdata")}

packages <- c("tidyr","dplyr","ggplot2","cowplot")
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




MWS_summary <- data  %>%
  group_by(Subject,word_lengths,new_pos) %>%
  summarize(mean_IKSI = mean(non_recursive_moving(IKSIs)$restricted)) %>%
  filter(new_pos != "last") %>%
  spread(new_pos,mean_IKSI) %>%
  mutate(diff = middle - second) %>%
  select(Subject, word_lengths, diff) %>%
  group_by(word_lengths) %>%
  summarize(
    N = n_distinct(Subject),
    mean = mean(diff),
    median = median(diff),
    sd = sd(diff),
    se = sd/sqrt(N),
    ci = se*qt(.95/2 + .5,N-1)
  )


MWS_graph <- ggplot(MWS_summary,aes(x=word_lengths,y=mean, group = 1))+
  geom_line(linetype = 1, size = 0.75)+
  geom_point( colour = "white", size = 3)+
  geom_errorbar(data = MWS_summary, aes(x = word_lengths, ymin = mean-ci, ymax = mean+ci), colour = "BLACK", width = 0.3, size = 0.9)+
  geom_point( colour = "black", size = 2.5)+
  theme_classic()+
  labs(title = "Mid-Word Slowing", y="IKSI Difference (Middle - 2nd)", x = "Word Length") +
  scale_y_continuous(limits=c(0,40),breaks=c(0,10,20,30,40), expand = c(0,0.05)) + 
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
  ) + 
  guides(shape = guide_legend(keywidth=.8,nrow =2,byrow = TRUE, title.position = "top",   override.aes = list(size = 2))) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = .75)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = .75)


####### FIRST LETTER SLOWING #########

FLS_summary <- the_data %>%
  select(Subject,IKSIs,let_pos,word_lengths) %>%
  filter(let_pos%in%c(seq(1:2)),
         word_lengths%in%c(seq(from = 2, to = 9, by = 1))
  )  %>%
  mutate(
    Subject = factor(Subject),
    let_pos = factor(let_pos),
    word_lengths = factor(word_lengths)
  )  %>%
  group_by(Subject,word_lengths,let_pos) %>%
  summarize(mean_IKSI = mean(non_recursive_moving(IKSIs)$restricted)) %>%
  spread(let_pos, mean_IKSI) %>%
  mutate(diff = `1` - `2`) %>%
  select(Subject, word_lengths, diff) %>%
  group_by(word_lengths) %>%
  summarize(
    N = n(),
    mean = mean(diff),
    median = median(diff),
    sd = sd(diff),
    se = sd/sqrt(N),
    ci = se*qt(.95/2 + .5,N-1)
  )
######


FLS_graph <- ggplot(FLS_summary,aes(x= word_lengths,y=mean, group = 1))+
  geom_line(linetype = 1, size = 0.75)+
  geom_point(colour = "white", size = 3)+
  geom_errorbar(data = FLS_summary, aes(x = word_lengths, ymin = mean-ci, ymax = mean+ci), colour = "BLACK", width = 0.3, size = 0.9)+
  geom_point( colour = "black", size = 2.5)+
  theme_classic()+
  labs(title = "First-Letter Slowing", y="IKSI Difference (1st - 2nd)", x = "Word Length") +
  scale_y_continuous(limits=c(40,120),breaks=c(40,50,60,70,80,90,100,110,120), expand = c(0,0.05)) + 
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
  ) + 
  guides(shape = guide_legend(keywidth=.8,nrow =2,byrow = TRUE, title.position = "top",   override.aes = list(size = 2))) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = .75)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = .75)

FLS_graph


figure2<-plot_grid(FLS_graph,NULL,MWS_graph, 
                   nrow = 1, 
                   rel_widths = c(1.1, 0.05, .9),
                   labels = c("A", "", "B"))

figure2
ggsave("figures/2_Figure_difference.pdf", device = "pdf", dpi = 600,
       width = 5, height = 3.5, units = "in") 
