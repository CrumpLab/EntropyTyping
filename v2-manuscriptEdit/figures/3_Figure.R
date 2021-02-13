packages <- c("dplyr","ggplot2")
lapply(packages, require, character.only = TRUE)

#data source: analysis/calculate-entropy.R
load("data/entropy-data.Rda")
names(entropy$uncertaintyDF)[names(entropy$uncertaintyDF)=="Condition"]  <- "Uncertainty"

summary <- entropy$uncertaintyDF %>%
    mutate(Uncertainty = factor(Uncertainty))%>%
    group_by(Uncertainty,word_lengths,let_pos) %>%
    summarize(
        N = n(),
        mean = mean(H),
        median = median(H)
    )

levels(summary$Uncertainty)<-c("Unigram", "Bigram")



figure3 <-  ggplot(summary,aes(x=let_pos,y=mean,group=Uncertainty,shape=Uncertainty))+
    geom_line(size = .4)+
    geom_point(size = 1.2)+
    theme_classic()+
    xlab("Letter Position")+
    ylab("Uncertainty (H)")+
    scale_y_continuous(limits=c(1.5,4.5),breaks=c(2,3,4), expand = c(0.01,0.01)) + 
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
   # theme(legend.position="none")+
    facet_grid(.~word_lengths)+
    theme(strip.text.x = element_text(margin = margin(.1,0,.1,0, "cm"), angle =  0, size = 10),
          strip.placement = "inside",
          strip.background = element_blank())+
  theme(legend.position="bottom",
        legend.background = element_rect(colour = "black", fill = "white"),
        legend.box.background = element_rect(colour = "black", size=.75),
        legend.margin = margin(t = 0.1, r = .25, b = 0, l = .25, unit = "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_blank())
  

figure3

ggsave("figures/3_figure.pdf", device = "pdf", dpi = 600,
       width = 6.875, height = 3, units = "in") 


