packages <- c("dplyr","ggplot2","cowplot")
lapply(packages, require, character.only = TRUE)


load("data/bigram-simulation-data.Rda")

toPlot<-all_sims_df %>%
  # filter(amount_of_practice== 50 | amount_of_practice == 500) %>%
  mutate(amount_of_practice = factor(amount_of_practice))

#names(toPlot)[names(toPlot)=="amount_of_practice"]  <- "Amount of Practice (Keystrokes)"

figure5<-ggplot(toPlot,aes(x=position, y=mean_letter_retrieval_time, group = amount_of_practice))+
  #geom_errorbar(data = summary, aes(x = let_pos, ymin = mean-ci, ymax = mean+ci), colour = "gray20", width = .8, size = 0.3)+
  geom_line(size = .5)+
  #scale_linetype_manual(values=c("twodash","dotted","dashed","solid"))+
  geom_point(size = 1.3, aes(shape = amount_of_practice))+
  theme_classic()+
  labs(x="Letter Position", y = "Simulated IKSI (ms)", shape= "Amount of Practice (Keystrokes)")+
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
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))+
  facet_grid(.~word_length)+
  theme(strip.text.x = element_text(margin = margin(.1,0,.1,0, "cm"), angle =  0, size = 10),
        strip.placement = "inside",
        strip.background = element_blank())

figure5

ggsave("figures/5_figure.pdf", device = "pdf", dpi = 600,
       width = 6.875, height = 3.25, units = "in") 
