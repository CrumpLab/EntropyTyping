packages <- c("dplyr","ggplot2","cowplot")
lapply(packages, require, character.only = TRUE)


load("data/bigram-simulation-data.Rda")

bigram_uncertainty <- entropy$uncertaintyDF[entropy$uncertaintyDF$Condition=="N-1",]



toPlot<-all_sims_df %>%
  # filter(amount_of_practice== 50 | amount_of_practice == 500) %>%
  mutate(amount_of_practice = factor(amount_of_practice))

bits <- c()
for(i in 1:dim(toPlot)[1]){

  bits[i] <- bigram_uncertainty[bigram_uncertainty$let_pos == toPlot[i,"position"] &
                                     bigram_uncertainty$word_lengths == toPlot[i,"word_length"],]$H
}

toPlot <- cbind(toPlot, bits)

#names(toPlot)[names(toPlot)=="amount_of_practice"]  <- "Amount of Practice (Keystrokes)"

source("figures/ggplot_regressioneqn.R")

figureInstanceBits <- ggplot(toPlot, aes(x=bits,y=mean_letter_retrieval_time, color=position, group=NA))+
  stat_smooth_func(geom="text",method="lm",hjust=0,
                   parse=TRUE,
                   size=2.5, show.legend = FALSE) +
  geom_smooth(method="lm",se=FALSE, color="black", size=.5)+
  geom_point()+
  scale_colour_grey(end=.8)+
  labs(color="Position")+
  theme_classic()+
  xlab("Bits (H)")+
  ylab("Simulated IKSI (ms)")+
  facet_wrap(~amount_of_practice, scales="free_y")

figureInstanceBits

ggsave("figures/5_figure_bits.pdf", device = "pdf", dpi = 600,
       width = 6.875, height = 3.25, units = "in") 
