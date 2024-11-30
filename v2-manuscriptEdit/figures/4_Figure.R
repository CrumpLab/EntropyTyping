
packages <- c("dplyr","ggplot2","cowplot")
lapply(packages, require, character.only = TRUE)

#data source: analysis/calculate-entropy.R
load("data/entropy-data.Rda")


## get the data if needed
if(!exists("the_data")){load("the_data.Rdata")}


sumN<-entropy$summary_N
sumN1<-entropy$summary_N1
lr_results<-entropy$lr_results
lr_results_bigram<-entropy$lr_results_bigram

sumN<-sumN %>% select(word_lengths,let_pos,mean_IKSIs,H,SE)
sumN$Condition<-"Unigram"



sumN1<-sumN1 %>% select(word_lengths,let_pos,mean_IKSIs,H_bigram,SE)
names(sumN1)[names(sumN1)=="H_bigram"]  <- "H"
sumN1$Condition<-"Bigram"
summaryH<-rbind(sumN,sumN1)
levels(summaryH$Condition)<-c("Single Letter","Bigram (n-1)")
summaryH$Condition = factor(summaryH$Condition , levels = c("Single Letter","Bigram (n-1)"))



eq <- substitute(italic(target) == a + b %.% italic(input)*","~italic(r)^2~"="~r2*","~italic(p)~"="~pvalue, 
                 list(target = "IKSI",
                      input = "H",
                      a = format(as.vector(coef(lr_results)[1]), digits = 2), 
                      b = format(as.vector(coef(lr_results)[2]), digits = 2), 
                      r2 = format(lr_results$r.squared, digits = 3),
                      # getting the pvalue is painful
                      pvalue = format(lr_results$coefficients[2,'Pr(>|t|)'], digits=2)
                 )
)    


figure4A <- ggplot(sumN,aes(x=H,y=mean_IKSIs))+
  geom_point(size = 1.5, color = "gray40", stroke = .9, aes(shape = let_pos))+
  scale_shape_manual(values=c(0,1,2,5,4,15,16,17,18))+
  geom_smooth(method="lm", color = "black", size = 1.1)+
  labs(x = "Unigram (H)", y = "Mean IKSI (ms)")+
  scale_y_continuous(limits=c(80,300),breaks=c(80,100,120,140,160,180,200,220,240,260,280,300), expand = c(0.05,0.05)) + 
  theme(panel.grid.major = element_line(colour="gray80", size=0.2),
        axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold"),
        axis.line=element_blank()) +
  theme(strip.text.x = element_text(margin = margin(.1,0,.1,0, "cm")))+
  theme(legend.position="none")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = .75)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = .75)+
  draw_label(as.expression(eq),1.75,290, size =7, hjust = 0)

eq1 <- substitute(italic(target) == a + b %.% italic(input)*","~italic(r)^2~"="~r2*","~italic(p)~"="~pvalue, 
                  list(target = "IKSI",
                       input = "H",
                       a = format(as.vector(coef(lr_results_bigram)[1]), digits = 2), 
                       b = format(as.vector(coef(lr_results_bigram)[2]), digits = 2), 
                       r2 = format(lr_results_bigram$r.squared, digits = 3),
                       # getting the pvalue is painful
                       pvalue = format(lr_results_bigram$coefficients[2,'Pr(>|t|)'], digits=2)
                  )
)

figure4B <-ggplot(sumN1,aes(x=H,y=mean_IKSIs))+
  geom_point(size = 1.5, color = "gray40", stroke = .9, aes(shape = let_pos))+
  scale_shape_manual(values=c(0,1,2,5,4,15,16,17,18))+
  geom_smooth(method="lm", color = "black", size = 1.1)+
  labs(x = "Bigram (H)", y = "Mean IKSI (ms)")+
  scale_y_continuous(limits=c(80,300),breaks=c(80,100,120,140,160,180,200,220,240,260,280,300), expand = c(0.05,0.05)) + 
  theme(panel.grid.major = element_line(colour="gray80", size=0.2),
        axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold"),
        axis.line=element_blank()) +
  theme(strip.text.x = element_text(margin = margin(.1,0,.1,0, "cm")))+
  theme(legend.position="none")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = .75)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = .75)+
  draw_label(as.expression(eq1),1.65,290, size =7, hjust = 0)



temp <-plot_grid(figure4A, figure4B, labels = c('A', 'B'))


legend <- get_legend( 
  ggplot(sumN,aes(x=H,y=mean_IKSIs))+
    geom_point(size = 1.5, color = "gray40", stroke = .9, aes(shape = let_pos))+
    scale_shape_manual(values=c(0,1,2,5,4,15,16,17,18))+
    geom_smooth(method="lm", color = "black", size = 1.1)+
    labs(x = "Bigram", y = "Mean IKSI (ms)", shape = "Letter\nPosition")+
    scale_y_continuous(limits=c(80,300),breaks=c(80,100,120,140,160,180,200,220,240,260,280,300), expand = c(0.05,0.05)) + 
    theme(panel.grid.major = element_line(colour="gray80", size=0.2),
          axis.text=element_text(size=8),
          axis.title=element_text(size=12,face="bold"),
          axis.line=element_blank(),
          legend.background = element_rect(colour = "black", fill = "white"),
          legend.box.background = element_rect(colour = "black", size=.75),
          legend.margin = margin(t = 0.1, r = .25, b = 0, l = .25, unit = "cm"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)
    )+ 
    guides(shape = guide_legend(keywidth=.8, title.position = "top",   override.aes = list(size = 2)))
)

figure4<-plot_grid(temp, legend, rel_widths = c(2, .25))

figure4
ggsave("figures/4_figure.pdf", device = "pdf", dpi = 600,
      width = 6.875, height = 3.5, units = "in") 


