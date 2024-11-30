load(file="simulation/bigram-simulation-data.Rda")
load(file="data/entropy-data.RDA")

all_sims_df= all_sims_df[ order(all_sims_df$amount_of_practice,
                                all_sims_df$word_length),]
head(entropy$summary_N1)
all_sims_df$IKSIemp= rep(entropy$summary_N1$mean_IKSIs,4)
cor(all_sims_df[1:45,]$mean_letter_retrieval_time,all_sims_df[1:45,]$IKSIemp)
library(ggplot2)
library(plyr)
library(cowplot)
library(dplyr)


eqEM=c()
lr_empirical_model=summary(lm(IKSIemp~mean_letter_retrieval_time, all_sims_df[1:45,]))
eqEM =c(eqEM, substitute(italic(target) == ~italic(r)^2~"="~r2*","~italic(p)~"="~pvalue, 
                         list(target = "Empirical",
                              input = "Model",
                              
                              r2 = format(lr_empirical_model$r.squared, digits = 3),
                              # getting the pvalue is painful
                              pvalue = format(lr_empirical_model$coefficients[2,'Pr(>|t|)'], digits=2)
                         )
)    )
lr_empirical_model=summary(lm(IKSIemp~mean_letter_retrieval_time, all_sims_df[46:90,]))
eqEM =c(eqEM, substitute(italic(target) == ~italic(r)^2~"="~r2*","~italic(p)~"="~pvalue, 
                         list(target = "Empirical",
                              input = "Model",
                              
                              r2 = format(lr_empirical_model$r.squared, digits = 3),
                              # getting the pvalue is painful
                              pvalue = format(lr_empirical_model$coefficients[2,'Pr(>|t|)'], digits=2)
                         )
)    )
lr_empirical_model=summary(lm(IKSIemp~mean_letter_retrieval_time, all_sims_df[91:135,]))
eqEM = c(eqEM,substitute(italic(target) == ~italic(r)^2~"="~r2*","~italic(p)~"="~pvalue, 
                         list(target = "Empirical",
                              input = "Model",
                              
                              r2 = format(lr_empirical_model$r.squared, digits = 3),
                              # getting the pvalue is painful
                              pvalue = format(lr_empirical_model$coefficients[2,'Pr(>|t|)'], digits=2)
                         )
)  )  
lr_empirical_model=summary(lm(IKSIemp~mean_letter_retrieval_time, all_sims_df[136:180,]))
eqEM =c(eqEM, substitute(italic(target) == ~italic(r)^2~"="~r2*","~italic(p)~"="~pvalue, 
                         list(target = "Empirical",
                              input = "Model",
                              
                              r2 = format(lr_empirical_model$r.squared, digits = 3),
                              # getting the pvalue is painful
                              pvalue = format(lr_empirical_model$coefficients[2,'Pr(>|t|)'], digits=2)
                         )
)   ) 

all_sims_df = all_sims_df %>%
  mutate(first=position==1)
#cors <- ddply(dat, .(grp1, grp2), summarise, cor = round(cor(val1, val2), 2))

r2= function(x) {
  lr_empirical_model=summary(lm(IKSIemp~mean_letter_retrieval_time, x))
  r2 = format(lr_empirical_model$r.squared, digits = 3)
  return(r2)
}

pValue = function(x){
  lr_empirical_model=summary(lm(IKSIemp~mean_letter_retrieval_time, x))
  pvalue = format(lr_empirical_model$coefficients[2,'Pr(>|t|)'], digits=2)
  return(pValue)
}

pValue=vector(length=4)
lr_empirical_model=summary(lm(IKSIemp~mean_letter_retrieval_time, all_sims_df[1:45,]))
pvalue[1] = format(lr_empirical_model$coefficients[2,'Pr(>|t|)'], digits=2)
lr_empirical_model=summary(lm(IKSIemp~mean_letter_retrieval_time, all_sims_df[46:90,]))
pvalue[2] = format(lr_empirical_model$coefficients[2,'Pr(>|t|)'], digits=2)
lr_empirical_model=summary(lm(IKSIemp~mean_letter_retrieval_time, all_sims_df[91:135,]))
pvalue[3] = format(lr_empirical_model$coefficients[2,'Pr(>|t|)'], digits=2)
lr_empirical_model=summary(lm(IKSIemp~mean_letter_retrieval_time, all_sims_df[136:180,]))
pvalue[4] = format(lr_empirical_model$coefficients[2,'Pr(>|t|)'], digits=2)

pvalue=pvalue[-5]
cors = all_sims_df %>%
        group_by(amount_of_practice) %>%
        summarise(r2=r2(data.frame(mean_letter_retrieval_time,IKSIemp)))
cors$p=pvalue
all_sims_df$r2=rep(cors$r2,each=45)
all_sims_df$p=rep(cors$p,each=45)
figure6=
  ggplot(all_sims_df,aes(x=mean_letter_retrieval_time,y=IKSIemp))+
  geom_point(alpha=0.3)+
  geom_smooth(method="lm")+
  facet_grid(amount_of_practice~.)+
  xlab("Model IKSI (ms)")+
  ylab("Empirical IKSI (ms)")+
  geom_text(data=cors,aes(label=paste("r=", r2, "p=",p ,sep=" ")),x=200,y=240)

figure6
ggsave("6_figure.pdf", plot = last_plot(), device = "pdf", dpi = 600,
       width = 6.875, height = 3.5, units = "in") 

  
  