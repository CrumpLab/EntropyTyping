if(!exists("the_data")){load("data/the_data.Rdata")}


packages <- c("dplyr","ggplot2","cowplot")
lapply(packages, require, character.only = TRUE)

source("analysis/vj_outlier.R")


############## FIGURE 1: Letter x Word Length #####################
subject_means <- the_data %>%
    select(Subject,IKSIs,let_pos,word_lengths) %>%
    filter(let_pos%in%c(seq(1:9)),
           word_lengths%in%c(seq(1:9))
    ) %>%
    mutate(let_pos = factor(let_pos),
           word_lengths = factor(word_lengths)
    ) %>% 
    group_by(Subject,word_lengths,let_pos) %>%
    summarize(mean_IKSI = mean(non_recursive_moving(IKSIs)$restricted))

summary <-  subject_means %>%
    group_by(word_lengths,let_pos) %>%
    summarize(
        N = n(),
        mean = mean(mean_IKSI),
        median = median(mean_IKSI),
        sd = sd(mean_IKSI),
        se = sd/sqrt(N),
        ci = se*qt(.95/2+.5,N-1),
        ciUp = t.test(mean_IKSI)$conf.int[2],
        ciDown = t.test(mean_IKSI)$conf.int[1]
    )


figure1 <- ggplot(summary,aes(x=let_pos, y=mean, group = 1))+
    geom_errorbar(data = summary, aes(x = let_pos, ymin = mean-ci, ymax = mean+ci), colour = "gray20", width = .8, size = 0.3)+
    geom_line(size = .5)+
    geom_point(size = 1.3)+
    theme_classic()+
    xlab("Letter Position")+
    ylab("Mean IKSI (ms)")+
    scale_y_continuous(limits=c(120,282),breaks=c(120,160,200,240,280), expand = c(0.01,0.01)) + 
    theme(axis.text=element_text(size=7.5),
          axis.title=element_text(size=10,face="bold"),
          axis.line=element_blank(),
          axis.ticks=element_line(size = .4),
          panel.spacing = unit(.1,"lines"),
          #panel.grid.minor = element_line(colour="gray90", size=0.5),
          panel.grid.minor.x = element_blank(),
         # panel.grid.major = element_line(colour="gray90", size=0.5),
          panel.grid.major.x = element_blank(),
          panel.border = element_rect(fill = NA, colour="black")) +
    theme(legend.position="none")+
    facet_grid(.~word_lengths)+
    theme(strip.text.x = element_text(margin = margin(.1,0,.1,0, "cm"), angle =  0, size = 10),
          strip.placement = "inside",
          strip.background = element_blank())


figure1
ggsave("figures/1_figure.pdf", device = "pdf", dpi = 600,
       width = 6.75, height = 3, units = "in") 

