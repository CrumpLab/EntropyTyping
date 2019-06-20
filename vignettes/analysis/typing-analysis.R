if(!exists("the_data")){load("data/the_data.Rdata")}
packages <- c("dplyr","ggplot2","cowplot","afex","papaja","tidyr")
lapply(packages, require, character.only = TRUE)


source("vignettes/analysis/vj_outlier.R")

get_fstat<-function(anova_table){
  output<-c()
  for(i in 1:nrow(anova_table)){
    output[i]<-paste("$F(",anova_table$`num Df`[i],", ",anova_table$`den Df`[i],") =  ", round(anova_table$`F`[i],digits=2),
                     "$, $\\mathit{MSE} = ",round(anova_table$MSE[i], digits = 2),
                     "$, $p = ", format(anova_table$`Pr(>F)`[i] , digits = 3, scientific = FALSE) ,
                     "$, $\\hat{\\eta}^2_\\textit{p} = ", format(anova_table$pes[i], digits = 2) ,"$",
                     sep = ""
    )
  }
  return(output)
}

############## First Letter slowing #####################
subject_means <- the_data %>%
  select(Subject,IKSIs,let_pos,word_lengths)  %>%
  mutate(let_pos = factor(let_pos),
         word_lengths = factor(word_lengths),
         Subject = factor(Subject)
  ) %>%
  group_by(Subject,word_lengths,let_pos) %>%
  summarise(mean_IKSI = mean(non_recursive_moving(IKSIs)$restricted)) %>%
  filter(let_pos%in%c(seq(1:2)),
         word_lengths%in%c(seq(from=2,to=9,by=1))
  ) %>%
  ungroup() %>%
  mutate(let_pos = factor(let_pos),
         word_lengths = factor(word_lengths))


first <- aov_car(mean_IKSI ~ let_pos*word_lengths + Error(Subject/let_pos*word_lengths), data = subject_means, anova_table = list(correction = "none", es = "pes"))
#sum_first<-nice(first, correction = "none", es="pes")


FLS<-get_fstat(first$anova_table)

FLS_summary <- the_data %>%
  select(Subject,IKSIs,let_pos,word_lengths)  %>%
  mutate(let_pos = factor(let_pos),
         word_lengths = factor(word_lengths),
         Subject = factor(Subject)
  ) %>%
  group_by(Subject,word_lengths,let_pos) %>%
  summarise(mean_IKSI = mean(non_recursive_moving(IKSIs)$restricted)) %>%
  filter(let_pos%in%c(seq(1:2)),
         word_lengths%in%c(seq(from=2,to=9,by=1))
  )  %>%
  #spread(let_pos,mean_IKSI) %>%
  #mutate(diff = `1` - `2`) %>%
  #select(Subject,word_lengths,diff) %>%
  group_by(Subject, let_pos) %>%
  summarise(mean_IKSI = mean(mean_IKSI)) %>%
  group_by(let_pos) %>%
  summarise(
    N = n(),
    mean = mean(mean_IKSI ),
    sd = sd(mean_IKSI ),
    se = sd/sqrt(N),
    ci = se*qt(.95/2 + .5,N-1)
  )


############## Mid Word Slowing #####################

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

subject_means <- data %>%
  select(Subject,IKSIs,new_pos,word_lengths)  %>%
  mutate(let_pos = factor(new_pos),
         word_lengths = factor(word_lengths),
         Subject = factor(Subject)
  ) %>%
  group_by(Subject,word_lengths,new_pos) %>%
  summarise(mean_IKSI = mean(non_recursive_moving(IKSIs)$restricted)) %>%
  filter(new_pos != "last") %>%
  select(Subject,word_lengths,new_pos,mean_IKSI) %>%
  mutate(new_pos = factor(new_pos))


mid <- aov_car(mean_IKSI ~ new_pos*word_lengths + Error(Subject/new_pos*word_lengths), data = subject_means, anova_table = list(correction = "none", es = "pes"))
#sum_first<-nice(first, correction = "none", es="pes")

MWS<-get_fstat(mid$anova_table)

MWS_summary <- data %>%
  select(Subject,IKSIs,new_pos,word_lengths)  %>%
  mutate(let_pos = factor(new_pos),
         word_lengths = factor(word_lengths),
         Subject = factor(Subject)
  ) %>%
  group_by(Subject,word_lengths,new_pos) %>%
  summarise(mean_IKSI = mean(non_recursive_moving(IKSIs)$restricted)) %>%
  filter(new_pos != "last") %>%
  select(Subject,word_lengths,new_pos,mean_IKSI) %>%
  mutate(new_pos = factor(new_pos)) %>%
  #spread(new_pos,mean_IKSI) %>%
  #mutate(diff = `middle` - `second`) %>%
  #select(Subject,word_lengths,diff) %>%
  group_by(Subject, new_pos) %>%
  summarise(mean_IKSI = mean(mean_IKSI)) %>%
  group_by(new_pos) %>%
  #ungroup() %>%
  summarise(
    N = n(),
    mean = mean(mean_IKSI),
    sd = sd(mean_IKSI),
    se = sd/sqrt(N),
    ci = se*qt(.95/2 + .5,N-1)
  )

typing_analysis_data <- list()
typing_analysis_data[[1]]<-FLS
typing_analysis_data[[2]]<-FLS_summary
typing_analysis_data[[3]]<-MWS
typing_analysis_data[[4]]<-MWS_summary

names(typing_analysis_data)<-c("FLS","FLS_summary","MWS","MWS_summary")

save(typing_analysis_data, file="vignettes/data/typing-analysis-data2.Rda")



apa_first <- apa_print(first, correction="none")
apa_middle <- apa_print(mid, correction="none")

save(apa_first,apa_middle, file="vignettes/data/fl_ml_anovas.Rda")



