# save the excel file with norvig data as csv
# set folder containing ngrams1.csv as working directory
letterPosits<-read.csv('ngrams1.csv')

unpredict<-c()
wordPosits<-c()
wordLengths<-c()

for(wordLength in 1:9)
{
  for(wordPosit in 1:wordLength)
  {  
   nameCol=(paste("X",wordLength,".",wordPosit,".",wordPosit,sep=""))
   letterz<-letterPosits[,which(colnames(letterPosits)==nameCol)]
   letterz[which(letterz==0)]<-1E-10
   letterz<-(letterz/sum(letterz))*log2(letterz/sum(letterz))
   wordLengths<-c(wordLengths,wordLength)
   wordPosits<-c(wordPosits,wordPosit)
   unpredict<-c(unpredict,-sum(letterz))
  }
}
lgthPstnH<-data.frame(wordLengths,wordPosits,unpredict)
# convert into proportions
library(ggplot2)
ggplot(lgthPstnH,aes(x=wordPosits,y=unpredict, group=wordLengths, color=wordLengths))+geom_point()+geom_line()