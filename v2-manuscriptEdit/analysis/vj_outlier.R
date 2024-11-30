#'Selst, M. V., & Jolicoeur, P. (1994). A solution to the effect of sample size on outlier elimination. 
#'The quarterly journal of experimental psychology, 47(3), 631-650.
#'
#'@parameter rts vector of response times
#'@returns list containing original vector of reaction times, vector of reaction times excluding outliers, and the proportion of removed rts
#'@example
#'x<-c(11,2,3,2,3,23,4,5,4,100)
#'modified_recursive_moving(x)$original_rts
#'modified_recursive_moving(x)$restricted
#'modified_recursive_moving(x)$prop_removed



non_recursive_moving<-function(rts){
  xsize <- c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 
             25, 30, 35, 50, 100)
  stds <- c(1.458, 1.68, 1.841, 1.961, 2.05, 2.12, 2.173, 
            2.22, 2.246, 2.274, 2.31, 2.326, 2.391, 2.41, 2.4305, 
            2.45, 2.48, 2.5)
  if(length(rts>=100)){
    sdc=2.5
  }else{
    sdc<-approx(xsize,stds,xout=length(rts))$y
  }
  mean_rts<-mean(rts)
  restricted_rts<-rts[rts > mean_rts - (sd(rts)*sdc) &
                      rts < mean_rts + (sd(rts)*sdc)]
  
  list(original_rts=rts,
       restricted=restricted_rts,
       prop_removed=(1-(length(restricted_rts)/length(rts)))
       )
}


modified_recursive_moving<-function(rts){
  xsize <- c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 
             25, 30, 35, 50, 100)
  stds <- c(8.00, 6.2, 5.3, 4.8, 4.475, 4.25, 4.11, 
            4.00, 3.92, 3.85, 3.80, 3.75, 3.64, 3.595, 3.55, 
            3.54, 3.51, 3.5)
  restricted_rts<-rts
  stop=FALSE
  while(stop==FALSE){
    if(length(restricted_rts>=100)){
      sdc=3.5
    }else{
      sdc<-approx(xsize,stds,xout=length(restricted_rts))$y
    }
    temporary_exclusion<-max(restricted_rts)
    descriptives<-c(mean(restricted_rts[restricted_rts<temporary_exclusion]),sd(restricted_rts[restricted_rts<temporary_exclusion]))
    lower_cutoff<-descriptives[1]-(descriptives[2]*sdc)
    upper_cutoff<-descriptives[1]+(descriptives[2]*sdc)
    retained_rts<-restricted_rts
    if(min(restricted_rts)<lower_cutoff){
      retained_rts<-restricted_rts[restricted_rts>min(restricted_rts)]
    }
    if(max(restricted_rts)>upper_cutoff){
      retained_rts<-restricted_rts[restricted_rts<max(restricted_rts)]
    }
    
    #check for breaking loop
    if(length(retained_rts)==length(restricted_rts)){
      stop<-TRUE
      restricted_rts<-retained_rts
    } else if (length(restricted_rts)<4){
      stop<-TRUE
      restricted_rts<-retained_rts
    }else{
      restricted_rts<-retained_rts
    }
  }
  list(original_rts=rts,restricted=restricted_rts,prop_removed=(1-(length(restricted_rts)/length(rts))))
}
#stde <- function(x) sd(x)/sqrt(length(x))
