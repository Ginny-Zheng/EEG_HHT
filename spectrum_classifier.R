setwd("C:/Users/jzz0121/Desktop/All EEG")
load("p20c4data.RData")
diff(event_label)
num <- 24

event_tag <- c(rep(c(0, 1), num/2), 0)
event_tag <- c(rep(c(0, 1), num/2))
event_tag <- as.factor(event_tag)

freq_matrix <- matrix(100, num, 20*64)
for (i in 1:num) {
  event <- data[, event_label[i]:event_label[i+1]]
  
  for (j in 1:64) {
    
    del<-1/500 # sampling interval
    x.spec <- spectrum(event[j, ],log="no",span=10,plot=FALSE)
    spx <- x.spec$freq/del
    spy <- 2*x.spec$spec
    
    spec_average <- rep(0, 20)
    #for (freq in 1:40) {
    #  spec_average[freq] <- mean(spy[1:5+5*(freq-1)]) 
    #}
    spec_average <- spy[c(10, 12, 14, 16, 18, 21, 24, 28, 32, 37, 
                          43, 49, 57, 65, 76, 87, 100, 115, 133, 153)]
    
    freq_matrix[i, c(1:20+20*(j-1))] <- spec_average
    
  }
  
}


#rf_classifier(freq_matrix, testnum = 2, event_label = event_tag)
# 0.73

imp_c1 <- rf_feature_importance(freq_matrix, event_label = event_tag)
#plot(imp_c1)
temp <- sort(imp_c1, decreasing = TRUE, index.return = TRUE)
ratio_idx <- temp$ix[1:30]

# ratio feature space
ratio_select <- freq_matrix[, ratio_idx]
rf_classifier(ratio_select, testnum = 7, event_label = event_tag)

#rf_classifier(ratio_select[which(diff(event_label)>2000), ], 
#              testnum = 11, event_label = event_tag[which(diff(event_label)>2000)])





