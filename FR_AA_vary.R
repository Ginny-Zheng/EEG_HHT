setwd("C:/Users/15305/Box/Desktop/research/All EEG")

load("freq_range.RData")

library(hht)
library(dplyr)
for (k in 1:16) {
  
  sub_id <- c(4:6, 8:13, 15, 17, 18, 20, 21, 23, 24)
  
  load(paste("p", sub_id[k], "c4data.RData", sep=""))

  wave_all <- list()
  wave_amp <- list()
  
  freq_range <- F[k, ]
  cut1 <- freq_range[5]
  cut2 <- freq_range[4]
  cut3 <- freq_range[3]
  cut4 <- freq_range[2]
  cut5 <- freq_range[1]
  
  for(i in 1:(length(event_label)-1)){
    
    event <- data[, (event_label[i]:event_label[(i+1)])]
    testtime <- 1:dim(event)[2]
    dt <- 1/500
    realtime <- testtime*dt
    
    wave <- matrix(0, 64, 16)
    temp1 <- matrix(0, 64, 16)
    
    for(j in 1: 64){
      emd.result <- Sig2IMF (event[j, ], realtime)
      
      ratio11 <- length(which(emd.result$hinstfreq[,1] >= cut2 &
                                emd.result$hinstfreq[,1] <= cut1))/dim(event)[2]
      
      ratio12 <- length(which(emd.result$hinstfreq[,1] >= cut3 &
                                emd.result$hinstfreq[,1] <= cut2))/dim(event)[2]
      
      ratio13 <- length(which(emd.result$hinstfreq[,1] >= cut4 &
                                emd.result$hinstfreq[,1] <= cut3))/dim(event)[2]
      
      ratio14 <- length(which(emd.result$hinstfreq[,1] >= cut5 &
                                emd.result$hinstfreq[,1] <= cut4))/dim(event)[2]
      
      ratio1 <- c(ratio11, ratio12, ratio13, ratio14)
      
      wave[j, 1:4] <- ratio1
      
      temp1[j, 1] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 1] <= cut1 & emd.result$hinstfreq[, 1] >= cut2), 1])
      temp1[j, 2] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 1] <= cut2 & emd.result$hinstfreq[, 1] >= cut3), 1])
      temp1[j, 3] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 1] <= cut3 & emd.result$hinstfreq[, 1] >= cut4), 1])
      temp1[j, 4] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 1] <= cut4 & emd.result$hinstfreq[, 1] >= cut5), 1])
      
      ratio21 <- length(which(emd.result$hinstfreq[,2] >= cut2 &
                                emd.result$hinstfreq[,2] <= cut1))/dim(event)[2]
      
      ratio22 <- length(which(emd.result$hinstfreq[,2] >= cut3 &
                                emd.result$hinstfreq[,2] <= cut2))/dim(event)[2]
      
      ratio23 <- length(which(emd.result$hinstfreq[,2] >= cut4 &
                                emd.result$hinstfreq[,2] <= cut3))/dim(event)[2]
      
      ratio24 <- length(which(emd.result$hinstfreq[,2] >= cut5 &
                                emd.result$hinstfreq[,2] <= cut4))/dim(event)[2]
      
      ratio2 <- c(ratio21, ratio22, ratio23, ratio24)
      
      wave[j, 5:8] <- ratio2
      
      temp1[j, 5] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 2] <= cut1 & emd.result$hinstfreq[, 2] >= cut2), 2])
      temp1[j, 6] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 2] <= cut2 & emd.result$hinstfreq[, 2] >= cut3), 2])
      temp1[j, 7] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 2] <= cut3 & emd.result$hinstfreq[, 2] >= cut4), 2])
      temp1[j, 8] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 2] <= cut4 & emd.result$hinstfreq[, 2] >= cut5), 2])
      
      
      if(dim(emd.result$hinstfreq)[2] < 3){
        break
      }
      
      ratio31 <- length(which(emd.result$hinstfreq[,3] >= cut2 &
                                emd.result$hinstfreq[,3] <= cut1))/dim(event)[2]
      
      ratio32 <- length(which(emd.result$hinstfreq[,3] >= cut3 &
                                emd.result$hinstfreq[,3] <= cut2))/dim(event)[2]
      
      ratio33 <- length(which(emd.result$hinstfreq[,3] >= cut4 &
                                emd.result$hinstfreq[,3] <= cut3))/dim(event)[2]
      
      ratio34 <- length(which(emd.result$hinstfreq[,3] >= cut5 &
                                emd.result$hinstfreq[,3] <= cut4))/dim(event)[2]
      
      ratio3 <- c(ratio31, ratio32, ratio33, ratio34)
      
      wave[j, 9:12] <- ratio3
      
      temp1[j, 9] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 3] <= cut1 & emd.result$hinstfreq[, 3] >= cut2), 3])
      temp1[j, 10] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 3] <= cut2 & emd.result$hinstfreq[, 3] >= cut3), 3])
      temp1[j, 11] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 3] <= cut3 & emd.result$hinstfreq[, 3] >= cut4), 3])
      temp1[j, 12] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 3] <= cut4 & emd.result$hinstfreq[, 3] >= cut5), 3])
      
      
      if(dim(emd.result$hinstfreq)[2] < 4){
        break
      }
      
      ratio41 <- length(which(emd.result$hinstfreq[,4] >= cut2 &
                                emd.result$hinstfreq[,4] <= cut1))/dim(event)[2]
      
      ratio42 <- length(which(emd.result$hinstfreq[,4] >= cut3 &
                                emd.result$hinstfreq[,4] <= cut2))/dim(event)[2]
      
      ratio43 <- length(which(emd.result$hinstfreq[,4] >= cut4 &
                                emd.result$hinstfreq[,4] <= cut3))/dim(event)[2]
      
      ratio44 <- length(which(emd.result$hinstfreq[,4] >= cut5 &
                                emd.result$hinstfreq[,4] <= cut4))/dim(event)[2]
      
      ratio4 <- c(ratio41, ratio42, ratio43, ratio44)
      
      wave[j, 13:16] <- ratio4
      
      temp1[j, 13] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 4] <= cut1 & emd.result$hinstfreq[, 4] >= cut2), 4])
      temp1[j, 14] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 4] <= cut2 & emd.result$hinstfreq[, 4] >= cut3), 4])
      temp1[j, 15] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 4] <= cut3 & emd.result$hinstfreq[, 4] >= cut4), 4])
      temp1[j, 16] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 4] <= cut4 & emd.result$hinstfreq[, 4] >= cut5), 4])
      
    }
    wave_all[[i]] <- wave
    wave_amp[[i]] <- temp1
  }
  
  filename <- paste("p", sub_id[k], "c4_wave_ratio_vary", sep="")
  save(wave_all, file = paste(filename, ".RData", sep=""))
  
  
  filename <- paste("p", sub_id[k], "c4_amplitude_vary", sep="")
  save(wave_amp, file = paste(filename, ".RData", sep=""))
  
}
