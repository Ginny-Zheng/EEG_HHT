#' Compute the frequency ratio of the EEG signals
#'
#' This code allows you to compute and save the frequency ratio of EEG signals.
#' @param data EEG signals.
#' @param event_label time labels which indicate the begin/end of each epoch.
#' @param R frequency range of neural oscillations.
#' @param dt sampling rate of EEG signals.
#' @keywords Frequency Ratio Computation
#' @import hht
#' @import dplyr
#' @examples
load("p9c1data.RData")  ## replace by EEG data of other participants
library(hht)
wave_all <- list()
for(i in 1:(length(event_label)-1)){
  
  event <- data[, (event_label[i]:event_label[(i+1)])]
  testtime <- 1:dim(event)[2]
  dt <- 1/500
  realtime <- testtime*dt
  
  wave <- matrix(0, 64, 16)
  for(j in 1: 64){
    emd.result <- Sig2IMF (event[j, ], realtime)
    hrender <- HHRender(emd.result, dfreq = 0.1,
                        freq.span = c(0, 30),
                        combine.imfs = FALSE)
    
    ratio11 <- length(which(hrender$hinstfreq[,1,1] >= 14 &
                              hrender$hinstfreq[,1,1] <= 30))/dim(event)[2]
    
    ratio12 <- length(which(hrender$hinstfreq[,1,1] >= 8 &
                              hrender$hinstfreq[,1,1] <= 14))/dim(event)[2]
    
    ratio13 <- length(which(hrender$hinstfreq[,1,1] >= 4 &
                              hrender$hinstfreq[,1,1] <= 8))/dim(event)[2]
    
    ratio14 <- length(which(hrender$hinstfreq[,1,1] >= 0.5 &
                              hrender$hinstfreq[,1,1] <= 4))/dim(event)[2]
    
    ratio1 <- c(ratio11, ratio12, ratio13, ratio14)
    
    wave[j, 1:4] <- ratio1
    
    ratio21 <- length(which(hrender$hinstfreq[,2,1] >= 14 &
                              hrender$hinstfreq[,2,1] <= 30))/dim(event)[2]
    
    ratio22 <- length(which(hrender$hinstfreq[,2,1] >= 8 &
                              hrender$hinstfreq[,2,1] <= 14))/dim(event)[2]
    
    ratio23 <- length(which(hrender$hinstfreq[,2,1] >= 4 &
                              hrender$hinstfreq[,2,1] <= 8))/dim(event)[2]
    
    ratio24 <- length(which(hrender$hinstfreq[,2,1] >= 0.5 &
                              hrender$hinstfreq[,2,1] <= 4))/dim(event)[2]
    
    ratio2 <- c(ratio21, ratio22, ratio23, ratio24)
    
    wave[j, 5:8] <- ratio2
    
    if(dim(hrender$hinstfreq[,,1])[2] < 3){
      break
    }
    
    ratio31 <- length(which(hrender$hinstfreq[,3,1] >= 14 &
                              hrender$hinstfreq[,3,1] <= 30))/dim(event)[2]
    
    ratio32 <- length(which(hrender$hinstfreq[,3,1] >= 8 &
                              hrender$hinstfreq[,3,1] <= 14))/dim(event)[2]
    
    ratio33 <- length(which(hrender$hinstfreq[,3,1] >= 4 &
                              hrender$hinstfreq[,3,1] <= 8))/dim(event)[2]
    
    ratio34 <- length(which(hrender$hinstfreq[,3,1] >= 0.5 &
                              hrender$hinstfreq[,3,1] <= 4))/dim(event)[2]
    
    ratio3 <- c(ratio31, ratio32, ratio33, ratio34)
    
    wave[j, 9:12] <- ratio3
    
    if(dim(hrender$hinstfreq[,,1])[2] < 4){
      break
    }
    
    ratio41 <- length(which(hrender$hinstfreq[,4,1] >= 14 &
                              hrender$hinstfreq[,4,1] <= 30))/dim(event)[2]
    
    ratio42 <- length(which(hrender$hinstfreq[,4,1] >= 8 &
                              hrender$hinstfreq[,4,1] <= 14))/dim(event)[2]
    
    ratio43 <- length(which(hrender$hinstfreq[,4,1] >= 4 &
                              hrender$hinstfreq[,4,1] <= 8))/dim(event)[2]
    
    ratio44 <- length(which(hrender$hinstfreq[,4,1] >= 0.5 &
                              hrender$hinstfreq[,4,1] <= 4))/dim(event)[2]
    
    ratio4 <- c(ratio41, ratio42, ratio43, ratio44)
    
    wave[j, 13:16] <- ratio4
    
  }
  wave_all[[i]] <- wave
}

filename <- paste("p9c1_wave_ratio", sep="")
save(wave_all, file = paste(filename, ".RData", sep=""))
