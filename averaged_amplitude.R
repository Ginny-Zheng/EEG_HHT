#' Compute the averaged amplitude of the EEG signals
#'
#' This code allows you to compute and save the averaged amplitude of EEG signals.
#' @param data EEG signals.
#' @param event_label time labels which indicate the begin/end of each epoch.
#' @param R frequency range of neural oscillations.
#' @param dt sampling rate of EEG signals.
#' @keywords Averaged Amplitude Computation
#' @import hht
#' @import dplyr
#' @examples
load("p4c1data.RData")  ## replace by EEG data of other participants
library(hht)
library(dplyr)
amplitude_IMF1 <- list()
amplitude_IMF2 <- list()
amplitude_IMF3 <- list()
amplitude_IMF4 <- list()
wave_amp <- list()

for(i in 1:(length(event_label)-1)){
  
  event <- data[, (event_label[i]:event_label[(i+1)])]
  testtime <- 1:dim(event)[2]
  dt <- 1/500
  realtime <- testtime*dt
  
  amplitude_temp_IMF1 <- matrix(0, 64, 30)
  amplitude_temp_IMF2 <- matrix(0, 64, 30)
  amplitude_temp_IMF3 <- matrix(0, 64, 30)
  amplitude_temp_IMF4 <- matrix(0, 64, 30)
  
  temp1 <- matrix(0, 64, 16)
  
  for (j in 1:64) {
    emd.result <- Sig2IMF(event[j, ], realtime)
    
    ## IMF1
    freq_amp_IMF1 <- data.frame(freq = round(emd.result$hinstfreq[,1]),
                                amp = emd.result$hamp[,1])
    test_IMF1 <- freq_amp_IMF1 %>%
      filter(freq %in% 1:30) %>%
      group_by(freq) %>%
      summarize(mean_amp = mean(amp))
    
    amplitude_temp_IMF1[j, test_IMF1$freq] <- test_IMF1$mean_amp
    
    temp1[j, 1] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 1] <= 30 & emd.result$hinstfreq[, 1] >= 14), 1])
    temp1[j, 2] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 1] <= 14 & emd.result$hinstfreq[, 1] >= 8), 1])
    temp1[j, 3] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 1] <= 8 & emd.result$hinstfreq[, 1] >= 4), 1])
    temp1[j, 4] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 1] <= 4 & emd.result$hinstfreq[, 1] >= 0.5), 1])
    
    ## IMF2
    freq_amp_IMF2 <- data.frame(freq = round(emd.result$hinstfreq[,2]),
                                amp = emd.result$hamp[,2])
    test_IMF2 <- freq_amp_IMF2 %>%
      filter(freq %in% 1:30) %>%
      group_by(freq) %>%
      summarize(mean_amp = mean(amp))
    
    amplitude_temp_IMF2[j, test_IMF2$freq] <- test_IMF2$mean_amp
    
    temp1[j, 5] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 2] <= 30 & emd.result$hinstfreq[, 2] >= 14), 2])
    temp1[j, 6] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 2] <= 14 & emd.result$hinstfreq[, 2] >= 8), 2])
    temp1[j, 7] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 2] <= 8 & emd.result$hinstfreq[, 2] >= 4), 2])
    temp1[j, 8] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 2] <= 4 & emd.result$hinstfreq[, 2] >= 0.5), 2])
    
    
    if(dim(emd.result$hinstfreq)[2] < 3){
      break
    }
    
    ## IMF3
    freq_amp_IMF3 <- data.frame(freq = round(emd.result$hinstfreq[,3]),
                                amp = emd.result$hamp[,3])
    test_IMF3 <- freq_amp_IMF3 %>%
      filter(freq %in% 1:30) %>%
      group_by(freq) %>%
      summarize(mean_amp = mean(amp))
    
    amplitude_temp_IMF3[j, test_IMF3$freq] <- test_IMF3$mean_amp
    
    temp1[j, 9] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 3] <= 30 & emd.result$hinstfreq[, 3] >= 14), 3])
    temp1[j, 10] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 3] <= 14 & emd.result$hinstfreq[, 3] >= 8), 3])
    temp1[j, 11] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 3] <= 8 & emd.result$hinstfreq[, 3] >= 4), 3])
    temp1[j, 12] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 3] <= 4 & emd.result$hinstfreq[, 3] >= 0.5), 3])
    
    if(dim(emd.result$hinstfreq)[2] < 4){
      break
    }
    
    ## IMF4
    freq_amp_IMF4 <- data.frame(freq = round(emd.result$hinstfreq[,4]),
                                amp = emd.result$hamp[,4])
    test_IMF4 <- freq_amp_IMF4 %>%
      filter(freq %in% 1:30) %>%
      group_by(freq) %>%
      summarize(mean_amp = mean(amp))
    
    amplitude_temp_IMF4[j, test_IMF4$freq] <- test_IMF4$mean_amp
    
    temp1[j, 13] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 4] <= 30 & emd.result$hinstfreq[, 4] >= 14), 4])
    temp1[j, 14] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 4] <= 14 & emd.result$hinstfreq[, 4] >= 8), 4])
    temp1[j, 15] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 4] <= 8 & emd.result$hinstfreq[, 4] >= 4), 4])
    temp1[j, 16] <- mean(emd.result$hamp[which(emd.result$hinstfreq[, 4] <= 4 & emd.result$hinstfreq[, 4] >= 0.5), 4])
  }
  amplitude_IMF1[[i]] <- amplitude_temp_IMF1
  amplitude_IMF2[[i]] <- amplitude_temp_IMF2
  amplitude_IMF3[[i]] <- amplitude_temp_IMF3
  amplitude_IMF4[[i]] <- amplitude_temp_IMF4
  
  wave_amp[[i]] <- temp1
  
}

filename <- paste("p4c1_amplitude_server", sep="")
save(amplitude_IMF1, amplitude_IMF2,
     amplitude_IMF3, amplitude_IMF4, wave_amp,
     file = paste(filename, ".RData", sep=""))
