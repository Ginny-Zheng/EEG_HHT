#' This code draws some figures in the paper
#'
#' This code allows you to obtain feature importance and compute classification accuracy.
#' @param feature_data the structured data.
#' @param event_label event label of each epoch: tasks number for between-tasks classifiers and movement label for within-task classifiers.
#' @param testnum number of epochs used as testing sets.
#' @keywords Feature importance and Classifier
#' @import ranger
#' @examples

library(ggplot2)
library(plyr)
library(hrbrthemes)
library(gplots)

## p9c1
event <- data[, (event_label[2]:event_label[3])]
time <- 1:dim(event)[2]
plot(time, event[1,], 'l')
plot(1:dim(data)[2], data[2, ], 'l')
dt <- 1/500

## EMD
i <- 40
try <- Sig2IMF(event[i, ], time*dt)
par(mar=c(3,2,0.1,0.1), mgp=c(1,1,0))
PlotIMFs(try, cex = 1.2, imf.list = 1:6)

#Plot spectrogram
hgram <- HHRender(try, dfreq = 0.3, freq.span = c(0,30), combine.imfs = FALSE)

HHGramImage(hgram, pretty = TRUE, freq.span = c(0, 30),
            imf.list = 1)   ## imf.list can be single IMF or multiple IMF
HHGramImage(hgram, pretty = TRUE, freq.span = c(0, 30),
            imf.list = 1, cex.lab = 1.2, img.x.lab = "Time", img.y.lab = "Frequency",
            img.x.format = "%.0f", img.y.format = "%.0f",
            colorbar.format = "%.1f", trace.format = "%.1f",
            amp.span = c(0, 10))

par(mar=c(4,3,1,2), mgp=c(7,4,0))
dat <- data.frame(Time = try$tt, 
                  Frequency = try$hinstfreq[, 2], 
                  Amplitude = try$hamp[, 2])

qplot(Time, Frequency, data=dat, colour=Amplitude) + 
  scale_colour_gradientn(colours = rainbow(10)) + 
  ylim(0, 30) + theme(axis.text.x = element_text(size = rel(1.5), margin = margin(10,0,0,0)),
                      axis.text.y = element_text(size = rel(1.5), margin = margin(0,10,0,0)),
                      plot.margin = margin(t = 10, r = 5, b = 10, l = 10, unit = "pt"),
                      axis.title.y = element_text(size = rel(1.5), margin = margin(0,20,0,0)),
                      axis.title.x = element_text(size = rel(1.5), margin = margin(10,0,0,0)))


## Table 1 in the paper
## calculate frequency ratio or use the function provided
ratio11 <- length(which(hgram$hinstfreq[,1,1] >= 14 &
                          hgram$hinstfreq[,1,1] <= 30))/dim(event)[2]
ratio12 <- length(which(hgram$hinstfreq[,1,1] >= 8 &
                          hgram$hinstfreq[,1,1] <= 14))/dim(event)[2]
ratio13 <- length(which(hgram$hinstfreq[,1,1] >= 4 &
                          hgram$hinstfreq[,1,1] <= 8))/dim(event)[2]
ratio14 <- length(which(hgram$hinstfreq[,1,1] >= 0.5 &
                          hgram$hinstfreq[,1,1] <= 4))/dim(event)[2]
ratio1 <- c(ratio11, ratio12, ratio13, ratio14)

ratio21 <- length(which(hgram$hinstfreq[,2,1] >= 14 &
                          hgram$hinstfreq[,2,1] <= 30))/dim(event)[2]
ratio22 <- length(which(hgram$hinstfreq[,2,1] >= 8 &
                          hgram$hinstfreq[,2,1] <= 14))/dim(event)[2]
ratio23 <- length(which(hgram$hinstfreq[,2,1] >= 4 &
                          hgram$hinstfreq[,2,1] <= 8))/dim(event)[2]
ratio24 <- length(which(hgram$hinstfreq[,2,1] >= 0.5 &
                          hgram$hinstfreq[,2,1] <= 4))/dim(event)[2]
ratio2 <- c(ratio21, ratio22, ratio23, ratio24)

ratio31 <- length(which(hgram$hinstfreq[,3,1] >= 14 &
                          hgram$hinstfreq[,3,1] <= 30))/dim(event)[2]
ratio32 <- length(which(hgram$hinstfreq[,3,1] >= 8 &
                          hgram$hinstfreq[,3,1] <= 14))/dim(event)[2]
ratio33 <- length(which(hgram$hinstfreq[,3,1] >= 4 &
                          hgram$hinstfreq[,3,1] <= 8))/dim(event)[2]
ratio34 <- length(which(hgram$hinstfreq[,3,1] >= 0.5 &
                          hgram$hinstfreq[,3,1] <= 4))/dim(event)[2]
ratio3 <- c(ratio31, ratio32, ratio33, ratio34)

ratio41 <- length(which(hgram$hinstfreq[,4,1] >= 14 &
                          hgram$hinstfreq[,4,1] <= 30))/dim(event)[2]
ratio42 <- length(which(hgram$hinstfreq[,4,1] >= 8 &
                          hgram$hinstfreq[,4,1] <= 14))/dim(event)[2]
ratio43 <- length(which(hgram$hinstfreq[,4,1] >= 4 &
                          hgram$hinstfreq[,4,1] <= 8))/dim(event)[2]
ratio44 <- length(which(hgram$hinstfreq[,4,1] >= 0.5 &
                          hgram$hinstfreq[,4,1] <= 4))/dim(event)[2]
ratio4 <- c(ratio41, ratio42, ratio43, ratio44)



## Hilbert Spectrum plot for two consecutive events
load("p18c3data.RData")
load("wave_result/p18c3_wave_ratio.RData")
load("amp_server_result/p18c3_amplitude_server.RData")

event <- data[, (event_label[2]:event_label[4])]
time <- 1:dim(event)[2]
plot(time, event[22,], 'l')
dt <- 1/500

## EMD
i <- 22
try <- Sig2IMF(event[i, ], time*dt)
par(mar=c(4,3,1,2), mgp=c(7,4,0))

## IMF 1 change
dat <- data.frame(Time = try$tt, 
                  Frequency = try$hinstfreq[, 1], 
                  Amplitude = try$hamp[, 1])
qplot(Time, Frequency, data=dat, colour=Amplitude) + 
  scale_colour_gradientn(colours = rainbow(10)) + 
  ylim(0, 30) + theme(axis.text.x = element_text(size = rel(1.5), margin = margin(10,0,0,0)),
                      axis.text.y = element_text(size = rel(1.5), margin = margin(0,10,0,0)),
                      plot.margin = margin(t = 10, r = 5, b = 10, l = 10, unit = "pt"),
                      axis.title.y = element_text(size = rel(1.5), margin = margin(0,20,0,0)),
                      axis.title.x = element_text(size = rel(1.5), margin = margin(10,0,0,0))) +
  geom_vline(xintercept = 5, lty = 5, lwd = 1.5)


## averaged amplitude histogram for channel 22, eyes open vs closed, all epochs
temp <- unlist(lapply(wave_amp, FUN = function(x) x[22,6]))
temp[is.na(temp)] <- 0
temp_data <- data.frame(value = temp, 
                        type = c(rep(c("eyes open", "eyes closed"), 54)))
temp_data <- temp_data[temp_data$value > 0, ]
  
mu <- ddply(temp_data, "type", summarise, grp.mean=mean(value))
head(mu)

p <- temp_data %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram(aes(y=..density..), binwidth = 0.3,
                 color="#e9ecef", alpha=0.7, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="") + geom_density(alpha = 0.4) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=type),
             linetype="dashed", lwd=2)+
  labs(x = "Amplitude Value", y = "Density")+
  theme(
    axis.title.x = element_text(color="black", size=15, face="bold"),
    axis.title.y = element_text(color="black", size=15, face="bold"),
    axis.text.x = element_text(size = rel(2), margin = margin(5,0,0,0)),
    axis.text.y = element_text(size = rel(2), margin = margin(0,5,0,0))
  )
p


## histogram for channel 22, all epochs
temp <- unlist(lapply(wave_amp, FUN = function(x) x[22,6]))
temp[is.na(temp)] <- 0
temp_data <- data.frame(value = temp, 
                        type = c(rep(c("eyes open", "eyes closed"), 21)))
temp_data <- temp_data[temp_data$value > 0, ]

library(plyr)
mu <- ddply(temp_data, "type", summarise, grp.mean=mean(value))
head(mu)

library(hrbrthemes)
p <- temp_data %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram(aes(y=..density..), binwidth = 0.3,
                 color="#e9ecef", alpha=0.7, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="") + geom_density(alpha = 0.4) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=type),
             linetype="dashed", lwd=2)+
  labs(x = "Amplitude Value", y = "Density")+
  theme(
    axis.title.x = element_text(color="black", size=15, face="bold"),
    axis.title.y = element_text(color="black", size=15, face="bold"),
    axis.text.x = element_text(size = rel(2), margin = margin(5,0,0,0)),
    axis.text.y = element_text(size = rel(2), margin = margin(0,5,0,0))
  )
p



## t.test for all channels during task 3
load("amp_server_result/p18c3_amplitude_server.RData")
eyeopen <- seq(1,length(wave_amp), by = 2)
eyeclose <- seq(2,length(wave_amp), by = 2)
pvalue <- matrix(0, 64, 16)
for(j in 1:64)
{
  for (i in 1:16) {
    temp <- unlist(lapply(wave_amp, FUN = function(x) x[j,i]))
    pvalue[j,i] <- t.test(temp[eyeopen], temp[eyeclose],
                          alternative = 'less')$p.value
  }
}
heatmap.2(pvalue, trace = 'none', keysize = 1, cexCol = 1.5)
pvalue_01 <- matrix(as.numeric(pvalue<0.05), 64, 16)
heatmap.2(pvalue_01, trace = 'none', keysize = 1, cexCol = 1.5, cexRow = 1.2)

p_test <- pvalue[, 6]
plot(p.adjust(p_test, method = "bonferroni"))
abline(h=0.05)
plot(p.adjust(p_test, method = "fdr"))
abline(h=0.05)
round(p.adjust(p_test, method = "fdr"), 3)


## t.test for all channels during task 4
load("amp_server_result/p18c4_amplitude_server.RData")
eyeopen <- seq(1,length(wave_amp), by = 2)
eyeclose <- seq(2,length(wave_amp), by = 2)
pvalue <- matrix(0, 64, 16)
for(j in 1:64)
{
  for (i in 1:16) {
    temp <- unlist(lapply(wave_amp, FUN = function(x) x[j,i]))
    pvalue[j,i] <- t.test(temp[eyeopen], temp[eyeclose],
                          alternative = 'less')$p.value
  }
}
heatmap.2(pvalue, trace = 'none', keysize = 1, cexCol = 1.5)
pvalue_01 <- matrix(as.numeric(pvalue<0.05), 64, 16)
heatmap.2(pvalue_01, trace = 'none', keysize = 1, cexCol = 1.5, cexRow = 1.2)

p_test <- pvalue[, 6]
plot(p.adjust(p_test, method = "bonferroni"))
abline(h=0.05)
plot(p.adjust(p_test, method = "fdr"))
abline(h=0.05)
round(p.adjust(p_test, method = "fdr"), 3)


