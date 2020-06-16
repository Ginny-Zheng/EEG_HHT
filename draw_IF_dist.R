a <- try$hinstfreq[,2]
a <- a[which(a<42)]
plot(a)
hist(a)
quantile(a, probs = seq(0,1,0.1))
length(which(a>=8 & a<=14))/length(a)

dat <- data.frame(label = 1:15000,
                  IMF1 = try$hinstfreq[, 1], 
                  IMF2 = try$hinstfreq[, 2],
                  IMF3 = try$hinstfreq[, 3],
                  IMF4 = try$hinstfreq[, 4])

library(tidyverse)
new_dat <- gather(dat, key = "label", value = "measurement", "IMF1":"IMF4")
new_dat <- new_dat[which(new_dat$measurement < 35), ]
library(hrbrthemes)

  ggplot(new_dat, aes(x=measurement, fill=label)) +
  geom_histogram(aes(y=..density..), color="#e9ecef", alpha=0.6, position = 'identity', bins = 50) +
  geom_density(alpha=.2, color="#bdc6cf") +
  theme_minimal() + xlab("Frequency") + ylab("Density") +
  #theme_ipsum() +
  labs(fill="") + 
  geom_segment(aes(x = 21.22, xend = 21.22, y=0, yend = 0.13), color = "#d63256", size = 1, linetype="dashed")+
  geom_segment(aes(x = 14.75, xend = 14.75, y=0, yend = 0.13), color = "#d63256", size = 1)+
  geom_segment(aes(x = 27.69, xend = 27.69, y=0, yend = 0.13), color = "#d63256", size = 1)+
  geom_text(x=16.3, y=0.135, label="14.75", color = "#d63256", size=4.8) +
  geom_text(x=26.2, y=0.135, label="27.69", color = "#d63256", size=4.8) +
  geom_segment(aes(x = 14.75, xend = 27.69, y=0.12, yend = 0.12), size = 1,
               arrow = arrow(ends = "both", length = unit(0.14, "inches")), 
               color = '#d63256') +
  
  geom_segment(aes(x = 10.99, xend = 10.99, y=0, yend = 0.2), color = "#57d633", size = 1, linetype="dashed")+
  geom_segment(aes(x = 7.55, xend = 7.55, y=0, yend = 0.2), color = "#57d633", size = 1)+
  geom_segment(aes(x = 14.42, xend = 14.42, y=0, yend = 0.2), color = "#57d633", size = 1)+
  geom_text(x=8.7, y=0.205, label="7.55", color = "#57d633", size=4.8) +
  geom_text(x=13, y=0.205, label="14.42", color = "#57d633", size=4.8) +
  geom_segment(aes(x = 7.55, xend = 14.42, y=0.19, yend = 0.19), size = 1,
                 arrow = arrow(ends = "both", length = unit(0.14, "inches")), 
                 color = '#57d633') +
  
  geom_segment(aes(x = 6.05, xend = 6.05, y=0, yend = 0.25), color = "#33b7d6", size = 1, linetype="dashed")+
  geom_segment(aes(x = 3.89, xend = 3.89, y=0, yend = 0.25), color = "#33b7d6", size = 1)+
  geom_segment(aes(x = 8.21, xend = 8.21, y=0, yend = 0.25), color = "#33b7d6", size = 1) +
  geom_text(x=4.6, y=0.26, label="3.89", color = "#33b7d6", size=4.8) +
  geom_text(x=7.4, y=0.26, label="8.21", color = "#33b7d6", size=4.8) +
  geom_segment(aes(x = 3.89, xend = 8.21, y=0.24, yend = 0.24), size = 1,
                 arrow = arrow(ends = "both", length = unit(0.14, "inches")), 
                 color = '#33b7d6') +
  
  geom_segment(aes(x = 2.83, xend = 2.83, y=0, yend = 0.32), color = "#d633a9", size = 1, linetype="dashed")+
  geom_segment(aes(x = 4.16, xend = 4.16, y=0, yend = 0.32), color = "#d633a9", size = 1)+
  geom_segment(aes(x = 1.5, xend = 1.5, y=0, yend = 0.32), color = "#d633a9", size = 1)+
  geom_text(x=4, y=0.33, label="4.16", color = "#d633a9", size=4.8) +
  geom_text(x=1.5, y=0.33, label="1.5", color = "#d633a9", size=4.8) +
  geom_segment(aes(x = 1.5, xend = 4.16, y=0.31, yend = 0.31), size = 1,
                 arrow = arrow(ends = "both", length = unit(0.14, "inches")), 
                 color = '#d633a9')+
    
  theme(legend.title = element_text(size=13), legend.text = element_text(size = 14),
          axis.title.y = element_text(size = rel(1.3), margin = margin(0,10,0,0)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = rel(1.3), margin = margin(5,0,0,0)),
          plot.margin = margin(t = 15, r = 10, b = 10, l = 10, unit = "pt")) 
  

geom_vline(xintercept = c(21.22, 14.75,27.69), color = "#d63256", size = 1) +
  geom_vline(xintercept = c(10.99, 7.55,14.42), color = "#57d633", size = 1) +
  geom_vline(xintercept = c(6.05, 3.89,8.21), color = "#33b7d6", size = 1) +
  geom_vline(xintercept = c(2.83, 4.16,1.5), color = "#d633a9", size = 1)
  
## compute mean and sd 
a <- dat$IMF1[which(dat$IMF1<35)]
mean(a)
mean(a) + sd(a)
mean(a) - sd(a)

a <- dat$IMF2[which(dat$IMF2<30)]
mean(a)
mean(a) + sd(a)
mean(a) - sd(a)

a <- dat$IMF3[which(dat$IMF3<30)]
mean(a)
mean(a) + sd(a)
mean(a) - sd(a)

a <- dat$IMF4[which(dat$IMF4<30)]
mean(a)
mean(a) + sd(a)
mean(a) - sd(a)

mean_IMF1 <- matrix(0, 64, 4)
mean_IMF2 <- matrix(0, 64, 4)
mean_IMF3 <- matrix(0, 64, 4)
mean_IMF4 <- matrix(0, 64, 4)
for (i in 1:64) {
  try <- Sig2IMF(event[i, ], time*dt)
  dat <- data.frame(
    IMF1 = try$hinstfreq[, 1], 
    IMF2 = try$hinstfreq[, 2],
    IMF3 = try$hinstfreq[, 3],
    IMF4 = try$hinstfreq[, 4])
  mean_IMF1[i, ] <- c(mean(dat$IMF1)-sd(dat$IMF1), mean(dat$IMF1)+sd(dat$IMF1),
                      mean(dat$IMF1[which(dat$IMF1<35)])-sd(dat$IMF1[which(dat$IMF1<35)]),
                      mean(dat$IMF1[which(dat$IMF1<35)])+sd(dat$IMF1[which(dat$IMF1<35)]))
  mean_IMF2[i, ] <- c(mean(dat$IMF2)-sd(dat$IMF2), mean(dat$IMF2)+sd(dat$IMF2),
                      mean(dat$IMF2[which(dat$IMF2<30)])-sd(dat$IMF2[which(dat$IMF2<30)]),
                      mean(dat$IMF2[which(dat$IMF2<30)])+sd(dat$IMF2[which(dat$IMF2<30)]))
  mean_IMF3[i, ] <- c(mean(dat$IMF3)-sd(dat$IMF3), mean(dat$IMF3)+sd(dat$IMF3),
                      mean(dat$IMF3[which(dat$IMF3<30)])-sd(dat$IMF3[which(dat$IMF3<30)]),
                      mean(dat$IMF3[which(dat$IMF3<30)])+sd(dat$IMF3[which(dat$IMF3<30)]))
  mean_IMF4[i, ] <- c(mean(dat$IMF4)-sd(dat$IMF4), mean(dat$IMF4)+sd(dat$IMF4),
                      mean(dat$IMF4[which(dat$IMF4<30)])-sd(dat$IMF4[which(dat$IMF4<30)]),
                      mean(dat$IMF4[which(dat$IMF4<30)])+sd(dat$IMF4[which(dat$IMF4<30)]))
  
}
mean_IMF1 %>% head()
apply(mean_IMF1, 2, mean)
apply(mean_IMF2, 2, mean)
apply(mean_IMF3, 2, mean)
apply(mean_IMF4, 2, mean)


## compute quantile
quantile(dat$IMF1, probs = c(0.1, 0.2, 0.25,0.75,0.8,0.9))
quantile(dat$IMF2, probs = c(0.1, 0.2, 0.25,0.75,0.8,0.9))
quantile(dat$IMF3, probs = c(0.1, 0.2, 0.25,0.75,0.8,0.9))
quantile(dat$IMF4, probs = c(0.1, 0.2, 0.25,0.75,0.8,0.9))

quan_IMF1 <- matrix(0, 64, 6)
quan_IMF2 <- matrix(0, 64, 6)
quan_IMF3 <- matrix(0, 64, 6)
quan_IMF4 <- matrix(0, 64, 6)
for (i in 1:64) {
  try <- Sig2IMF(event[i, ], time*dt)
  dat <- data.frame(label = 1:15000,
                    IMF1 = try$hinstfreq[, 1], 
                    IMF2 = try$hinstfreq[, 2],
                    IMF3 = try$hinstfreq[, 3],
                    IMF4 = try$hinstfreq[, 4])
  quan_IMF1[i, ] <- quantile(dat$IMF1, probs = c(0.1, 0.2, 0.25,0.75,0.8,0.9))
  quan_IMF2[i, ] <- quantile(dat$IMF2, probs = c(0.1, 0.2, 0.25,0.75,0.8,0.9))
  quan_IMF3[i, ] <- quantile(dat$IMF3, probs = c(0.1, 0.2, 0.25,0.75,0.8,0.9))
  quan_IMF4[i, ] <- quantile(dat$IMF4, probs = c(0.1, 0.2, 0.25,0.75,0.8,0.9))
  
}
heatmap.2(quan_IMF1, trace = 'none', Colv = NA)
apply(quan_IMF4, 2, median)

