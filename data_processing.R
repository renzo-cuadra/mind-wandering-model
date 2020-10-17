library(tidyr)
library(rstatix)

# Import data (double check path)
path = "~/mind-wandering-model/model_output"
setwd(path)
responses <- vector()
rts <- vector()
file.names <- dir(path, pattern = ".csv")
for(i in 1:length(file.names)) {
  file <- read.table(file.names[i], header=TRUE)
  responses <- rbind(responses, file[,4])
  rts <- rbind(rts, file[,5])
}

nr <- ncol(responses) #Number of responses per participant
trials <- nr/4 # Number of trials in 1 block)

block1 <- vector()
block2 <- vector()
block3 <- vector()
block4 <- vector()

for (i in 1:length(file.names)) {
  block1 <- rbind(block1, table(responses[i,1:trials]))
  block2 <- rbind(block2, table(responses[i,(trials+1):(trials*2)]))
  block3 <- rbind(block3, table(responses[i,(trials*2+1):(trials*3)]))
  block4 <- rbind(block4, table(responses[i,(trials*3+1):(trials*4)]))
}

data <- cbind(block1[,1], block2[,1], block3[,1], block4[,1])
data_long <- data.frame()

for (i in 1:length(file.names)) {
  for (j in 1:4) {
    temp <- data.frame(Id=i, Block=j, Wandering=data[i,j])
    data_long <- rbind(data_long, temp)
  }
}

data_long$Id <- factor(data_long$Id)
data_long$Block <- factor(data_long$Block)
test <- anova_test(data_long, dv=Wandering, wid=Id, within=Block)
get_anova_table(test)

post <- data_long %>% pairwise_t_test(Wandering~Block, paired=TRUE, p.adjust.method = "bonferroni")
post

#Response times
rtblock1 <- rbind(rts[, 1:trials])
rtblock2 <- rbind(rts[, (trials+1):(trials*2)])
rtblock3 <- rbind(rts[, (trials*2+1):(trials*3)])
rtblock4 <- rbind(rts[, (trials*3+1):(trials*4)])

meanrt_block1 <- mean(rtblock1)
meanrt_block2 <- mean(rtblock2)
meanrt_block3 <- mean(rtblock3)
meanrt_block4 <- mean(rtblock4)

SErtb1 <- sd(rtblock1)/sqrt(length(rtblock1))
SErtb2 <- sd(rtblock2)/sqrt(length(rtblock2))
SErtb3 <- sd(rtblock3)/sqrt(length(rtblock3))
SErtb4 <- sd(rtblock4)/sqrt(length(rtblock4))

sd_rtblock1 <- vector()
sd_rtblock2 <- vector()
sd_rtblock3 <- vector()
sd_rtblock4 <- vector()

for (i in 1:length(file.names)) {
  sd_rtblock1 <- rbind(sd_rtblock1, sd(rtblock1[i,]))
  sd_rtblock2 <- rbind(sd_rtblock2, sd(rtblock2[i,]))
  sd_rtblock3 <- rbind(sd_rtblock3, sd(rtblock3[i,]))
  sd_rtblock4 <- rbind(sd_rtblock4, sd(rtblock4[i,]))
}

meansd_block1 <- mean(sd_rtblock1)
meansd_block2 <- mean(sd_rtblock2)
meansd_block3 <- mean(sd_rtblock3)
meansd_block4 <- mean(sd_rtblock4)

SEsdb1 <- sd(sd_rtblock1)/sqrt(length(sd_rtblock1))
SEsdb2 <- sd(sd_rtblock2)/sqrt(length(sd_rtblock2))
SEsdb3 <- sd(sd_rtblock3)/sqrt(length(sd_rtblock3))
SEsdb4 <- sd(sd_rtblock4)/sqrt(length(sd_rtblock4))

