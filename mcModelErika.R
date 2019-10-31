
# Markov Chain 

# Import dataset
# V1 ~ V10 represents the sequence of persuasive strategies performed by the persuader
# 6 persuasive strategies (emotional appeal, credibility appeal, task information, logical appeal, proposition, other (=not persuasive))
# role_a = 0 (persuader) role_p = 1 (persuadee)

# package for sequence analysis
# install.packages("TraMineR")
library(TraMineR)
library(tidyverse)
library(plyr)
library(dplyr)
# install.packages("markovchain")
library(markovchain)
# install.packages("MmgraphR")
library(MmgraphR)
# pkgs <- c("dplyr", "tidyr", "broom")
# install.packages(pkgs) #install 
sapply(pkgs, require, character.only = T) #load 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# pred_wide1 <- read.csv("dat_agg_pred1.csv", stringsAsFactors = F)
# pred_wide1

# Importing persuadee sequence data
# pred_ee_alpha <- read.csv("pred_ee_alpha.csv", stringsAsFactors = F)
# pred_ee_alpha
# 
# # merge persuader's sequence with persuadee's sequence
# pred_er_ee <- left_join(pred_wide1, pred_ee_alpha, by="B2")
# pred_er_ee
# write.csv(pred_er_ee, "pred_er_ee.csv")


# Import clean dataset (Please use this file as possible)
df <- read.csv("MarkovChain_clean.csv", stringsAsFactors = F)
df



############################################################
### Transitional probablity matrix - PART 1 
# Here, we compare two groups (donated vs. not donated)
# Also, the assumption here is that the distribution of Markov chain is "stationary"
# Order of presentation

# 1) Transition probability matrix
# 2) Visualization
# 3) Distance calculation
# 4) Further investigation  (t-test)

#############################################################

# df[3:22] indicates the sequence of strategies/ and linguistic patterns used throughout the 10 turns.
# To calculate transition probabilities, we will use this 
# For the FULL dataset (persuader+persuadee):
df[3:22]

# 1) Transition probability matrix
############################################################
# Using persuader-persuadee sequences

# people who donated (pred_wide1$donate_p==1)
sequence_1 <- df[3:22] %>% filter(df$donate_p==1)
sequence_1
fit_1 <- markovchainFit(data=sequence_1)
fit_1 #this gives out 5 outputs (estimate, standard error,etc)

df_fit1 <- fit_1$estimate[1:14] # transition probability matrix

# people who did NOT donate (pred_wide1$donate_p==0)
sequence_0 <- df[3:22] %>% filter(df$donate_p==0)
sequence_0
fit_0 <- markovchainFit(data=sequence_0)
fit_0 #this gives out 5 outputs (estimate, standard error,etc)

df_fit0 <- fit_0$estimate[1:14] # transition probability matrix

df_fit1 - df_fit0


# 2) Visualization
############################################################
# Plotting a probability transition matrix

# FULL data
# Basic plots
trmatplot(df_fit1) # Donated 
trmatplot(df_fit0) # Not donated

# most probable
trmatplot(df_fit1, seed = 2, cpal = c("antiquewhite4","aquamarine","blue","blueviolet","brown","chartreuse",
                                      "chocolate","cornflowerblue","darkgoldenrod1","deeppink","dimgray",
                                      "lightskyblue","lightpink","navy"), pfilter = "tmax", num = 15)
trmatplot(df_fit0, seed = 2, cpal = c("antiquewhite4","aquamarine","blue","blueviolet","brown","chartreuse",
                                      "chocolate","cornflowerblue","darkgoldenrod1","deeppink","dimgray",
                                      "lightskyblue","lightpink","navy"), pfilter = "tmax", num = 15)

# 3) Distance calculation
############################################################
# Here, we calculate the distance between two matrices (donated - not donated)
# FULL data 
distance <- sqrt(sum((df_fit1[1:14] - df_fit0[1:14])^2))
distance # the distance between donated and non-donated groups is 0.027 


# 4) Further investigation (t-test)
############################################################
# To explore the specific differences among the transition probability scores, 
# We could possibly perform t-tests (since MarkovChain function gives out all the 
# necessary values such as standard errors).
# It should be noted that MarkovchainFit function uses bootstraping method.

# FULL data
# Creating t value output matrix
mat1 <- df_fit1 # people who donated
mat0 <- df_fit0 # people who did NOT donate
se1<- fit_1$standardError # standard error for each score (donated)
se0 <- fit_0$standardError # standard error for each score (NOT donated)
n1 <- nrow(sequence_1) # sample size for donated 
n0 <-  nrow(sequence_0) # sample size for Not donated

i <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
states <- c("opening","inquiry_p","provide_p","pos_remark","off-task","inquiry_t","agree","disagree","cre","emo","log","other","pro","task") # the order of strategies
t_new <- matrix(data=NA, nrow=14, ncol=14, dimnames=list(states,states)) # t value output matrix 
p_new <- matrix(data=NA, nrow=14, ncol=14, dimnames=list(states,states)) # p value output matrix
for (a in i){
  for (b in i){
    se <- sqrt( ((se1[a,b]*sqrt(n1))^2/n1) + ((se0[a,b]*sqrt(n0))^2/n0) )
    DF <- n1+n0-2
    t <- ((mat1[a,b]-mat0[a,b])/se)
    p <- 2*pt(-abs(t),DF)
    t_new[a,b] <- t
    p_new[a,b] <- p
  }
}
t_new #matrix of t-values between donated and non-donated
p_new


############################################################
### Transitional probablity matrix - PART 2 
# Again, we compare two groups (donated vs. not donated)
# Here, we compare step-by-step (one step) transition probability matrices
# Order of presentation

# 1) Transition probability matrix
# 2) Visualization
# 3) Distance calculation
# 4) Further investigation  (t-test)


# 1) Transition probability matrix
############################################################
# Since there are 10 turns (for each persuader and persuadee), we can generate 19 transition probability matrices in total

#####
# FULL data (donated)
#P1 (t1~t2 transition)
seq1_1 <- df[3:4] %>% filter(df$donate_p==1)
fit1_1 <- markovchainFit(data=seq1_1)
#P2 (t2~t3 transition)
seq1_2 <- df[4:5] %>% filter(df$donate_p==1)
fit1_2<- markovchainFit(data=seq1_2)
#P3 (t3~t4 transition)
seq1_3 <- df[5:6] %>% filter(df$donate_p==1)
fit1_3 <- markovchainFit(data=seq1_3)
#P4 (t4~t5 transition)
seq1_4 <- df[6:7] %>% filter(df$donate_p==1)
fit1_4 <- markovchainFit(data=seq1_4)
#P5 (t5~t6 transition)
seq1_5 <- df[7:8] %>% filter(df$donate_p==1)
fit1_5 <- markovchainFit(data=seq1_5)
#P6 (t6~t7 transition)
seq1_6 <- df[8:9] %>% filter(df$donate_p==1)
fit1_6 <- markovchainFit(data=seq1_6)
#P7 (t7~t8 transition)
seq1_7 <- df[9:10] %>% filter(df$donate_p==1)
fit1_7 <- markovchainFit(data=seq1_7)
#P8 (t8~t9 transition)
seq1_8 <- df[10:11] %>% filter(df$donate_p==1)
fit1_8 <- markovchainFit(data=seq1_8)
#P9 (t9~t10 transition)
seq1_9 <- df[11:12] %>% filter(df$donate_p==1)
fit1_9 <- markovchainFit(data=seq1_9)
#P10
seq1_10 <- df[12:13] %>% filter(df$donate_p==1)
fit1_10 <- markovchainFit(data=seq1_10)
#P11
seq1_11 <- df[13:14] %>% filter(df$donate_p==1)
fit1_11<- markovchainFit(data=seq1_11)
#P12
seq1_12 <- df[14:15] %>% filter(df$donate_p==1)
fit1_12 <- markovchainFit(data=seq1_12)
#P13
seq1_13 <- df[15:16] %>% filter(df$donate_p==1)
fit1_13 <- markovchainFit(data=seq1_13)
#P14
seq1_14 <- df[16:17] %>% filter(df$donate_p==1)
fit1_14 <- markovchainFit(data=seq1_14)
#P15
seq1_15 <- df[17:18] %>% filter(df$donate_p==1)
fit1_15 <- markovchainFit(data=seq1_15)
#P16
seq1_16 <- df[18:19] %>% filter(df$donate_p==1)
fit1_16 <- markovchainFit(data=seq1_16)
#P17
seq1_17 <- df[19:20] %>% filter(df$donate_p==1)
fit1_17 <- markovchainFit(data=seq1_17)
#P18
seq1_18 <- df[20:21] %>% filter(df$donate_p==1)
fit1_18 <- markovchainFit(data=seq1_18)
#P19
seq1_19 <- df[21:22] %>% filter(df$donate_p==1)
fit1_19 <- markovchainFit(data=seq1_19)


######
# FULL data (not donate)
seq0_1 <- df[3:4] %>% filter(df$donate_p==0)
fit0_1 <- markovchainFit(data=seq0_1)
#P2 (t2~t3 transition)
seq0_2 <- df[4:5] %>% filter(df$donate_p==0)
fit0_2<- markovchainFit(data=seq0_2)
#P3 (t3~t4 transition)
seq0_3 <- df[5:6] %>% filter(df$donate_p==0)
fit0_3 <- markovchainFit(data=seq0_3)
#P4 (t4~t5 transition)
seq0_4 <- df[6:7] %>% filter(df$donate_p==0)
fit0_4 <- markovchainFit(data=seq0_4)
#P5 (t5~t6 transition)
seq0_5 <- df[7:8] %>% filter(df$donate_p==0)
fit0_5 <- markovchainFit(data=seq0_5)
#P6 (t6~t7 transition)
seq0_6 <- df[8:9] %>% filter(df$donate_p==0)
fit0_6 <- markovchainFit(data=seq0_6)
#P7 (t7~t8 transition)
seq0_7 <- df[9:10] %>% filter(df$donate_p==0)
fit0_7 <- markovchainFit(data=seq0_7)
#P8 (t8~t9 transition)
seq0_8 <- df[10:11] %>% filter(df$donate_p==0)
fit0_8 <- markovchainFit(data=seq0_8)
#P9 (t9~t10 transition)
seq0_9 <- df[11:12] %>% filter(df$donate_p==0)
fit0_9 <- markovchainFit(data=seq0_9)
#P10
seq0_10 <- df[12:13] %>% filter(df$donate_p==0)
fit0_10 <- markovchainFit(data=seq0_10)
#P11
seq0_11 <- df[13:14] %>% filter(df$donate_p==0)
fit0_11<- markovchainFit(data=seq0_11)
#P12
seq0_12 <- df[14:15] %>% filter(df$donate_p==0)
fit0_12 <- markovchainFit(data=seq0_12)
#P13
seq0_13 <- df[15:16] %>% filter(df$donate_p==0)
fit0_13 <- markovchainFit(data=seq0_13)
#P14
seq0_14 <- df[16:17] %>% filter(df$donate_p==0)
fit0_14 <- markovchainFit(data=seq0_14)
#P15
seq0_15 <- df[17:18] %>% filter(df$donate_p==0)
fit0_15 <- markovchainFit(data=seq0_15)
#P16
seq0_16 <- df[18:19] %>% filter(df$donate_p==0)
fit0_16 <- markovchainFit(data=seq0_16)
#P17
seq0_17 <- df[19:20] %>% filter(df$donate_p==0)
fit0_17 <- markovchainFit(data=seq0_17)
#P18
seq0_18 <- df[20:21] %>% filter(df$donate_p==0)
fit0_18 <- markovchainFit(data=seq0_18)
#P19
seq0_19 <- df[21:22] %>% filter(df$donate_p==0)
fit0_19 <- markovchainFit(data=seq0_19)


# 2) Visualization
############################################################


# 3) Distance calculation
############################################################
# d1 to d19 refers to the distance between two matrices (donate/non-donate)

#####
# FULL data
d1 <- sqrt(sum((fit1_1$estimate[1:10,1:10]- fit0_1$estimate[1:10,1:10])^2)) #no A12,A17,A19,task 
d2 <- sqrt(sum((fit1_2$estimate[1:10,1:10]- fit0_2$estimate[1:10,1:10])^2)) #no A12,A17,A19, task ..
d3 <- sqrt(sum((fit1_3$estimate[1:13,1:13]- fit0_3$estimate[1:13,1:13])^2)) #add task ..
d4 <- sqrt(sum((fit1_4$estimate[1:14]- fit0_4$estimate[1:14])^2))
d5 <- sqrt(sum((fit1_5$estimate[1:13]- fit0_5$estimate[1:13])^2)) #no A1
d6 <- sqrt(sum((fit1_6$estimate[1:13]- fit0_6$estimate[1:13])^2)) #no A1
d7 <- sqrt(sum((fit1_7$estimate[1:13]- fit0_7$estimate[1:13])^2)) #no A1
d8 <- sqrt(sum((fit1_8$estimate[1:13]- fit0_8$estimate[1:13])^2)) #no A1
d9 <- sqrt(sum((fit1_9$estimate[1:13]- fit0_9$estimate[1:13])^2)) #no A1
d10 <- sqrt(sum((fit1_10$estimate[1:13]- fit0_10$estimate[1:13])^2)) #no A1
d11 <- sqrt(sum((fit1_11$estimate[1:13]- fit0_11$estimate[1:13])^2))#no A1
d12 <- sqrt(sum((fit1_12$estimate[1:13]- fit0_12$estimate[1:13])^2))#no A1
d13 <- sqrt(sum((fit1_13$estimate[1:13]- fit0_13$estimate[1:13])^2))#no A1
d14 <- sqrt(sum((fit1_14$estimate[1:13]- fit0_14$estimate[1:13])^2))#no A1
d15 <- sqrt(sum((fit1_15$estimate[1:13]- fit0_15$estimate[1:13])^2))#no A1
d16 <- sqrt(sum((fit1_16$estimate[1:13]- fit0_16$estimate[1:13])^2))#no A1
d17 <- sqrt(sum((fit1_17$estimate[2:14,2:14]- fit0_17$estimate[1:13])^2)) #there's A1 for donated
d18 <- sqrt(sum((fit1_18$estimate[2:14,2:14]- fit0_18$estimate[1:13])^2))#there's A1 for donated
d19 <- sqrt(sum((fit1_19$estimate[1:13]- fit0_19$estimate[1:13])^2))#no A1

distance <- c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19)
distance
mean(distance) #0.8966522
table(distance)

#distance plot
distance <- c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19)
threshold <- c(0.8966522,0.8966522,0.8966522,0.8966522,0.8966522,0.8966522,0.8966522,0.8966522,0.8966522,0.8966522,0.8966522,0.8966522,0.8966522,0.8966522,0.8966522,0.8966522,0.8966522,0.8966522,0.8966522)
plot(distance)
d_range <- range(0, distance)
plot(distance, ylim=d_range, axes=FALSE, ann=FALSE)
axis(1, at=1:19, lab=c("p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","p11","p12","p13","p14","p15","p16","p17","p18","p19"))
axis(2,las=1,at=c(0,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6))
box()
lines(threshold, col="red")
title(xlab="Transition probability", col.lab=rgb(0,0.5,0))
title(ylab="Distance", col.lab=rgb(0,0.5,0))


# 4) Further investigation (t-test)
############################################################

### FULL data
# Creating t value output matrix

# Example: p1

mat1 <- fit1_16$estimate[1:13] # people who donated
mat0 <- fit0_16$estimate[1:13] # people who did NOT donate
se1<- fit1_16$standardError # standard error for each score (donated)
se0 <- fit0_16$standardError # standard error for each score (NOT donated)
n1 <- nrow(seq1_16) # sample size for donated 
n0 <-  nrow(seq0_16) # sample size for Not donated

i <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
#i <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
states <- c("opening","provide_p","inquiry_t","agree","disagree","cre","emo","log","other","pro")
#states <- c("opening","inquiry_p","provide_p","pos_remark","off-task","inquiry_t","agree","disagree","cre","emo","log","other","pro","task")
t_new <- matrix(data=NA, nrow=13, ncol=13, dimnames=list(states,states)) # t value output matrix 
p_new <- matrix(data=NA, nrow=13, ncol=13, dimnames=list(states,states)) # p value output matrix
for (a in i){
  for (b in i){
    se <- sqrt( ((se1[a,b]*sqrt(n1))^2/n1) + ((se0[a,b]*sqrt(n0))^2/n0) )
    DF <- n1+n0-2
    t <- ((mat1[a,b]-mat0[a,b])/se)
    p <- 2*pt(-abs(t),DF)
    t_new[a,b] <- t
    p_new[a,b] <- p
  }
}
t_new #matrix of t-values between donated and non-donated
p_new



