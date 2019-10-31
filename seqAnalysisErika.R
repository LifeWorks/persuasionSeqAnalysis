
# Sequence Analysis

# Import dataset
# V1 ~ V10 represents the sequence of persuasive strategies performed by the persuader
# 6 persuasive strategies (emotional appeal, credibility appeal, task information, logical appeal, proposition, other (=not persuasive))
# role_a = 0 (persuader) role_p = 1 (persuadee)

# package for sequence analysis
install.packages("TraMineR")
library(TraMineR)

library(tidyverse)
library(dplyr)
library(plyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pred_wide1 <- read.csv("dat_agg_pred1.csv", stringsAsFactors = F)
pred_wide1

pred_wide1.alphab <- c("emo", "log", "cre", "pro","task", "other")

pred_wide1.seq <- seqdef(pred_wide1, 3:12, xtstep = 6, alphabet = pred_wide1.alphab)

# Visualization based on individual sequences #first ten sequence
seqiplot(pred_wide1.seq, border = NA, main = "Ten first sequences")

# Generate 10 most frequent sequences
seqfplot(pred_wide1.seq, border = NA, main = "Sequence frequency plot")
seqlegend(pred_wide1.seq, fontsize = 1.3)

# The state distribution by time points #with.legend = F, 
seqdplot(pred_wide1.seq, border = NA, main = "State distribution plot")

#seqlegend(pred_wide.seq, cex = 1.3)
#seqlegend(pred_wide.seq, fontsize = 1.3)

# Chronogram
seqdplot(pred_wide1.seq)


#Do the sequences differ by donation behavior?
seqdplot(pred_wide1.seq, group = pred_wide1$donate_p, border = NA)


# Entropy #this may potentially work as an iv
##################################################
# Plotting entropy
seqHtplot(pred_wide1.seq, main = "Entropy index")

# Relationship between entropy and donation behavior
entropies <- seqient(pred_wide1.seq)
table(entropies)
summary(entropies) 

lm.ent <- lm(donate_p ~ entropies, pred_wide1)
summary(lm.ent)


# Turbulence #this may potentially work as an iv
##################################################
Turbulence <- seqST(pred_wide1.seq)
summary(Turbulence)
hist(Turbulence, col = "cyan", main = "Sequence turbulence")

# Relationship between turbulence and donation behavior
lm.tur <- lm(donate_p ~ Turbulence, pred_wide1)
summary(lm.tur)


# Optimal matching
# hierarchical clustering ,dissimilarity matrix.Ward's method
library("cluster")
pred_wide1.om <- seqdist(pred_wide1.seq, method = "OM", with.missing = TRUE, indel = 1, sm = "TRATE") #651 distinct sequences!
#dist.om1 <- seqdist(pred_wide.seq, method = "OM", indel = 1, sm = pred_wide.om)
summary(pred_wide1.om)


# Cluster (4-class solution)
##################################################
#Make a typology of the trajectories: load the cluster library, 
#build a Ward hierarchical clustering of the sequences from the optimal matching distances 
#and retrieve for each individual sequence the cluster membership of the 4 class solution.

clusterward <- agnes(pred_wide1.om, diss = TRUE, method = "ward")
pred_wide1.cl4 <- cutree(clusterward, k = 4)
cl4.lab <- factor(pred_wide1.cl4, labels = paste("Cluster", 1:4))

pred_wide1$cl4.lab <- factor(pred_wide1.cl4, labels = paste("Cluster", 1:4))


# Plotting of the 4 clusters
seqdplot(pred_wide1.seq, group = cl4.lab, border = NA)
seqfplot(pred_wide1.seq, group = cl4.lab, border = NA)

# Recoding clusters
# Orginal clusters
# "Others" strategy based cluster == cluster 4
# For better interpretation, recoding is needed

# Recoding
# Cluster 1 (Mixed strategy cluster) -> Cluster 4
# Cluster 4 (others strategy cluster) -> Cluster 1
library(car)
pred_wide1$cl4.lab1 <- NA
pred_wide1$cl4.lab1[pred_wide1$cl4.lab=="Cluster 4"] <- "Cluster 1"
pred_wide1$cl4.lab1[pred_wide1$cl4.lab=="Cluster 2"] <- "Cluster 2"
pred_wide1$cl4.lab1[pred_wide1$cl4.lab=="Cluster 3"] <- "Cluster 3"
pred_wide1$cl4.lab1[pred_wide1$cl4.lab=="Cluster 1"] <- "Cluster 4"

# Original
pred_wide1$cl4.lab

# Recoded
pred_wide1$cl4.lab1

# Regression analysis
# "Others" cluster is the reference group
lm.cluster4 <- lm(donate_p ~ cl4.lab1, pred_wide1)
summary(lm.cluster4)


# Cluster (6-class solution)
########################################################################

clusterward <- agnes(pred_wide1.om, diss = TRUE, method = "ward")
pred_wide1.cl4 <- cutree(clusterward, k = 6)
cl4.lab <- factor(pred_wide1.cl4, labels = paste("Cluster", 1:6))

pred_wide1$cl4.lab <- factor(pred_wide1.cl4, labels = paste("Cluster", 1:6))


# Plotting of the 6 clusters
seqdplot(pred_wide1.seq, group = cl4.lab, border = NA)

# Recoding cluster (6 clusters)

pred_wide1$cl4.lab2 <- NA
pred_wide1$cl4.lab2[pred_wide1$cl4.lab=="Cluster 6"] <- "Cluster 1"
pred_wide1$cl4.lab2[pred_wide1$cl4.lab=="Cluster 2"] <- "Cluster 2"
pred_wide1$cl4.lab2[pred_wide1$cl4.lab=="Cluster 3"] <- "Cluster 3"
pred_wide1$cl4.lab2[pred_wide1$cl4.lab=="Cluster 4"] <- "Cluster 4"
pred_wide1$cl4.lab2[pred_wide1$cl4.lab=="Cluster 5"] <- "Cluster 5"
pred_wide1$cl4.lab2[pred_wide1$cl4.lab=="Cluster 1"] <- "Cluster 6"

# Original
pred_wide1$cl4.lab

# Recoded
pred_wide1$cl4.lab2

# "others" cluster == cluster 1 (reference group)
lm.cluster6 <- lm(donate_p ~ cl4.lab2, pred_wide1)
summary(lm.cluster6)


# Regression results (entropy, turbulence)
########################################################################

reg <- lm(donate_p ~ entropies+Turbulence, pred_wide1)
summary(reg)


# Individual sequence
data(pred_wide1[3:12])
pred_wide1.seq <- seqdef(pred_wide1[3:12])
pred_wide1.seq
summary(pred_wide)
#seqlength(pred_wide.seq)


#################
# Basic Analysis
#################


#Main effects of demographic (persuadee's) on donation (persuadee's)
###################################################################################

demo <- glm(donate_p ~ age_p + gender_p + race_p + edu_p + marital_p + employment_p + income_p + religion_p +ideology_p,
            family = "binomial",
            data = pred_wide1)
summary(demo)  

demo <- glm(donation_p ~ age_p + gender_p + race_p + edu_p + marital_p + employment_p + income_p + religion_p +ideology_p,
            family = "binomial",
            data = pred_wide1)
summary(demo)    

# Main effects of Big five personality traits (persuadee's) on donation (persuadee's)
###################################################################################

big5 <- glm(donate_p ~ bf_agreeable_p + bf_extrovert_p+ bf_neurotic_p+bf_open_p+bf_conscientious_p,
            family = "binomial",
            data = pred_wide1)
summary(big5)

big5 <- glm(donation_p ~ bf_agreeable_p + bf_extrovert_p+ bf_neurotic_p+bf_open_p+bf_conscientious_p,
            family = "binomial",
            data = pred_wide1)
summary(big5)

# Call:
#   glm(formula = donate_p ~ bf_agreeable_p + bf_extrovert_p + bf_neurotic_p + 
#         bf_open_p + bf_conscientious_p, family = "binomial", data = pred_wide1)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.9018  -1.1711   0.7624   1.0447   1.8618  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -0.9841793  0.4172667  -2.359  0.01834 *  
#   bf_agreeable_p      0.1648479  0.0273206   6.034  1.6e-09 *** 
#   bf_extrovert_p     -0.0697927  0.0217823  -3.204  0.00135 ** 
#   bf_neurotic_p       0.0285878  0.0182516   1.566  0.11727    
# bf_open_p          -0.0005647  0.0303209  -0.019  0.98514    
# bf_conscientious_p -0.0014567  0.0261692  -0.056  0.95561    
# ---
#   Signif. codes:  0 ??***?? 0.001 ??**?? 0.01 ??*?? 0.05 ??.?? 0.1 ?? ?? 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 895.90  on 651  degrees of freedom
# Residual deviance: 848.69  on 646  degrees of freedom
# AIC: 860.69
# 
# Number of Fisher Scoring iterations: 4





# Main effects of Persuasive strategies (persuader's) on donation (persuadee's)
###################################################################################

strategy <- glm(donate_p ~ E2_a+L1_a+L3_a+P1_a+TF_a+other_a,
            family = "binomial",
            data = pred_wide1)
summary(strategy)

strategy <- glm(donation_p ~ E2_a+L1_a+L3_a+P1_a+TF_a+other_a,
                family = "binomial",
                data = pred_wide1)
summary(strategy)

# Call:
#   glm(formula = donate_p ~ E2_a + L1_a + L3_a + P1_a + TF_a + other_a, 
#       family = "binomial", data = pred_wide1)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.5448  -1.2373   0.9586   1.0819   1.3313  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)  0.20168    0.36951   0.546    0.585
# E2_a        -0.07297    0.05258  -1.388    0.165
# L1_a         0.03198    0.05879   0.544    0.587
# L3_a         0.11314    0.06924   1.634    0.102
# P1_a         0.05288    0.12872   0.411    0.681
# TF_a         0.11506    0.09721   1.184    0.237
# other_a     -0.02370    0.03848  -0.616    0.538
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 895.90  on 651  degrees of freedom
# Residual deviance: 886.26  on 645  degrees of freedom
# AIC: 900.26
# 
# Number of Fisher Scoring iterations: 4





# Main effect of linguistic style/responses (persuadee's) on donation (persuadee's)
###################################################################################

response <- glm(donate_p ~ A1_p+A3_p+A5_p+A6_p+A11_p+A12_p+A16_p+A17_p+A18_p+A19_p,
                family = "binomial",
                data = pred_wide1)
summary(response)

# Results
# Call:
#   glm(formula = donate_p ~ A1_p + A3_p + A5_p + A6_p + A11_p + 
#         A12_p + A16_p + A17_p + A18_p + A19_p, family = "binomial", 
#       data = pred_wide1)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.9661  -1.2147   0.8508   1.0629   1.6667  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept) -0.71558    1.13134  -0.633  0.52706   
# A1_p        -0.03248    0.15487  -0.210  0.83387   
# A3_p         0.09197    0.11825   0.778  0.43673   
# A5_p         0.29640    0.12985   2.283  0.02245 * (showing agreement)
# A6_p        -0.62711    0.20816  -3.013  0.00259 ** (showing disagreement)
# A11_p       -2.18601    1.25429  -1.743  0.08136 . 
# A12_p       -0.28637    0.25559  -1.120  0.26254   
# A16_p        0.06447    0.11770   0.548  0.58384   
# A17_p        0.35652    0.15521   2.297  0.02162 * (positive remarks, closing)
# A18_p        0.96924    2.02076   0.480  0.63148   
# A19_p        0.11072    0.11565   0.957  0.33838   
# ---
#   Signif. codes:  0 ??***?? 0.001 ??**?? 0.01 ??*?? 0.05 ??.?? 0.1 ?? ?? 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 895.90  on 651  degrees of freedom
# Residual deviance: 855.55  on 641  degrees of freedom
# AIC: 877.55
# 
# Number of Fisher Scoring iterations: 4



############################################################
### Transitional probablity matrix
#############################################################

# These 3 codes below are most important (you can just run these 12 lines)
install.packages("markovchain")
library(markovchain)

# sequence <- c("a", "b", "a", "a", "a", "a", "b", "a", "b", "a", 
#               "b", "a", "a", "b", "b", "b", "a")
# mcFit <- markovchainFit(data=sequence)


# Using markovchain codes,
# All inclusive data- donate and not donate (works fine -  I cross-checked with my own codes)
sequence <- pred_wide1[3:12]
sequence
fit <- markovchainFit(data=sequence)
fit

#people who donated 
sequence_1 <- pred_wide1[3:12] %>% filter(pred_wide1$donate_p==1)
sequence_1
fit_1 <- markovchainFit(data=sequence_1)
fit_1

#people who did not donate
sequence_0 <- pred_wide1[3:12] %>% filter(pred_wide1$donate_p==0)
sequence_0
fit_0 <- markovchainFit(data=sequence_0)
fit_0


# calculating transition probablity (cross-validation)
# here, the order for the matrix is: emo;log;cre;pro;task;other
#############################################################
# number of total occurrances of strategies V1(t0) to V9(t8) 
t0 <- table(pred_wide1[3])
t1 <- table(pred_wide1[4])
t2 <- table(pred_wide1[5])
t3 <- table(pred_wide1[6])
t4 <- table(pred_wide1[7])
t5 <- table(pred_wide1[8])
t6 <- table(pred_wide1[9])
t7 <- table(pred_wide1[10])
t8 <- table(pred_wide1[11])


emo_sum <- sum(t0[2]+t1[2]+t2[2]+t3[2]+t4[2]+t5[2]+t6[2]+t7[2]+t8[2])
cre_sum <- sum(t0[1]+t1[1]+t2[1]+t3[1]+t4[1]+t5[1]+t6[1]+t7[1]+t8[1])
log_sum <- sum(t0[3]+t1[3]+t2[3]+t3[3]+t4[3]+t5[3]+t6[3]+t7[3]+t8[3])
other_sum <- sum(t0[4]+t1[4]+t2[4]+t3[4]+t4[4]+t5[4]+t6[4]+t7[4]+t8[4])
pro_sum <- sum(t0[5]+t1[5]+t2[5]+t3[5]+t4[5]+t5[5]+t6[5]+t7[5]+t8[5])
task_sum <- sum(t0[6]+t1[6]+t2[6]+t3[6]+t4[6]+t5[6]+t6[6]+t7[6]+t8[6])


All_sum <- sum(emo_sum, cre_sum, log_sum,other_sum,pro_sum,task_sum)
All_sum


# Sum the number of pairs across 9 transitions (6*6 = 36 values)

time <- c(3,4,5,6,7,8,9,10,11)
next_time <- c(4,5,6,7,8,9,10,11,12)

#########################emo-x transition
#ee
a_ee<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
    ee <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="emo" & pred_wide1[n]=="emo"))
    print(ee)
    a_ee<-append(a_ee,ee)
  }}}
a_11<-sum(a_ee) 

#el
a_el<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      el <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="emo" & pred_wide1[n]=="log"))
      print(el)
      a_el<-append(a_el,el)
    }}}
a_12<-sum(a_el)

#ec
a_ec<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      ec <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="emo" & pred_wide1[n]=="cre"))
      print(ec)
      a_ec<-append(a_ec,ec)
    }}}
a_13<-sum(a_ec)

#ep
a_ep<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      ep <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="emo" & pred_wide1[n]=="pro"))
      print(ep)
      a_ep<-append(a_ep,ep)
    }}}
a_14<-sum(a_ep)

#et
a_et<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      et <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="emo" & pred_wide1[n]=="task"))
      print(et)
      a_et<-append(a_et,et)
    }}}
a_15<-sum(a_et)


#eo
a_eo<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      eo <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="emo" & pred_wide1[n]=="other"))
      print(eo)
      a_eo<-append(a_eo,eo)
    }}}
a_16<-sum(a_eo)

##############log-x transition

#le
a_le<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      le <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="log" & pred_wide1[n]=="emo"))
      print(le)
      a_le<-append(a_le,le)
    }}}
a_21<-sum(a_le) 

#ll
a_ll<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      ll <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="log" & pred_wide1[n]=="log"))
      print(ll)
      a_ll<-append(a_ll,ll)
    }}}
a_22<-sum(a_ll) 

#lc
a_lc<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      lc <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="log" & pred_wide1[n]=="cre"))
      print(lc)
      a_lc<-append(a_lc,lc)
    }}}
a_23<-sum(a_lc)

#lp
a_lp<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      lp <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="log" & pred_wide1[n]=="pro"))
      print(lp)
      a_lp<-append(a_lp,lp)
    }}}
a_24<-sum(a_lp)

#lt
a_lt<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      lt <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="log" & pred_wide1[n]=="task"))
      print(lt)
      a_lt<-append(a_lt,lt)
    }}}
a_25<-sum(a_lt)

#lo
a_lo<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      lo <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="log" & pred_wide1[n]=="other"))
      print(lo)
      a_lo<-append(a_lo,lo)
    }}}
a_26<-sum(a_lo)

##################################cre-x transition

#ce
a_ce<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      ce <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="cre" & pred_wide1[n]=="emo"))
      print(ce)
      a_ce<-append(a_ce,ce)
    }}}
a_31<-sum(a_ce) 

a_cl<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      cl <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="cre" & pred_wide1[n]=="log"))
      print(cl)
      a_cl<-append(a_cl,cl)
    }}}
a_32<-sum(a_cl) 


a_cc<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      cc <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="cre" & pred_wide1[n]=="cre"))
      print(cc)
      a_cc<-append(a_cc,cc)
    }}}
a_33<-sum(a_cc) 


a_cp<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      cp <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="cre" & pred_wide1[n]=="pro"))
      print(cp)
      a_cp<-append(a_cp,cp)
    }}}
a_34<-sum(a_cp)


a_ct<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      ct <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="cre" & pred_wide1[n]=="task"))
      print(ct)
      a_ct<-append(a_ct,ct)
    }}}
a_35<-sum(a_ct) 


a_co<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      co <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="cre" & pred_wide1[n]=="other"))
      print(co)
      a_co<-append(a_co,co)
    }}}
a_36<-sum(a_co)

##################################pro-x transition

#pe
a_pe<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      pe <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="pro" & pred_wide1[n]=="emo"))
      print(pe)
      a_pe<-append(a_pe,pe)
    }}}
a_41<-sum(a_pe) 

#pl
a_pl<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      pl <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="pro" & pred_wide1[n]=="log"))
      print(pl)
      a_pl<-append(a_pl,pl)
    }}}
a_42<-sum(a_pl) 

#pc
a_pc<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      pc <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="pro" & pred_wide1[n]=="cre"))
      print(pc)
      a_pc<-append(a_pc,pc)
    }}}
a_43<-sum(a_pc) 

#pp
a_pp<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      pp <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="pro" & pred_wide1[n]=="pro"))
      print(pp)
      a_pp<-append(a_pp,pp)
    }}}
a_44<-sum(a_pp)

#pt
a_pt<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      pt <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="pro" & pred_wide1[n]=="task"))
      print(pt)
      a_pt<-append(a_pt,pt)
    }}}
a_45<-sum(a_pt) 

#po
a_po<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      po <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="pro" & pred_wide1[n]=="other"))
      print(po)
      a_po<-append(a_po,po)
    }}}
a_46<-sum(a_po)


#################################task-x transition

#te
a_te<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      te <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="task" & pred_wide1[n]=="emo"))
      print(te)
      a_te<-append(a_te,te)
    }}}
a_51<-sum(a_te) 

#tl
a_tl<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      tl <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="task" & pred_wide1[n]=="log"))
      print(tl)
      a_tl<-append(a_tl,tl)
    }}}
a_52<-sum(a_tl) 

#tc
a_tc<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      tc <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="task" & pred_wide1[n]=="cre"))
      print(tc)
      a_tc<-append(a_tc,tc)
    }}}
a_53<-sum(a_tc) 

#tp
a_tp<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      tp <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="task" & pred_wide1[n]=="pro"))
      print(tp)
      a_tp<-append(a_tp,tp)
    }}}
a_54<-sum(a_tp)

#tt
a_tt<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      tt <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="task" & pred_wide1[n]=="task"))
      print(tt)
      a_tt<-append(a_tt,tt)
    }}}
a_55<-sum(a_tt) 

#po
a_to<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      to <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="task" & pred_wide1[n]=="other"))
      print(to)
      a_to<-append(a_to,to)
    }}}
a_56<-sum(a_to)


#################################other-x transition

#oe
a_oe<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      oe <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="other" & pred_wide1[n]=="emo"))
      print(oe)
      a_oe<-append(a_oe,oe)
    }}}
a_61<-sum(a_oe) 

#ol
a_ol<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      ol <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="other" & pred_wide1[n]=="log"))
      print(ol)
      a_ol<-append(a_ol,ol)
    }}}
a_62<-sum(a_ol) 

#oc
a_oc<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      oc <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="other" & pred_wide1[n]=="cre"))
      print(oc)
      a_oc<-append(a_oc,oc)
    }}}
a_63<-sum(a_oc) 

#op
a_op<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      op <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="other" & pred_wide1[n]=="pro"))
      print(op)
      a_op<-append(a_op,op)
    }}}
a_64<-sum(a_op)

#ot
a_ot<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      ot <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="other" & pred_wide1[n]=="task"))
      print(ot)
      a_ot<-append(a_ot,ot)
    }}}
a_65<-sum(a_ot) 

#oo
a_oo<-c()
for (t in time){
  for (n in next_time){
    if (n==t+1){
      oo <- nrow(pred_wide1 %>% filter(pred_wide1[t]=="other" & pred_wide1[n]=="other"))
      print(oo)
      a_oo<-append(a_oo,oo)
    }}}
a_66<-sum(a_to)


# matrix of frequency of pair occurrances (emo;log;cre;pro;task;other)
matrix <- matrix(c(a_11,a_21,a_31,a_41,a_51,a_61,
                   a_12,a_22,a_32,a_42,a_52,a_62,
                   a_13,a_23,a_33,a_43,a_53,a_63,
                   a_14,a_24,a_34,a_44,a_54,a_64,
                   a_15,a_25,a_35,a_45,a_55,a_65,
                   a_16,a_26,a_36,a_46,a_56,a_66), ncol =6) #1~36 6 columns
matrix


# matrix of transition probability

t_prob <- matrix(c(a_11/emo_sum,a_21/log_sum,a_31/cre_sum,a_41/pro_sum,a_51/task_sum,a_61/other_sum,
                   a_12/emo_sum,a_22/log_sum,a_32/cre_sum,a_42/pro_sum,a_52/task_sum,a_62/other_sum,
                   a_13/emo_sum,a_23/log_sum,a_33/cre_sum,a_43/pro_sum,a_53/task_sum,a_63/other_sum,
                   a_14/emo_sum,a_24/log_sum,a_34/cre_sum,a_44/pro_sum,a_54/task_sum,a_64/other_sum,
                   a_15/emo_sum,a_25/log_sum,a_35/cre_sum,a_45/pro_sum,a_55/task_sum,a_65/other_sum,
                   a_16/emo_sum,a_26/log_sum,a_36/cre_sum,a_46/pro_sum,a_56/task_sum,a_66/other_sum), ncol =6) #1~36 6 columns
t_prob





