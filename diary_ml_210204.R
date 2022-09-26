# diary_ml_210204.R replacing id
# diary_ml_200616.R supporting diary-ml-29.tex 
# diary_ml_200604.R (revising the diary_ml paper)
# merch_200515.R corresponds to merch-20.tex submitted to Nancy C for RDR

setwd("~/Papers/Papers_inactive/diary_ml/diary_ml_coding")# Set working directory on your computer
# Your working directory would be different from the above!

# Packages used for this coding: 
library(dplyr)
#library(tidyr)# function "spread()" OUTDATED pivot_longer does not work
#library(formattable)# formats tables to allow trailing zeros (w/ numeric format)
#library(plotrix)# weighted histograms
#library(spatstat)# for weighted median
#library(lubridate) # extracts day of the week
library(ggplot2); theme_set(theme_bw())
library(rpart)
library(rpart.plot)
library(partykit)# modifies rpart tree plot
library("randomForest")
library(class) #for knn 
library("xtable") #exporting to LaTeX
#library(mfx) # binomial logit regressions with marginal effects
library(nnet)# multinomial regressions
#library(neuralnet) #for neural networks [canceled]
library(e1071) # for SVM

# Define functions
#the normalization function is created # normlization of numerical variables
nor <-function(x) { (x -min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)) }
# mode function (also works with factors, and used to replace NA factors with their mode)
#getmode <- function(v) {  uniqv <- unique(v)  uniqv[which.max(tabulate(match(v, uniqv)))]}

#
dir()
m1=readRDS("dcpc_2019_merged_210204.rds")# Read data
names(m1)
dim(m1)

### Main variables from 2019 diary
# transaction (payment-specific) variables
summary(m1$Amount) 
table(m1$Method)
table(m1$device)
colnames(m1)[colnames(m1)=="device"] = "Device"# capitalizing variable
table(m1$in_person)
colnames(m1)[colnames(m1)=="in_person"] = "In_person"# capitalizing variable
table(m1$In_person)
table(m1$merch)
colnames(m1)[colnames(m1)=="merch"] = "Merch"# capitalizing variable
table(m1$type)
colnames(m1)[colnames(m1)=="type"] = "Type"# capitalizing variable
table(m1$bill)
colnames(m1)[colnames(m1)=="bill"] = "Bill"# capitalizing variable
names(m1)
# Mobile-related payments
#names(select(m1, contains("mobile"))) does not work! Why?
# table(m1$mobile_funding)
# sum(table(m1$mobile_funding))
# table(m1$mobile_method)# ==> look at payment_app
# table(m1$mobile_whichapp)

#
# demographic/individual variables
summary(m1$Age)
table(m1$HH_size)
summary(m1$HH_income)
table(m1$Work)
table(m1$Gender)
table(m1$Education)
table(m1$Marital)
m1$Marital = factor(m1$Marital)
# csh_adopt, chk_acnt_adopt, sav_acnt_adopt,  bnk_acnt_adopt, chk_adopt, mon_adopt, dc_adopt, cc_adopt, svc_adopt, obbp_adopt,  banp_adopt, income_adopt, abp_adopt
#

## Restricting to payments only
m2 = subset(m1, Type=="Payment")
dim(m2)
# Remove payments with Amount == 0
m2 = subset(m2, Amount > 0)
dim(m2)

## subsetting payments (not individuals) by income group 
m3 = m2
# m3_0_10k = subset(m3, HH_income >= 0     & HH_income < 10000)
# m3_10_20k = subset(m3, HH_income >= 10000 & HH_income < 20000)
# m3_20_30k = subset(m3, HH_income >= 20000 & HH_income < 30000)
# m3_30_40k = subset(m3, HH_income >= 30000 & HH_income < 40000)
# m3_40_60k = subset(m3, HH_income >= 40000 & HH_income < 60000)
# m3_60_80k = subset(m3, HH_income >= 60000 & HH_income < 80000)
# m3_80_120k = subset(m3, HH_income >= 80000 & HH_income < 120000)
# m3_120_180k = subset(m3, HH_income >= 120000 & HH_income < 180000)
# m3_inf  = subset(m3, HH_income >= 180000)

m4 = m3

table(m4$Method)
100*prop.table(table(m4$Method))

# Removing PI used less than 0.5% & mobile_app (to avoid duplication)
str(m4$Method)
m5 = m4
nrow(m5)# num payments
length(unique(m5$id))# num resp
m5$Method = as.factor(m5$Method)
# the following info (payment use by PI is listed in Section 2)
table(m5$Method)
100*prop.table(table(m5$Method))
#
m6= m5 %>% filter(Method %in% c("Acct_num", "Acct_to_acct", "Online_bill", "Cash", "Check", "Credit", "Debit", "Prepaid"))
table(m6$Method)
sum(table(m6$Method))
m6$Method = factor(m6$Method)
table(m6$Method)
100*prop.table(table(m6$Method))

# Rename payment methods
levels(m6$Method)
levels(m6$Method)[levels(m6$Method)=="Acct_num"] = "BANP"
levels(m6$Method)[levels(m6$Method)=="Acct_to_acct"] = "Acct2acct"
levels(m6$Method)[levels(m6$Method)=="Online_bill"] = "OBBP"

names(m6)
table(m6$Method)
100*(prop.table(table(m6$Method)))

### Grouping PI into 3 groups: paper, card, and electronic
m7 = m6
dim(m7)
table(m7$Method)
m7$pi_group = NA
m7[m7$Method %in% c("Cash", "Check"), "pi_group"] = "Paper"
m7[m7$Method %in% c("Credit", "Debit", "Prepaid"), "pi_group"] = "Card"
m7[m7$Method %in% c("BANP", "Acct2acct", "OBBP"), "pi_group"] = "Electronic"
m7$pi_group = factor(m7$pi_group)
table(m7$pi_group) #reported in Section 5
round(100*prop.table(table(m7$pi_group)),1)

table(m7$In_person)

## Removing unused variables
#m8 = subset(m7, select = c(id, Method, pi_group, Amount, In_person, Bill, Marital, Gender, Age, Work, HH_size, HH_income, Education))
m8 = subset(m7, select = c(id, Method, pi_group, Amount, In_person, Marital, Gender, Age, HH_size, HH_income, Education))
dim(m8)
dim(m8[complete.cases(m8),])
nrow(m8) - nrow(m8[complete.cases(m8),]) # num payments lost by removing all NA
length(unique(m8$id)) - length(unique(m8[complete.cases(m8),]$id))# resp lost
m8 = m8[complete.cases(m8),]
dim(m8)

### Section 2: Describing the data
dim(m8)# num payments
length(unique(m8$id))# num respondents

## Start Table 1: Payments by method
(banp_num = nrow(subset(m8, Method == "BANP")))
(banp_frac = banp_num/nrow(m8))
(banp_val_avg = mean(subset(m8, Method == "BANP")$Amount))
(banp_val_med = median(subset(m8, Method == "BANP")$Amount))
(banp_in_person_num = nrow(subset(m8, Method == "BANP" & In_person=="Yes")))
(banp_in_person_frac = nrow(subset(m8, Method == "BANP" & In_person=="Yes"))/nrow(subset(m8, In_person=="Yes")))
(banp_in_person_val_avg = mean(subset(m8, Method == "BANP" & In_person=="Yes")$Amount))
(banp_in_person_val_med = median(subset(m8, Method == "BANP" & In_person=="Yes")$Amount))
(banp_remote_num = nrow(subset(m8, Method == "BANP" & In_person=="No")))
(banp_remote_frac = nrow(subset(m8, Method == "BANP" & In_person=="No"))/nrow(subset(m8, In_person=="No")))
(banp_remote_val_avg = mean(subset(m8, Method == "BANP" & In_person=="No")$Amount))
(banp_remote_val_med = median(subset(m8, Method == "BANP" & In_person=="No")$Amount))
(banp.vec = c(banp_num, 100*banp_frac, banp_val_avg, banp_val_med, banp_in_person_num, 100*banp_in_person_frac, banp_in_person_val_avg, banp_in_person_val_med, banp_remote_num, 100*banp_remote_frac, banp_remote_val_avg, banp_remote_val_med))
#
(obbp_num = nrow(subset(m8, Method == "OBBP")))
(obbp_frac = obbp_num/nrow(m8))
(obbp_val_avg = mean(subset(m8, Method == "OBBP")$Amount))
(obbp_val_med = median(subset(m8, Method == "OBBP")$Amount))
(obbp_in_person_num = nrow(subset(m8, Method == "OBBP" & In_person=="Yes")))
(obbp_in_person_frac = nrow(subset(m8, Method == "OBBP" & In_person=="Yes"))/nrow(subset(m8, In_person=="Yes")))
(obbp_in_person_val_avg = mean(subset(m8, Method == "OBBP" & In_person=="Yes")$Amount))
(obbp_in_person_val_med = median(subset(m8, Method == "OBBP" & In_person=="Yes")$Amount))
(obbp_remote_num = nrow(subset(m8, Method == "OBBP" & In_person=="No")))
(obbp_remote_frac = nrow(subset(m8, Method == "OBBP" & In_person=="No"))/nrow(subset(m8, In_person=="No")))
(obbp_remote_val_avg = mean(subset(m8, Method == "OBBP" & In_person=="No")$Amount))
(obbp_remote_val_med = median(subset(m8, Method == "OBBP" & In_person=="No")$Amount))
(obbp.vec = c(obbp_num, 100*obbp_frac, obbp_val_avg, obbp_val_med, obbp_in_person_num, 100*obbp_in_person_frac, obbp_in_person_val_avg, obbp_in_person_val_med, obbp_remote_num, 100*obbp_remote_frac, obbp_remote_val_avg, obbp_remote_val_med))
#
(acct2acct_num = nrow(subset(m8, Method == "Acct2acct")))
(acct2acct_frac = acct2acct_num/nrow(m8))
(acct2acct_val_avg = mean(subset(m8, Method == "Acct2acct")$Amount))
(acct2acct_val_med = median(subset(m8, Method == "Acct2acct")$Amount))
(acct2acct_in_person_num = nrow(subset(m8, Method == "Acct2acct" & In_person=="Yes")))
(acct2acct_in_person_frac = nrow(subset(m8, Method == "Acct2acct" & In_person=="Yes"))/nrow(subset(m8, In_person=="Yes")))
(acct2acct_in_person_val_avg = mean(subset(m8, Method == "Acct2acct" & In_person=="Yes")$Amount))
(acct2acct_in_person_val_med = median(subset(m8, Method == "Acct2acct" & In_person=="Yes")$Amount))
(acct2acct_remote_num = nrow(subset(m8, Method == "Acct2acct" & In_person=="No")))
(acct2acct_remote_frac = nrow(subset(m8, Method == "Acct2acct" & In_person=="No"))/nrow(subset(m8, In_person=="No")))
(acct2acct_remote_val_avg = mean(subset(m8, Method == "Acct2acct" & In_person=="No")$Amount))
(acct2acct_remote_val_med = median(subset(m8, Method == "Acct2acct" & In_person=="No")$Amount))
(acct2acct.vec = c(acct2acct_num, 100*acct2acct_frac, acct2acct_val_avg, acct2acct_val_med, acct2acct_in_person_num, 100*acct2acct_in_person_frac, acct2acct_in_person_val_avg, acct2acct_in_person_val_med, acct2acct_remote_num, 100*acct2acct_remote_frac, acct2acct_remote_val_avg, acct2acct_remote_val_med))
#
(debit_num = nrow(subset(m8, Method == "Debit")))
(debit_frac = debit_num/nrow(m8))
(debit_val_avg = mean(subset(m8, Method == "Debit")$Amount))
(debit_val_med = median(subset(m8, Method == "Debit")$Amount))
(debit_in_person_num = nrow(subset(m8, Method == "Debit" & In_person=="Yes")))
(debit_in_person_frac = nrow(subset(m8, Method == "Debit" & In_person=="Yes"))/nrow(subset(m8, In_person=="Yes")))
(debit_in_person_val_avg = mean(subset(m8, Method == "Debit" & In_person=="Yes")$Amount))
(debit_in_person_val_med = median(subset(m8, Method == "Debit" & In_person=="Yes")$Amount))
(debit_remote_num = nrow(subset(m8, Method == "Debit" & In_person=="No")))
(debit_remote_frac = nrow(subset(m8, Method == "Debit" & In_person=="No"))/nrow(subset(m8, In_person=="No")))
(debit_remote_val_avg = mean(subset(m8, Method == "Debit" & In_person=="No")$Amount))
(debit_remote_val_med = median(subset(m8, Method == "Debit" & In_person=="No")$Amount))
(debit.vec = c(debit_num, 100*debit_frac, debit_val_avg, debit_val_med, debit_in_person_num, 100*debit_in_person_frac, debit_in_person_val_avg, debit_in_person_val_med, debit_remote_num, 100*debit_remote_frac, debit_remote_val_avg, debit_remote_val_med))
#
(credit_num = nrow(subset(m8, Method == "Credit")))
(credit_frac = credit_num/nrow(m8))
(credit_val_avg = mean(subset(m8, Method == "Credit")$Amount))
(credit_val_med = median(subset(m8, Method == "Credit")$Amount))
(credit_in_person_num = nrow(subset(m8, Method == "Credit" & In_person=="Yes")))
(credit_in_person_frac = nrow(subset(m8, Method == "Credit" & In_person=="Yes"))/nrow(subset(m8, In_person=="Yes")))
(credit_in_person_val_avg = mean(subset(m8, Method == "Credit" & In_person=="Yes")$Amount))
(credit_in_person_val_med = median(subset(m8, Method == "Credit" & In_person=="Yes")$Amount))
(credit_remote_num = nrow(subset(m8, Method == "Credit" & In_person=="No")))
(credit_remote_frac = nrow(subset(m8, Method == "Credit" & In_person=="No"))/nrow(subset(m8, In_person=="No")))
(credit_remote_val_avg = mean(subset(m8, Method == "Credit" & In_person=="No")$Amount))
(credit_remote_val_med = median(subset(m8, Method == "Credit" & In_person=="No")$Amount))
(credit.vec = c(credit_num, 100*credit_frac, credit_val_avg, credit_val_med, credit_in_person_num, 100*credit_in_person_frac, credit_in_person_val_avg, credit_in_person_val_med, credit_remote_num, 100*credit_remote_frac, credit_remote_val_avg, credit_remote_val_med))
#
(prepaid_num = nrow(subset(m8, Method == "Prepaid")))
(prepaid_frac = prepaid_num/nrow(m8))
(prepaid_val_avg = mean(subset(m8, Method == "Prepaid")$Amount))
(prepaid_val_med = median(subset(m8, Method == "Prepaid")$Amount))
(prepaid_in_person_num = nrow(subset(m8, Method == "Prepaid" & In_person=="Yes")))
(prepaid_in_person_frac = nrow(subset(m8, Method == "Prepaid" & In_person=="Yes"))/nrow(subset(m8, In_person=="Yes")))
(prepaid_in_person_val_avg = mean(subset(m8, Method == "Prepaid" & In_person=="Yes")$Amount))
(prepaid_in_person_val_med = median(subset(m8, Method == "Prepaid" & In_person=="Yes")$Amount))
(prepaid_remote_num = nrow(subset(m8, Method == "Prepaid" & In_person=="No")))
(prepaid_remote_frac = nrow(subset(m8, Method == "Prepaid" & In_person=="No"))/nrow(subset(m8, In_person=="No")))
(prepaid_remote_val_avg = mean(subset(m8, Method == "Prepaid" & In_person=="No")$Amount))
(prepaid_remote_val_med = median(subset(m8, Method == "Prepaid" & In_person=="No")$Amount))
(prepaid.vec = c(prepaid_num, 100*prepaid_frac, prepaid_val_avg, prepaid_val_med, prepaid_in_person_num, 100*prepaid_in_person_frac, prepaid_in_person_val_avg, prepaid_in_person_val_med, prepaid_remote_num, 100*prepaid_remote_frac, prepaid_remote_val_avg, prepaid_remote_val_med))
#
(cash_num = nrow(subset(m8, Method == "Cash")))
(cash_frac = cash_num/nrow(m8))
(cash_val_avg = mean(subset(m8, Method == "Cash")$Amount))
(cash_val_med = median(subset(m8, Method == "Cash")$Amount))
(cash_in_person_num = nrow(subset(m8, Method == "Cash" & In_person=="Yes")))
(cash_in_person_frac = nrow(subset(m8, Method == "Cash" & In_person=="Yes"))/nrow(subset(m8, In_person=="Yes")))
(cash_in_person_val_avg = mean(subset(m8, Method == "Cash" & In_person=="Yes")$Amount))
(cash_in_person_val_med = median(subset(m8, Method == "Cash" & In_person=="Yes")$Amount))
(cash_remote_num = nrow(subset(m8, Method == "Cash" & In_person=="No")))
(cash_remote_frac = nrow(subset(m8, Method == "Cash" & In_person=="No"))/nrow(subset(m8, In_person=="No")))
(cash_remote_val_avg = mean(subset(m8, Method == "Cash" & In_person=="No")$Amount))
(cash_remote_val_med = median(subset(m8, Method == "Cash" & In_person=="No")$Amount))
(cash.vec = c(cash_num, 100*cash_frac, cash_val_avg, cash_val_med, cash_in_person_num, 100*cash_in_person_frac, cash_in_person_val_avg, cash_in_person_val_med, cash_remote_num, 100*cash_remote_frac, cash_remote_val_avg, cash_remote_val_med))
#
(check_num = nrow(subset(m8, Method == "Check")))
(check_frac = check_num/nrow(m8))
(check_val_avg = mean(subset(m8, Method == "Check")$Amount))
(check_val_med = median(subset(m8, Method == "Check")$Amount))
(check_in_person_num = nrow(subset(m8, Method == "Check" & In_person=="Yes")))
(check_in_person_frac = nrow(subset(m8, Method == "Check" & In_person=="Yes"))/nrow(subset(m8, In_person=="Yes")))
(check_in_person_val_avg = mean(subset(m8, Method == "Check" & In_person=="Yes")$Amount))
(check_in_person_val_med = median(subset(m8, Method == "Check" & In_person=="Yes")$Amount))
(check_remote_num = nrow(subset(m8, Method == "Check" & In_person=="No")))
(check_remote_frac = nrow(subset(m8, Method == "Check" & In_person=="No"))/nrow(subset(m8, In_person=="No")))
(check_remote_val_avg = mean(subset(m8, Method == "Check" & In_person=="No")$Amount))
(check_remote_val_med = median(subset(m8, Method == "Check" & In_person=="No")$Amount))
(check.vec = c(check_num, 100*check_frac, check_val_avg, check_val_med, check_in_person_num, 100*check_in_person_frac, check_in_person_val_avg, check_in_person_val_med, check_remote_num, 100*check_remote_frac, check_remote_val_avg, check_remote_val_med))
# All payments 
(all_num = nrow(m8))
(all_frac = all_num/nrow(m8))
(all_val_avg = mean(m8$Amount))
(all_val_med = median(m8$Amount))
(all_in_person_num = nrow(subset(m8, In_person=="Yes")))
(all_in_person_frac = nrow(subset(m8, In_person=="Yes"))/nrow(m8))
(all_in_person_val_avg = mean(subset(m8, In_person=="Yes")$Amount))
(all_in_person_val_med = median(subset(m8, In_person=="Yes")$Amount))
(all_remote_num = nrow(subset(m8, In_person=="No")))
(all_remote_frac = nrow(subset(m8, In_person=="No"))/nrow(m8))
(all_remote_val_avg = mean(subset(m8, In_person=="No")$Amount))
(all_remote_val_med = median(subset(m8, In_person=="No")$Amount))
(all.vec = c(all_num, 100*all_frac, all_val_avg, all_val_med, all_in_person_num, 100*all_in_person_frac, all_in_person_val_avg, all_in_person_val_med, all_remote_num, 100*all_remote_frac, all_remote_val_avg, all_remote_val_med))
#

# Finalizing Table 1 (stat by payment instrument)
(stat_var.vec = c("Number of payments", "Percentage (%)", "Avg. value ($)", "Med. value ($)", "Number of in-person", "In-person (%)", "In-person avg. value ($)", "In-person med. value ($)", "Number of remote", "Remote (%)", "Remote avg. value ($)", "Remote med. value ($)"))
(stat1.df = data.frame("Variable"=stat_var.vec, "BANP"=banp.vec, "OBBP"=obbp.vec, "Acct2acct"=acct2acct.vec, "Debit"=debit.vec, "Credit"=credit.vec, "Prepaid"=prepaid.vec, "Cash"=cash.vec, "Check"=check.vec, "All"=all.vec))
# verify that fractions sum up to 100%
sum(stat1.df[2, 2:9])
sum(stat1.df[6, 2:9])
sum(stat1.df[10, 2:9])

dim(stat1.df)
(digitm = matrix(c(rep(0,10+1), rep(1,10+1), rep(2,10+1), rep(2,10+1)  ), nrow = 12, ncol = 10+1, byrow = T))
dim(digitm)
#
print(xtable(stat1.df, digits = digitm), include.rownames = F, hline.after = c(0,4,8))
#
#caption Table 1
nrow(m8)# total num all payments
length(unique(m8$id))# num respondents

# Other variables described in Section 2:
m8_resp = m8[!duplicated(m8$id), ]# each respondent appears only once (to generate demographic stats)
dim(m8_resp)
names(m8_resp)
#
summary(m8$Amount)
summary(m8_resp$Age) 
summary(m8_resp$Gender)
100*round(prop.table(table(m8_resp$Gender)),3)
table(m8_resp$Marital)
100*round(prop.table(table(m8_resp$Marital)),3)
#summary(m8_resp$Work)
#round(prop.table(table(m8_resp$Work)),2)
table(m8_resp$Education)
100*round(prop.table(table(m8_resp$Education)),3)
summary(m8_resp$HH_income)
summary(m8_resp$HH_size)

### Start classification tree  Section 3.1
# defining regression model (classification tree)
names(m8)
method_model1 = Method~ Amount +In_person +Age +Gender +Marital +Education +HH_income +HH_size # +Work removed
#

# Tree on entire sample (not just training) to generate Fig.1 in paper, & Not tuning to Optimal tree cp, just to demonstrate. For confusion table, see below
set.seed(2)# to be able to reproduce the rpart CV below
method_tree1 = rpart(method_model1, data = m8, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
#Below, plot a tree (Note: Longer than optimal, but needed for later prunning and redrawing). 
prp(method_tree1, type = 3, box.palette = "auto", extra = 100, under = T, tweak = 1.0, varlen = 0, faclen = 0)#faclet=0 avoids abvreviations, tweak for char size
#now search for optimal cp, rpart has cp table built in
plotcp(method_tree1)# plot cp: Not used for this demo plot. See training data below
names(method_tree1)
method_tree1$cptable # List cp, number of splits and errors
# Below, I choose cp to use for prunning (highest rel error below the dashed line)
(cp.choice = method_tree1$cptable[5, "CP"]) # Corresponds to 6 splits (just for demonstration)
method_prune1 = prune.rpart(method_tree1, cp=cp.choice)
prp(method_prune1, type = 3, box.palette = "auto", legend.x=NA, legend.y=NA, extra = 100, under = T, tweak = 1.1, varlen = 0, faclen = 0, Margin = 0.0, digits = -2)#faclet=0 avoids abvreviations, tweak for char size

# caption Fig 1 (tree)
nrow(m8)
length(unique(m8$id))

# 
## Creating confusion matrix for the cp-optimized tree prediction
# Splitting data into training and testing (not needed for rpart b/c rpart uses CV)
dim(m8)
set.seed(1)
m8_index_train = sample(1:nrow(m8), size=0.8*nrow(m8), replace = F)
length(m8_index_train) 
m8_train = m8[m8_index_train, ]# 80% of diary
dim(m8_train)
m8_test = m8[-m8_index_train, ]# 20% of diary
dim(m8_test)
#
####rpart on train data 
method_tree1_train=rpart(method_model1, data = m8_train, method = "class", control = rpart.control(cp=0.001)) ## Extremely-long tree first, then prune it
#
plotcp(method_tree1_train)# plot cp
# The goal is to minimize xerror (cross-validation error). If no minimum, then...
#Pick the first cp where its cv error (xerror) + 1xstd is within the min (dashed line)
# any of those whose xerror value is within [min(xerror) - xstd; min(xerror) + xstd].
#Note, we don't use min CV error to avoid over fitting (b/c xerror is random)
# Another way (used here): Look at confusion matrices on the test data
#    as often there are tradeoffs among different variables w.r.t. min classification errors
# Here, I picked the smallest tree size just above the dashed line => 29 splits
method_tree1_train$cptable # List cp, number of splits and errors, pick 27 splits
#
method_prune1_train = prune.rpart(method_tree1_train, cp=0.0017)# prune trained model
#
method_prune1_pred=predict(method_prune1_train, newdata = m8_test, type="class")# predict test data
(method_prune1_table=table(method_prune1_pred, m8_test$Method, dnn = c("Predict","Actual")))# confusion table (top of Table 2, confusion tree)
# 
# General stats generated by this table (discussed in the paper)
dim(method_prune1_table)# be
# Table to be appended below the confusion table
(method_error1 = data.frame("BANP" = c(NA,NA,NA), "Acct2acct" = c(NA,NA,NA), "Cash" = c(NA,NA,NA), "Check" = c(NA,NA,NA), "Credit" = c(NA,NA,NA), "Debit" = c(NA,NA,NA), "OBBP" = c(NA,NA,NA), "Prepaid" = c(NA,NA,NA)))
row.names(method_error1) = c("Total actual", "Correct predictions", "Correct rate (%)")
# 1st row: Total actual
names(method_error1)
(method_error1[1, "BANP"] = length(m8_test$Method[m8_test$Method == "BANP"]))# Actual BANP trans
(method_error1[1, "Acct2acct"] = length(m8_test$Method[m8_test$Method == "Acct2acct"]))# Actual A2A trans
(method_error1[1, "Cash"] = length(m8_test$Method[m8_test$Method == "Cash"]))# Actual cash trans
(method_error1[1, "Check"] = length(m8_test$Method[m8_test$Method == "Check"]))# Actual Check trans
(method_error1[1, "Credit"] = length(m8_test$Method[m8_test$Method == "Credit"]))# Actual Credit trans
(method_error1[1, "Debit"] = length(m8_test$Method[m8_test$Method == "Debit"]))# Actual Debit trans
(method_error1[1, "OBBP"] = length(m8_test$Method[m8_test$Method == "OBBP"]))# Actual OBBP trans
(method_error1[1, "Prepaid"] = length(m8_test$Method[m8_test$Method == "Prepaid"]))# Actual Pcard trans

# 2nd row: No. correct predictions (%) [true positive rate]
names(method_error1)
(method_error1[2, "BANP"] =  method_prune1_table[1,1])
(method_error1[2, "Acct2acct"] =  method_prune1_table[2,2])
(method_error1[2, "Cash"] =  method_prune1_table[3,3])
(method_error1[2, "Check"] =  method_prune1_table[4,4])
(method_error1[2, "Credit"] =  method_prune1_table[5,5])
(method_error1[2, "Debit"] =  method_prune1_table[6,6])
(method_error1[2, "OBBP"] =  method_prune1_table[7,7])
(method_error1[2, "Prepaid"] =  method_prune1_table[8,8])

# 3rd row: Correct predictions (%) [true positive rate]
names(method_error1)
(method_error1[3, "BANP"] = 100*method_prune1_table[1,1]/method_error1[1, "BANP"])
(method_error1[3, "Acct2acct"] = 100*method_prune1_table[2,2]/method_error1[1, "Acct2acct"])
(method_error1[3, "Cash"] =  100*method_prune1_table[3,3]/method_error1[1, "Cash"])
(method_error1[3, "Check"] =  100*method_prune1_table[4,4]/method_error1[1, "Check"])
(method_error1[3, "Credit"] =  100*method_prune1_table[5,5]/method_error1[1, "Credit"])
(method_error1[3, "Debit"] =  100*method_prune1_table[6,6]/method_error1[1, "Debit"])
(method_error1[3, "OBBP"] =  100*method_prune1_table[7,7]/method_error1[1, "OBBP"])
(method_error1[3, "Prepaid"] =  100*method_prune1_table[8,8]/method_error1[1, "Prepaid"])
#
method_error1
dim(method_error1)
method_prune1_table
dim(method_prune1_table)
#
# Converting the above 2 tables to LaTeX:
xtable(method_prune1_table) # confusion table
xtable(method_error1, digits = 0) # Stats to append at the bottom of confusion table

#caption Table 2 (tree confusion)
nrow(m8_train)
length(unique(m8_train$id))
nrow(m8_test)
length(unique(m8_test$id))

### Start Multinomial regression section 3.2
method_multinom=multinom(method_model1, data = m8_train)
summary(method_multinom) #Coeff are not marginal effects (hard to do with multinom)
# 
# #Figuring out p-values (nnet multinom does not compute p-values)
# z=summary(method_multinom)$coefficients/summary(method_multinom)$standard.errors #z-values
# p=(1-pnorm(abs(z),0,1))*2 #2-tailed test p-values
# round(p, digits=4) #not used in the paper
#
# Start predictions on tesing data
method_demog_multi_pred = predict(method_multinom, m8_test, type = "class")# This predicts payment instruments
length(method_demog_multi_pred)
length(m8_test$Method)
head(method_demog_multi_pred)
# method_multinom_prob=predict(method_multinom, diary_test, type = "probs")# predicts probabilities of use
# method_multinom_prob=as.data.frame(method_multinom_pred)
# names(method_multinom_prob)
# head(method_multinom_prob)
# dim(method_multinom_prob)
#
# Creating confusion table for multilogit (middle part of Table 1)
method_demog_multi_table=table(method_demog_multi_pred, m8_test$Method, dnn = c("Predict","Actual"))
method_demog_multi_table # confusion table
dim(method_demog_multi_table)
#
# General stats for the middle part of Table 1 (multinomial)
# Goal, to present this table at the bottom of the multinomial confusion matrix
dim(method_demog_multi_table)# before the loop, need to know table dimension
method_demog_multi_correct=0# Initialize 
for(i in 1:8){method_demog_multi_correct= method_demog_multi_correct + method_demog_multi_table[i,i]}
# method_demog_multi_correct #correct prediction on the table (sum diagonal)
# method_demog_multi_correct/nrow(diary_test)#frac of correct predictions
# (method_demog_multi_table[1,1]+method_demog_multi_table[2,2])/
#   (method_demog_multi_table[1,1]+method_demog_multi_table[2,2]
#    +(method_demog_multi_table[2,1]+method_demog_multi_table[1,2]))#frac correct for cash and credit
#
# Table to be appended below the confusion table
(method_multinom_error1 = data.frame("BANP" = c(NA,NA,NA), "Acct2acct" = c(NA,NA,NA), "Cash" = c(NA,NA,NA), "Check" = c(NA,NA,NA), "Credit" = c(NA,NA,NA), "Debit" = c(NA,NA,NA), "OBBP" = c(NA,NA,NA), "Prepaid" = c(NA,NA,NA)))
row.names(method_multinom_error1) = c("Total actual", "Correct predictions", "Correct rate (%)")
# 1st row: Total actual
names(method_multinom_error1)
(method_multinom_error1[1, "BANP"] = length(m8_test$Method[m8_test$Method == "BANP"]))# Actual BANP trans
(method_multinom_error1[1, "Acct2acct"] = length(m8_test$Method[m8_test$Method == "Acct2acct"]))# Actual A2A trans
(method_multinom_error1[1, "Cash"] = length(m8_test$Method[m8_test$Method == "Cash"]))# Actual cash trans
(method_multinom_error1[1, "Check"] = length(m8_test$Method[m8_test$Method == "Check"]))# Actual Check trans
(method_multinom_error1[1, "Credit"] = length(m8_test$Method[m8_test$Method == "Credit"]))# Actual Credit trans
(method_multinom_error1[1, "Debit"] = length(m8_test$Method[m8_test$Method == "Debit"]))# Actual Debit trans
(method_multinom_error1[1, "OBBP"] = length(m8_test$Method[m8_test$Method == "OBBP"]))# Actual OBBP trans
(method_multinom_error1[1, "Prepaid"] = length(m8_test$Method[m8_test$Method == "Prepaid"]))# Actual Pcard trans

# 2nd row: No. correct predictions (%) [true positive rate]
names(method_multinom_error1)
(method_multinom_error1[2, "BANP"] =  method_demog_multi_table[1,1])
(method_multinom_error1[2, "Acct2acct"] =  method_demog_multi_table[2,2])
(method_multinom_error1[2, "Cash"] =  method_demog_multi_table[3,3])
(method_multinom_error1[2, "Check"] =  method_demog_multi_table[4,4])
(method_multinom_error1[2, "Credit"] =  method_demog_multi_table[5,5])
(method_multinom_error1[2, "Debit"] =  method_demog_multi_table[6,6])
(method_multinom_error1[2, "OBBP"] =  method_demog_multi_table[7,7])
(method_multinom_error1[2, "Prepaid"] =  method_demog_multi_table[8,8])

# 3rd row: Correct predictions (%) [true positive rate]
names(method_multinom_error1)
(method_multinom_error1[3, "BANP"] = 100*method_demog_multi_table[1,1]/method_multinom_error1[1, "BANP"])
(method_multinom_error1[3, "Acct2acct"] = 100*method_demog_multi_table[2,2]/method_multinom_error1[1, "Acct2acct"])
(method_multinom_error1[3, "Cash"] =  100*method_demog_multi_table[3,3]/method_multinom_error1[1, "Cash"])
(method_multinom_error1[3, "Check"] =  100*method_demog_multi_table[4,4]/method_multinom_error1[1, "Check"])
(method_multinom_error1[3, "Credit"] =  100*method_demog_multi_table[5,5]/method_multinom_error1[1, "Credit"])
(method_multinom_error1[3, "Debit"] =  100*method_demog_multi_table[6,6]/method_multinom_error1[1, "Debit"])
(method_multinom_error1[3, "OBBP"] =  100*method_demog_multi_table[7,7]/method_multinom_error1[1, "OBBP"])
(method_multinom_error1[3, "Prepaid"] =  100*method_demog_multi_table[8,8]/method_multinom_error1[1, "Prepaid"])
#
method_multinom_error1
dim(method_multinom_error1)
method_demog_multi_table
dim(method_demog_multi_table)
#
# Converting the above 2 tables to LaTeX:
xtable(method_demog_multi_table) # confusion table
xtable(method_multinom_error1, digits = 0) # Stats to append at the bottom of confusion table

#caption Table 3 (multi confusion)
nrow(m8_train)
length(unique(m8_train$id))
nrow(m8_test)
length(unique(m8_test$id))

### Start Random Forest section 3.3
method_model1# recall the regression model
#First, entire data is used for visualization, creating variable importance plot
method_on_demog_rf=randomForest(method_model1, data=m8, mtry=3, importance=T, na.action=na.roughfix)
importance(method_on_demog_rf) # Table of variable importance
# Below, Plot of variable importance (displayed in paper)
varImpPlot(method_on_demog_rf, type = 1, main ='' )#default type 1&2, 
#
#Now, use only training data to prepare prediction
method_on_demog_rf=randomForest(method_model1, data=m8_train, mtry=3, importance=T, na.action=na.roughfix) # no NAs in this code
#importance(method_on_demog_rf) # Table of variable importance
#varImpPlot(method_on_demog_rf) # Plot of variable importance (not displayed in paper)
#
#Start predictions and building confusion matrix (bottom of Table 1)
method_on_demog_rf_pred=predict(method_on_demog_rf, newdata = m8_test)# predict test data
(method_rf_table=table(method_on_demog_rf_pred, m8_test$Method, dnn = c("Predict","Actual")))
#

# Table to be appended below the confusion table
(method_rf_error1 = data.frame("BANP" = c(NA,NA,NA), "Acct2acct" = c(NA,NA,NA), "Cash" = c(NA,NA,NA), "Check" = c(NA,NA,NA), "Credit" = c(NA,NA,NA), "Debit" = c(NA,NA,NA), "OBBP" = c(NA,NA,NA), "Prepaid" = c(NA,NA,NA)))
row.names(method_rf_error1) = c("Total actual", "Correct predictions", "Correct rate (%)")
# 1st row: Total actual
names(method_rf_error1)
(method_rf_error1[1, "BANP"] = length(m8_test$Method[m8_test$Method == "BANP"]))# Actual BANP trans
(method_rf_error1[1, "Acct2acct"] = length(m8_test$Method[m8_test$Method == "Acct2acct"]))# Actual A2A trans
(method_rf_error1[1, "Cash"] = length(m8_test$Method[m8_test$Method == "Cash"]))# Actual cash trans
(method_rf_error1[1, "Check"] = length(m8_test$Method[m8_test$Method == "Check"]))# Actual Check trans
(method_rf_error1[1, "Credit"] = length(m8_test$Method[m8_test$Method == "Credit"]))# Actual Credit trans
(method_rf_error1[1, "Debit"] = length(m8_test$Method[m8_test$Method == "Debit"]))# Actual Debit trans
(method_rf_error1[1, "OBBP"] = length(m8_test$Method[m8_test$Method == "OBBP"]))# Actual OBBP trans
(method_rf_error1[1, "Prepaid"] = length(m8_test$Method[m8_test$Method == "Prepaid"]))# Actual Pcard trans

# 2nd row: No. correct predictions (%) [true positive rate]
names(method_rf_error1)
(method_rf_error1[2, "BANP"] =  method_rf_table[1,1])
(method_rf_error1[2, "Acct2acct"] =  method_rf_table[2,2])
(method_rf_error1[2, "Cash"] =  method_rf_table[3,3])
(method_rf_error1[2, "Check"] =  method_rf_table[4,4])
(method_rf_error1[2, "Credit"] =  method_rf_table[5,5])
(method_rf_error1[2, "Debit"] =  method_rf_table[6,6])
(method_rf_error1[2, "OBBP"] =  method_rf_table[7,7])
(method_rf_error1[2, "Prepaid"] =  method_rf_table[8,8])

# 3rd row: Correct predictions (%) [true positive rate]
names(method_rf_error1)
(method_rf_error1[3, "BANP"] = 100*method_rf_table[1,1]/method_rf_error1[1, "BANP"])
(method_rf_error1[3, "Acct2acct"] = 100*method_rf_table[2,2]/method_rf_error1[1, "Acct2acct"])
(method_rf_error1[3, "Cash"] =  100*method_rf_table[3,3]/method_rf_error1[1, "Cash"])
(method_rf_error1[3, "Check"] =  100*method_rf_table[4,4]/method_rf_error1[1, "Check"])
(method_rf_error1[3, "Credit"] =  100*method_rf_table[5,5]/method_rf_error1[1, "Credit"])
(method_rf_error1[3, "Debit"] =  100*method_rf_table[6,6]/method_rf_error1[1, "Debit"])
(method_rf_error1[3, "OBBP"] =  100*method_rf_table[7,7]/method_rf_error1[1, "OBBP"])
(method_rf_error1[3, "Prepaid"] =  100*method_rf_table[8,8]/method_rf_error1[1, "Prepaid"])
#
method_rf_error1
dim(method_rf_error1)
method_rf_table
dim(method_rf_table)
#
# Converting the above 2 tables to LaTeX:
xtable(method_rf_table) # confusion table
xtable(method_rf_error1, digits = 0) # Stats to append at the bottom of confusion table

### kNN begings Section 5 
# Create a vector of train data Method (needed for knn)
m8_train_method.vec = m8_train$Method # A vector of training method
length(m8_train_method.vec)
table(m8_train_method.vec)
m8_test_method.vec = m8_test$Method # A vector of test method
length(m8_test_method.vec)
table(m8_test_method.vec)

# Convert factor variables into numeric between 0 and 1 [kNN uses only numeric var]
names(m8_train)
table(m8_train$In_person)
m8_train$In_person_num = NULL
m8_train$In_person_num = ifelse(m8_train$In_person == "Yes", 1, 0)
table(m8_train$In_person_num)
table(m8_train$Marital)
m8_train$Marital_num = NULL
m8_train$Marital_num = ifelse(m8_train$Marital == "Married", 1, 0)
table(m8_train$Marital_num)
table(m8_train$Gender)
m8_train$Gender_num = NULL
m8_train$Gender_num = ifelse(m8_train$Gender == "Male", 1, 0)
table(m8_train$Gender_num)
table(m8_train$Education)
m8_train$Education_num = m8_train$Education
m8_train = m8_train %>% mutate(Education_num = recode(Education_num, "Elem_or_less"=0, "High_school"=1/3, "Assoc_or_college"=2/3, "MA_or_higher"=1))
table(m8_train$Education_num)# numeric 0, 1/3, 2/3, 1

# normalize numeric features for the kNN
names(m8_train)
m8_train$Amount_norm = nor(m8_train$Amount)
m8_train$Age_norm = nor(m8_train$Age)
m8_train$HH_size_norm = nor(m8_train$HH_size)
m8_train$HH_income_norm = nor(m8_train$HH_income)

names(m8_test)
table(m8_test$In_person)
m8_test$In_person_num = NULL
m8_test$In_person_num = ifelse(m8_test$In_person == "Yes", 1, 0)
table(m8_test$In_person_num)
table(m8_test$Marital)
m8_test$Marital_num = NULL
m8_test$Marital_num = ifelse(m8_test$Marital == "Married", 1, 0)
table(m8_test$Marital_num)
table(m8_test$Gender)
m8_test$Gender_num = NULL
m8_test$Gender_num = ifelse(m8_test$Gender == "Male", 1, 0)
table(m8_test$Gender_num)
table(m8_test$Education)
m8_test$Education_num = m8_test$Education
m8_test = m8_test %>% mutate(Education_num = recode(Education_num, "Elem_or_less"=0, "High_school"=1/3, "Assoc_or_college"=2/3, "MA_or_higher"=1))
table(m8_test$Education_num)# numeric 0, 1/3, 2/3, 1

# normalize numeric features for the kNN
names(m8_test)
m8_test$Amount_norm = nor(m8_test$Amount)
m8_test$Age_norm = nor(m8_test$Age)
m8_test$HH_size_norm = nor(m8_test$HH_size)
m8_test$HH_income_norm = nor(m8_test$HH_income)

# Recall model 1
method_model1 # for reference, kNN uses normalized and numeric var
# Reduce the number of features numerical valued only (also ignoring HH_size) 
# knn does not work with categorical features unless distance is meaningful
# Also, delete "Method" for which the above vectors will be used
knn_train=subset(m8_train, select = c("Amount_norm", "In_person_num", "Gender_num", "Marital_num", "Education_num", "HH_income_norm", "HH_size_norm"))
knn_test=subset(m8_test, select = c("Amount_norm", "In_person_num", "Gender_num", "Marital_num", "Education_num", "HH_income_norm", "HH_size_norm"))
head(knn_train, 10)
head(knn_test)
dim(knn_train)
dim(knn_test)
str(knn_train)
str(knn_test)
nrow(knn_train) + nrow(knn_test) # Sample size
length(m8_train_method.vec)
length(m8_test_method.vec)
length(m8_train_method.vec)+length(m8_test_method.vec)

### knn for all 8 payment instruments and all 8 features
# Constructing confusion matrix 
# knn starts here, first just "testing" for k=10 (non-optimal k)
# Note: 1st entry is training data, 2nd is test data, 3rd is classification vector (Cash, Dcard, Others)
knn_10 = knn(knn_train, knn_test, m8_train_method.vec, k = 10)
summary(knn_10)
tail(knn_10) # predicted
tail(m8_test_method.vec) # actual
sum(m8_test_method.vec != knn_10)/length(m8_test_method.vec) # high error rate (non-optimal k)
#
# Find the optimal (error-reducing) k
(knn_seq = rep(1,40)) # initialize before loop (k between 1 and 40)
for(i in 1:40) {
  ki = knn(knn_train, knn_test, m8_train_method.vec, k = i);
  knn_seq[i]=sum(m8_test_method.vec != ki)/length(m8_test_method.vec) # error rate
}
knn_seq # Error rates for each k between 1 and 40
min(knn_seq) # Find the smallest error rate
(kmin = which(knn_seq == min(knn_seq))) # gives the k corresonding to min error rate
#
knn_min = knn(knn_train, knn_test, m8_train_method.vec, k = kmin) # kNN w/ optimal k
#
sum(m8_test_method.vec != knn_min)/length(m8_test_method.vec) # lower error rate (optimal k)

# Start confusion matrix for knn w/ 8 payment methods (bottom of table 1)
(knn_table = table(knn_min, m8_test_method.vec, dnn = c("Predict","Actual")))
#


# Table to be appended below the confusion table
(method_knn_error1 = data.frame("BANP" = c(NA,NA,NA), "Acct2acct" = c(NA,NA,NA), "Cash" = c(NA,NA,NA), "Check" = c(NA,NA,NA), "Credit" = c(NA,NA,NA), "Debit" = c(NA,NA,NA), "OBBP" = c(NA,NA,NA), "Prepaid" = c(NA,NA,NA)))
row.names(method_knn_error1) = c("Total actual", "Correct predictions", "Correct rate (%)")
# 1st row: Total actual
names(method_knn_error1)
(method_knn_error1[1, "BANP"] = length(m8_test$Method[m8_test$Method == "BANP"]))# Actual BANP trans
(method_knn_error1[1, "Acct2acct"] = length(m8_test$Method[m8_test$Method == "Acct2acct"]))# Actual A2A trans
(method_knn_error1[1, "Cash"] = length(m8_test$Method[m8_test$Method == "Cash"]))# Actual cash trans
(method_knn_error1[1, "Check"] = length(m8_test$Method[m8_test$Method == "Check"]))# Actual Check trans
(method_knn_error1[1, "Credit"] = length(m8_test$Method[m8_test$Method == "Credit"]))# Actual Credit trans
(method_knn_error1[1, "Debit"] = length(m8_test$Method[m8_test$Method == "Debit"]))# Actual Debit trans
(method_knn_error1[1, "OBBP"] = length(m8_test$Method[m8_test$Method == "OBBP"]))# Actual OBBP trans
(method_knn_error1[1, "Prepaid"] = length(m8_test$Method[m8_test$Method == "Prepaid"]))# Actual Pcard trans

# 2nd row: No. correct predictions (%) [true positive rate]
names(method_knn_error1)
(method_knn_error1[2, "BANP"] =  knn_table[1,1])
(method_knn_error1[2, "Acct2acct"] =  knn_table[2,2])
(method_knn_error1[2, "Cash"] =  knn_table[3,3])
(method_knn_error1[2, "Check"] =  knn_table[4,4])
(method_knn_error1[2, "Credit"] =  knn_table[5,5])
(method_knn_error1[2, "Debit"] =  knn_table[6,6])
(method_knn_error1[2, "OBBP"] =  knn_table[7,7])
(method_knn_error1[2, "Prepaid"] =  knn_table[8,8])

# 3rd row: Correct predictions (%) [true positive rate]
names(method_knn_error1)
(method_knn_error1[3, "BANP"] = 100*knn_table[1,1]/method_knn_error1[1, "BANP"])
(method_knn_error1[3, "Acct2acct"] = 100*knn_table[2,2]/method_knn_error1[1, "Acct2acct"])
(method_knn_error1[3, "Cash"] =  100*knn_table[3,3]/method_knn_error1[1, "Cash"])
(method_knn_error1[3, "Check"] =  100*knn_table[4,4]/method_knn_error1[1, "Check"])
(method_knn_error1[3, "Credit"] =  100*knn_table[5,5]/method_knn_error1[1, "Credit"])
(method_knn_error1[3, "Debit"] =  100*knn_table[6,6]/method_knn_error1[1, "Debit"])
(method_knn_error1[3, "OBBP"] =  100*knn_table[7,7]/method_knn_error1[1, "OBBP"])
(method_knn_error1[3, "Prepaid"] =  100*knn_table[8,8]/method_knn_error1[1, "Prepaid"])
#
method_knn_error1
dim(method_knn_error1)
knn_table
dim(knn_table)
#
# Converting the above 2 tables to LaTeX:
xtable(knn_table) # confusion table
xtable(method_knn_error1, digits = 0) # Stats to append at the bottom of confusion table

## Graphical illustration of kNN w/ 2 features: In-house and amount, section~5.1
knn_train2=subset(m8_train, select = c("Amount_norm", "In_person_num"))
knn_test2=subset(m8_test, select = c("Amount_norm", "In_person_num"))
head(knn_train2)
head(knn_test2)
dim(knn_train2)
dim(knn_test2)
str(knn_train2)
str(knn_test2)
nrow(knn_train2) + nrow(knn_test2) # Sample size
length(m8_train_method.vec)
length(m8_test_method.vec)
length(m8_train_method.vec)+length(m8_test_method.vec)
# 
### knn for all 8 payment instruments restriced to 2 features: In-house and Amount
# Need to choose 2 numeric features to avoid drawings vertical lines at 0 and 1.
# Hence, look at a subset of In-person only
m8_per = subset(m8, In_person == "Yes")
dim(m8_per)
# add 2 var Amount_norm and HH_norm (normalized)
m8_per$Amount_norm = nor(m8_per$Amount)
m8_per$HH_income_norm = nor(m8_per$HH_income)
names(m8_per)

# Split training from testing. 
set.seed(1955)
m8_per_index_train = sample(1:nrow(m8_per), size=0.8*nrow(m8_per), replace = F)
length(m8_per_index_train) 
m8_per_train = m8_per[m8_per_index_train, ]# 80% of in-person
dim(m8_per_train)
m8_per_test = m8_per[-m8_per_index_train, ]# 20% of in-person
dim(m8_per_test)
# Method as a separate vector
m8_per_train_method.vec = m8_per_train$Method
length(m8_per_train_method.vec)
m8_per_test_method.vec = m8_per_test$Method
length(m8_per_test_method.vec)

# pick 2 numerical features: Amount and HH_income
names(m8_per_train)
m8_per_train2 = subset(m8_per_train, select = c(Amount_norm, HH_income_norm))
m8_per_test2 = subset(m8_per_test, select = c(Amount_norm, HH_income_norm))

# knn starts here, first just "testing" for k=10 (non-optimal k)
# Note: 1st entry is training data, 2nd is test data, 3rd is classification vector (Cash, Dcard, Others)
knn_2_10 = knn(m8_per_train2, m8_per_test2, m8_per_train_method.vec, k = 10)
summary(knn_2_10)
tail(knn_2_10) # predicted
tail(m8_per_test_method.vec) # actual
sum(m8_per_test_method.vec != knn_2_10)/length(m8_per_test_method.vec) # high error rate (non-optimal k)
#
set.seed(1955)
# Find the optimal (error-reducing) k
(knn_seq_2 = rep(1,40)) # initialize before loop (k between 1 and 40)
for(i in 1:40) {
  ki_2 = knn(m8_per_train2, m8_per_test2, m8_per_train_method.vec, k = i);
  knn_seq_2[i]=sum(m8_per_test_method.vec != ki_2)/length(m8_per_test_method.vec) # error rate
}
knn_seq_2 # Error rates for each k between 1 and 40
min(knn_seq_2) # Find the smallest error rate
(kmin_2 = which(knn_seq_2 == min(knn_seq_2))) # gives the k corresonding to min error rate
#
knn_2_min = knn(m8_per_train2, m8_per_test2, m8_per_train_method.vec, k = kmin_2[1]) # kNN w/ optimal k
sum(m8_per_test_method.vec != knn_2_min)/length(m8_per_test_method.vec) # higher overall error rate (only 2 features)

## start drawing predictions in 2-feature space
length(knn_2_min)# vector of predicted PI using test subsample
dim(knn_test2)
# now combine the above into a single df
knn_2.df = data.frame("Method" = knn_2_min, m8_per_test2)
dim(knn_2.df)
head(knn_2.df)
table(knn_2.df$Method)
# predicted on test data Figure 3 knn bottom 
dev.off()
ggplot(knn_2.df, aes(x=log(HH_income_norm), y=log(Amount_norm), shape=Method, color=Method)) + geom_point()+ xlab("Log of normalized household income")  + ylab("Log of normalized payment dollar amount")+ guides(color = guide_legend(override.aes = list(size = 4))) + theme(plot.title = element_text(size = 14, face = "bold"), legend.title=element_text(size=14), legend.text=element_text(size=14), legend.position = c(0.22, 0.2), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14)) 

# Now plot the actual test data in the same space
names(m8_per_test2)
m8_per_test_actual.df = data.frame("Method" = m8_per_test_method.vec, m8_per_test2)
head(m8_per_test_actual.df)
# removing a the few BANP, Acct2acct, OBBP, and Prepaid (should not be in-person)
m8_per_test_actual2.df = subset(m8_per_test_actual.df, Method %in% c("Cash", "Check", "Credit", "Debit"))
head(m8_per_test_actual2.df)
table(m8_per_test_actual2.df$Method)
#
# Actual on test data Figure 3 knn top
ggplot(m8_per_test_actual2.df, aes(x=log(HH_income_norm), y=log(Amount_norm), shape=Method, color=Method)) + geom_point() + xlab("Log of normalized household income")  + ylab("Log of normalized payment dollar amount")+ guides(color = guide_legend(override.aes = list(size = 4))) + theme(plot.title = element_text(size = 14, face = "bold"), legend.title=element_text(size=14), legend.text=element_text(size=14), legend.position = c(0.22, 0.2), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14)) 

# for the caption of figure 3 kNN
nrow(m8_per_train2)
nrow(m8_per_test2)
nrow(m8_per_train2)+nrow(m8_per_test2)

### Start Support Vector Machines section 6
## section 6.1: illustration with 2 features for in-person (same data used for KNN, but with 2 classifications)
# recall from kNN illustration (section 5.1)
names(m8_per_train)
names(m8_per_test)
dim(m8_per_train)
dim(m8_per_test)
dim(m8_per)
table(m8_per_train$Method)
#
m8_per_train3.df = subset(m8_per_train, select = c(Method, Amount, Age))
m8_per_test3.df = subset(m8_per_test, select = c(Method, Amount, Age))
dim(m8_per_train3.df)
dim(m8_per_test3.df)
#
# adding binary column 1 for cash, 0 non-cash
table(m8_per_train3.df$Method)
m8_per_train3.df$Cash_bin = ifelse(m8_per_train3.df$Method=="Cash", "Cash", "Non-cash")
m8_per_train3.df$Cash_bin = factor(m8_per_train3.df$Cash_bin)
table(m8_per_train3.df$Cash_bin)
table(m8_per_test3.df$Method)
m8_per_test3.df$Cash_bin = ifelse(m8_per_test3.df$Method=="Cash", "Cash", "Non-cash")
m8_per_test3.df$Cash_bin = factor(m8_per_test3.df$Cash_bin)
table(m8_per_test3.df$Cash_bin)
#
# Actually, the demo can use all in-person data (train+test) because we don't test for errors in this illustration
m8_per3.df = rbind(m8_per_train3.df, m8_per_test3.df)
dim(m8_per3.df)
names(m8_per3.df)
str(m8_per3.df)
#
svmdata1 = subset(m8_per3.df, select = c(-Method))
names(svmdata1)
str(svmdata1)
svmdata1 = subset(svmdata1, Amount<=40 & Age <= 80 )
dim(svmdata1)
str(svmdata1)
head(svmdata1)
# 
svmdata1_features.mat = matrix(c(svmdata1$Age, svmdata1$Amount) , ncol=2)
str(svmdata1_features.mat)
head(svmdata1_features.mat)
# 
svmdata1_cash_bin.vec = svmdata1$Cash_bin
table(svmdata1_cash_bin.vec)
# svmdata1_cash_bin2.vec = ifelse(svmdata1_cash_bin.vec=="Cash", 1, 0)
# table(svmdata1_cash_bin2.vec)
# str(svmdata1_cash_bin2.vec)

plot(svmdata1_features.mat , col=svmdata1_cash_bin.vec)# not needed because I plot the svmfit 

svmfit1 = svm(Cash_bin ~ Amount + Age, data = svmdata1, kernel = "radial", cost = 0.8, scale = FALSE) # Tuning implies cost =0.7. I use 0.8 for demonstration
svmfit1

# tuning the cost parameter (time consuming)
#svmfit_tune <- tune.svm(Cash_bin ~ Amount + Age, data = svmdata1, cost = seq(0.5,1,0.1), kernel = "radial") # yields cost=0.7
#svmfit_tune <- tune.svm(Cash_bin ~ Amount + Age, data = svmdata1, cost = seq(0.64,0.76,0.02), kernel = "radial") # yields cost=0.7
#svmfit_tune
#par(mar=c(4,4,0,0))
plot(svmfit1, svmdata1, symbolPalette = c("black", "red"), svSymbol = "*", dataSymbol = "*", col=c("grey", "white"))# need remove title with Acrobat

#Confusion matrix to check the accuracy (also caption). 
nrow(svmdata1)# num payments
table(svmdata1$Cash_bin) # num cash payments
table(predicted=svmfit1$fitted, actual=svmdata1$Cash_bin)
# accuracy rate
100*prop.table(table(svmfit1$fitted== svmdata1$Cash_bin))

##### THE END OF diary_ml_xxxxxx.R 


