# merch_210204.R replacing id
# merch_200619.R adding 2x8 regressions of %PI (vol and val) on 3 vars, line 
# merch_200618.R adding %-in-person and %-remote to Table 3. Regressing HHI line 918 (results failed, low correlations)
# merch_200617.R adding some analysis, starting line 870
# merch_200515.R corresponds to merch-20.tex submitted to Nancy C for RDR
# merch_200430.R adding total (remote+in-person), removing some of the separated in-person from remote. 
# merch_200419.R sent to Claire (suggested not to separate in-person from remote). Typed on Merch-9.tex
# merch_200322.R Start: How merchants get paid. 
# mobile_200228.R start Mobile Payments and Financial Inclusion
# Packages used for this coding: 
library(dplyr)
library(tidyr)# function "spread()" OUTDATED pivot_longer does not work
library(plyr)# for "join" function
#library(formattable)# formats tables to allow trailing zeros (w/ numeric format)
#library(plotrix)# weighted histograms
library(spatstat)# for weighted median
#library(lubridate) # extracts day of the week
library(ggplot2); theme_set(theme_bw())
#library(rpart)
#library(rpart.plot)
#library(partykit)# modifies rpart tree plot
library("xtable") #exporting to LaTeX
#library(mfx) # logit regressions with marginal effects
#
setwd("~/Papers/Papers_inactive/Merch/merch_coding")# Set working directory on your computer
# Your working directory would be different from the above!
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
table(m1$in_person)
table(m1$merch)
table(m1$type)
table(m1$bill)
#
# Mobile-related payments
names(select(m1, contains("mobile")))
table(m1$mobile_funding)
sum(table(m1$mobile_funding))
table(m1$mobile_method)# ==> look at payment_app
table(m1$mobile_whichapp)

#
# demographic/individual variables
summary(m1$Age)
table(m1$HH_size)
summary(m1$HH_income)
table(m1$Work)
table(m1$Gender)
table(m1$Education)
table(m1$Marital)
# csh_adopt, chk_acnt_adopt, sav_acnt_adopt,  bnk_acnt_adopt, chk_adopt, mon_adopt, dc_adopt, cc_adopt, svc_adopt, obbp_adopt,  banp_adopt, income_adopt, abp_adopt
#

## Restricting to payments only
m2 = subset(m1, type=="Payment")
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

## Vector of 21 merchant names (Table 1)
(merch1_name = "Grocery stores, convenience stores without gas stations, pharmacies")
(merch2_name = "Gas stations")
(merch3_name = "Sit-down restaurants and bars")
(merch4_name = "Fast food restaurants, coffee shops, cafeterias, food trucks")
(merch5_name = "General merchandise stores, department stores, other stores, online shopping")
(merch6_name = "General services: hair dressers, auto repair, parking lots, laundry or dry cleaning, etc.")
(merch7_name = "Arts, entertainment, recreation")
(merch8_name = "Utilities not paid to the government: electricity, natural gas, water, sewer, trash, heating oil")
(merch9_name = "Taxis, airplanes, delivery")
(merch10_name = "Telephone, internet, cable or satellite TV, video or music streaming services, movie theaters")
(merch11_name = "Building contractors, plumbers, electricians, HVAC, etc.")
(merch12_name = "Professional services: legal, accounting, architectural services; veterinarians, photographers or photo
processers")
(merch13_name = "Hotels, motels, RV parks, campsites")
(merch14_name = "Rent for apartments, homes, or other buildings, real estate companies, property managers, etc.")
(merch15_name = "Mortgage companies, credit card companies, banks, insurance companies, stock brokers, IRA funds, mutual funds, credit unions, sending remittances")
(merch16_name = "Can be a gift or repayment to a family member, friend, or co-worker. Can be a payment to somebody who did a small job for you.")
(merch17_name = "Charitable or religious donations")
(merch18_name = "Hospital, doctor, dentist, nursing homes, etc.")
(merch19_name = "Government taxes or fees")
(merch20_name = "Schools, colleges, childcare centers")
(merch21_name = "Public transportation and tolls")
# Make names a vector
(merch_name.vec = c(merch1_name, merch2_name, merch3_name, merch4_name, merch5_name, merch6_name, merch7_name, merch8_name, merch9_name, merch10_name, merch11_name, merch12_name, merch13_name, merch14_name, merch15_name, merch16_name, merch17_name, merch18_name, merch19_name, merch20_name, merch21_name))
# Finanlizing merchant name table
merch_num.vec = 1:21
(merch_name.df = data.frame(merch_num.vec, merch_name.vec))
dim(merch_name.df)
# finalizing Table merchant names (Table 1)
# below, create matrix w\ 1 extra column to indicate number of digits for each row
#(digitm = matrix(c(rep(1,6+1), rep(1,6+1), rep(2,6+1) , rep(2,6+1) , rep(2,6+1) , rep(2,6+1), rep(1,6+1), rep(1,6+1), rep(1,6+1), rep(1,6+1), rep(1,6+1), rep(1,6+1), rep(1,6+1)), nrow = 13, ncol = 6+1, byrow = T))
#
#print(xtable(use.df, digits = digitm), include.rownames = F, hline.after = c(0,2,6,13))
print(xtable(merch_name.df), include.rownames = F, hline.after = c(0))

### Abreviating Table 1's 21 merchant description (from Brian's slides)
# to be used in tables (see below further merchX_abv_fig for figures)
(merch1_abv = "1. Grocery store")
(merch2_abv = "2. Gas station")
(merch3_abv = "3. Restaurant/bar")
(merch4_abv = "4. Fast food/coffee shop")
(merch5_abv = "5. General merchandise store")
(merch6_abv = "6. General service")
(merch7_abv = "7. Art/entertainment")
(merch8_abv = "8. Non-government utility")
(merch9_abv = "9. Taxi/airplane/delivery")
(merch10_abv = "10. Phone/internet/cable")
(merch11_abv = "11. Contractor/plumber/ electrician")
(merch12_abv = "12. Professional service")
(merch13_abv = "13. Hotel/motel/campsite")
(merch14_abv = "14. Rent")
(merch15_abv = "15. Mortgage/insurance/credit card")
(merch16_abv = "16. Person-to-person")
(merch17_abv = "17. Charitable/religious donation")
(merch18_abv = "18. Hospital/doctor/dentist")
(merch19_abv = "19. Government taxes")
(merch20_abv = "20. School/college/childcare centers")
(merch21_abv = "21. Public transport/tolls")
# Make names a vector
(merch_abv.vec = c(merch1_abv, merch2_abv, merch3_abv, merch4_abv, merch5_abv, merch6_abv, merch7_abv, merch8_abv, merch9_abv, merch10_abv, merch11_abv, merch12_abv, merch13_abv, merch14_abv, merch15_abv, merch16_abv, merch17_abv, merch18_abv, merch19_abv, merch20_abv, merch21_abv))

# adding merch abv (shorter for figures) description
(merch1_abv_fig = "1. Grocery store")
(merch2_abv_fig = "2. Gas station")
(merch3_abv_fig = "3. Restaurant/bar")
(merch4_abv_fig = "4. Fast food/coffee shop")
(merch5_abv_fig = "5. General merchandise")
(merch6_abv_fig = "6. General service")
(merch7_abv_fig = "7. Art/entertainment")
(merch8_abv_fig = "8. Non-gov't utility")
(merch9_abv_fig = "9. Taxi/airplane/delivery")
(merch10_abv_fig = "10. Phone/internet/cable")
(merch11_abv_fig = "11. Contractor")
(merch12_abv_fig = "12. Professional service")
(merch13_abv_fig = "13. Hotel/motel/campsite")
(merch14_abv_fig = "14. Rent")
(merch15_abv_fig = "15. Mortg/insur/credit c")
(merch16_abv_fig = "16. Person-to-person")
(merch17_abv_fig = "17. Charitable/religious")
(merch18_abv_fig = "18. Hospital/doctor/dentist")
(merch19_abv_fig = "19. Government taxes")
(merch20_abv_fig = "20. Education/childcare")
(merch21_abv_fig = "21. Public transport/tolls")
# Make names a vector
(merch_abv_fig.vec = c(merch1_abv_fig, merch2_abv_fig, merch3_abv_fig, merch4_abv_fig, merch5_abv_fig, merch6_abv_fig, merch7_abv_fig, merch8_abv_fig, merch9_abv_fig, merch10_abv_fig, merch11_abv_fig, merch12_abv_fig, merch13_abv_fig, merch14_abv_fig, merch15_abv_fig, merch16_abv_fig, merch17_abv_fig, merch18_abv_fig, merch19_abv_fig, merch20_abv_fig, merch21_abv_fig))

## subsetting payments by 21 merchant types
# check for missing merchant types
m3[is.na(m3$merch), "merch"]
dim(m3)
m4 = m3[!is.na(m3$merch), ]# remove few NA merch
dim(m4)

table(m4$in_person)
sum(table(m4$in_person))

# before subsetting, create 2 sets of scaled weights: in_person and remote
m4_in_person = subset(m4, in_person == "Yes")
nrow(m4_in_person)
(sum(m4_in_person$ind_weight))
m4$weight_in_person = m4$ind_weight*nrow(m4_in_person)/sum(m4_in_person$ind_weight)
sum(subset(m4, in_person=="Yes")$weight_in_person)# verify equals nrow in person
#
m4_remote = subset(m4, in_person == "No")
nrow(m4_remote)
(sum(m4_remote$ind_weight))
m4$weight_remote = m4$ind_weight*nrow(m4_remote)/sum(m4_remote$ind_weight)
sum(subset(m4, in_person=="No")$weight_remote)# verify equals nrow remote
#
table(m4$merch)
str(m4$merch)
m4_merch1 = subset(m4, merch==1)
m4_merch2 = subset(m4, merch==2)
m4_merch3 = subset(m4, merch==3)
m4_merch4 = subset(m4, merch==4)
m4_merch5 = subset(m4, merch==5)
m4_merch6 = subset(m4, merch==6)
m4_merch7 = subset(m4, merch==7)
m4_merch8 = subset(m4, merch==8)
m4_merch9 = subset(m4, merch==9)
m4_merch10 = subset(m4, merch==10)
m4_merch11 = subset(m4, merch==11)
m4_merch12 = subset(m4, merch==12)
m4_merch13 = subset(m4, merch==13)
m4_merch14 = subset(m4, merch==14)
m4_merch15 = subset(m4, merch==15)
m4_merch16 = subset(m4, merch==16)
m4_merch17 = subset(m4, merch==17)
m4_merch18 = subset(m4, merch==18)
m4_merch19 = subset(m4, merch==19)
m4_merch20 = subset(m4, merch==20)
m4_merch21 = subset(m4, merch==21)

### Start new Table 2: Sample stat & HH income for ALL: in-person and remote combined
# vector of Volume by merchant
(merch_vol.vec = c(nrow(m4_merch1), nrow(m4_merch2), nrow(m4_merch3), nrow(m4_merch4), nrow(m4_merch5), nrow(m4_merch6), nrow(m4_merch7), nrow(m4_merch8), nrow(m4_merch9), nrow(m4_merch10), nrow(m4_merch11), nrow(m4_merch12), nrow(m4_merch13), nrow(m4_merch14), nrow(m4_merch15), nrow(m4_merch16), nrow(m4_merch17), nrow(m4_merch18), nrow(m4_merch19), nrow(m4_merch20), nrow(m4_merch21)))

## vector of average-value by merchant
(merch_avg_val.vec = c(mean(m4_merch1$Amount), mean(m4_merch2$Amount), mean(m4_merch3$Amount), mean(m4_merch4$Amount), mean(m4_merch5$Amount), mean(m4_merch6$Amount), mean(m4_merch7$Amount), mean(m4_merch8$Amount), mean(m4_merch9$Amount), mean(m4_merch10$Amount), mean(m4_merch11$Amount), mean(m4_merch12$Amount), mean(m4_merch13$Amount), mean(m4_merch14$Amount), mean(m4_merch15$Amount), mean(m4_merch16$Amount), mean(m4_merch17$Amount), mean(m4_merch18$Amount), mean(m4_merch19$Amount), mean(m4_merch20$Amount), mean(m4_merch21$Amount)))

## vector of median-value by merchant
(merch_med_val.vec = c(median(m4_merch1$Amount), median(m4_merch2$Amount), median(m4_merch3$Amount), median(m4_merch4$Amount), median(m4_merch5$Amount), median(m4_merch6$Amount), median(m4_merch7$Amount), median(m4_merch8$Amount), median(m4_merch9$Amount), median(m4_merch10$Amount), median(m4_merch11$Amount), median(m4_merch12$Amount), median(m4_merch13$Amount), median(m4_merch14$Amount), median(m4_merch15$Amount), median(m4_merch16$Amount), median(m4_merch17$Amount), median(m4_merch18$Amount), median(m4_merch19$Amount), median(m4_merch20$Amount), median(m4_merch21$Amount)))

## Median buyer income by merchant type
names(select(m4, contains("income")))
(merch_med_income.vec = c(median(m4_merch1$HH_income, na.rm = T), median(m4_merch2$HH_income, na.rm = T), median(m4_merch3$HH_income, na.rm = T), median(m4_merch4$HH_income, na.rm = T), median(m4_merch5$HH_income, na.rm = T), median(m4_merch6$HH_income, na.rm = T), median(m4_merch7$HH_income, na.rm = T), median(m4_merch8$HH_income, na.rm = T), median(m4_merch9$HH_income, na.rm = T), median(m4_merch10$HH_income, na.rm = T), median(m4_merch11$HH_income, na.rm = T), median(m4_merch12$HH_income, na.rm = T), median(m4_merch13$HH_income, na.rm = T), median(m4_merch14$HH_income, na.rm = T), median(m4_merch15$HH_income, na.rm = T), median(m4_merch16$HH_income, na.rm = T), median(m4_merch17$HH_income, na.rm = T), median(m4_merch18$HH_income, na.rm = T), median(m4_merch19$HH_income, na.rm = T), median(m4_merch20$HH_income, na.rm = T), median(m4_merch21$HH_income, na.rm = T )))
# make it weighted
(merch_med_income_w.vec = c(weighted.median(m4_merch1$HH_income, m4_merch1$ind_weight), weighted.median(m4_merch2$HH_income, m4_merch2$ind_weight), weighted.median(m4_merch3$HH_income, m4_merch3$ind_weight), weighted.median(m4_merch4$HH_income, m4_merch4$ind_weight), weighted.median(m4_merch5$HH_income, m4_merch5$ind_weight), weighted.median(m4_merch6$HH_income, m4_merch6$ind_weight), weighted.median(m4_merch7$HH_income, m4_merch7$ind_weight), weighted.median(m4_merch8$HH_income, m4_merch8$ind_weight), weighted.median(m4_merch9$HH_income, m4_merch9$ind_weight), weighted.median(m4_merch10$HH_income, m4_merch10$ind_weight), weighted.median(m4_merch11$HH_income, m4_merch11$ind_weight), weighted.median(m4_merch12$HH_income, m4_merch12$ind_weight), weighted.median(m4_merch13$HH_income, m4_merch13$ind_weight), weighted.median(m4_merch14$HH_income, m4_merch14$ind_weight), weighted.median(m4_merch15$HH_income, m4_merch15$ind_weight), weighted.median(m4_merch16$HH_income, m4_merch16$ind_weight), weighted.median(m4_merch17$HH_income, m4_merch17$ind_weight), weighted.median(m4_merch18$HH_income, m4_merch18$ind_weight), weighted.median(m4_merch19$HH_income, m4_merch19$ind_weight), weighted.median(m4_merch20$HH_income, m4_merch20$ind_weight), weighted.median(m4_merch21$HH_income, m4_merch21$ind_weight)))

# Preparing Table on basic stats by merchant type
# Reduced table stats below (removing vol(w) from both in-person and remote), and renaming vol to obs (number of observations). For val and income, display (w) only
# Displaying unweighted only, except for HH INcome
(merch_all_stat3.df = data.frame("Merchant"=merch_abv.vec, "Obs"=merch_vol.vec, "Avg.Val"=merch_avg_val.vec, "Med.Val"=merch_med_val.vec, "Med.Income"=merch_med_income.vec, "Med.Income.w"=merch_med_income_w.vec))
dim(merch_all_stat3.df)
print(xtable(merch_all_stat3.df, digits = 0), include.rownames = F, hline.after = c(0:21))

# Table 2 caption
length(unique(m4$id))
nrow(m4)# 
sum(merch_vol.vec)# check sum of obs in Table 2

### Started separating in-person vs. remote for Table 3 

table(m4$in_person)
m4_merch1_in_person = subset(m4_merch1, in_person=="Yes")
m4_merch2_in_person = subset(m4_merch2, in_person=="Yes")
m4_merch3_in_person = subset(m4_merch3, in_person=="Yes")
m4_merch4_in_person = subset(m4_merch4, in_person=="Yes")
m4_merch5_in_person = subset(m4_merch5, in_person=="Yes")
m4_merch6_in_person = subset(m4_merch6, in_person=="Yes")
m4_merch7_in_person = subset(m4_merch7, in_person=="Yes")
m4_merch8_in_person = subset(m4_merch8, in_person=="Yes")
m4_merch9_in_person = subset(m4_merch9, in_person=="Yes")
m4_merch10_in_person = subset(m4_merch10, in_person=="Yes")
m4_merch11_in_person = subset(m4_merch11, in_person=="Yes")
m4_merch12_in_person = subset(m4_merch12, in_person=="Yes")
m4_merch13_in_person = subset(m4_merch13, in_person=="Yes")
m4_merch14_in_person = subset(m4_merch14, in_person=="Yes")
m4_merch15_in_person = subset(m4_merch15, in_person=="Yes")
m4_merch16_in_person = subset(m4_merch16, in_person=="Yes")
m4_merch17_in_person = subset(m4_merch17, in_person=="Yes")
m4_merch18_in_person = subset(m4_merch18, in_person=="Yes")
m4_merch19_in_person = subset(m4_merch19, in_person=="Yes")
m4_merch20_in_person = subset(m4_merch20, in_person=="Yes")
m4_merch21_in_person = subset(m4_merch21, in_person=="Yes")
#

## now NOT in person
table(m4$in_person)
m4_merch1_remote = subset(m4_merch1, in_person=="No")
m4_merch2_remote = subset(m4_merch2, in_person=="No")
m4_merch3_remote = subset(m4_merch3, in_person=="No")
m4_merch4_remote = subset(m4_merch4, in_person=="No")
m4_merch5_remote = subset(m4_merch5, in_person=="No")
m4_merch6_remote = subset(m4_merch6, in_person=="No")
m4_merch7_remote = subset(m4_merch7, in_person=="No")
m4_merch8_remote = subset(m4_merch8, in_person=="No")
m4_merch9_remote = subset(m4_merch9, in_person=="No")
m4_merch10_remote = subset(m4_merch10, in_person=="No")
m4_merch11_remote = subset(m4_merch11, in_person=="No")
m4_merch12_remote = subset(m4_merch12, in_person=="No")
m4_merch13_remote = subset(m4_merch13, in_person=="No")
m4_merch14_remote = subset(m4_merch14, in_person=="No")
m4_merch15_remote = subset(m4_merch15, in_person=="No")
m4_merch16_remote = subset(m4_merch16, in_person=="No")
m4_merch17_remote = subset(m4_merch17, in_person=="No")
m4_merch18_remote = subset(m4_merch18, in_person=="No")
m4_merch19_remote = subset(m4_merch19, in_person=="No")
m4_merch20_remote = subset(m4_merch20, in_person=="No")
m4_merch21_remote = subset(m4_merch21, in_person=="No")

## vector of in person Volume by merchant
(merch_in_person_vol.vec = c(nrow(m4_merch1_in_person), nrow(m4_merch2_in_person), nrow(m4_merch3_in_person), nrow(m4_merch4_in_person), nrow(m4_merch5_in_person), nrow(m4_merch6_in_person), nrow(m4_merch7_in_person), nrow(m4_merch8_in_person), nrow(m4_merch9_in_person), nrow(m4_merch10_in_person), nrow(m4_merch11_in_person), nrow(m4_merch12_in_person), nrow(m4_merch13_in_person), nrow(m4_merch14_in_person), nrow(m4_merch15_in_person), nrow(m4_merch16_in_person), nrow(m4_merch17_in_person), nrow(m4_merch18_in_person), nrow(m4_merch19_in_person), nrow(m4_merch20_in_person), nrow(m4_merch21_in_person)))

## vector of in person fraction Volume by merchant
(merch_in_person_vol_frac.vec = c(nrow(m4_merch1_in_person)/nrow(m4_merch1), nrow(m4_merch2_in_person)/nrow(m4_merch2), nrow(m4_merch3_in_person)/nrow(m4_merch3), nrow(m4_merch4_in_person)/nrow(m4_merch4), nrow(m4_merch5_in_person)/nrow(m4_merch5), nrow(m4_merch6_in_person)/nrow(m4_merch6), nrow(m4_merch7_in_person)/nrow(m4_merch7), nrow(m4_merch8_in_person)/nrow(m4_merch8), nrow(m4_merch9_in_person)/nrow(m4_merch9), nrow(m4_merch10_in_person)/nrow(m4_merch10), nrow(m4_merch11_in_person)/nrow(m4_merch11), nrow(m4_merch12_in_person)/nrow(m4_merch12), nrow(m4_merch13_in_person)/nrow(m4_merch13), nrow(m4_merch14_in_person)/nrow(m4_merch14), nrow(m4_merch15_in_person)/nrow(m4_merch15), nrow(m4_merch16_in_person)/nrow(m4_merch16), nrow(m4_merch17_in_person)/nrow(m4_merch17), nrow(m4_merch18_in_person)/nrow(m4_merch18), nrow(m4_merch19_in_person)/nrow(m4_merch19), nrow(m4_merch20_in_person)/nrow(m4_merch20), nrow(m4_merch21_in_person)/nrow(m4_merch21)))

## vector of remote Volume by merchant
(merch_remote_vol.vec = c(nrow(m4_merch1_remote), nrow(m4_merch2_remote), nrow(m4_merch3_remote), nrow(m4_merch4_remote), nrow(m4_merch5_remote), nrow(m4_merch6_remote), nrow(m4_merch7_remote), nrow(m4_merch8_remote), nrow(m4_merch9_remote), nrow(m4_merch10_remote), nrow(m4_merch11_remote), nrow(m4_merch12_remote), nrow(m4_merch13_remote), nrow(m4_merch14_remote), nrow(m4_merch15_remote), nrow(m4_merch16_remote), nrow(m4_merch17_remote), nrow(m4_merch18_remote), nrow(m4_merch19_remote), nrow(m4_merch20_remote), nrow(m4_merch21_remote)))

## vector of remote fraction Volume by merchant
(merch_remote_vol_frac.vec = c(nrow(m4_merch1_remote)/nrow(m4_merch1), nrow(m4_merch2_remote)/nrow(m4_merch2), nrow(m4_merch3_remote)/nrow(m4_merch3), nrow(m4_merch4_remote)/nrow(m4_merch4), nrow(m4_merch5_remote)/nrow(m4_merch5), nrow(m4_merch6_remote)/nrow(m4_merch6), nrow(m4_merch7_remote)/nrow(m4_merch7), nrow(m4_merch8_remote)/nrow(m4_merch8), nrow(m4_merch9_remote)/nrow(m4_merch9), nrow(m4_merch10_remote)/nrow(m4_merch10), nrow(m4_merch11_remote)/nrow(m4_merch11), nrow(m4_merch12_remote)/nrow(m4_merch12), nrow(m4_merch13_remote)/nrow(m4_merch13), nrow(m4_merch14_remote)/nrow(m4_merch14), nrow(m4_merch15_remote)/nrow(m4_merch15), nrow(m4_merch16_remote)/nrow(m4_merch16), nrow(m4_merch17_remote)/nrow(m4_merch17), nrow(m4_merch18_remote)/nrow(m4_merch18), nrow(m4_merch19_remote)/nrow(m4_merch19), nrow(m4_merch20_remote)/nrow(m4_merch20), nrow(m4_merch21_remote)/nrow(m4_merch21)))

# verify that in-person + remote frac = 1
merch_in_person_vol_frac.vec + merch_remote_vol_frac.vec

## vector of in_person average-value by merchant
(merch_in_person_avg_val.vec = c(mean(m4_merch1_in_person$Amount), mean(m4_merch2_in_person$Amount), mean(m4_merch3_in_person$Amount), mean(m4_merch4_in_person$Amount), mean(m4_merch5_in_person$Amount), mean(m4_merch6_in_person$Amount), mean(m4_merch7_in_person$Amount), mean(m4_merch8_in_person$Amount), mean(m4_merch9_in_person$Amount), mean(m4_merch10_in_person$Amount), mean(m4_merch11_in_person$Amount), mean(m4_merch12_in_person$Amount), mean(m4_merch13_in_person$Amount), mean(m4_merch14_in_person$Amount), mean(m4_merch15_in_person$Amount), mean(m4_merch16_in_person$Amount), mean(m4_merch17_in_person$Amount), mean(m4_merch18_in_person$Amount), mean(m4_merch19_in_person$Amount), mean(m4_merch20_in_person$Amount), mean(m4_merch21_in_person$Amount)))

## vector of remote average-value by merchant
(merch_remote_avg_val.vec = c(mean(m4_merch1_remote$Amount), mean(m4_merch2_remote$Amount), mean(m4_merch3_remote$Amount), mean(m4_merch4_remote$Amount), mean(m4_merch5_remote$Amount), mean(m4_merch6_remote$Amount), mean(m4_merch7_remote$Amount), mean(m4_merch8_remote$Amount), mean(m4_merch9_remote$Amount), mean(m4_merch10_remote$Amount), mean(m4_merch11_remote$Amount), mean(m4_merch12_remote$Amount), mean(m4_merch13_remote$Amount), mean(m4_merch14_remote$Amount), mean(m4_merch15_remote$Amount), mean(m4_merch16_remote$Amount), mean(m4_merch17_remote$Amount), mean(m4_merch18_remote$Amount), mean(m4_merch19_remote$Amount), mean(m4_merch20_remote$Amount), mean(m4_merch21_remote$Amount)))

## vector of in in_person median-value by merchant
(merch_in_person_med_val.vec = c(median(m4_merch1_in_person$Amount), median(m4_merch2_in_person$Amount), median(m4_merch3_in_person$Amount), median(m4_merch4_in_person$Amount), median(m4_merch5_in_person$Amount), median(m4_merch6_in_person$Amount), median(m4_merch7_in_person$Amount), median(m4_merch8_in_person$Amount), median(m4_merch9_in_person$Amount), median(m4_merch10_in_person$Amount), median(m4_merch11_in_person$Amount), median(m4_merch12_in_person$Amount), median(m4_merch13_in_person$Amount), median(m4_merch14_in_person$Amount), median(m4_merch15_in_person$Amount), median(m4_merch16_in_person$Amount), median(m4_merch17_in_person$Amount), median(m4_merch18_in_person$Amount), median(m4_merch19_in_person$Amount), median(m4_merch20_in_person$Amount), median(m4_merch21_in_person$Amount)))

## vector of remote median-value by merchant
(merch_remote_med_val.vec = c(median(m4_merch1_remote$Amount), median(m4_merch2_remote$Amount), median(m4_merch3_remote$Amount), median(m4_merch4_remote$Amount), median(m4_merch5_remote$Amount), median(m4_merch6_remote$Amount), median(m4_merch7_remote$Amount), median(m4_merch8_remote$Amount), median(m4_merch9_remote$Amount), median(m4_merch10_remote$Amount), median(m4_merch11_remote$Amount), median(m4_merch12_remote$Amount), median(m4_merch13_remote$Amount), median(m4_merch14_remote$Amount), median(m4_merch15_remote$Amount), median(m4_merch16_remote$Amount), median(m4_merch17_remote$Amount), median(m4_merch18_remote$Amount), median(m4_merch19_remote$Amount), median(m4_merch20_remote$Amount), median(m4_merch21_remote$Amount)))

# Preparing Table 3 on basic stats by merchant type: In-person vs remote
# Reduced table stats below (removing vol(w) from both in-person and remote), and renaming vol to obs (number of observations). For val and income, display (w) only
# Displaying unweighted only, except of HH INcome
(merch_stat3.df = data.frame("Merchant"=merch_abv.vec, "Obs"=merch_in_person_vol.vec, "%" = 100*merch_in_person_vol_frac.vec, "Avg.Val"=merch_in_person_avg_val.vec, "Med.Val"=merch_in_person_med_val.vec, "Obs"=merch_remote_vol.vec, "%" = 100*merch_remote_vol_frac.vec, "Avg.Val"=merch_remote_avg_val.vec, "Med.Val"=merch_remote_med_val.vec))
dim(merch_stat3.df)
print(xtable(merch_stat3.df, digits = 0), include.rownames = F, hline.after = c(0:21))

# Table 3 caption 
length(unique(m4$id))
nrow(m4)
sum(merch_in_person_vol.vec)+sum(merch_remote_vol.vec)
nrow(m4[is.na(m4$in_person), ])# 14 payments did not report whether "in-person", so they are not listed in Table 3. 
length(unique(m4[!is.na(m4$in_person), ]$id))

### Start, Table 4 merchant type and percenate use of each PI 
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

# frac of PI used by merchant type
table(m6$merch)
# cash vector by merch type
(cash_frac.vec = c(rep(0, 21)))
for (x in 1:21) {
cash_frac.vec[x] = nrow(subset(m6, merch==x & Method == "Cash"))/nrow(subset(m6, merch==x))
}
cash_frac.vec
#
# check vector by merch type
(check_frac.vec = c(rep(0, 21)))
for (x in 1:21) {
check_frac.vec[x] = nrow(subset(m6, merch==x & Method == "Check"))/nrow(subset(m6, merch==x))
}
check_frac.vec
#
# credit vector by merch type
(credit_frac.vec = c(rep(0, 21)))
for (x in 1:21) {
  credit_frac.vec[x] = nrow(subset(m6, merch==x & Method == "Credit"))/nrow(subset(m6, merch==x))
}
credit_frac.vec
#
# debit vector by merch type
(debit_frac.vec = c(rep(0, 21)))
for (x in 1:21) {
  debit_frac.vec[x] = nrow(subset(m6, merch==x & Method == "Debit"))/nrow(subset(m6, merch==x))
}
debit_frac.vec
#
# prepaid vector by merch type
(prepaid_frac.vec = c(rep(0, 21)))
for (x in 1:21) {
  prepaid_frac.vec[x] = nrow(subset(m6, merch==x & Method == "Prepaid"))/nrow(subset(m6, merch==x))
}
prepaid_frac.vec
#
# Acct_to_acct vector by merch type
(acct_to_acct_frac.vec = c(rep(0, 21)))
for (x in 1:21) {
  acct_to_acct_frac.vec[x] = nrow(subset(m6, merch==x & Method == "Acct_to_acct"))/nrow(subset(m6, merch==x))
}
acct_to_acct_frac.vec
#
# Acct_num vector by merch type
(acct_num_frac.vec = c(rep(0, 21)))
for (x in 1:21) {
  acct_num_frac.vec[x] = nrow(subset(m6, merch==x & Method == "Acct_num"))/nrow(subset(m6, merch==x))
}
acct_num_frac.vec
#
# Online_bill vector by merch type
(online_bill_frac.vec = c(rep(0, 21)))
for (x in 1:21) {
  online_bill_frac.vec[x] = nrow(subset(m6, merch==x & Method == "Online_bill"))/nrow(subset(m6, merch==x))
}
online_bill_frac.vec
#
# construct new row: frac use of Method for ALL  the 21 merchants:
(all.vec = c(nrow(subset(m6, Method == "Cash")), nrow(subset(m6, Method == "Check")), nrow(subset(m6, Method == "Credit")), nrow(subset(m6, Method == "Debit")), nrow(subset(m6, Method == "Prepaid")), nrow(subset(m6, Method == "Acct_num")), nrow(subset(m6, Method == "Online_bill")), nrow(subset(m6, Method == "Acct_to_acct"))))
(all_frac.vec = all.vec/nrow(m6)) # bottom row to be added below
sum(all_frac.vec)# verify sums to 1
  
# Finalizing table 4 frac vol by merch type (all merch)
(all_frac.df = data.frame("Merchant"=merch_abv.vec, "Cash"=100*cash_frac.vec, "Check"=100*check_frac.vec, "Credit"=100*credit_frac.vec, "Debit"=100*debit_frac.vec, "Prepaid"=100*prepaid_frac.vec, "BANP"=100*acct_num_frac.vec, "OBBP"=100*online_bill_frac.vec, "Acct2acct"=100*acct_to_acct_frac.vec))
dim(all_frac.df)
sum_verify.vec = rep(0, 21)# initialize vector
for (x in 1:21) { #verify that each row sums up to 100%
sum_verify.vec[x] =sum(all_frac.df[x, c(2:9) ])
}
sum_verify.vec
#
# adding HHI for by merchant type
hhi.vec = rep(0, 21) # initialize vector
for (x in 1:21) { # HHI by merch type
  hhi.vec[x] =sum((all_frac.df[x, c(2:9) ])^2)
}
hhi.vec
# adding HHI for All payments (not by merch) 
(hhi_all = sum(100^2*all_frac.vec*all_frac.vec))
(hhi2.vec = c(hhi.vec, hhi_all))# combining by-merch with all
length(hhi2.vec) # 21 merch + all

# adding bottom row (All merchants, Table 4)
str(all_frac.df)
(all_frac2.df = rbind(all_frac.df, c(0, 100*all_frac.vec)))# define All as merch 0 (does not work b/c merch is a factor)
dim(all_frac.df)
(all_frac3.df = cbind(all_frac2.df, "HHI"=hhi2.vec))# adding column HHI
str(all_frac3.df)
dim(all_frac3.df)

# sort according HHI (decending)
(all_frac4.df = all_frac3.df[order(-all_frac3.df$HHI), ])

(digitm = matrix(c(0,0,1,1,1,1,1,1,1,1,0  ), nrow = 22, ncol = 10+1, byrow = T))
dim(digitm)
#
print(xtable(all_frac4.df, digits = digitm), include.rownames = F, hline.after = c(0:21))
#
#caption Table 4
nrow(m6)# total num payments
length(unique(m6$id))# num respondents

### start table 5: frac VALUE payment methods by merchant type
# frac of PI used by merchant type VALUE
table(m6$merch)

# cash vector by merch type value
(cash_frac_val.vec = c(rep(0, 21)))
for (x in 1:21) {
  cash_frac_val.vec[x] = sum(subset(m6, merch==x & Method == "Cash")$Amount)/sum(subset(m6, merch==x)$Amount)
}
cash_frac_val.vec
#
#
# check vector by merch type value
(check_frac_val.vec = c(rep(0, 21)))
for (x in 1:21) {
  check_frac_val.vec[x] = sum(subset(m6, merch==x & Method == "Check")$Amount)/sum(subset(m6, merch==x)$Amount)
}
check_frac_val.vec
#
# credit vector by merch type value
(credit_frac_val.vec = c(rep(0, 21)))
for (x in 1:21) {
  credit_frac_val.vec[x] = sum(subset(m6, merch==x & Method == "Credit")$Amount)/sum(subset(m6, merch==x)$Amount)
}
credit_frac_val.vec
#
# debit vector by merch type value
(debit_frac_val.vec = c(rep(0, 21)))
for (x in 1:21) {
  debit_frac_val.vec[x] = sum(subset(m6, merch==x & Method == "Debit")$Amount)/sum(subset(m6, merch==x)$Amount)
}
debit_frac_val.vec
#
# prepaid vector by merch type value
(prepaid_frac_val.vec = c(rep(0, 21)))
for (x in 1:21) {
  prepaid_frac_val.vec[x] = sum(subset(m6, merch==x & Method == "Prepaid")$Amount)/sum(subset(m6, merch==x)$Amount)
}
prepaid_frac_val.vec
#
# Acct_to_acct vector by merch type value
(acct_to_acct_frac_val.vec = c(rep(0, 21)))
for (x in 1:21) {
  acct_to_acct_frac_val.vec[x] = sum(subset(m6, merch==x & Method == "Acct_to_acct")$Amount)/sum(subset(m6, merch==x)$Amount)
}
acct_to_acct_frac_val.vec
#
# Acct_num vector by merch type value
(acct_num_frac_val.vec = c(rep(0, 21)))
for (x in 1:21) {
  acct_num_frac_val.vec[x] = sum(subset(m6, merch==x & Method == "Acct_num")$Amount)/sum(subset(m6, merch==x)$Amount)
}
acct_num_frac_val.vec
#
# Online_bill vector by merch type value
(online_bill_frac_val.vec = c(rep(0, 21)))
for (x in 1:21) {
  online_bill_frac_val.vec[x] = sum(subset(m6, merch==x & Method == "Online_bill")$Amount)/sum(subset(m6, merch==x)$Amount)
}
online_bill_frac_val.vec
#
# construct bottom row: frac use of Method for all the 21 merchants:
(all_val.vec = c(sum(subset(m6, Method == "Cash")$Amount), sum(subset(m6, Method == "Check")$Amount), sum(subset(m6, Method == "Credit")$Amount), sum(subset(m6, Method == "Debit")$Amount), sum(subset(m6, Method == "Prepaid")$Amount), sum(subset(m6, Method == "Acct_num")$Amount), sum(subset(m6, Method == "Online_bill")$Amount), sum(subset(m6, Method == "Acct_to_acct")$Amount)))
(all_frac_val.vec = all_val.vec/sum(m6$Amount)) # bottom row to be added below
sum(all_frac_val.vec)# verify sums to 1

# Finalizing table 5 frac VAL by merch type (all merch)
(all_frac_val.df = data.frame("Merchant"=merch_abv.vec, "Cash"=100*cash_frac_val.vec, "Check"=100*check_frac_val.vec, "Credit"=100*credit_frac_val.vec, "Debit"=100*debit_frac_val.vec, "Prepaid"=100*prepaid_frac_val.vec, "BANP"=100*acct_num_frac_val.vec, "OBBP"=100*online_bill_frac_val.vec, "Acct2acct"=100*acct_to_acct_frac_val.vec))
dim(all_frac_val.df)
sum_verify.vec = rep(0, 21)# initialize vector
for (x in 1:21) { #verify that each row sums up to 100%
  sum_verify.vec[x] =sum(all_frac_val.df[x, c(2:9) ])
}
sum_verify.vec

# adding bottom row (All merchants, Table 5)
str(all_frac_val.df)
(all_frac2_val.df = rbind(all_frac_val.df, c(0, 100*all_frac_val.vec)))# define All as merch 0 (does not work b/c merch is a factor)
dim(all_frac2_val.df)

# adding HHI_value for by merchant type
hhi_val.vec = rep(0, 21) # initialize vector
for (x in 1:21) { # HHI by merch type by value
  hhi_val.vec[x] =sum((all_frac_val.df[x, c(2:9) ])^2)
}
hhi_val.vec
# adding HHI for All payments (not by merch) 
(hhi_val_all = sum(100^2*all_frac_val.vec*all_frac_val.vec))
(hhi_val2.vec = c(hhi_val.vec, hhi_val_all))# combining by-merch with all
length(hhi2.vec) # 21 merch + all

# sort according HHI (decending)
(all_frac3_val.df = cbind(all_frac2_val.df, "HHI"=hhi_val2.vec))# adding column HHI
names(all_frac3_val.df)
(all_frac4_val.df = all_frac3_val.df[order(-all_frac3_val.df$HHI), ])
#
(digitm = matrix(c(0,0,1,1,1,1,1,1,1,1,0  ), nrow = 22, ncol = 10+1, byrow = T))
dim(digitm)
#
print(xtable(all_frac4_val.df, digits = digitm), include.rownames = F, hline.after = c(0:21))
#
#caption Table 5
nrow(m6)# total num all payments
length(unique(m6$id))# num respondents

### Start Bar charts of payment methods sorted by merchant concentration
names(m6)
table(m6$Method)
100*(prop.table(table(m6$Method)))

# plot Method combination for each merchant type [not used, see below with HHI]
str(m6$merch)
m6$merch = as.factor(m6$merch)
table(m6$merch)
dim(m6)
#par(mar=c(1,1,1,1))
#plot(Method~merch, data = m4_in_person4)
#ggplot(m4_in_person4, aes(x=merch, fill=Method))+geom_bar()# by vol => need to make it proportional
#ggplot(m6, aes(x=merch, fill=Method)) + geom_bar(position = "fill") + scale_y_continuous(labels = scales::percent)# 

table(m6$merch, exclude = NULL)
m7 = subset(m6, !is.na(merch))# remove 1 NA from merch (otherwise, adding hhi below generates an error)
dim(m6)
dim(m7)
#
# add hhi column. Recall,
hhi.vec
m7$hhi = NULL
str(m7$merch)
m7[m7$merch=="1", "hhi"] = hhi.vec[1]
m7[m7$merch=="2", "hhi"] = hhi.vec[2]
m7[m7$merch=="3", "hhi"] = hhi.vec[3]
m7[m7$merch=="4", "hhi"] = hhi.vec[4]
m7[m7$merch=="5", "hhi"] = hhi.vec[5]
m7[m7$merch=="6", "hhi"] = hhi.vec[6]
m7[m7$merch=="7", "hhi"] = hhi.vec[7]
m7[m7$merch=="8", "hhi"] = hhi.vec[8]
m7[m7$merch=="9", "hhi"] = hhi.vec[9]
m7[m7$merch=="10", "hhi"] = hhi.vec[10]
m7[m7$merch=="11", "hhi"] = hhi.vec[11]
m7[m7$merch=="12", "hhi"] = hhi.vec[12]
m7[m7$merch=="13", "hhi"] = hhi.vec[13]
m7[m7$merch=="14", "hhi"] = hhi.vec[14]
m7[m7$merch=="15", "hhi"] = hhi.vec[15]
m7[m7$merch=="16", "hhi"] = hhi.vec[16]
m7[m7$merch=="17", "hhi"] = hhi.vec[17]
m7[m7$merch=="18", "hhi"] = hhi.vec[18]
m7[m7$merch=="19", "hhi"] = hhi.vec[19]
m7[m7$merch=="20", "hhi"] = hhi.vec[20]
m7[m7$merch=="21", "hhi"] = hhi.vec[21]
table(m7$hhi)

m8 = m7 # adding column merch abv desc
merch_abv_fig.vec
m8$merch_abv_fig = NULL # initializing column
str(m8$merch_abv_fig)
m8[m8$merch=="1", "merch_abv_fig"] = merch_abv_fig.vec[1]
m8[m8$merch=="2", "merch_abv_fig"] = merch_abv_fig.vec[2]
m8[m8$merch=="3", "merch_abv_fig"] = merch_abv_fig.vec[3]
m8[m8$merch=="4", "merch_abv_fig"] = merch_abv_fig.vec[4]
m8[m8$merch=="5", "merch_abv_fig"] = merch_abv_fig.vec[5]
m8[m8$merch=="6", "merch_abv_fig"] = merch_abv_fig.vec[6]
m8[m8$merch=="7", "merch_abv_fig"] = merch_abv_fig.vec[7]
m8[m8$merch=="8", "merch_abv_fig"] = merch_abv_fig.vec[8]
m8[m8$merch=="9", "merch_abv_fig"] = merch_abv_fig.vec[9]
m8[m8$merch=="10", "merch_abv_fig"] = merch_abv_fig.vec[10]
m8[m8$merch=="11", "merch_abv_fig"] = merch_abv_fig.vec[11]
m8[m8$merch=="12", "merch_abv_fig"] = merch_abv_fig.vec[12]
m8[m8$merch=="13", "merch_abv_fig"] = merch_abv_fig.vec[13]
m8[m8$merch=="14", "merch_abv_fig"] = merch_abv_fig.vec[14]
m8[m8$merch=="15", "merch_abv_fig"] = merch_abv_fig.vec[15]
m8[m8$merch=="16", "merch_abv_fig"] = merch_abv_fig.vec[16]
m8[m8$merch=="17", "merch_abv_fig"] = merch_abv_fig.vec[17]
m8[m8$merch=="18", "merch_abv_fig"] = merch_abv_fig.vec[18]
m8[m8$merch=="19", "merch_abv_fig"] = merch_abv_fig.vec[19]
m8[m8$merch=="20", "merch_abv_fig"] = merch_abv_fig.vec[20]
m8[m8$merch=="21", "merch_abv_fig"] = merch_abv_fig.vec[21]
table(m8$merch_abv_fig)

par(mar=c(1,1,1,1))# finalizing Figure 1
#plot(Method~merch, data = m4_in_person4)
#ggplot(m4_in_person4, aes(x=merch, fill=Method))+geom_bar()# by vol => need to make it proportional
(hhi_fig = ggplot(m8, aes(x=reorder(merch_abv_fig, hhi), fill=Method)) + geom_bar(position = "fill") + scale_y_continuous(labels = scales::percent))# 
(hhi_fig = hhi_fig + xlab("") + ylab("Payment method (%)"))
(hhi_fig = hhi_fig + annotate("text", x = m8$merch_abv_fig, y = 1.03,                      label = round(m7$hhi,0), size = 3.5))
(hhi_fig = hhi_fig + theme(axis.text=element_text(size=12, angle = 60,hjust=0.95,vjust=0.2)))
#
# for the caption of Figure 1
nrow(m8)# num trans
length(unique(m8$id))# num resp

### Section 3.2: Sorting merchants by intensity use of each PI
# recall Table 4
all_frac.df
#
sort_cash = all_frac.df[order(-all_frac.df$Cash),]
(sort_cash = sort_cash %>% mutate_at(vars(Cash), funs(round(., 1))))
sort_cash[, c("Merchant", "Cash")]
#
sort_check = all_frac.df[order(-all_frac.df$Check),]
(sort_check = sort_check %>% mutate_at(vars(Check), funs(round(., 1))))
sort_check[, c("Merchant", "Check")]
#
sort_credit = all_frac.df[order(-all_frac.df$Credit),]
(sort_credit = sort_credit %>% mutate_at(vars(Credit), funs(round(., 1))))
sort_credit[, c("Merchant", "Credit")]
#
sort_debit = all_frac.df[order(-all_frac.df$Debit),]
(sort_debit = sort_debit %>% mutate_at(vars(Debit), funs(round(., 1))))
sort_debit[, c("Merchant", "Debit")]
#
sort_prepaid = all_frac.df[order(-all_frac.df$Prepaid),]
(sort_prepaid = sort_prepaid %>% mutate_at(vars(Prepaid), funs(round(., 1))))
sort_prepaid[, c("Merchant", "Prepaid")]
#
sort_banp = all_frac.df[order(-all_frac.df$BANP),]
(sort_banp = sort_banp %>% mutate_at(vars(BANP), funs(round(., 1))))
sort_banp[, c("Merchant", "BANP")]
#
sort_obbp = all_frac.df[order(-all_frac.df$OBBP),]
(sort_obbp = sort_obbp %>% mutate_at(vars(OBBP), funs(round(., 1))))
sort_obbp[, c("Merchant", "OBBP")]
#
sort_acct2acct = all_frac.df[order(-all_frac.df$Acct2acct),]
(sort_acct2acct = sort_acct2acct %>% mutate_at(vars(Acct2acct), funs(round(., 1))))
sort_acct2acct[, c("Merchant", "Acct2acct")]

### Section 4: Dollar value. Builds on Table 5 above
# Recall Table 5:
all_frac_val.df # Table 5 just 21 merchants
hhi_val.vec# HHI by merchant type (value, Table 5)
hhi.vec# HHI by merchant type (volume, Table 4)
cor(hhi.vec, hhi_val.vec)# correlation between vol HHI and val HHI

### Sorting merchants by intensity use of each PI: by VALUE
# recall Table 4
#
round(cor(all_frac.df$Cash, all_frac_val.df$Cash),2)# corr between frac cash by vol and val
sort_cash_val = all_frac_val.df[order(-all_frac_val.df$Cash),]
(sort_cash_val = sort_cash_val %>% mutate_at(vars(Cash), funs(round(., 1))))
sort_cash_val[, c("Merchant", "Cash")]
#
round(cor(all_frac.df$Check, all_frac_val.df$Check),2)# corr between frac check by vol and val
sort_check_val = all_frac_val.df[order(-all_frac_val.df$Check),]
(sort_check_val = sort_check_val %>% mutate_at(vars(Check), funs(round(., 1))))
sort_check_val[, c("Merchant", "Check")]
#
round(cor(all_frac.df$Credit, all_frac_val.df$Credit),2)# corr between frac credit by vol and val
sort_credit_val = all_frac_val.df[order(-all_frac_val.df$Credit),]
(sort_credit_val = sort_credit_val %>% mutate_at(vars(Credit), funs(round(., 1))))
sort_credit_val[, c("Merchant", "Credit")]
#
round(cor(all_frac.df$Debit, all_frac_val.df$Debit),2)# corr between frac debit by vol and val
sort_debit_val = all_frac_val.df[order(-all_frac_val.df$Debit),]
(sort_debit_val = sort_debit_val %>% mutate_at(vars(Debit), funs(round(., 1))))
sort_debit_val[, c("Merchant", "Debit")]
#
round(cor(all_frac.df$Prepaid, all_frac_val.df$Prepaid),2)# corr between frac prepaid by vol and val
sort_prepaid_val = all_frac_val.df[order(-all_frac_val.df$Prepaid),]
(sort_prepaid_val = sort_prepaid_val %>% mutate_at(vars(Prepaid), funs(round(., 1))))
sort_prepaid_val[, c("Merchant", "Prepaid")]
#
round(cor(all_frac.df$BANP, all_frac_val.df$BANP),2)# corr between frac BANP by vol and val
sort_banp_val = all_frac_val.df[order(-all_frac_val.df$BANP),]
(sort_banp_val = sort_banp_val %>% mutate_at(vars(BANP), funs(round(., 1))))
sort_banp_val[, c("Merchant", "BANP")]
#
round(cor(all_frac.df$OBBP, all_frac_val.df$OBBP),2)# corr between frac OBBP by vol and val
sort_obbp_val = all_frac_val.df[order(-all_frac_val.df$OBBP),]
(sort_obbp_val = sort_obbp_val %>% mutate_at(vars(OBBP), funs(round(., 1))))
sort_obbp_val[, c("Merchant", "OBBP")]
#
round(cor(all_frac.df$Acct2acct, all_frac_val.df$Acct2acct),2)# corr between frac A2A by vol and val
sort_acct2acct_val = all_frac_val.df[order(-all_frac_val.df$Acct2acct),]
(sort_acct2acct_val = sort_acct2acct_val %>% mutate_at(vars(Acct2acct), funs(round(., 1))))
sort_acct2acct_val[, c("Merchant", "Acct2acct")]

### Section 5: grouping PI into 3 groups: paper, card, and electronic
dim(m7)
table(m7$Method)
m7$pi_group = NA
m7[m7$Method %in% c("Cash", "Check"), "pi_group"] = "Paper"
m7[m7$Method %in% c("Credit", "Debit", "Prepaid"), "pi_group"] = "Card"
m7[m7$Method %in% c("Acc_nu,", "Acct_to_acct", "Online_bill"), "pi_group"] = "Electronic"
table(m7$pi_group) #reported in Section 5
round(100*prop.table(table(m7$pi_group)),1)

three.df = subset(m7, select = c(merch, pi_group) )
table(three.df)
#saveRDS(three.df, "three.rds")
dim(table(three.df))

three1.df <- table(three.df) %>% as.data.frame.matrix
three_perc.df <- apply( three1.df,
                  MARGIN = 1,
                  FUN = function(x){ x/sum(x) } )
three_perc.df <- three_perc.df %>% t %>% as.data.frame # apply returns a matrix
three_perc.df <- three_perc.df*100
rowSums(three_perc.df)# verify sum to 100%
three_perc.df$merch <- merch_abv_fig.vec
three_perc.df

# plot perc cash payments
ggplot(three_perc.df, aes(x = reorder(merch, Paper), y = Paper)) +
  geom_bar(stat = "identity", fill = "#4d7aff") +
  coord_flip() +
  theme_bw() +
  geom_text(aes(label = round(Paper, 0)),
            hjust = -0.2, size = 3) +
  ylab("Percentage of paper instrument payments") + xlab("Merchant type") +
  theme(
    axis.title.x = element_text(color="black", size=14, face="plain"),
    axis.title.y = element_text(color="black", size=14, face="plain")) +
  theme(text = element_text(color="black", face = "bold", size=14))
# caption
sum(three1.df)
length(unique(m7$id))

# plot perc card payments
ggplot(three_perc.df, aes(x = reorder(merch, Card), y = Card)) +
  geom_bar(stat = "identity", fill = "#4d7aff") +
  coord_flip() +
  theme_bw() +
  geom_text(aes(label = round(Card, 0)),
            hjust = -0.2, size = 3) +
  ylab("Percentage of card payments") + xlab("Merchant type") +
  theme(
    axis.title.x = element_text(color="black", size=14, face="plain"),
    axis.title.y = element_text(color="black", size=14, face="plain")) +
  theme(text = element_text(color="black", face = "bold", size=14))

# plot perc electronic payments
ggplot(three_perc.df, aes(x = reorder(merch, Electronic), y = Electronic)) +
  geom_bar(stat = "identity", fill = "#4d7aff") +
  coord_flip() +
  theme_bw() +
  geom_text(aes(label = round(Electronic, 0)),
            hjust = -0.2, size = 3) +
  ylab("Percentage of card payments") + xlab("Merchant type") +
  theme(
    axis.title.x = element_text(color="black", size=14, face="plain"),
    axis.title.y = element_text(color="black", size=14, face="plain")) +
  theme(text = element_text(color="black", face = "bold", size=14))


### Section 6: Mobile plot

### Dot graph: percentage mobile_app by merch type 
# create column percentage mobile_app by merch type
# create column percentage cash by merch type
m5 = m4[!is.na(m4$merch), ] # remove NA in merch column
table(m5$merch)
# I add mobile app to the PI analyzed in the rest of the paper
m5_mobile = m5 %>% filter(Method %in% c("Acct_num", "Acct_to_acct", "Online_bill", "Cash", "Check", "Credit", "Debit", "Prepaid", "Mobile_app"))
table(m5_mobile$Method)
100*prop.table(table(m5_mobile$Method))
sum(table(m5_mobile$Method))
m5_mobile$Method = factor(m5_mobile$Method)
table(m5_mobile$Method)
100*prop.table(table(m5_mobile$Method))

m5_mobile$mobile_perc = NULL # add column percentage mobile by merch type
m5_mobile[m5_mobile$merch=="1", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="1" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="1"))
m5_mobile[m5_mobile$merch=="2", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="2" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="2"))
m5_mobile[m5_mobile$merch=="3", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="3" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="3"))
m5_mobile[m5_mobile$merch=="4", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="4" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="4"))
m5_mobile[m5_mobile$merch=="5", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="5" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="5"))
m5_mobile[m5_mobile$merch=="6", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="6" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="6"))
m5_mobile[m5_mobile$merch=="7", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="7" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="7"))
m5_mobile[m5_mobile$merch=="8", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="8" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="8"))
m5_mobile[m5_mobile$merch=="9", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="9" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="9"))
m5_mobile[m5_mobile$merch=="10", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="10" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="10"))
m5_mobile[m5_mobile$merch=="11", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="11" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="11"))
m5_mobile[m5_mobile$merch=="12", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="12" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="12"))
m5_mobile[m5_mobile$merch=="13", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="13" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="13"))
m5_mobile[m5_mobile$merch=="14", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="14" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="14"))
m5_mobile[m5_mobile$merch=="15", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="15" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="15"))
m5_mobile[m5_mobile$merch=="16", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="16" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="16"))
m5_mobile[m5_mobile$merch=="17", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="17" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="17"))
m5_mobile[m5_mobile$merch=="18", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="18" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="18"))
m5_mobile[m5_mobile$merch=="19", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="19" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="19"))
m5_mobile[m5_mobile$merch=="20", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="20" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="20"))
m5_mobile[m5_mobile$merch=="21", "mobile_perc"] = 100*nrow(subset(m5_mobile, merch=="21" & Method=="Mobile_app"))/nrow(subset(m5_mobile, merch=="21"))
#
head(m5_mobile[,c("merch", "mobile_perc")])# verifying

merch_abv_fig.vec
m5_mobile$merch_abv_fig = NULL # initializing column
str(m5_mobile$merch_abv_fig)
m5_mobile[m5_mobile$merch=="1", "merch_abv_fig"] = merch_abv_fig.vec[1]
m5_mobile[m5_mobile$merch=="2", "merch_abv_fig"] = merch_abv_fig.vec[2]
m5_mobile[m5_mobile$merch=="3", "merch_abv_fig"] = merch_abv_fig.vec[3]
m5_mobile[m5_mobile$merch=="4", "merch_abv_fig"] = merch_abv_fig.vec[4]
m5_mobile[m5_mobile$merch=="5", "merch_abv_fig"] = merch_abv_fig.vec[5]
m5_mobile[m5_mobile$merch=="6", "merch_abv_fig"] = merch_abv_fig.vec[6]
m5_mobile[m5_mobile$merch=="7", "merch_abv_fig"] = merch_abv_fig.vec[7]
m5_mobile[m5_mobile$merch=="8", "merch_abv_fig"] = merch_abv_fig.vec[8]
m5_mobile[m5_mobile$merch=="9", "merch_abv_fig"] = merch_abv_fig.vec[9]
m5_mobile[m5_mobile$merch=="10", "merch_abv_fig"] = merch_abv_fig.vec[10]
m5_mobile[m5_mobile$merch=="11", "merch_abv_fig"] = merch_abv_fig.vec[11]
m5_mobile[m5_mobile$merch=="12", "merch_abv_fig"] = merch_abv_fig.vec[12]
m5_mobile[m5_mobile$merch=="13", "merch_abv_fig"] = merch_abv_fig.vec[13]
m5_mobile[m5_mobile$merch=="14", "merch_abv_fig"] = merch_abv_fig.vec[14]
m5_mobile[m5_mobile$merch=="15", "merch_abv_fig"] = merch_abv_fig.vec[15]
m5_mobile[m5_mobile$merch=="16", "merch_abv_fig"] = merch_abv_fig.vec[16]
m5_mobile[m5_mobile$merch=="17", "merch_abv_fig"] = merch_abv_fig.vec[17]
m5_mobile[m5_mobile$merch=="18", "merch_abv_fig"] = merch_abv_fig.vec[18]
m5_mobile[m5_mobile$merch=="19", "merch_abv_fig"] = merch_abv_fig.vec[19]
m5_mobile[m5_mobile$merch=="20", "merch_abv_fig"] = merch_abv_fig.vec[20]
m5_mobile[m5_mobile$merch=="21", "merch_abv_fig"] = merch_abv_fig.vec[21]
table(m5_mobile$merch_abv_fig)


mobile_dotplot = ggplot(m5_mobile, aes(x=mobile_perc, y=reorder(merch_abv_fig, mobile_perc))) + geom_point()
(mobile_dotplot = mobile_dotplot + xlab("Percentage of  mobile app payments (%)") + ylab("Merchant type"))
(mobile_dotplot = mobile_dotplot +geom_text(aes(label=round(mobile_perc,0)),hjust=-0.5, vjust=0.3))
(mobile_dotplot = mobile_dotplot + theme(
  axis.title.x = element_text(color="black", size=14, face="plain"),
  axis.title.y = element_text(color="black", size=14, face="plain"),
))
(mobile_dotplot = mobile_dotplot + theme(text = element_text(color="black", face = "bold")))
(mobile_dotplot = mobile_dotplot+ theme(axis.text=element_text(size=13)))# higher font size => more time consuming!

# For the caption:
nrow(m5_mobile)
length(unique(m5_mobile$id))

### merch_200617.R adding some analysis, starting line 870

## start plotting HHI value vs. HHI volume
# recall merch definitions
merch_name.vec
merch_abv.vec
merch_abv_fig.vec

# Recall HHI
hhi.vec # # hhi volume merch 1 to 21, without all (21 merch), by volume
hhi2.vec # with all (22 length), by volume
hhi_val.vec # by value
hhi_val2.vec # by value

# Recall Table 4 
all_frac3.df #volume
all_frac3_val.df# value

(merch_hhi_vol1.df = all_frac3.df[-22, ]) # deleting all
(merch_hhi_val1.df = all_frac3_val.df[-22, ]) # deleting all
#
names(merch_hhi_vol1.df)
(merch_hhi_vol2.df = subset(merch_hhi_vol1.df, select = c("Merchant", "HHI")) )
colnames(merch_hhi_vol2.df)[colnames(merch_hhi_vol2.df)=="HHI"] = "HHI_vol"
#
names(merch_hhi_val1.df)
(merch_hhi_val2.df = subset(merch_hhi_val1.df, select = c("Merchant", "HHI")) )
colnames(merch_hhi_val2.df)[colnames(merch_hhi_val2.df)=="HHI"] = "HHI_val"
#
# merge vol and val
(merch_hhi_vol_val = join(merch_hhi_vol2.df, merch_hhi_val2.df))
names(merch_hhi_vol_val)
(merch_hhi_vol_val2 = cbind(merch_hhi_vol_val, merch_abv.vec, merch_abv_fig.vec))
#
#ggplot(merch_hhi_vol_val, aes(x=HHI_vol, y=HHI_val, label=Merchant))+geom_text(check_overlap = T, hjust=0)# check_overlaps removes obs

ggplot(merch_hhi_vol_val, aes(x=HHI_vol, y=HHI_val, label=merch_abv_fig.vec))+geom_text( hjust=0, size =3)

### merch_200618.R Regressing HHI line 918
# Regressing HHI volume
hhi.vec # hhi volume merch 1 to 21
#  regress on:
merch_in_person_vol_frac.vec
merch_remote_vol_frac.vec
merch_med_val.vec # median purchase value
merch_med_income.vec # median HH income of buyers
# put the above in a dataframe
(hhi_vol.df = data.frame("hhi_vol" = hhi.vec, "in_person_frac" = merch_in_person_vol_frac.vec, "med_val" = merch_med_val.vec, "med_hh_income" = merch_med_income.vec))

hhi_vol_model = hhi_vol ~ in_person_frac + med_val + med_hh_income
#
(hhi_vol.lm = lm(hhi_vol_model, data = hhi_vol.df))
summary(hhi_vol.lm)
# try correlations
with(hhi_vol.df, cor(hhi.vec, merch_in_person_vol_frac.vec))
with(hhi_vol.df, cor(hhi.vec, merch_med_val.vec))
with(hhi_vol.df, cor(hhi.vec, merch_med_income.vec))

hhi_val.vec # hhi value merch 1 to 21
#  regress on:
merch_in_person_vol_frac.vec
merch_remote_vol_frac.vec
merch_med_val.vec # median purchase value
merch_med_income.vec # median HH income of buyers
# put the above in a dataframe
(hhi_val.df = data.frame("hhi_val" = hhi_val.vec, "in_person_frac" = merch_in_person_vol_frac.vec, "med_val" = merch_med_val.vec, "med_hh_income" = merch_med_income.vec))

hhi_val_model = hhi_val ~ in_person_frac + med_val + med_hh_income
#
(hhi_val.lm = lm(hhi_val_model, data = hhi_val.df))
summary(hhi_val.lm)
# try correlations
with(hhi_val.df, cor(hhi.vec, merch_in_person_vol_frac.vec))
with(hhi_val.df, cor(hhi.vec, merch_med_val.vec))
with(hhi_val.df, cor(hhi.vec, merch_med_income.vec))

### merch_200619.R adding 2x8 regressions of PI (vol and val) the the above 3 vars
# recall Table 4
all_frac.df # fraction of use of each PI by merch type
dim(all_frac.df)
all_frac_reg.df = cbind(all_frac.df, "in_person_frac" = merch_in_person_vol_frac.vec, "med_val" = merch_med_val.vec, "med_hh_income" = merch_med_income.vec)
names(all_frac_reg.df)

# start 8 PI % vol regressions
cash_frac_vol_reg_model = Cash ~ in_person_frac + med_val + med_hh_income
check_frac_vol_reg_model = Check ~ in_person_frac + med_val + med_hh_income
credit_frac_vol_reg_model = Credit ~ in_person_frac + med_val + med_hh_income
debit_frac_vol_reg_model = Debit ~ in_person_frac + med_val + med_hh_income
prepaid_frac_vol_reg_model = Prepaid ~ in_person_frac + med_val + med_hh_income
banp_frac_vol_reg_model = BANP ~ in_person_frac + med_val + med_hh_income
obbp_frac_vol_reg_model = OBBP ~ in_person_frac + med_val + med_hh_income
acct2acct_frac_vol_reg_model = Acct2acct ~ in_person_frac + med_val + med_hh_income
#
cash_frac_vol_reg.lm = lm(cash_frac_vol_reg_model, data = all_frac_reg.df)
check_frac_vol_reg.lm = lm(check_frac_vol_reg_model, data = all_frac_reg.df)
credit_frac_vol_reg.lm = lm(credit_frac_vol_reg_model, data = all_frac_reg.df)
debit_frac_vol_reg.lm = lm(debit_frac_vol_reg_model, data = all_frac_reg.df)
prepaid_frac_vol_reg.lm = lm(prepaid_frac_vol_reg_model, data = all_frac_reg.df)
banp_frac_vol_reg.lm = lm(banp_frac_vol_reg_model, data = all_frac_reg.df)
obbp_frac_vol_reg.lm = lm(obbp_frac_vol_reg_model, data = all_frac_reg.df)
acct2acct_frac_vol_reg.lm = lm(acct2acct_frac_vol_reg_model, data = all_frac_reg.df)
#
summary(cash_frac_vol_reg.lm)
summary(credit_frac_vol_reg.lm)
summary(debit_frac_vol_reg.lm)
summary(prepaid_frac_vol_reg.lm)
summary(banp_frac_vol_reg.lm)
summary(obbp_frac_vol_reg.lm)
summary(acct2acct_frac_vol_reg.lm)

## redo by val
all_frac_val.df # fraction of use of each PI by merch type
dim(all_frac_val.df)
all_frac_reg_val.df = cbind(all_frac_val.df, "in_person_frac" = merch_in_person_vol_frac.vec, "med_val" = merch_med_val.vec, "med_hh_income" = merch_med_income.vec)
names(all_frac_reg_val.df)

# start 8 PI % vol regressions
cash_frac_val_reg_model = Cash ~ in_person_frac + med_val + med_hh_income
check_frac_val_reg_model = Check ~ in_person_frac + med_val + med_hh_income
credit_frac_val_reg_model = Credit ~ in_person_frac + med_val + med_hh_income
debit_frac_val_reg_model = Debit ~ in_person_frac + med_val + med_hh_income
prepaid_frac_val_reg_model = Prepaid ~ in_person_frac + med_val + med_hh_income
banp_frac_val_reg_model = BANP ~ in_person_frac + med_val + med_hh_income
obbp_frac_val_reg_model = OBBP ~ in_person_frac + med_val + med_hh_income
acct2acct_frac_val_reg_model = Acct2acct ~ in_person_frac + med_val + med_hh_income
#
cash_frac_val_reg.lm = lm(cash_frac_val_reg_model, data = all_frac_reg_val.df)
check_frac_val_reg.lm = lm(check_frac_val_reg_model, data = all_frac_reg_val.df)
credit_frac_val_reg.lm = lm(credit_frac_val_reg_model, data = all_frac_reg_val.df)
debit_frac_val_reg.lm = lm(debit_frac_val_reg_model, data = all_frac_reg_val.df)
prepaid_frac_val_reg.lm = lm(prepaid_frac_val_reg_model, data = all_frac_reg_val.df)
banp_frac_val_reg.lm = lm(banp_frac_val_reg_model, data = all_frac_reg_val.df)
obbp_frac_val_reg.lm = lm(obbp_frac_val_reg_model, data = all_frac_reg_val.df)
acct2acct_frac_val_reg.lm = lm(acct2acct_frac_val_reg_model, data = all_frac_reg_val.df)
#
summary(cash_frac_val_reg.lm)
summary(credit_frac_val_reg.lm)
summary(debit_frac_val_reg.lm)
summary(prepaid_frac_val_reg.lm)
summary(banp_frac_val_reg.lm)
summary(obbp_frac_val_reg.lm)
summary(acct2acct_frac_val_reg.lm)



##########
### THE END OF merch_200510.R ###

## Unsued code (cancelled Fig 2)
# ### Start figure 2: VAL share by merc type [Cancelled, use ]
# # Does not work on old tidyr, use home machine
# all_frac2_val.df
# (all_frac3_val.df = all_frac2_val.df[-22, ]) # Remove line 22 (all) b/c it is used only in Table
# (all_frac3_val.df$Merchant = merch_abv_fig.vec)# use abrv merchant description
# 
# #(all_frac4_val.df = all_frac3_val.df)
# #(all_frac4_val.df$Merchant = as.factor(c(1:21)))
# 
# ########################
# # pivot longer ### Cannot run on Work machine (outdated tiryr!!!)
# #all_frac3_val_long.df <- all_frac3_val.df %>% pivot_longer(cols = c("Cash", "Check", "Credit", "Debit", "Prepaid", "BANP", "OBBP", "Acct2acct"), names_to = "Method", values_to = "amount")
# # change chr column to factor column
# #all_frac3_val_long.df$Merchant <- all_frac3_val_long.df$Merchant %>% as.factor
# # reorder levels of factor, sorted by alphabetical order
# #levels(all_frac3_val_long.df$Merchant) <- all_frac3_val_long.df$Merchant %>% unique %>% sort
# ########################
# # for work machine, use gather (old version of tidyr)
# all_frac4_val_long.df <- all_frac3_val.df %>% gather(key = Method, value = amount, -Merchant) # -Merchant means all columns except Merchant.
# head(all_frac4_val_long.df) 
# 
# 
# # change chr column to factor column
# all_frac4_val_long.df$Merchant <- all_frac4_val_long.df$Merchant %>% as.factor
# 
# # reorder levels of factor, sorted by alphabetical order (for some reason, on work machine the sorfting works w/o the sort)
# levels(all_frac4_val_long.df$Merchant) <- all_frac4_val_long.df$Merchant %>% unique %>% sort
# levels(all_frac4_val_long.df$Merchant) <- all_frac4_val_long.df$Merchant %>% unique 
# 
# all_val_fig = ggplot(all_frac4_val_long.df, aes(x = Merchant, y = amount, fill = Method)) + geom_bar(stat = "identity", position = "fill")
# (all_val_fig = all_val_fig + xlab("") + ylab("Payment method (%)"))
# #(all_val_fig = all_val_fig + annotate("text", x = m8$merch_abv_fig, y = 1.03,                      label = round(m7$hhi,0), size = 3.5))
# (all_val_fig = all_val_fig + theme(axis.text=element_text(size=12, angle = 60,hjust=0.95,vjust=0.2)))
# #

