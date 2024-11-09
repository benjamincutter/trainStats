#Set working directory

traindir <- "./traindata2024"
sourcedir <-"/Users/bcutter/UVA/statisticalModeling/trains"


# Load the data
setwd(sourcedir)
source("AccidentInput.R")

#load libraries
library(ggplot2)
library(GGally)
library(devtools) # for ggbiplot
library(ggbiplot)
library(psych)
library(ggpubr)
library(lattice)


acts <- file.inputl(traindir)

totacts <- combine.data(acts)


# Create a new variable called Casualty which is the sum of TOTKLD and TOTINJ over all years of data.
totacts$Casualty <- totacts$TOTKLD + totacts$TOTINJ

# describe totalacts$Casualty
describe(totacts$Casualty)

# Produce a box plot for total casualties (TOTKLD + TOTINJ) across all years of rail accidents. Which best describes the results shown in this plot?
casualty.boxplot = ggplot(totacts, aes(y = Casualty)) +
  geom_boxplot() +
  labs(title = "Boxplot of Casualties", x = "", y = "Casualties")

casualty.boxplot


# find the proportion of casualties that has at least 1 casualty
prop.table(table(totacts$Casualty >= 1))

# Only 5.8% of the data has at least 1 casualty

# create a dataframe with only accidents with one or more casualties called totacts_posCas
totacts_posCas <- totacts[totacts$Casualty >= 1,]

# find the size of totacts_posCas
dup_size <- dim(totacts_posCas)


# remove duplicates from totacts_posCas
totacts_posCas_nodup <- totacts_posCas[!(duplicated(totacts_posCas[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]
dim(totacts_posCas_nodup)

# 95 dupes removed

# Produce box plots of rail accidents each year (e.g., use bwplot())
bwplot(totacts_posCas_nodup$YEAR ~ totacts_posCas_nodup$Casualty, main = "Boxplot of Casualties by Year", xlab = "Casualties", ylab = "Year")

# get the row with max casualties
totacts_posCas_nodup[totacts_posCas_nodup$Casualty == max(totacts_posCas_nodup$Casualty),]

# after looking at this row a bit more, I decided we will leave it in. 
# while it is a bit of a n outlier I think it's a realistic piece that should be included in the analysis

# compare casualties to visibility
ggplot(totacts_posCas_nodup, aes(x = VISIBLTY, y = Casualty)) +
  geom_point() +
  labs(title = "Casualties by Visibility", x = "Visibility", y = "Casualties")

ggpairs(totacts_posCas_nodup[,c("Casualty", "TRNSPD", "CARS", "TEMP", "VISIBLTY", "WEATHER")])

boxplot(totacts_posCas_nodup$Casualty ~ totacts_posCas_nodup$TYPTRK, xlab = "Track Type", ylab = "Casualties")

# create scatter plot of casualties by visibility
ggplot(totacts_posCas_nodup, aes(x = VISIBLTY, y = Casualty)) +
  geom_point() +
  labs(title = "Casualties by Visibility", x = "Visibility", y = "Casualties")

# linear model for casualties, track type, and train speed
lm_casualty <- lm(Casualty ~ TYPTRK * TRNSPD, data = totacts_posCas_nodup)
summary(lm_casualty)

#summarize speed by track type
summary(totacts_posCas_nodup$TRNSPD[totacts_posCas_nodup$TYPTRK == 1])
summary(totacts_posCas_nodup$TRNSPD[totacts_posCas_nodup$TYPTRK == 2])
summary(totacts_posCas_nodup$TRNSPD[totacts_posCas_nodup$TYPTRK == 3])
summary(totacts_posCas_nodup$TRNSPD[totacts_posCas_nodup$TYPTRK == 4])

# create a bar plot of sum of casualties and method
ggplot(totacts_posCas_nodup, aes(x = METHOD, y = Casualty)) +
  geom_bar(stat = "identity") +
  labs(title = "Casualties by Method", x = "Method", y = "Casualties")

boxplot(totacts_posCas_nodup$Casualty ~ totacts_posCas_nodup$METHOD, xlab = "Method", ylab = "Casualties")

lm_casualty_method <- lm(Casualty ~  TRNSPD * CARS * VISIBLTY, data = totacts_posCas_nodup)
summary(lm_casualty_method)

# create a bar plot of sum of casualties and visibility
ggplot(totacts_posCas_nodup, aes(x = VISIBLTY, y = Casualty)) +
  geom_bar(stat = "identity") +
  labs(title = "Casualties by Visibility", x = "Visibility", y = "Casualties")

# creat a bar plot for the sum of casualties by track type
ggplot(totacts_posCas_nodup, aes(x = TYPTRK, y = Casualty)) +
  geom_bar(stat = "identity") +
  labs(title = "Casualties by Track Type", x = "Track Type", y = "Casualties")

# bar plot plot by month
ggplot(totacts_posCas_nodup, aes(x = DIVISION, y = Casualty)) +
  geom_bar(stat = "identity") +
  labs(title = "Casualties by Month", x = "Month", y = "Casualties")

# bar plot of only trains going east by visibility
ggplot(totacts_posCas_nodup[totacts_posCas_nodup$TRNDIR == 4,], aes(x = VISIBLTY, y = Casualty)) +
  geom_bar(stat = "identity") +
  labs(title = "Casualties by Visibility for Eastbound Trains", x = "Visibility", y = "Casualties")
