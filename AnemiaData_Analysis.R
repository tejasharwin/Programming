###################################################################################
###################################################################################
######
###### 
###### 
######                        
######  This script is used to conduct statistical analysis on the Haemoglobin 
######  levels (Hb levels) of women in Afghanistan. The aim of the analysis is 
######  to understand the variation in Hb levels and thereafter 
######  construct a tatistical model to predict Hb levels among a subset of
######  Afghan women.
######  
######  
######  Structure:
######  
######  Part1 : The script starts by seprating the 'AnemiaData.csv' file into
######          two groups. 'Group1', for which Hb levels are given and 'Group2' 
######          for which Hb levels are missing (given by -1 in 'AnemiaData.csv' 
######          file in the Haemoglobin column.
######  
######  Part 2a: Exploratory data analysis is conducted on 'Group1' to scout for
######           covariates that help explain variation in Hb levels and pave the 
######           path towards model building.
######
######  Part 2b: Continued Exploratory data analysis on Interaction Effects
######           
######
######  Part 3:  Model building process
######
######
######  Part 4:  Predicitions and graphs for report
######
######
######  This scipt uses the 'RColourBrewer' package.
######
####################################################################################
####################################################################################
library("RColorBrewer", lib.loc="~/R/win-library/3.5")
#
#
#
#######################################################################
# Part 1. Data Preprocessing
#######################################################################
#
#
#
AnemiaData <- read.csv("AnemiaData.csv",na.strings="-1")  # setting missing value fields as NA
#
Group1 <- AnemiaData[!is.na(AnemiaData$Haemoglobin), ]      # Group 1 will be fitting data
#
sum(is.na(Group1)) # double checking if I have any missing values
#
#
#
#######################################################################
# Part 2a. Exploratory data analysis: Each covariate
#######################################################################
#
# In this part of the script, I aim to gain useful insights
# about the relationships between covariates and Hb levels and between the covariates themselves.
# Consequently, I will use the insight gained to guide me with model building in Part 3.
#
# Some questions to bear in mind:
#
## Are there any obvious outliers that should be investigated?
## Are relationships given by literature apparent, for instance, 
## relationship with Age, Pregnancy, Nutrition, Hygiene?
## Do any variables show linear dependence with Hb levels?
#
#
#
# Varible of Interest: Hb levels g/Dl
#
#
#
cat("Summary of Hb levels g/Dl:\n")
print(summary(Group1$Haemoglobin))
#
par(mfrow=c(1,1))
plot(density(Group1$Haemoglobin),      # How does the empirical distribution of Hb levels look?
      col = "firebrick",                     
      lwd = 3, 
      main = "ePDF of Hb levels", 
      xlab = "Hb Level g/Dl")
#
#
#
plot(Haemoglobin ~ ID, data = Group1, pch = 16,
     main = "Hb levels g/Dl of Fitting Data")     # Visual inspection of points Hb levels outside   
abline(h = c(11,16), lwd = 2, col = "blue")         # medical range 
#
#
#
Out.of.Range <- 100 *nrow(Group1[Group1$Haemoglobin > 16 | Group1$Haemoglobin < 11,])/nrow(Group1)
cat("\n\nPercentage of females whose Hb level is outside the medical range is:\n",Out.of.Range,"%")
#
# General knowledge tells me Hb levels below 5 and above 20 are extremely concerning and considering the lowest Hb level
# observed was 1.9, I wanted to get a feel of how many extremely unusual, perhaps implausible observations exist.
#
abline(h = c(5,20), lwd = 3, las = 1, col = "red")
box(lwd=2)
#
dev.copy(pdf,"Hblevels.pdf",width=8,height=6)
dev.off()
#
cat("\n\nHb levels of those below 5 g/Dl:\n")
print(as.matrix(Group1$Haemoglobin[Group1$Haemoglobin <= 5]))
#
# These low Hb levels made me hesitate. However, considering heterogeneity in such a large sample, consulting with UCL medical
# students and viewing sources of women with Hb levels as low as 2.0, and considering the authors of the 2016 
# suvey didn't drop the extremely low datapoints, I have decided to accept them in my dataset and made a note
# to investigate them further on, perhaps they are influential and/or levearage points.
#
#
#
# Investigation of covariates:
#
#
#
# The funciton 'plotv2', ie plot version 2, will save me from using the cor function everytime and will 
# display the correlation as the title of the graph which is more pleasing.
#
#
plotv2 <- function(x,y,...){
  title <- paste("Correlation:",round(cor(x,y),3))
  plot(x,y, main = title, cex.main = 1.5,font.main = 3, ...)}
#
#
#
##### Covariates with known effects on Hb levels #####
#
#
#
# Age:
#
#
cat("\n\nSummary of Age among respondents g/Dl:\n")
print(summary(Group1$Age))
{par(mfrow=c(2,1));boxplot(Group1$Age, horizontal = T,xlab = "Age")
  barplot(table(Group1$Age), 
          col = "firebrick", 
          xlab = "Age", 
          ylab = "Frequency")}
#
par(mfrow=c(1,1))
#
plotv2(Group1$Age,
       Group1$Haemoglobin,
       xlab = "Age", 
       ylab = "Hb level g/Dl")
abline(h = c(10,16), col = "blue", lwd = 2)
abline(h = c(5,20), lwd = 3, col = "red")
#
Age.Below30.Percent <- 100 * length(Group1$Age[Group1$Age <= 30])/nrow(Group1)
cat("\nPercentage of females below or equal to 30 years old:\n",Age.Below30.Percent,"%")
#
AgeSDs <- tapply(Group1$Haemoglobin, Group1$Age, FUN = sd)      # standard deviation for each age category
AgeMeans<- tapply(Group1$Haemoglobin, Group1$Age, FUN = mean)  # mean deviation for each age category
#
cat("\n\nStandard deviation of Hb levels for each age group in ascending order:\n\n")  
print(sort(AgeSDs))    #output in ascending order
#
cat("\n\nMean of Hb levels for each age group in ascending order:\n\n")
print(sort(AgeMeans))
#
# Plot the average and standard deviations of Hb levels for each age group
#
plotv2(as.numeric(rownames(AgeSDs)),as.numeric(AgeSDs),
       xlab = "Age", 
       ylab = "Hb level g/Dl")
#
# Locally weighted scatterplot smoothing curve to add visual appeal
#
lines(lowess(as.numeric(rownames(AgeSDs)),as.numeric(AgeSDs)), lwd = 2, col = "firebrick")
#
plotv2(as.numeric(rownames(AgeMeans)),as.matrix(AgeMeans), 
       xlab = "Age", 
       ylab = "Average Hb level g/Dl",
       pch = 16,
       las = 1)
lines(lowess(as.numeric(rownames(AgeMeans)),as.numeric(AgeMeans)), 
      pch = 16,
      lwd = 3,
      col = "firebrick")
box(lwd=2)
#
dev.copy(pdf,"AgeMeans.pdf",width=6,height=6)
dev.off()
#
#
barplot(table(Group1$Age), col = "firebrick", xlab = "Age", ylab = "Frequency")
#
boxplot(Group1$Haemoglobin ~ Group1$Age, horizontal = T,       # Eye inspection of any trends
        ylab = "Age",
        xlab = "Hb level g/Dl", 
        col = "firebrick")
#
#
#
# One way Anova
#
#
#
Age.aov <- aov(Haemoglobin ~ as.factor(Age), data = Group1)
cat("\n\nResults from One-way ANOVA of AgeGroup:\n\n")
print(summary(Age.aov))
cat("\n\nSufficient evidence at the 10% significance level to suggest the average Hb level for 
    at least one age group is different:\n\n")

#
## Create new variable called Age group, in ORGINAL DATASET
#
AgeGroup <- c()                                                        # If aged 25-33, then
for (i in 1:length(AnemiaData$Province)){                              # classify as MidAge
  if (any(AnemiaData$Age[i]==c(25:33) )){AgeGroup[i] <- "MidAge"}      # Otherwhise, Young.or.Aged
  else {AgeGroup[i] <- "Young.or.Aged"} 
}
AnemiaData <- cbind(AgeGroup,AnemiaData)                               
Group1 <- AnemiaData[!is.na(AnemiaData$Haemoglobin), ]    # Subset Group to continue analysis
#
#
#
## Reminder: TRY TWO WAY ANOVA test with other covariates in Part2b.
#
# t-test between means between the levels of the factor 'AgeGroup'
#
var.test(Group1$Haemoglobin[Group1$AgeGroup=="MidAge"],           # Before t-test, test whether the two
         Group1$Haemoglobin[Group1$AgeGroup=="Young.or.Aged"])    # population variances for Hb levels are the same?
#
cat("\nResults from One-Sided t-test on AgeGroup:\n\n")
print(t.test(Group1$Haemoglobin[Group1$AgeGroup=="MidAge"],
       Group1$Haemoglobin[Group1$AgeGroup=="Young.or.Aged"],
       var.equal = FALSE, alternative = "less"))
cat("\nSufficient evidence at the 5% significance level to suggest the average Hb level for
    those aged 25-33 is lower than those out of the range:\n\n")              
#
#
# Subsequent analysis follows a similar format for the remaining covariates...
#
#
#
# Weath Score: constructed using PCA by 2016 research paper authors:
#
#
#
par(mfrow=c(1,1))
cat("\n\nSummary of WealthScore among respondents g/Dl:\n")
print(summary(Group1$WealthScore))
boxplot(Group1$WealthScore, horizontal = T,xlab = "WealthScore")
#
plotv2(Group1$WealthScore, Group1$Haemoglobin, 
       xlab = "Wealth Score", 
       ylab = "Hb level g/Dl")
#
quantile(Group1$WealthScore)
cat("\nThe median WealthScore among the respondents is:\n", quantile(Group1$WealthScore, 0.5),"%")
#
# From the WealthScore vs Hb Level plot, we see more variance for those below the median...
# Let's make a new variable that has two levels, one for those below the median WeathScore,
# and one for those above.
#
# Ensuring we're creating the variable in the ORIGINAL dataset
#
WealthScoreGroup <- c()
for (i in 1:length(AnemiaData$WealthScore)){
  if (any(AnemiaData$WealthScore[i] <= as.numeric(quantile(AnemiaData$WealthScore, 0.5)) )){WealthScoreGroup[i] <- "Below.Median"}
  else {WealthScoreGroup[i] <- "Above.Median"} 
}
AnemiaData <- cbind(WealthScoreGroup,AnemiaData)
Group1 <- AnemiaData[!is.na(AnemiaData$Haemoglobin), ]    # subset again to continue analysis
#
#
#
# t-test of means of Hb levels between females whose WealthScore is below the median and those above
#
var.test(Group1$Haemoglobin[Group1$WealthScoreGroup=="Below.Median"],
         Group1$Haemoglobin[Group1$WealthScoreGroup=="Above.Median"])
#
cat("\nResults from One-Sided t-test on WealthScoreGroup:\n\n")
#
print(t.test(Group1$Haemoglobin[Group1$WealthScoreGroup=="Below.Median"],
             Group1$Haemoglobin[Group1$WealthScoreGroup=="Above.Median"], 
             alternative = c("less")))
#
cat("\nSufficient evidence at the 5% significance level to suggest the average Hb level for
    females whose WealthScore is below the median to be lower than those above\n\n")
#
#
#
# Pregnant:
#
#
#
summary(Group1$Pregnant)
boxplot(Haemoglobin ~ Pregnant, data = Group1, horizontal = T, 
        ylab = "Was individual pregnant during the blood test?", xlab = "Hb level g/Dl")
#
# Quick visual inspection of differences in average Hb levels between the two levels
# ie Pregnant and NOT Pregnant
#
abline(v = c(tapply(Group1$Haemoglobin,Group1$Pregnant, mean)), col = "firebrick")
#
#
# t-test of means of Hb levels between pregnant and non-pregnant females.
#
var.test(Group1$Haemoglobin[Group1$Pregnant=="Yes"],Group1$Haemoglobin[Group1$Pregnant=="No"])
t.test(Group1$Haemoglobin[Group1$Pregnant=="Yes"],Group1$Haemoglobin[Group1$Pregnant=="No"], var.equal = T)
#
#
#
# RecentBirth:
#
#
#
## Those who have had a recent birth can't have total children as 0, just as a check!
##
cat("\n\nIs it TRUE that there exists individuals who have had a recent birth but the total children column takes the value 0 ?\n\n")
cat(isTRUE(Group1[Group1$RecentBirth=="Yes" & Group1$TotalChildren==0,]))
##
##
summary(Group1$RecentBirth)
#
boxplot(Haemoglobin ~ RecentBirth, data = Group1, horizontal = T, 
        ylab = "Did the individual give birth in the last two years?", 
        xlab = "Hb level g/Dl")
abline(v = c(tapply(Group1$Haemoglobin,Group1$RecentBirth, mean)), col = "firebrick")
#
#
# t-test of means of Hb levels between females who have and have not given birth in the last two years
#
var.test(Group1$Haemoglobin[Group1$RecentBirth=="Yes"],Group1$Haemoglobin[Group1$RecentBirth=="No"])
t.test(Group1$Haemoglobin[Group1$RecentBirth=="Yes"],Group1$Haemoglobin[Group1$RecentBirth=="No"], var.equal = T)
#
#
#
# Does the household own any Sheep?
#
#
#
summary(Group1$Sheep)
boxplot(Haemoglobin ~ Sheep, data = Group1, horizontal = T, 
        ylab = "Did the individual give birth in the last two years?",
        xlab = "Hb level g/Dl")
abline(v = c(tapply(Group1$Haemoglobin,Group1$Sheep, mean)), col = "firebrick")
#
#
# t-test of means of Hb levels between females who have and have not given birth in the last two years
#
var.test(Group1$Haemoglobin[Group1$Sheep=="Yes"],Group1$Haemoglobin[Group1$Sheep=="No"])
t.test(Group1$Haemoglobin[Group1$Sheep=="Yes"],Group1$Haemoglobin[Group1$Sheep=="No"], var.equal = FALSE)
#
#
#
# Does the individual live in a rural area?
#
#
#
summary(Group1$Rural)
boxplot(Haemoglobin ~ Rural, data = Group1, horizontal = T, 
        ylab = "Does the individual live in a rural area?", 
        xlab = "Hb level g/Dl")
abline(v = c(tapply(Group1$Haemoglobin,Group1$Rural, mean)), col = "firebrick")
#
#
# t-test of means of Hb levels between females who have and have not given birth in the last two years
#
var.test(Group1$Haemoglobin[Group1$Rural=="Yes"],Group1$Haemoglobin[Group1$Rural=="No"])
t.test(Group1$Haemoglobin[Group1$Rural=="Yes"],Group1$Haemoglobin[Group1$Rural=="No"], var.equal = T)
#
#
#
# Findings from exploring the focused covariates:
#
# Pregnant and RecentBirth look to be the most significant.
#
#
#
##### Other covariates  #####
#
#
#
# Does any household member own agricultural land?
#
#
summary(Group1$AgricLandOwn)
boxplot(Haemoglobin ~ AgricLandOwn, data = Group1, horizontal = T, 
        ylab = "Does the individual live in a AgricLandOwn area?", 
        xlab = "Hb level g/Dl")
abline(v = c(tapply(Group1$Haemoglobin,Group1$AgricLandOwn, mean)), col = "firebrick")
#
#
var.test(Group1$Haemoglobin[Group1$AgricLandOwn=="Yes"],Group1$Haemoglobin[Group1$AgricLandOwn=="No"])
t.test(Group1$Haemoglobin[Group1$AgricLandOwn=="Yes"],Group1$Haemoglobin[Group1$AgricLandOwn=="No"], var.equal = T)
#
#
# Ethnicity 
#
#
cat("\n\nPercentage of respondents who are the following ethnicity (%):\n")
print(100 * (table(Group1$Ethnicity)) / 4382)
boxplot(Haemoglobin ~ Ethnicity, data = Group1, horizontal = T, 
        ylab = "Ethnicity", 
        lab = "Hb level g/Dl")
EthnicMeans <- sort((tapply(Group1$Haemoglobin,Group1$Ethnicity, mean)))
EthnicNames <- rownames(sort((tapply(Group1$Haemoglobin,Group1$Ethnicity, mean))))
#
plot(EthnicMeans ~ factor(EthnicNames),  # plot the average Hb level for each Ethnic Group
     xlab = "Ethnicity",
     ylab = "Hb level g/Dl")  
#
##
Ethnicity.aov <- aov(Haemoglobin ~ Ethnicity, data = Group1)
cat("\nResults from One-way ANOVA of Ethnicity:\n\n")
print(summary(Ethnicity.aov))
cat("\nSufficient evidence at the 5% significance level to suggest the average Hb level for
    at least one Ethnicity is different:\n\n")
#
#
#
# Does the household have Electricity?
#
#
summary(Group1$Electricity)
boxplot(Haemoglobin ~ Electricity, data = Group1, horizontal = T, 
        ylab = "Does the individual live in a Electricity area?",
        xlab = "Hb level g/Dl")
#
abline(v = c(tapply(Group1$Haemoglobin,Group1$Electricity, mean)), col = "firebrick")
#
#
var.test(Group1$Haemoglobin[Group1$Electricity=="Yes"],Group1$Haemoglobin[Group1$Electricity=="No"])
t.test(Group1$Haemoglobin[Group1$Electricity=="Yes"],Group1$Haemoglobin[Group1$Electricity=="No"], var.equal = T)
#
#
#
# Does the household have treated water?
#
#
summary(Group1$TreatedWater)
boxplot(Haemoglobin ~ TreatedWater, data = Group1, horizontal = T, 
        ylab = "Does the household have access to treated water?", 
        xlab = "Hb level g/Dl")
#
abline(v = c(tapply(Group1$Haemoglobin,Group1$TreatedWater, mean)), col = "firebrick")
#
#
var.test(Group1$Haemoglobin[Group1$TreatedWater=="Yes"],Group1$Haemoglobin[Group1$TreatedWater=="No"])
t.test(Group1$Haemoglobin[Group1$TreatedWater=="Yes"],Group1$Haemoglobin[Group1$TreatedWater=="No"], var.equal = T)
#
#
#
# Agriculture land Area 
#
#
#
summary(Group1$AgricArea)
barplot(table(Group1$AgricArea), 
        col = "firebrick", 
        xlab = "Agricultural Area", 
        ylab = "Frequency")
#
par(mfrow=c(2,1))
#
plotv2(Group1$AgricArea, Group1$Haemoglobin, 
       xlab = "Agricultural Area", 
       ylab = "Hb level g/Dl")
AgricMeans <- sort((tapply(Group1$Haemoglobin, Group1$AgricArea, FUN =mean)))
plotv2(as.numeric(rownames(AgricMeans)),as.numeric(AgricMeans),
       xlab = "Agricultural Area", 
       ylab = "Mean Hb level g/Dl")
#
#
#
# Household size:
#
#
#
summary(Group1$HHSize)
boxplot(Group1$HHSize, horizontal = T,xlab = "Household Size")
barplot(table(Group1$HHSize), 
        col = "firebrick",
        xlab = "Household Size", 
        ylab = "Frequency")
#
par(mfrow=c(1,1))
#
plotv2(Group1$HHSize, Group1$Haemoglobin, xlab = "No.of household members", ylab = "Hb level g/Dl")
abline(h = c(10,16), lwd = 2, col = "blue")
abline(h = c(5,20), lwd = 3, col = "red")
#
HHSizeMeans <- sort((tapply(Group1$Haemoglobin, Group1$HHSize, FUN =mean)))
#
plotv2(as.numeric(rownames(HHSizeMeans)),as.matrix(HHSizeMeans),
       xlab = "Household Size",
       ylab = "Mean Hb level g/Dl")
lines(lowess(as.numeric(rownames(HHSizeMeans)),as.numeric(HHSizeMeans)), lwd = 2, col = "firebrick")
#
boxplot(Haemoglobin ~ HHSize, data = Group1, xlab = "HHSize", ylab = "Hb level g/Dl")
#
#
#
# Number of children below the age of 5 in household:
#
#
#
par(mfrow=c(2,1))
summary(Group1$HHUnder5s)
boxplot(Group1$HHUnder5s, horizontal = T,xlab = "No.of childen under 5")
barplot(table(Group1$HHUnder5s), 
        col = "firebrick", 
        xlab = "No.of childen under 5",
        ylab = "Frequency")
#
plotv2(Group1$HHUnder5s, Group1$Haemoglobin, xlab = "No.of children below 5 in household", ylab = "Hb level g/Dl")
#
HHUnder5s.Means <- sort((tapply(Group1$Haemoglobin, Group1$HHUnder5s, FUN =mean)))
par(mfrow=c(1,1));plot(as.numeric(rownames(HHUnder5s.Means)),as.matrix(HHUnder5s.Means), xlab = "No.of children below 5 in household", ylab = "Mean Hb level g/Dl")
#
#
#
# One household have 9 kids below the age of 5.
# Relationship with Hb levels: Does not reveal much. The kids can be of other women in the household.
# I may expect some relationship with the individual's own kids. Perhaps some links with
# Pregnancy-related anemia, which stems from iron-deficiency anemia...
#
#
#
# Vehicle ownership of Household:
#
#
#
summary(Group1$BikeScootCar)
barplot(table(Group1$BikeScootCar), 
        col = "firebrick", 
        xlab = "Proportion of vehicles owned",
        ylab = "Frequency")
#
plotv2(Group1$BikeScootCar, Group1$Haemoglobin, xlab = "No.of children below 5 in household", ylab = "Hb level g/Dl")
#
#
#
# No.of children born to individual:
#
#
#
par(mfrow=c(2,1))
summary(Group1$TotalChildren)
boxplot(Group1$TotalChildren, horizontal = T,xlab = "No.of childen under 5")
barplot(table(Group1$TotalChildren), 
        col = "firebrick",
        xlab = "Total Children", 
        ylab = "Frequency")
#
par(mfrow=c(1,1))
#
plotv2(Group1$TotalChildren, Group1$Haemoglobin, xlab = "No.of children born to individual", ylab = "Hb level g/Dl")
#
#
#
# Education level
#
#
table(Group1$Education)
boxplot(Haemoglobin ~ Education, data = Group1, horizontal = T, 
        ylab = "To what level is the individual educated?", 
        xlab = "Hb level g/Dl")
#
abline(v = c(tapply(Group1$Haemoglobin,Group1$Education, mean)), col = "firebrick")
#
tapply(Group1$Haemoglobin,Group1$Education, mean)
Education.aov <- aov(Haemoglobin ~ Education, data = Group1)
summary(Education.aov)
#
#
#
# Region 
#
#
cat("\n\nProportion of females by Region (%):\n")
print(100 * (table(Group1$Region)) / 4382)
boxplot(Haemoglobin ~ Region, data = Group1, horizontal = T, 
        ylab = "Region", 
        xlab = "Hb level g/Dl")
RegionMeans <- sort((tapply(Group1$Haemoglobin,Group1$Region, mean)))
RegionNames <- rownames(sort((tapply(Group1$Haemoglobin,Group1$Region, mean))))
plot(RegionMeans ~ factor(RegionNames), xlab = "Region", ylab = "average Hb level g/Dl")
#
Region.aov <- (aov(Haemoglobin ~ Region, data = Group1 ))
cat("\n\nResults from One-way ANOVA of Region:\n\n")
print(summary(Region.aov))
cat("\nSufficient evidence at the 5% significance level to suggest the average Hb level for
    at least one Region is different:\n\n")
#
#
#
# Province 
#
#
#
cat("\n\nProportion of females by Province (%):\n")
print(100 * (table(Group1$Province)) / 4382)
boxplot(Haemoglobin ~ Province, data = Group1, horizontal = T, 
        ylab = "Province", 
        xlab = "Hb level g/Dl")
#
ProvinceMeans <- sort((tapply(Group1$Haemoglobin,Group1$Province, mean)))
ProvinceNames <- rownames(sort((tapply(Group1$Haemoglobin,Group1$Province, mean))))
#
plot(ProvinceMeans ~ factor(ProvinceNames), xlab = "Province", ylab = "average Hb level g/Dl")
#
Province.aov <- aov(Haemoglobin ~ Province, data = Group1 )
cat("\n\nResults from One-way ANOVA of Province:\n\n")
print(summary(Region.aov))
cat("\nSufficient evidence at the 5% significance level to suggest the average Hb level for
    at least one Province is different:\n\n")
#
#
#
#######################################################################
# Part 2b. Exploratory data analysis: Interactions
#######################################################################
#
#
#
# In this section, I aim to seek insight on covariates fare in the prescence of Province and
# test sensible interactions which make sense when considering the context and literature. Especially 
# those with literature.
# 
#
#
#
summary(aov(Haemoglobin ~ Province*Rural, data = Group1))
summary(aov(Haemoglobin ~ Province*Pregnant, data = Group1))
summary(aov(Haemoglobin ~ Province*AgeGroup, data = Group1))
summary(aov(Haemoglobin ~ Province*WealthScoreGroup, data = Group1))
summary(aov(Haemoglobin ~ Province*Electricity, data = Group1))
summary(aov(Haemoglobin ~ Province*AgricLandOwn, data = Group1))
summary(aov(Haemoglobin ~ Province*TreatedWater, data = Group1))
summary(aov(Haemoglobin ~ Province*Ethnicity, data = Group1))
summary(aov(Haemoglobin ~ Province*Education, data = Group1))
#
#
#
# Do interactive effects exist with Rural?
#
summary(aov(Haemoglobin ~ Rural*RecentBirth, data = Group1))
summary(aov(Haemoglobin ~ Rural*Sheep, data = Group1))
summary(aov(Haemoglobin ~  Rural*Education, data = Group1))
summary(aov(Haemoglobin ~ Rural*Electricity, data = Group1))
#
#
#
# Do interactive effects exist with Sheep?
#
summary(aov(Haemoglobin ~ Sheep*WealthScoreGroup, data = Group1))
summary(aov(Haemoglobin ~ Sheep*TreatedWater, data = Group1))
#
#
#
# Do interactive effects exist with WealthScoreGroup?
#
summary(aov(Haemoglobin ~ WealthScoreGroup*AgricLandOwn, data = Group1))
summary(aov(Haemoglobin ~ WealthScoreGroup*Education, data = Group1))
#
#
#
#######################################################################
# Part 3. Model Building
#######################################################################
#
#
#
#msumzero <- (lm (Group1$Haemoglobin ~ RecentBirth:Pregnant + Province + Pregnant + RecentBirth, data = Group1,contrasts=list(Province="contr.sum")))
#
model0 <- (lm (Haemoglobin ~ Province + RecentBirth + Pregnant, data = Group1))
par(mfrow=c(2,2))
plot(model0)                                              # How do the diagnostic plots look?
par(mfrow=c(1,1));plot(density(model0$fitted.values))     # What does the distribution of model zero's fitted values look like?
summary(model0)
AIC(model0)
#
# 
# There are several coefficients to estimate and the standard errors seem high. 
# The high number of levels willcause issues when looking to combinine Province with other covariates. 
# Try to reduce the number of levels by clustering...
#
#
#
################################################
#########      Eye-Ball Clustering     #########
################################################
#
#
#
boxplot(Haemoglobin ~ Province, data = Group1, horizontal = T, 
        xlab = "Hb level g/Dl", xlab = "", col = "firebrick", las = 1, cex.axis = 0.7);box(lwd=2)
#
Prov.Means.Ascend <- (sort((tapply(Group1$Haemoglobin,Group1$Province, mean))))
#
plot(Prov.Means.Ascend, xlab = "Province number", ylab = "mean Hb level g/Dl",
     main = "Mean Hb levels across Provinces", pch = 16)
#
Group1$Province <- factor(Group1$Province, levels= c(rownames(Prov.Means.Ascend)), ordered = TRUE)
#
boxplot(Haemoglobin ~ Province, data = Group1, horizontal = T,  # in ascending order
        xlab = "Hb level g/Dl", 
        xlab = "",
        main = "Ascendence by Mean Hb level ",
        col = "firebrick", 
        las = 1, 
        cex.axis = 0.7)
box(lwd=2)
#
dev.copy(pdf,"ProvinceBoxPlots.pdf",width=7,height=12)
dev.off()
#
# plot the average Hb levels for each province with Province name on x-axis rather than a number.
#
Prov.Names.Ascend <- as.factor(rownames(Prov.Means.Ascend))
#
# change the order in which the levels to help you plot the coloured graph below...
#
Prov.Names.Ascend <- factor(Prov.Names.Ascend, levels= c(rownames(Prov.Means.Ascend)), ordered = TRUE)
#
plot(Prov.Means.Ascend ~ factor(Prov.Names.Ascend),ylab = "Hb level g/Dl" ,las = 2, cex.axis = 0.8, xlab = "")
#
# Try splitting Provinces into those with low and higher average Hb levels and
# visually inspect how it looks
#
Prov.Group <- c()
for (i in 1:length(Prov.Means.Ascend)){
  if (any(i==c(1:6))){Prov.Group[i] <- "Low1"}
  else if (any(i==c(7:11))) {Prov.Group[i] <- "Low2"}
  else if (any(i==c(12:17))) {Prov.Group[i] <- "Medium1"}
  else if (any(i==c(18:22))) {Prov.Group[i] <- "Medium2"}
  else if (any(i==c(23:26))) {Prov.Group[i] <- "High1"}
  else {Prov.Group[i] <- "High2"}
}
#
#
#
# Research on Wiki, for similaries of population, wealth, which provinces are bordered,
# which provinces are near the capital, which provinces have more area, look for clues online...
#
# Plot the average Hb levels, colour code according to levels of ProvGroup...

# NOTE: plot(Prov.Means.Ascend) was used rather than factor(Prov.Names.Ascend)
#       BECAUSE otherwise, colouring the graph according to ProvGroup levels won't be possible.
#       Hence plot(Prov.Means.Ascend) was used, the orignial x-axis was then removed and 
#       replaced by the Province Names in the appropriate ascending order.
#
Prov.Group<-as.factor(Prov.Group)
Prov.Group<-factor(Prov.Group, levels=c('Low1','Low2','Medium1','Medium2', 'High1', 'High2'), ordered = FALSE)
colours <- RColorBrewer::brewer.pal(7, "Set1")[-6]
#
plot(Prov.Means.Ascend, xlab = "",xaxt='n', ylab = "Average Hb level g/Dl", 
     main = "Average Hb level across Provinces",
     cex = 1.3,
     pch = 16, 
     las = 1, 
     col = colours[Prov.Group]);box(lwd = 2)
axis(1, c(1:33), as.character(Prov.Names.Ascend), cex.axis = 0.8,las = 2)
legend("topleft",pch=16,col=colours,legend=levels(Prov.Group),
       cex=0.85,title="Groups",title.col=grey(0.4))

dev.copy(pdf,"ProvinceMeans.pdf",width=10,height=6)
dev.off()
#
#
#
## Apply to dataset
#
# ENSURE to apply to the ORIGINAL DATASET
#
Prov.Names.Ascend <- as.character(Prov.Names.Ascend)  # you want to conveted the factor back into character
#                                                     # as you will need it in the FOR loop
#
#
Prov.Group <- c()
for (i in 1:length(AnemiaData$Province)){
  if (any(AnemiaData$Province[i]==Prov.Names.Ascend[1:6] )){Prov.Group[i] <- "Low1"}
  else if (any(AnemiaData$Province[i]==Prov.Names.Ascend[7:11])) {Prov.Group[i] <- "Low2"}
  else if (any(AnemiaData$Province[i]==Prov.Names.Ascend[12:17])) {Prov.Group[i] <- "Medium1"}
  else if (any(AnemiaData$Province[i]==Prov.Names.Ascend[18:22])) {Prov.Group[i] <- "Medium2"}
  else if (any(AnemiaData$Province[i]==Prov.Names.Ascend[23:26])) {Prov.Group[i] <- "High1"}
  else {Prov.Group[i] <- "High2"}
}
#
Prov.Group <- as.factor(Prov.Group)
#
#
#
# Again, change the order of the levels in Prov.Group so you create an ascending Boxplot
#
Prov.Group<-factor(Prov.Group, levels=c('Low1','Low2','Medium1','Medium2', 'High1', 'High2'))
AnemiaData <- cbind(Prov.Group,AnemiaData)
Group1 <- AnemiaData[!is.na(AnemiaData$Haemoglobin), ] 
#
par(mfrow=c(1,1));boxplot(Haemoglobin ~ Prov.Group, data = Group1, col = "firebrick", 
                          ylab = "Province Group", xlab = "Hb level g/Dl", horizontal = T, las = 0);box(lwd = 2)

dev.copy(pdf,"Prov.Group_BoxPlot.pdf",width=10,height=7.5)
dev.off()
#
#
#
model0.1 <- update(model0, . ~ .-Province + Prov.Group, Group1 )  # Model 0 with Province replaced by our
par(mfrow=c(2,2));plot(model0.1)                              # newly created variable, Prov.Group
summary(model0.1)
AIC(model0.1)                                                 # AIC reduced and all variable are significant
model0.1fit <- model0.1$fitted.values                             # distribution of fitted values?
par(mfrow=c(1,1));plot(density(model0.1fit), lwd = 2)
model0fit <- model0$fitted.values
lines(density(model0fit), col = colours[1], lwd = 2)
realfit <- Group1$Haemoglobin[Group1$Haemoglobin < 15 & Group1$Haemoglobin >11]
lines(density(realfit), col = colours[2], lwd = 2)
anova(model0.1,model0)
#
#
#
################################################
#########    Hierarchical Clustering   #########
################################################
#
#
# Instead of choosing the groups manually like above, try using a statistical method
#
#
#
H.col <- which( colnames(Group1)=="Haemoglobin" )
#
ProvSummaries <-  aggregate(Group1[,H.col], by=list(Group1$Province), FUN=function(x) c(Mean=mean(x)))
#
ProvSummaries.Scaled <- scale(ProvSummaries[,-1])
#
rownames(ProvSummaries.Scaled) <- ProvSummaries[,1]
#
Distances <- dist(ProvSummaries.Scaled) 
ClusTree <- hclust(Distances, method="complete") 
plot(ClusTree, xlab="Prov group", ylab="Separation")
NewProvGroups <- paste("ProvGrp", cutree(ClusTree, k = 8),sep="")
table(rownames(ProvSummaries), NewProvGroups)
colSums(table(rownames(ProvSummaries), NewProvGroups))
#
#
#
AnemiaData <-  merge(AnemiaData, data.frame(Province=rownames(ProvSummaries.Scaled), NewProvGroups))
Group1 <- AnemiaData[!is.na(AnemiaData$Haemoglobin), ] 
#
#
#
# So which provinces fall into these groups, how does it compare to your intial eyeballing?
#
table(Group1$Province, Group1$NewProvGroups)
table(Group1$Province, Group1$Prov.Group)

#
model0.2 <- update(model0, .~. - Province + NewProvGroups)         # Model 0 with Province replaced by our
# newly created variable, Prov.Group
par(mfrow=c(2,2));plot(model0.2, c(1:4))
summary(model0.2)
AIC(model0.2);AIC(model0.1);AIC(model0)
anova(model0.2,model0)
model0.1fit <- model0.1$fitted.values                             # distribution of fitted values?
par(mfrow=c(1,1))
#
model0.2fit <- model0.2$fitted.values                               # plot the fitted value distributions again
#
plot(density(model0fit), ylim=c(0,0.65), lwd = 4,
     xlab = "Hb level g/Dl",
     ylab = "Density",
     main = "Fitted values' Densities",
     las = 1)
lines(density(model0.1fit), col = colours[1], lwd = 3)
lines(density(model0.2fit), col = colours[2],lwd = 3)
legend("topleft", legend=c("Province", "Prov.Group", "NewProvGroups"),
       col=c("black",colours[1] ,colours[2]), lwd = c(4,3,3), cex=0.8)
box(lwd = 2)

dev.copy(pdf,"Densities.pdf",width=9,height=6)
dev.off()
#
#
#
model1 <- update(model0.2, . ~ . + AgeGroup )
par(mfrow=c(2,2));plot(model1,c(1:4))
summary(model1)
AIC(model1);AIC(model0.2)
anova(model0.2,model1)
#
#
#
model2 <- update(model1, . ~ . + AgricLandOwn )
plot(model2,c(1:4))
summary(model2)
AIC(model2);AIC(model1);AIC(model0.2)
anova(model1,model2)
#
#
#
model3 <- update(model2, . ~ . + WealthScoreGroup )
plot(model3,c(1:4))
summary(model3)
AIC(model3);AIC(model2);AIC(model1);AIC(model0.2)
anova(model2,model3)
#
#
#
model4 <- update(model2, . ~ . +  Education)
plot(model4,c(1:4))
summary(model4)
AIC(model4);AIC(model2);AIC(model1);AIC(model0.2)
anova(model2, model4)
#
#
#
model5 <- update(model2, . ~ . + Ethnicity )
summary(model5)
AIC(model5);AIC(model2);AIC(model1);AIC(model0.2)
anova(model2,model5)
#
#
#
model6 <- update(model2, . ~ . + Sheep + Sheep*Rural )
summary(model6)
AIC(model6);AIC(model2);AIC(model1)
anova(model2, model6)
#
#
#
model7 <- update(model2, . ~ . + NewProvGroups*Sheep)
plot(model7, c(1:4))
summary(model7)
AIC(model7);AIC(model2)
anova(model2,model7)
#
#
#
model8 <- update(model2, . ~ . + NewProvGroups:RecentBirth)
plot(model8, c(1:4))
summary(model8)
AIC(model8);AIC(model2)
anova(model2,model8)
#
#
#
model9 <- update(model8, . ~ . + Electricity )
plot(model9, c(1:4))
summary(model9)
AIC(model9);AIC(model8)
anova(model9,model8)
#
#
#
model10 <- update(model9, . ~ . + AgeGroup:RecentBirth )
plot(model10, c(1:4))
summary(model10)
AIC(model10);AIC(model9)
anova(model10,model9)
#
#
#
model11 <- update(model9, . ~ . + Pregnant:RecentBirth )
plot(model11, c(1:4))
summary(model11)
AIC(model11);AIC(model9)
anova(model11, model9)
#
#
#
# Out of sample model testing
#
# How will model0.2, the one with the most relevant variables and the most parsimonious model fare?
# Let us see.....by actually predicting...
#
#
#
set.seed(1)
Group1Sample <- sample(1:nrow(Group1),size = 3067)     # 70% of Group1 subsetted as Fitting Data
Group1Subset <- Group1[Group1Sample,]                  # Fitting Data
Group1SubsetPredict <- Group1[-Group1Sample,]          # Prediction Data
actual <- as.matrix(Group1SubsetPredict[6])            # Actual Hb levels
#
#
model11test <- update(model11, data = Group1Subset)
model11.Pred <- as.matrix(predict(model11test, Group1SubsetPredict))
#
#
model0.2test <- update(model0.2, data = Group1Subset)
model0.2.Pred <- as.matrix(predict(model0.2test, Group1SubsetPredict)) 
#
#
sqrt(mean((actual - model11.Pred)^2))
sqrt(mean((actual - model0.2.Pred)^2))
#
#
FinalModel <- model11
#
#
#
################################################
#########        Predictions           #########
################################################
#
#
#
# File should contain NO HEADER, 3 COLUMNS(ID,Hb level, se..)



Group2 <- AnemiaData[is.na(AnemiaData$Haemoglobin), ] 
Group2.predictions <- predict(FinalModel, newdata = Group2, se.fit = T) # predictions of Hb levels 
# along with predictions errors

Group2.predictions <- data.frame(ID=Group2$ID,                          # Name columns for viewing convenience   
                                 Predicted.Hblevel=Group2.predictions$fit,
                                 Predicted.se=Group2.predictions$se.fit)

Group2.predictions <- Group2.predictions[order(Group2.predictions$ID),] # the predictions start from 4383
# order them in the same for completeness
write.table(Group2.predictions, file="16040526_pred.dat", 
            col.names=FALSE, row.names=FALSE)                           # ensure no columns and rownames as specified
#
#
#  
#######################################################################################
#####################                End of Script                #####################       
#######################################################################################