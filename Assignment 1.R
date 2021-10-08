##################################################
###                                             ##
### RStudio - Assignment 1                          ## 
##################################################
#                                               ##
##################################################
# Written by Keerthy Raaj Shanmugam
# ID: 8779954
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear all plots
if(!is.null(dev.list())) dev.off()

# Clear entire console
cat("\014") 

# Clean and clear theworkspace
rm(list=ls())

#Set work directory to an appropriate location
setwd("C:/Users/keert/Documents/Data")

#Read the Comma Seperated Values dataset
Data_KR <- read.csv("PROG8430_Assign_Explore.csv",header=TRUE,sep=",")
str(Data_KR)
head(Data_KR,10)


#### 1.Summarizing Data


#1.a.Create a table to show the total income by each category of marital status.
df1_KR <- aggregate(Data_KR$income, by=list(Data_KR$m.status), FUN = sum, na.rm=TRUE)
df1_KR

#2.a.Calculate the mean age of respondents born in Asia. 
df2_KR <- round(mean(Data_KR$age[Data_KR$nation == "Asia" ]),2)
df2_KR

#2.b.Calculate the mean age of respondents born in Asia weighted by the number of children they have.
df3_KR <- Data_KR[Data_KR[, 4] == "Asia",]
df4_KR <- aggregate(df3_KR$age, by=list(df3_KR$n.child), FUN = mean, na.rm=TRUE)
df4_KR

#3.a.Create a table to show the mean score on the political awareness test for males compared to females.
df5_KR <- aggregate(Data_KR$score, by=list(Data_KR$gender), FUN = mean, na.rm=TRUE)
df5_KR

#4.Calculate the 34th and 63rd percentiles of percentage of time taken on the test.
df6_KR <- quantile(Data_KR$time1, c(.34, .63))
df6_KR


#### 2.Organizing Data


#1.a.Create a pie chart showing the number of respondents by Political Affiliation.

df7_KR <- data.frame(table(Data_KR$political))
df7_KR
Pol <- df7_KR$Freq
names(pol) <- df7_KR$Var1
lbls <- paste(df7_KR$Freq, "\n", df7_KR$Var1, sep="")
pie(Pol, labels = lbls, main="The number of respondents by Political Affiliation")

#2.a.Create a table that shows the percentage of respondents from each Region that are in the Treatment group.
df8_KR <- (table(Data_KR$nation)/nrow(Data_KR)) * 100
df8_KR

#3.a.Create a bar chart showing the mean Standardized Test Scoreon the Political Awareness Test for each Region.
df9_KR <-tapply(Data_KR$scr, Data_KR$nation, mean)
df9_KR
barplot(df9_KR, main="mean Standardized Test Score (each region)", ylab="Test Score")

#4.a.Create a histogram with 5 bins showing the distribution of the percentage of household income going to food. 
df10_KR <- Data_KR$food
hist(df10_KR, breaks=5, prob=TRUE, main="Percentage of household  income to food",xlab="food")

#5.a.Create a sequence of box plots showing the distribution of income separated by marital status.
boxplot(income ~ m.status, data=Data_KR, main="Distribution of income separated by marital status",xlab="Marital Status",  pch= 20)

#6.a.Create a histogram for income.
df11_KR <- Data_KR$income
hist(df11_KR, prob=TRUE, main="Histogram for income",xlab="Income")

#6.b.Create a histogram for standardized score. 
df12_KR <- Data_KR$scr
hist(df12_KR, prob=TRUE, main="Histogram for standardized score",xlab="standardized score")

#6.C.Create a scatter plot showing the relationship between the income and standardized score. 
plot(scr ~ income, data=Data_KR, color="violet", pch=20,main="relation between income & standardized score")

#6.e
cor(Data_KR$income, Data_KR$scr)



#### 3.Inference 


#1.a.Create a QQ Normal plot of the Political Awareness Test Score.
qqnorm(Data_KR$score, main="QQ Normal plot")
qqline(Data_KR$score)

#1.b.Conduct a statistical test for normality on the Political Awareness Test Score.
shapiro.test(Data_KR$score)

#2.a.Compare Political Awareness Test Scores between the treatment and control group using a suitable hypothesis test.
#2.b.Explain why you chose the test you did. 
#2.c.Do you have strong evidence that the average test scoresare different between the treatment and control groups?

#shapiro_test
shapiro.test(Data_KR$score)

#f_test
res.ftest <- var.test(score ~ group, data = Data_KR)
res.ftest

#t_test
res <- t.test(score ~ group, data = Data_KR, var.equal= TRUE)
res


#3.a.Determine if the Score on the Political Awareness Test varies by Region using ANOVA (statistical) and a sequence of boxplots (graphical).
#using boxplots (graphical),
boxplot(score ~ nation, data=Data_KR ,main="Score on the Test varies by Region ",range=0)
#using ANOVA (statistical),
summary(aov(score ~ nation, data=Data_KR))

#3.b.Determine if the Measure of Political Involvement (Pol) varies by Political Affiliation using ANOVA and a sequence of boxplots.
#using boxplots (graphical),
boxplot(Pol ~ political, data=Data_KR ,main="(Pol) varies by Political",range=0)
#using ANOVA (statistical),
summary(aov(Pol ~ political, data=Data_KR))