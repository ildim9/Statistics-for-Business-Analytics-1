
library(psych)
library(nortest)
library(car)
library(Hmisc)
library(lawstat)
# QUESTION 1
setwd("C:\\Users\\elgr9\\OneDrive\\Desktop\\LAB FILES -20211122")
require(foreign)
salary <- read.spss("salary.sav", to.data.frame = T)

str(salary)#understanding the structure of the dataframe
head(salary)
  



# QUESTION 2 
numerical_statistics <- sapply(salary,class)=="numeric"

# summary statistics only for the numerical variables 


round(t(describe(salary[,numerical_statistics])),2)


par(mfrow=c(2,3))

# TESTING NORMALLITY WITH QQNORM PLOTS FOR THE NUMERIC VARIABLES OF THE DATASET
# no one of the numerical variables are normally distributed 

qqnorm(salary$salbeg,main="Beginning Salary")
qqline(salary$salbeg)
qqnorm(salary$time,main="Seniority")
qqline(salary$time)
qqnorm(salary$age,main="Age Of Respondent")
qqline(salary$age)
qqnorm(salary$salnow,main="Salary Now")
qqline(salary$salnow)
qqnorm(salary$edlevel,main="ED Level")
qqline(salary$edlevel)
qqnorm(salary$work,main="Work")
qqline(salary$work)

# we can also check the normallity of the data using the nortest 



sapply(salary[,numerical_statistics],lillie.test)


# QUESTION 3 


beginning_salary <- salary$salbeg

length(beginning_salary)


lillie.test(beginning_salary) # normality not ok 
shapiro.test(beginning_salary) # normaliy not ok 



# Both test doens accept normallity in the sample 
# so we test symmetry 

mean(beginning_salary)
median(beginning_salary)

symmetry.test(beginning_salary)

# we dont have symetric observations in the sample so we proceed doing wilcoxn test for one sample 

wilcox.test(beginning_salary,mu=1000)

# based on the pvalue of the wilcoxon test we reject h0 



# QUESTION 4 


diff <-salary$salnow-salary$salbeg
head(diff)


lillie.test(diff) # normality not ok 
shapiro.test(diff) # normality not ok 
# a > p-value so we reject the Ho hypothesis 

# is the sample large ?

nrow(salary) # the sample has 474 observations so yes , we have to check the summetry of the sample 



mean(diff)
median(diff)

#the mean and the median differ a lot so we dont have symmetry , we proceed doing wilcoxon test for dependent samples



wilcox.test(diff)

# we reject the H0 hypothesis and we proceed doing the box plot of differnece 

boxplot(salary$salbeg,salary$salnow,names=c("Salary Beg","Salary Now"),notch = T)




# QUESTION 5


by(salary$salbeg,salary$sex,lillie.test) # normality not ok 
by(salary$salbeg,salary$sex,shapiro.test) # normality not ok 


dataset1 <- with(salary,split(salbeg,sex))

# our samples are ok as fas as the size is concerned so we proceed for the symmetry test 


by(salary$salbeg,salary$sex,symmetry.test)

# we procceed on test for equality of medians wilcoxon test

dataset1 <- with(salary,split(salbeg,sex))

wilcox.test(dataset1$MALES,dataset1$FEMALES) # reject h0 


boxplot(dataset1)

boxplot(dataset1$MALES,dataset1$FEMALES,names=levels(salary$sex)[1:2] ,notch=TRUE )

# QUESTION 6 


age_cut <- cut2(salary$age,breaks=c(23.00,29.67,39.75,64.50))

cutframe2 <- data.frame(salary,age_cut) # assign the age_cut in the salary dataframe


# creating a sorted df based on age_cut 

df<-cutframe2[order(cutframe2$age_cut),]
View(df)

head(df)
summary(df)
# we can observe that there are significant differences on the average salary and median  among the age_cut groups so there is no symmetry 

groups <-aggregate( salbeg~age_cut, df, mean) 
groups
groups1 <- aggregate(salbeg~age_cut,df,median)

# so we proceed with the anova


anova1 <- aov(salbeg~age_cut,df)

summary(anova1)

# we need to check normallity for the residuals

lillie.test(anova1$residuals) # normallity not ok 
shapiro.test(anova1$residuals) # normallity not ok 

data <- with(salary,split(salbeg,age_cut))

qqnorm(anova1$residuals,main="Residuals")
qqline(anova1$residuals)




kruskal.test(df$salbeg,df$age_cut) # reject h0

pairwise.wilcox.test(df$salbeg,df$age_cut) # the 2nd differs from the other two 

boxplot(data,main="Box Plot for Each Level of the groups")


# QUESTION 7 

chisq.test(salary$minority,salary$sex) #independence ok 

View(salary)



sum_of_white_males<-salary$sex[salary$sex=="MALES" & salary$minority=="WHITE"]

length(sum_of_white_males)

white_males_proportion <-length(sum_of_white_males)/sum(salary$minority=="WHITE")


sum_of_white_females<-salary$sex[salary$sex=="FEMALES" & salary$minority=="WHITE"]

length(sum_of_white_females)


white_females_proportion <-length(sum_of_white_females)/sum(salary$minority=="WHITE")

# after computing the two probabilitiess aboves we are ready to run the test for equality of proportions 

proportions <- c(white_males_proportion,white_females_proportion)
names(proportions) <- c('White Males','White Females')
proportions
size <- c(194,176)

final_counts <- proportions*size


prop.test(final_counts,size) #reject h0


