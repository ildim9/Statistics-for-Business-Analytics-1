Bikes<- read.csv2("C:\\Users\\elgr9\\OneDrive\\Desktop\\FINAL R PROJECT\\bike_02.csv")
View(Bikes)


require(tidyverse)
require(ggplot2)
require(corrplot)


# REMOVING THE VARIABLES WE DONT NEED FOR OUR PREDICTION 
str(Bikes)
Bikes$X <- NULL
Bikes$instant <- NULL
Bikes$dteday <- NULL

Bikes$hr <- as.factor(Bikes$hr)

#############################################

# understanting the dataset ########################

sum(is.na(Bikes))
sum(is.null(Bikes))

# UPDATING THE DATASET BASED ON THE DATASET CHARACTERISTICS 

# UPDATING SEASON 
Bikes$season <- factor(format(Bikes$season,format="%A"),levels = c('1','2','3','4'),labels = c('Springer','Summer','Fall','Winter'))

table(Bikes$season)

#UPDATING YEAR

Bikes$yr <- factor(format(Bikes$yr,format="%A"),levels = c('0','1'),labels = c('2011','2012'))


table(Bikes$yr)

#UPDATING WEATHER SIT

Bikes$weathersit <- factor(format(Bikes$weathersit,format="%A"),levels = c('1','2','3','4'),labels = c('Good','Medium','Bad','Really Bad'))

table(Bikes$weathersit)

#UPDATING HOLIDAY 
Bikes$holiday <- factor(format(Bikes$holiday,format="%A"),levels = c('0','1'),labels = c("Regular Working Day",'Holiday'))

table(Bikes$holiday)

# UPDATING WORKING DAY
Bikes$workingday <- factor(format(Bikes$workingday,format="%A"),levels=c(0,1),labels= c("No","Yes"))

table(Bikes$workingday)


# UPDATE MONTH


Bikes$mnth<-month.name[Bikes$mnth]  

table(Bikes$mnth)

Bikes$mnth <- as.factor(Bikes$mnth)  

str(Bikes)


# UPDATE WEEKDAY


Bikes$weekday <- factor(format(Bikes$weekday,format="%A"),levels=c(0,1,2,3,4,5,6),labels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

table(Bikes$weekday)


#UPDATING THE TEMP , ATEMP,HUM,WINDSPEED VARIABLES IN ORDER TO BE NORMALIZED LIKE THE INSTRUCTIONS

Bikes$temp <- Bikes$temp*41
Bikes$atemp <- Bikes$atemp * 50
Bikes$hum <- Bikes$hum*100
Bikes$windspeed <- Bikes$windspeed*67

str(Bikes)



View(Bikes)

# fixing the mismatch between the season and the month


fall <- c("September","October","November")
winter <- c("December","January","February")
springer <- c("March","April","May")
summer <-c("June","July","August")
for (i in (1:length(Bikes$mnth))){
  if (Bikes$mnth[i] %in% fall){
    Bikes$season[i] = "Fall"
  } else if (Bikes$mnth[i] %in% winter){
    Bikes$season[i] = "Winter"
  } else if (Bikes$mnth[i] %in% springer){
    Bikes$season[i] = "Springer"
  } else if (Bikes$mnth[i] %in% summer)
    Bikes$season[i] = "Summer"
}

str(Bikes)



############# DESCRIPTIVE  ANALYTICS FOR THE VARIABLES ############

summary(Bikes) # analysis for each variable 
require(ggpubr)





a <- ggplot(Bikes,aes(yr,..count..),color=("Red")) + geom_bar(position = "dodge") +  coord_flip() + ggtitle("Year")

b <- ggplot(Bikes,aes(weathersit,..count..)) + geom_bar(position = "dodge")+  coord_flip() + ggtitle("Weathersit")

c <- ggplot(Bikes,aes(mnth,..count..)) + geom_bar(position = "dodge") +  coord_flip() + ggtitle("Month")

d <- ggplot(Bikes,aes(weekday,..count..)) + geom_bar(position = "dodge")+  coord_flip() + ggtitle("Weekday") 

e <- ggplot(Bikes,aes(holiday,..count..)) + geom_bar(position = "dodge")+  coord_flip() + ggtitle("Holiday")

f <- ggplot(Bikes,aes(season,..count..)) + geom_bar(position = "dodge")+  coord_flip() + ggtitle("Season")


plot <-ggarrange(a,b,c,d,e,f) 
annotate_figure(plot, top = text_grob("Univariate Analysis for factor Variables", 
                                      color = "black", face = "bold", size = 16))




#Analysis for factor variables 





options(repr.plot.width=12, repr.plot.height = 8)
hist(Bikes$cnt,  xlab = "number of bicycles",ylab = "Frequency", main = "Histogram of total number of bike rentals",
     col = 'red', probability = F, cex.main = 1, cex.lab = 1, cex.axis = 1)
lines(density(Bikes$cnt))

#As you can see the total number of bike rentals per hour shows a strongly positive skewed distribution. Therefore during the 
#considered time period, the frequency of total bike rentals are most probably 
#ower than 200 bikes per hour. Recorded maximum bike rentals was 977 bikes per hour


##################### Bike rentals vs. user type ##############################




df = data.frame('lables' = c('Registered', 'Casual'), 'value' = c(mean(Bikes$registered)/(mean(Bikes$registered)+mean(Bikes$casual)), 
                                                                  mean(Bikes$casual)/(mean(Bikes$registered)+mean(Bikes$casual))))
# Create a basic bar
pie = ggplot(df, aes(x="", y=value, fill=lables)) + geom_bar(stat="identity", width=1)
# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(value*100), "%")), 
                                                  position = position_stack(vjust = 0.5), size = 10)
# Add color scale (hex colors)
pie = pie + scale_fill_manual(values=c("yellow", "pink", "puple", "green"))
# Remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Registered Users vs. Casual Users")
# Tidy up the theme
pie = pie + theme_classic() + theme(text = element_text(size = 20),
                                    axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(size = 10, hjust = 0.5, face="bold"))
pie

# registered and causal customers
options(repr.plot.width=10, repr.plot.height = 8)
boxplot(x=Bikes$registered,Bikes$casual,names = c("registered","casual"),col="#33658A",
        ylab="Bike rentals",xlab="Types of users", main = 'Bike rental vs User Type', 
        cex.main = 1, cex.lab = 1, cex.axis = 1)


#From the first glance you can see the majority of the bike users falls 
#under the category of registered users. The mean bike rental for the 
#registered users are clearly higher than that of casual users. So we can point 
#out that the majority of the customers in this bike rental program are registered
#users and these people are more likely to be regular bike riders.
#It can be clearly seen that regardless of the user type both categories
#have strongly positive skewed distributions with considerable number of outliers.


################Bike rentals vs. season ###########################################


options(repr.plot.width=11, repr.plot.height = 8)
options(scipen = 3)
tw = aggregate(cnt~season, sum , data = Bikes)
xx <- barplot(tw$cnt, col = c("#55DDE0",  "#F6AE2D", "#F26419", "#33658A", "#2F4858", "#999999"),
              ylim = c(0,100000), names.arg = c("Springer", "Summer", "Fall", "Winter"),
              xlab = "season", ylab = "no. of bike rentals", main = "Bike rentals vs. season",
              cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.3, cex = 1.2)
text(x = xx, y = tw$cnt, label = tw$cnt, pos = 3, cex = 1.2, col = "red")

#boxplot of rental v.s. season 
ggplot(Bikes, aes(x = season, y = cnt, fill = factor(season))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 1), na.rm = TRUE) +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_fill_manual(values = c("#55DDE0",  "#F6AE2D", "#F26419", "#33658A", "#2F4858", "#999999"), 
                    name="Season:",
                    breaks=c(1, 2, 3, 4),
                    labels=c("Spring", "Summer", "Fall","Winter"))+
  theme(text = element_text(size = 20), plot.title = element_text(size = 24, face="bold"))





#As you can see highest bike rental was recorded in the Summer season and second 
#highest rental was recorded in the Spring season. Total number of bike rentals in 
#the Summer season is 89,144 and the total bike rentals in Spring season is 75,103. 
#We can assume the reason behind this behavior is that 
#the summer and spring seasons provides the most suitable climate for bike riding.


################## Bike rentals vs. feel temperature ######################## 

scatter.smooth(Bikes$atemp,Bikes$cnt, col = "green", xlab = "Feeling Temperature", ylab = "Number of Bike Rentals", 
               main = "Bike rentals vs. Feel temperature", cex.main = 1, cex.lab = 1.5, cex.axis = 1.3)
plot2 <- ggplot(Bikes, aes(atemp, cnt)) + geom_smooth(aes(color = cnt))
plot2 + xlab("Feel Temperature") + ylab("Average number of Bike Rentals") +
  theme_light(base_size = 10) + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme(text = element_text(size = 10), plot.title = element_text(size = 10, face="bold"))

#Above scatter plot shows the distribution of no. of bike rentals against the feel temperature. 
#The "feels like" temperature is a measurement of how hot or cold it really feels like outside. 
#The "Feels Like" temperature relies on environmental data including the 
#ambient air temperature, relative humidity, and wind speed to determine how weather conditions feel to bare skin.
#Since the above plot shows weak yet positive relation between no. bike rentals per hour and 
#the feel temperature, we can assume that people are more likely to ride bikes as the feel temperature gets higher.



############################### Bike rentals vs. holiday ##################################



boxplot(Bikes$cnt~Bikes$holiday,main="Bike Rentals on Holidays",xlab="Regular Working Day or Holiday",ylab="Number of Bike Rentals",col=c(4,5))


#Even though there is no huge difference in no. of bike rentals per hour on a holiday 
#and a normal working day, the average bike rentals were relatively less on holidays. 
#Also there were lots of upper end outliers present in working days. 
#Therefore we can assume that there can be regular bike riders who use the rides to get their work places.




#################### Bike rentals vs. weather #########################


tb = aggregate(cnt~weathersit, sum, data = Bikes)
xx <- barplot(tb$cnt, col = c("#55DDE0",  "#F6AE2D", "#F26419", "#33658A", "#2F4858", "#999999"),
              names.arg = c("Good", "Medium", "Bad", "Really Bad"), ylim = c(0, 220000),
              xlab = "Weather Conditions ", ylab = "Number of Bikes", main = "Bike rentals vs. Weather ", 
              cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex = 1.2)
## Add text at top of bars
text(x = xx, y = tb$cnt, label = tb$cnt, pos = 3, cex = 1.2, col = "red")
cl = subset(Bikes, weathersit == "Good")
summary(cl$cnt)
mi = subset(Bikes, weathersit == "Medium")
summary(mi$cnt)
ls = subset(Bikes, weathersit == "Bad")
summary(ls$cnt)

#It can be clearly seen that highest bike rentals are recorded under clear weather. 
#Compered to clear weather there are very small number of bike rentals happened during mist, light snow or heavy rain. 
#Since all of these bad weather conditions can increase the possibility of road accidents because of low visibility and slippery roads, 
#people rarely choose to ride bikes.




ch = aggregate(cnt~hr+weekday, mean, data = Bikes)
ggplot(ch, aes(x = hr, y = cnt, color = as.factor(weekday))) + 
  geom_line(size = 1) +
  theme_light(base_size = 11) +
  xlab("Hour of the Day") +
  ylab("Average no. of Bike Rentals") +
  ggtitle("Average bike rentals in each hour vs. day of the week\n") +
  scale_color_discrete("") + coord_cartesian(xlim = c(0,23), ylim = c(-5, 600)) + 
  theme(text = element_text(size = 20), plot.title = element_text(size = 24, face="bold"))






#############Bike rentals vs. hour of the day #############################


ch = aggregate(cnt~hr, mean, data = Bikes)
plot(ch$hr, ch$cnt, type = "b", col = "#33658A", lwd = 2.5, xlab = "Hour of the day",
     ylab = "Average no. of bike rentals", main = "Bike rentals vs. hour of the day", 
     cex.main = 1, cex.lab = 1, cex.axis = 1, cex = 1.2)


#As you can see there are 2 peaks during 7am to 9am and 4pm to 7pm. 
#These 2 are normal rush times of the day, therefore we can assume this happens because of excess bike rentals of people 
#who are arriving and leaving from workplaces. 
#Apart from this 10 am to 2pm time interval has average bike rentals between 200 to 300 bikes.



################### PAIRWISE COMPARISONS ########################################

require(nortest)
Bikesnum

sapply(Bikesnum,lillie.test)[]





############################### Correlation plot for bike rentals dataset ################################

require(ggcorrplot)

model.matrix(~0+., data=Bikes) %>%
  cor(use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag = T, type="upper", lab=TRUE, lab_size=2)


###Correlation tests between Bike Rental Count, temp , ATemp, Windspeed and Humidity ##########



Bikes_cor<- Bikes %>% select (cnt,temp,atemp,hum,windspeed,casual,registered)
Bikes_cor<- data.frame(Bikes_cor)

cor(Bikes_cor)


corrplot(cor(Bikes_cor), method="number",type="upper",main="Correlation Plot")




######################### MAKING THE DATASET CENTERED #######################################


require(psych)
index<- sapply(Bikes, class) == "numeric" #logical vector where TRUEs are the numeric variables
Bikesnum <- Bikes[,index] #saving only the numeric variables to the new dataframe usdatanum
round(t(describe(Bikesnum)),2) #getting the summary statistic of the numeric variables




Bikes_centered <- as.data.frame(scale(Bikesnum, center = TRUE, scale = F))

season <- Bikes$season
yr<- Bikes$yr
mnth <-Bikes$mnth
hr <- Bikes$hr
holiday <- Bikes$holiday
weekday <- Bikes$weekday
workingday <- Bikes$workingday
weathersit <- Bikes$weathersit
casual <- Bikes$casual
registered <- Bikes$registered
cnt <- Bikes$cnt


Bikes_centered <- cbind(Bikes_centered,season)
Bikes_centered <- cbind(Bikes_centered,yr)
Bikes_centered <- cbind(Bikes_centered,mnth)
Bikes_centered <- cbind(Bikes_centered,hr)
Bikes_centered <- cbind(Bikes_centered,holiday)
Bikes_centered <- cbind(Bikes_centered,weekday)
Bikes_centered <- cbind(Bikes_centered,workingday)
Bikes_centered <- cbind(Bikes_centered,weathersit)
Bikes_centered <- cbind(Bikes_centered,casual)
Bikes_centered <- cbind(Bikes_centered,registered)
Bikes_centered <- cbind(Bikes_centered,cnt)
View(Bikes_centered)



############################### QUESTION 2 ##########################################


full_model <- lm(cnt~.-registered-casual,Bikes)

summary(full_model)

# we observe that we have negative intercept 

full_centered_model <- lm(cnt~.-registered-casual,Bikes_centered)


summary(full_centered_model)



require(glmnet) 
X <- model.matrix(full_centered_model)[,-1]
lasso <- glmnet(X, Bikes_centered$cnt)
plot(lasso, xvar = "lambda", label = T)
#Use cross validation to find a reasonable value for lambda 
lasso1 <- cv.glmnet(X, Bikes_centered$cnt, alpha = 1)
lasso1$lambda
lasso1$lambda.min
lasso1$lambda.1se
plot(lasso1) 

coef(lasso1, s = "lambda.min")
coef(lasso1, s = "lambda.1se")
plot(lasso1$glmnet.fit, xvar = "lambda")
abline(v=log(c(lasso1$lambda.min, lasso1$lambda.1se)), lty =2)


# lasso model and we try to find the best model  via step
require(car)
lasso_model <- lm(cnt~season+yr+mnth+ hr + holiday+weekday+ workingday+weathersit+temp+hum+windspeed,Bikes_centered)
summary(lasso_model)

step_model<-step(lasso_model,direction = "both")

vif(step_model) # month out vif > 3.16

summary(step_model)

test1 <- lm(cnt~yr+ hr + holiday+weekday+ workingday+weathersit+temp+hum+windspeed,Bikes_centered)

x1 <- step(test1,direction = "both")

vif(x1)

summary(x1)


final_model <-  lm(cnt~yr+ hr + holiday+ workingday+weathersit+temp+hum+windspeed,Bikes_centered)
summary(final_model)




###########################  QUESTION 3 ############################## 


###################### ASSUPTIONS ! #################


################# NORMALITY OF RESIDUALS #############


require(nortest)

lillie.test(final_model$residuals)
shapiro.test(final_model$residuals)
# normality not ok 


############ LINEARITY ###############
require(car)

par(mfrow=c(1,1))

residualPlot(final_model,type='rstudent')
residualPlots(final_model,type='rstudent',plot=F)


############ HOMOSKEDASITY ##############


ncvTest(final_model) # p value to small homoskedasity violated
yhat <- fitted(final_model)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
leveneTest(rstudent(final_model)~yhat.quantiles)

########## INDEPENDENCE #################
require(randtests)
require(lmtest)
durbinWatsonTest(final_model)
runs.test(final_model$residuals)

dwtest(final_model)
# ok independence

########### FIXING THE PROBLEMS WITH LOGS #############


logmodel <-lm(log(cnt)~yr+ hr + holiday+ workingday+weathersit+temp+hum+windspeed,Bikes_centered)

summary(logmodel)



# normality of log's model residuals 


lillie.test(logmodel$residuals)
shapiro.test(logmodel$residuals)




#INDEPENDENCE
require(randtests)
require(lmtest)
durbinWatsonTest(logmodel)
runs.test(logmodel$residuals)

dwtest(logmodel)

summary(logmodel)

#HOMOSCEDASITY

ncvTest(logmodel)
yhat <- fitted(logmodel)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(logmodel)~yhat.quantiles)
boxplot(rstudent(logmodel)~yhat.quantiles)

#linarity 


residualPlot(logmodel, type='rstudent')
residualPlots(logmodel, plot=F, type = "rstudent")




##### TRYING TO USE POLYNIMA STO LOGMODEL 


# 2/4 ASSUSMPTIONS 


poly_log_model <-lm(log(cnt)~yr+ hr + holiday+ workingday+weathersit+temp+hum+windspeed+poly(temp,4)+poly(hum,3)+poly(windspeed,2),Bikes_centered)




summary(poly_log_model)

#NORMALITY 

lillie.test(poly_log_model$residuals)
shapiro.test(poly_log_model$residuals)




#INDEPENDENCE ok 
require(randtests)
require(lmtest)
durbinWatsonTest(poly_log_model)
runs.test(poly_log_model$residuals)

dwtest(poly_log_model)



#HOMOSCEDASITY

ncvTest(logmodel)
yhat <- fitted(logmodel)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(logmodel)~yhat.quantiles)
boxplot(rstudent(logmodel)~yhat.quantiles)

#linarity ok 


residualPlot(logmodel, type='rstudent')
residualPlots(logmodel, plot=F, type = "rstudent")




summary(poly_log_model)



final_poly_log_model <- lm(log(cnt)~+yr+hr+holiday+workingday+weathersit+temp+hum+windspeed+I(temp^2)+I(hum^2),Bikes_centered)


summary(final_poly_log_model)


##### TRYING TO FIX HOMOSCEDASTITY WITH WEIGHT LEAST SQUEARES ###########


wt <- 1 / lm(abs(final_poly_log_model$residuals) ~ final_poly_log_model$fitted.values)$fitted.values^2


wt_poly_log_model <- lm(log(cnt)~+yr+hr+holiday+workingday+weathersit+temp+hum+windspeed+I(temp^2)+I(hum^2),Bikes_centered,weights = wt)


summary(wt_poly_log_model)



require(car)
par(mfrow=c(1,3))
ncvTest(wt_poly_log_model) #homoscedasity ok 

yhat <- fitted(wt_poly_log_model)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
leveneTest(rstudent(wt_poly_log_model)~yhat.quantiles)

boxplot(rstudent(wt_poly_log_model)~yhat.quantiles,col=2:5, main="Homoscedasticity Assumption ")




residualPlot(wt_poly_log_model, type='rstudent',col="red",main=" Linearity Assumption  ")
residualPlots(wt_poly_log_model, plot=F, type = "rstudent") # linearity ok 

durbinWatsonTest(wt_poly_log_model) # indepedence ok 
runs.test(wt_poly_log_model$residuals) # independence ok 

plot(rstudent(wt_poly_log_model), type='l',col="pink",main="Indepedence Assumption") # indepedence ok 




plot(wt_poly_log_model,which = 2) # normality not ok 

summary(wt_poly_log_model)




######################## QUESTION 5 #######################################


Bikes_test <- read.csv2("C:\\Users\\elgr9\\OneDrive\\Desktop\\FINAL R PROJECT\\bike_test.csv")


# REMOVING THE VARIABLES WE DONT NEED FOR OUR PREDICTION 
str(Bikes_test)
Bikes_test$X <- NULL
Bikes_test$instant <- NULL
Bikes_test$dteday <- NULL

Bikes_test$hr <- as.factor(Bikes_test$hr)

#############################################

# understanting the dataset ########################

sum(is.na(Bikes_test))
sum(is.null(Bikes_test))

# UPDATING THE DATASET BASED ON THE DATASET CHARACTERISTICS 

# UPDATING SEASON 
Bikes_test$season <- factor(format(Bikes_test$season,format="%A"),levels = c('1','2','3','4'),labels = c('Springer','Summer','Fall','Winter'))

table(Bikes$season)

#UPDATING YEAR

Bikes_test$yr <- factor(format(Bikes_test$yr,format="%A"),levels = c('0','1'),labels = c('2011','2012'))


table(Bikes_test$yr)

#UPDATING WEATHER SIT

Bikes_test$weathersit <- factor(format(Bikes_test$weathersit,format="%A"),levels = c('1','2','3','4'),labels = c('Good','Medium','Bad','Really Bad'))

table(Bikes_test$weathersit)

#UPDATING HOLIDAY 
Bikes_test$holiday <- factor(format(Bikes_test$holiday,format="%A"),levels = c('0','1'),labels = c("Regular Working Day",'Holiday'))

table(Bikes_test$holiday)

# UPDATING WORKING DAY
Bikes_test$workingday <- factor(format(Bikes_test$workingday,format="%A"),levels=c(0,1),labels= c("No","Yes"))

table(Bikes_test$workingday)


# UPDATE MONTH


Bikes_test$mnth<-month.name[Bikes_test$mnth]  

table(Bikes_test$mnth)

Bikes_test$mnth <- as.factor(Bikes_test$mnth)  

str(Bikes_test)


# UPDATE WEEKDAY ####


Bikes_test$weekday <- factor(format(Bikes_test$weekday,format="%A"),levels=c(0,1,2,3,4,5,6),labels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

table(Bikes_test$weekday)


#UPDATING THE TEMP , ATEMP,HUM,WINDSPEED VARIABLES IN ORDER TO BE NORMALIZED LIKE THE INSTRUCTIONS

Bikes_test$temp <- Bikes_test$temp*41
Bikes_test$atemp <- Bikes_test$atemp * 50
Bikes_test$hum <- Bikes_test$hum*100
Bikes_test$windspeed <- Bikes_test$windspeed*67


View(Bikes_test)

# fixing the missmatch between the season and the month


fall <- c("September","October","November")
winter <- c("December","January","February")
springer <- c("March","April","May")
summer <-c("June","July","August")
for (i in (1:length(Bikes_test$mnth))){
  if (Bikes_test$mnth[i] %in% fall){
    Bikes_test$season[i] = "Fall"
  } else if (Bikes_test$mnth[i] %in% winter){
    Bikes_test$season[i] = "Winter"
  } else if (Bikes_test$mnth[i] %in% springer){
    Bikes_test$season[i] = "Springer"
  } else if (Bikes_test$mnth[i] %in% summer)
    Bikes_test$season[i] = "Summer"
}





View(Bikes_test)


full_model_5 <- lm(cnt~.-registered-casual,Bikes_centered) # full model 
null_model_5 <- lm(cnt~1,Bikes_centered)
lasso_model_5 <- lm(cnt~season+yr+mnth+ hr + holiday+weekday+ workingday+weathersit+temp+hum+windspeed,Bikes_centered)
final_model_5 <-  lm(cnt~yr+ hr + holiday+ workingday+weathersit+temp+hum+windspeed,Bikes_centered)




##################################### QUESTION 6 ########################################



library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)



# in order to describe each season i need to split the dataset into 4 smaller ones based on every season 



Season_Dataset <-split(Bikes,Bikes$season)

Season_Dataset$Springer

Season_Dataset$Summer

Season_Dataset$Fall

Season_Dataset$Winter






############## FOR SPRINGER #########################################

Spring <- Season_Dataset$Springer%>% select ( temp, atemp,hum,windspeed,casual,registered,cnt)

describe(Spring)

summary(Season_Dataset$Springer)


Spring <-  ggplot(data=Season_Dataset$Springer, aes(x=hr, y=cnt, color=weathersit)) +
  geom_line(stat="identity", position = 'dodge')+
  theme_classic(base_family = "white")+ylab("Rentals Per Hour")+ggtitle("Bike Rentals in Spring Season ")  + scale_fill_brewer(palette="Purples")

Spring




################## FOR WINTER #####################################





describeBy(Season_Dataset$Winter)



winternum <- Season_Dataset$Winter %>% select ( temp, atemp,hum,windspeed,casual,registered,cnt)

describe(winternum)



Winter <-  ggplot(data=Season_Dataset$Winter, aes(x=hr, y=cnt)) +
  geom_line(stat="identity",position=position_dodge())+
  theme_minimal()+ylab("Total Rentals")+ggtitle("Bike Rentals Per Hour")  + scale_fill_brewer(palette="Blues")

Winter


Winter_temp <-  ggplot(data=Season_Dataset$Winter, aes(x=mean(temp), y=cnt, fill=weathersit)) +
  geom_bar(stat="identity", position = 'dodge')+
  theme_minimal()+ylab("Rentals Per Hour")+ggtitle("Bike Rentals Per Weather Conditions") +xlab("Average Temperature") + scale_fill_brewer(palette="Purples") 
Winter_temp





################# FOR SUMMER ##########################



Summer <- Season_Dataset$Summer %>%  select( temp, atemp,hum,windspeed,casual,registered,cnt)

describe(Summer)





Summer<-  ggplot(data=Season_Dataset$Summer, aes(x=hr, y=cnt, color=weathersit)) +
  geom_line(stat="identity", position = 'dodge')+
  theme_classic(base_family = "white")+ylab("Rentals Per Hour")+ggtitle("Bike Rentals in Summer Season ")  + scale_fill_brewer(palette="Blues")

Summer


Summer_temp <-  ggplot(data=Season_Dataset$Summer, aes(x=hr, y=cnt,fill=weathersit)) +
  geom_bar(stat="identity", position = 'dodge')+
  theme_classic()+ylab("Rentals Per Hour")+ggtitle("Bike Rentals Per Weekday") +xlab("Average Temperature") + scale_fill_brewer(palette="Blues") 
Summer_temp




##################### FOR FALL #######################################

Fall <-  ggplot(data=Season_Dataset$Fall, aes(x=hr, y=cnt,color=weathersit)) +
  geom_line(stat="identity",position=position_dodge())+theme_classic(base_family = "white")+
  ylab("Total Rentals")+ggtitle("Bike Rentals Per Hour in Fall Season") + scale_color_brewer(palette = "Reds")
Fall

Fall_temp <-  ggplot(data=Season_Dataset$Fall, aes(x=mean(temp), y=cnt, fill=weathersit)) +
  geom_bar(stat="identity", position = 'dodge')+
  theme(panel.border = element_blank())+ylab("Rentals Per Hour")+ggtitle("Bike Rentals Per Weekday") +xlab("Average Temperature") + scale_fill_brewer(palette="Blues") 
Fall_temp



Fall <- Season_Dataset$Fall %>%  select( temp, atemp,hum,windspeed,casual,registered,cnt)

describe(Fall)














