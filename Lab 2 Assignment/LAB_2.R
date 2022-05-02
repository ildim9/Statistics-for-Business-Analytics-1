

setwd('C:\\Users\\elgr9\\OneDrive\\Desktop\\R_lab_files')



#QUESTION 1
Drugs <- read.csv('Drugs.txt')

Drugs$FLAG_CODES <- NULL

View(Drugs)

#QUESTION 2
loc<-sort(table(Drugs$LOCATION))
loc
cat("The dataset has",length(loc),"countries")

#QUESTION 3
sub<-subset(loc,loc>quantile(loc,0.75))
sub

#QUESTION 4

# We have to make a new data frame named DrugsV2 which will contain all the observations at the top 25%

library(dplyr)

selected <- c("NLD","AUS","CAN","DEU","FIN","ISL","SWE","KOR","NOR")

DrugsV2 <- Drugs[Drugs$LOCATION %in% selected,]

View(DrugsV2)




#Building 4 plots in one figure
library(ggplot2) # the library needed for creating the plots
library(ggpubr)  # the library needed to combine the x,y,z,k plots into one figure


x<-ggplot(DrugsV2,aes(x=TIME,y=PC_HEALTHXP))+geom_line(aes(colour=LOCATION))+ggtitle("Drug Spending in PC_HEALTHXP")

y<-ggplot(DrugsV2,aes(x=TIME,y=PC_GDP))+geom_line(aes(colour=LOCATION))+ggtitle("Drug Spending in PC_GDP")

z<-ggplot(DrugsV2,aes(x=TIME,y=USD_CAP))+geom_line(aes(colour=LOCATION))+ggtitle("Drugs Spending in USD_CAP")

k<-ggplot(DrugsV2,aes(x=TIME,y=TOTAL_SPEND))+geom_line(aes(colour=LOCATION))+ ggtitle("Drugs Spending in TOTAL SPEND")


ggarrange(x,y,z,k)


# QUESTION 5 


Data <- subset(Drugs,LOCATION=='BEL')
View(Data)


Years <- c(min(Data$TIME),max(Data$TIME))

cat("the Minimum year is : ",min(Data$TIME)," and the Maximum year is :",max(Data$TIME))

Data.points <- table(Data$LOCATION)
Data.points






# we find the years that the expenditure was higher than the year before

# FOR THE PC_HEALTHXP

y<-0
for(i in 2:nrow(Data))
{
  if(Data$PC_HEALTHXP[i] > Data$PC_HEALTHXP[i-1])
  { y<-y+1}
}
cat(" The years where the expenditure was higher than the year before are : ",y)



# so the probability is  : 

PC_HEALTHXP_PROBABILITY <- 14/length(diff(Data$PC_HEALTHXP))




#FOR THE GDP
y<-0
for(i in 2:nrow(Data))
{
  if(Data$PC_GDP[i] > Data$PC_GDP[i-1])
  { y <- y+1}
}
cat(" The years where the expenditure was higher than the year before are : ",y)

# so the probability is  :
PC_GDP_PROBABILITY <- 21/length(diff(Data$PC_GDP))




# FOR THE USA_CAP
y<-0
for(i in 2:nrow(Data))
{
  if(Data$USD_CAP[i] > Data$USD_CAP[i-1])
  { y <- y+1}
}
cat(" The years where the expenditure was higher than the year before are : ",y)



# so the probability is  :
USD_CAP_PROBABILITY <- 36/length(diff(Data$USD_CAP))



#FOR THE TOTAL SPEND 
y<-0
for(i in 2:nrow(Data))
{
  if(Data$TOTAL_SPEND[i] > Data$TOTAL_SPEND[i-1])
  {y<-y+1}
}
cat(" The years where the expenditure was higher than the year before are : ",y)


# so the probability is  :
TOTAL_SPEND_PROBABILITY <- 37/length(diff(Data$TOTAL_SPEND))







# SO WE CREATE A VECTOR WITH THE 4 FINAL PROBABILITIES ABOVE.


YearlyProbs <- c('PC_HEATHXP_PROBABILITY'=PC_HEALTHXP_PROBABILITY ,'PC_GDP_PROBABILITY'=PC_GDP_PROBABILITY ,
                 'USD_CAP_PROBABILITY'=USD_CAP_PROBABILITY,'TOTAL_SPEND_PROBABILITY'=TOTAL_SPEND_PROBABILITY)
YearlyProbs


# COMPUTING THE fiveYeProbs




PC_HEALTHXP_PROBABILITY <- 14/40
PC_GDP_PROBABILITY <- 21/40
USD_CAP_PROBABILITY <- 36/40
TOTAL_SPEND_PROBABILITY <- 37/40


FIVE_YEARS_PROB_PC_HEALTHXP <- dbinom(4,5,14/40) + dbinom(5,5,14/40)
FIVE_YEARS_PROB_PC_GDP <- dbinom(4,5,21/40) + dbinom(5,5,21/40)
FIVE_YEARS_PROB_USD_CAP <- dbinom(4,5,36/40) + dbinom(5,5,36/40)
FIVE_YEARS_PROB_TOTAL_SPEND <- dbinom(4,5,37/40) + dbinom(5,5,37/40)

FiveYeProbs <- c("FIVE_YEARS_PROB_PC_HEALTHXP"=FIVE_YEARS_PROB_PC_HEALTHXP,"FIVE_YEARS_PROB_PC_GDP"=FIVE_YEARS_PROB_PC_GDP,
                 "FIVE_YEARS_PROB_USD_CAP"=FIVE_YEARS_PROB_USD_CAP,"FIVE_YEARS_PROB_TOTAL_SPEND"=FIVE_YEARS_PROB_TOTAL_SPEND)
FiveYeProbs



#CREATING THE FINAL LIST 

FINAL <-list(Data,Years,Data.points,YearlyProbs,FiveYeProbs) 
FINAL






# QUESTION 6 



# THE START OF THE FUNCTION
library(bannerCommenter)
estimation<- function(DATA=country,metric,nofY)
{
  
   cat(banner("The proper names of the countries in order to compute the probability are : \n
  RUS,ISR,TUR,LTU,LVA,POL,SVN,USA,EST,MEX,SVK,LUX,HUN,NZL,AUT,CZE,GRC,ITA,\n
  FRA,CHE,GBR,ESP,JPN,DNK,PRT,BEL,IRL,NLD,AUS,CAN,DEU,FIN,ISL,SWE,KOR,NOR"))
  
  
  
  
  country <-readline(" Give one of the above countries : ")
  

  if(country %in% Drugs$LOCATION)
  {print("Correct Input!Continue")
  }else
  {return("Wrong input!Try to input a different format of the Country's name")}
  
  
  
  cat(banner("THE DEFINITIONS OF EACH METRIC\n
   pc.gdp = % of GDP that is spent on pharmaceutical drugs(PC.GDP)\n
   pc.tot = % of Health spending that is spent on pharmaceutical drugs(PC HEALTHXP)\n
   per.ca = USD per capita (using economy-wide PPPs) spent on pharmaceutical drugs (USD_CAP)\n
   total = Total spending in millions USD (TOTAL SPEND)"))
   
  metric <- readline("Give me the metric values (pc.gdp or pc.tot or per.ca or total) :")

  if(metric %in% c("pc.gdp","pc.tot","per.ca","total"))
  {print("Correct input")
  }else
  {return("WRONG!Give me one of the metrics above.Function Ended")}

  
  
  
  countrycode<- subset(Drugs,LOCATION==country)
  
  datapoints <- nrow(countrycode)
  
  minYear <- min(countrycode$TIME)
  
  maxYear <- max(countrycode$TIME)
if(datapoints<10)
{
  return(" Unable to calculate the probability (n<10) ")
}

 else{
   
 
  
        a<-0
        for(i in 2:nrow(countrycode))
        {
          if(countrycode$PC_HEALTHXP[i] >countrycode$PC_HEALTHXP[i-1])
          { a<-a+1}
        }
        
        z<-0
        for(i in 2:nrow(countrycode))
        {
          if(countrycode$PC_GDP[i] >countrycode$PC_GDP[i-1])
          { z <-z+1}
        }
        
        k<-0
        for(i in 2:nrow(countrycode))
        {
          if(countrycode$USD_CAP[i] >countrycode$USD_CAP[i-1])
          { k <-k+1}
        }
        
        
        
        b<-0
        for(i in 2:nrow(countrycode))
        {
          if(countrycode$TOTAL_SPEND[i] >countrycode$TOTAL_SPEND[i-1])
          { b<-b+1}
        }
        
        
        if(metric %in% 'pc.gdp')
        {
          
         estimated_probability <- dbinom(nofY-1,nofY,z/length(diff(countrycode$PC_GDP))) + dbinom(nofY,nofY,z/length(diff(countrycode$PC_GDP)))
         
         
         }
        if(metric %in% 'pc.tot')
        {
          estimated_probability <- dbinom(nofY-1,nofY,a/length(diff(countrycode$PC_HEALTHXP))) + dbinom(nofY,nofY,a/length(diff(countrycode$PC_HEALTHXP)))
          
        }
        if(metric %in% 'per.ca')
        {
          estimated_probability <- dbinom(nofY-1,nofY,k/length(diff(countrycode$USD_CAP))) + dbinom(nofY,nofY,k/length(diff(countrycode$USD_CAP)))
          
        }
        
        if(metric %in% 'total')
        {
          estimated_probability <- dbinom(nofY-1,nofY,b/length(diff(countrycode$TOTAL_SPEND))) + dbinom(nofY,nofY,b/length(diff(countrycode$TOTAL_SPEND)))
          
        }
 }
  
  cat("Based on",paste(datapoints),"datapoints from years", paste(minYear),
      "to", paste(maxYear),"the probability that",paste(country),"will increase its drug expenditure, in terms of",paste(metric),"in at least",
      paste(nofY-1),"years in the period",paste(maxYear+1),"to",(maxYear+1+nofY),"is",paste(round(estimated_probability,5)))
  
}



estimation(country,metric,nofY=5)





#EXAMPLES SECTION 


# Country = RUS , metric = total

#[1] "Correct input"
#[1] " Unable to calculate the probability (n<10) "


#Country = USA , metric = pc.tot

# [1] "Correct input"
# Based on 16 datapoints from years 2000 to 2015 the probability that USA will increase its drug expenditure, in terms of pc.tot in at least 4 years in the period 2016 to 2021 is 0.14861


#Country = BEL , metric = pc.gdp

# [1] "Correct input"
# Based on 41 datapoints from years 1970 to 2015 the probability that BEL will increase its drug expenditure, in terms of pc.gdp in at least 4 years in the period 2016 to 2021 is 0.22031


#Country = DNK , metric = per.ca


# [1] "Correct input"
# Based on 36 datapoints from years 1980 to 2015 the probability that DNK will increase its drug expenditure, in terms of per.ca in at least 4 years in the period 2016 to 2021 is 0.89676
       