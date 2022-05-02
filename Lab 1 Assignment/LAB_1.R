
library(robotstxt)
library(rvest)
# ADD HERE A SHORT DESCRIPTION OF WHAT THE WHOLE CODE DOES
# The code below is scraping the site metacritic.com and for every page with the help of the for statement changes the pages 
#  and takes the metascore,critic,date for every page until the 28th one(by default the items of the page are 100 and they are sorted By Date) .After this point ,with the if statements at the end of the code, imports them in every iteration into a data frame called df.tot.
#  In every iteration imports to the existing data frame 100 movies from the page.
#  The page of the site are changing with the help of the loop for that starts from the 1st page and reaches to the 28th.

# Check whether scraping is allowed from this webpage (returns TRUE)
# the scraping is allowed after importing the URL we want to read 


# ATTENTION: PUT THE WHOLE URL IN ONE LINE WHEN RUNNING THE CODE
paths_allowed("https://www.metacritic.com/publication/washington-post?filter=movies&num_items=100&sort_options=date&page=0")
# Define character element "main.page", to be used recursively for defining
# multiple pages from metacritic.com
# ATTENTION: PUT THE WHOLE URL IN ONE LINE WHEN RUNNING THE CODE

str(main.page)
# main.page is the variable that saves the URL where we will emport our data , the last item in the url (page=) is missing because the i pointer below is connected 
#with the step.page variable in order to change the pages

main.page <- "https://www.metacritic.com/publication/washington-post?filter=movies&num_items=100&sort_options=date&page="
main.page

for (i in 0:27){ # This is a "for" loop.
  # This means that all the lines until the closure of }
  # will be repeated for different values of object i
  # thus, on the first run i=0, second run i=1,... last run i=27                            



  # for each step, define...
  step.page <- paste(main.page,i,sep="") # in every iteration the "paste" function combines the given URL with the pointer i that corresponds to each page from the URL and imports to the "step.page" variable the page that the program will take the data. 
  # the step.page variable reads the pages from 1 to 28.So when the pointer i has 27 value that means that the program reads data from the 28th page.
  
  
  webdata <-read_html(step.page) # OK
  webdata
  # Vector ... is created which includes .....
  title <-c(webdata %>% html_nodes("div.review_product") %>% html_nodes("a") %>%
              html_text()) # vector "title" is created which includes for every iteration the titles of each movie.
  
  metascore <- c(webdata %>% html_nodes("li.review_product_score.brief_metascore") %>%
                   html_nodes("span.metascore_w") %>% html_text()) #vector "metascore" is created which includes in every "iteration" the metascores" of each movie
  
  critic <- c(webdata %>% html_nodes("li.review_product_score.brief_critscore") %>%
                html_nodes("span.metascore_w") %>% html_text())#vector "critic" is created which includes in every iteration the "critic" of each movie
  date <- c(webdata %>% html_nodes("li.review_action.post_date") %>% html_text()) #vector "date" is created which includes in every iteration the "date" of each movie
  
  if (length(date)<100 ){for (j in length(date):100){ date[j] <- date[length(date)]}} #OK
  a <- substr(date,12,13) # with the command "substr" the program takes from the variable "date" in every iteration the characters that belong to the 12th to 13th position.In our case these positions belong to the DATE the review was created 
  
  b <- substr(date,8,10) #with the command "substr" the program takes from the variable "date" in every iteration the characters that belong to the 8th to 10th position.In our case these positions belong to the MONTH the review was created
  
  
  d <- substr(date,16,19)#with the command "substr" the program takes from the variable "date" in every iteration the characters that belong to the 16th to 19th position.In our case these positions belong to the YEAR the review was created
  lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")#OK
  lct
  date2 <- apply(cbind(a,b,d),1,paste,collapse="/") #in this case we have a combination of commands,the command "cbind" joins per columns the  variables a,b,d in every iteration , the elements "1","paste","collapse", belong to the apply command.That means that per row(def="1") the program joins the elements (command paste) by "/" (command collapse)
  
  date3 <- as.Date(date2,"%d/%b/%Y")# with this command the program converts "date2" into a calendar value with "YEAR-MONTH-DAY" format
  
  Sys.setlocale("LC_TIME", lct) #OK
  df = data.frame(title,metascore,critic,date3)# in every iteration the program builds a data frame with the columns "title" "metascore" "critic" "date3"
  
  colnames(df) <- c("title", "metascore", "critic","date") # imports to the data frame the name of the columns changing date3 to date,the other names remain the same  
  colnames(df)
  df$metascore <- as.numeric(as.character(df$metascore))# at first converts the "metascore" into a character variable and then converts it into a numeric variable 
  
  df$critic <- as.numeric(as.character(df$critic))# at first converts the "critic" into a character variable and then converts it into a numeric variable
  #str(df$critic)
  df <- df[complete.cases(df), ] # removes from the df data frame all the rows containing NA values, and imports them into a new one with the same name.
  df
  if (i==0){ #OK
    df.tot <- df
    df.tot
    } #OK
  if (i>0){ #the i > 0 means that this iteration will work from the second page 2 until the page 28 (pointer i ==1)
    df.tot <- rbind(df.tot,df) # with the rbind function the program joins by rows all the data frames that they have been created in every iteration with the first data frame  that been created in the first iteration ( for i=0) , those data frames contain all the movies (100 movies ) from each page.
    } 
}


df.tot$title <- as.character(df.tot$title) #transforms the title variable into a character variable 
df.tot$title

# QUESTION 2 

str(df.tot)
# Using "str" function help us to understand the type of each observation of our data frame.In our case we have a data frame with 2776 observations,4 variables that they are assigned in 4 columns,
# and they each one of them has a unique type(title is a character variable,metascore and critic are numeric variables,date is assigned as Date variable with a specific format)

#QUESTION 3 

df.tot$ratio <- df.tot$metascore / df.tot$critic


df.tot$perc.meta <-rank(df.tot$metascore)/nrow(df.tot)

df.tot$perc.critic <-rank(df.tot$critic)/nrow(df.tot)

x<-strsplit(as.character.Date(df.tot$date),split="-")

for(i in 1:length(x))
{
  df.tot$year[i]<-x[[i]][1]
  
  
}

#QUESTION 4 


df.tot$title[which.max(df.tot$metascore)]


# QUESTION 5

#from the result we can observe that the data are distributed symmetrically around the median in every year 

boxplot(perc.meta~year,df.tot,outchar=T,outline=F,col=1:11)
        
        
abline(h=0.5, col = "black")



#QUESTION 6
# some "ratio" variable have "Inf" values because some observations have 0 critic ,therefore when we divide with 0 the outcome is Inf.

df.tot2 <-df.tot[apply(df.tot!=0,1,all),]

View(df.tot2)


#QUESTION 7 
matrix<-cbind(df.tot2$metascore,df.tot2$critic)
colnames(matrix) <- c("Metascore","Critic")
head(matrix,10)
df.tot2$average_metascore_critic<-apply(matrix,1,mean) #(importing the view variable into the df.tot2 dataframe for greater convenience)

str(df.tot2)

#QUESTION 8 



plot(perc.meta~date,df.tot2,main="Metascore percentiles",xlab ="date",ylab="Perc.meta",pch=16,frame=TRUE,col=ifelse(df.tot2$metascore>50,"red","orange"))

abline(h=df.tot2$perc.meta[df.tot2$metascore==50],col="black",lty=2,lwd=3)





#QUESTION 9 


# in our scatter we can observe that from 2011 to 2014 we have a increasing rate of the perc.metascores,from 2014  to  2020 we have a steady pace of the metascores
# and from 2020 to 2021 the metascores are starting to decreasing.
#also because the dots are so close each other we can figure out that there is a high correlation between the two variables(perc.meta and year)
# also because of the dashed-line we can observe that  most of the movies have metacore above 50.

















