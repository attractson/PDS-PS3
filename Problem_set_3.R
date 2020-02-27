# Danny Kim : Problem set #3
rm(list=ls())

## calling the ggplot2 and dplyr
library(ggplot2)


# Q1  - Done
## set-up & beautify the start_Date
primaryPolls <- read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%y")
##codes based on the lecture & https://ggplot2.tidyverse.org/reference/
## by States
primaryPolls<-primaryPolls[primaryPolls$state%in%c("Alabama", "Arkansas", "California", "Colorado", "Maine", 
                                                   "Massachusetts", "Minnesota", "North Carolina", "Oklahoma", "Tennessee", "Texas", "Utah", "Vermont", "Virginia"),]
# by Candidate names - vectoring the top candidates
primaryPolls<-primaryPolls[primaryPolls$candidate_name%in%c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"),]

# plot based on candidates
plot1 <- ggplot(data=primaryPolls,mapping = aes(x=start_date, y=pct, color=candidate_name)) +
  geom_point(alpha=.2) +
  labs(x="Date", y="Percentage", color = "Candidates")+
  geom_smooth(method = lm, se = FALSE,size=.4)+
  facet_wrap(~ candidate_name, nrow=2)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1, size=7)) 
plot1

#by State -  plots based on states

plot2 <- ggplot(data=primaryPolls,mapping = aes(x=start_date, y=pct, color=candidate_name))+
  geom_point(mapping = aes(x=start_date, y=pct,  color=candidate_name), alpha=.5)+
  geom_smooth(method = lm, se = FALSE,size=.1)+
  labs(x="Date", y="Percentage", color = "Candidates")+
  theme_minimal()+
  facet_wrap(~ state, nrow=5)+
  theme(axis.text.x=element_text(angle=45, hjust=1, size=7)) 
plot2

## geom_smooth : deleted the greyish-background thingy -> y ranges to the negative side especially for bloomberg because he joined the race late

##   

# Q2 <- Done
library(dplyr)
library(tidyverse)
##same initial set up with q1
primaryPolls2 <- read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
## limitting down to only the relavant candidates
primaryPolls2$start_date<-as.Date(primaryPolls2$start_date, "%m/%d/%y")

## new set : i think the purpose for this is to use some codes realted to tidyverse.
## https://rdrr.io/github/tidyverse/tidyr/man/pivot_wider.html 

set1<- primaryPolls2 %>%
  filter(candidate_name %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg")) %>%
  pivot_wider(names_from = start_date,values_from=pct)
## I tried using state for my names_from : somehow it is not working. I used the start_date instead and cound see state/candidate name in one row

## compare size of dataset to original sets by comparing the object.size commander 
object.size(set1) # 13612672 bytes

object.size(primaryPolls2) # 3982792 bytes

### new set is much larger than primarypolls2


#Q3
## Initial setup
library(fivethirtyeight)
library(tidyverse)
polls <- read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
Endorsements <- endorsements_2020
## Change the Endorsements variable name endorsee to candidate_name
Endorsements <- rename(Endorsements, candidate_name = endorsee)
## Change the Endorsements dataframe into a tibble object.
Endorsements <- as_tibble(Endorsements)
## filter the poll var. 6 candidates
pollSix <- polls %>%
  filter(candidate_name %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg")) %>%
  select(candidate_name, sample_size, start_date, party, pct)
##Comparing the candidate names * two sets
## check if names are same
unique(Endorsements$candidate_name)
## [1] "John Delaney"       "Joe Biden"          "Julian Castro"     
## [4] "Kamala Harris"      "Bernie Sanders"     "Cory Booker"       
## [7] "Amy Klobuchar"      "Elizabeth Warren"   "Jay Inslee"        
## [10] "John Hickenlooper"  "Beto O'Rourke"      "Kirsten Gillibrand"
## [13] "Pete Buttigieg"     "Eric Swalwell"      "Steve Bullock" 
unique(pollSix$candidate_name)
## [1] "Bernard Sanders"     "Pete Buttigieg"      "Joseph R. Biden Jr."
## [4] "Amy Klobuchar"       "Elizabeth Warren"    "Michael Bloomberg" 

## Biden and sanders are different + Bloomberg not included
##recode () : https://dplyr.tidyverse.org/reference/recode.html
Endorsements$candidate_name<-recode(Endorsements$candidate_name, 'Bernie Sanders'='Bernard Sanders')
Endorsements$candidate_name<-recode(Endorsements$candidate_name, 'Joe Biden'='Joseph R. Biden Jr.')

##combining the candidatenames
combined_dataset <- pollSix %>%
  inner_join(Endorsements, by = "candidate_name")
unique(combined_dateset$candidate_name)
## [1] "Bernard Sanders"     "Pete Buttigieg"      "Joseph R. Biden Jr."
## [4] "Amy Klobuchar"       "Elizabeth Warren" 
colnames(Endorsements)
colnames(combined_dateset)
## Number of endorsement
## using count - part of dpylr-
endorsementnumber <- combined_dataset %>%
   count(candidate_name)
###1	Amy Klobuchar	8740
###2	Bernard Sanders	15360
###3	Elizabeth Warren	8667
###4	Joseph R. Biden Jr.	28159
###5	Pete Buttigieg	4450
library(ggplot2)
colnames(endorsementnumber)

p <- ggplot(data=endorsementnumber) +
  geom_col(mapping = aes(x=candidate_name, y=n, fill=candidate_name))
p

#darkmode
p+theme_dark()
p+xlab("Candidate Name")+labs(title="Endorsements of Candidates")+
  theme_classic() +
  theme(axis.text.x=element_text(angle=20, hjust=1, size=8)) +
  labs(x="Candidate", y="Endorsement", color = "Candidates", size = 8)+
  theme_minimal() 
  


## More 


#Q4
library(tidyverse)
library(tm)
library(lubridate)
library(wordcloud)
tweets <- read_csv('https://politicaldatascience.com/PDS/Datasets/trump_tweets.csv')
# 1st Task - date & year separate
## https://www.rdocumentation.org/packages/tidyr/versions/0.8.3/topics/separate <= data,col,into = c( )
tweets <- tweets%>%
  separate(created_at,into=c("date","Time"),sep=" ")
range(tweets$date)
### [1] "1/1/2014" "9/9/2019"


# 2nd Task - dpylr subset to remove retweers & show top 5 texts 
## Retweet : RT @ or column <- decide to use the column
realTrump <- tweets %>%
  filter(is_retweet=="FALSE")
## removed retweers and show the text of top 5 
realTrump[match(tail(sort(realTrump$favorite_count),5),realTrump$favorite_count),]
realTrump[match(tail(sort(realTrump$retweet_count),5),realTrump$retweet_count),]

#3rd Task - Incomplete
## Remove whitespace & remove numbers and puncuation, convert everything to lowercase 
## package tm does these for me
## https://www.rdocumentation.org/packages/tm/versions/0.7-7
temp = realTrump

tempLine= temp$text

tempLine <-removePunctuation(tempLine) 
tempLine <-removeNumbers(tempLine)
tempLine <-tolower(tempLine)
tempLine <-stripWhitespace(tempLine)
tempLine <-str_remove(tempLine,"@")
tempLine <-removeWords(tempLine, stopwords("english"))
tempLine <-removeWords(tempLine, c("see", "people","new","want","one","even","must","need", "done","back",
                                    "just","going", "know", "can", "said", "like","many","like","realdonaldtrump"))

# 4th wordcloud -> visualize top 50 words 
tempCor <- Corpus(VectorSource(tempLine))
wordcloud(tempCor,max.words = 50,random.order=FALSE,scale=c(.5,1))

## https://www.rdocumentation.org/packages/wordcloud/versions/2.6/topics/wordcloud
#5th
fifth <- DocumentTermMatrix(tempCor,control = list(weighting = weightTfIdf))

#6th
freq6<-findFreqTerms(fifth, lowfreq = 0.8)
freq6

