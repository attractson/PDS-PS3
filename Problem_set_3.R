# Danny Kim : Problem set #3
rm(list=ls())

## calling the ggplot2 and dplyr
library(ggplot2)


# Q1  -Done
primaryPolls <- read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%y")
# by States
primaryPolls<-primaryPolls[primaryPolls$state%in%c("Alabama", "Arkansas", "California", "Colorado", "Maine", 
                                                   "Massachusetts", "Minnesota", "North Carolina", "Oklahoma", "Tennessee", "Texas", "Utah", "Vermont", "Virginia"),]
# by Candidate names
primaryPolls<-primaryPolls[primaryPolls$candidate_name%in%unique(primaryPolls$candidate_name),]

graph <- ggplot(data=primaryPolls,mapping = aes(x=start_date, y=pct, color=candidate_name)) +
  geom_point(, alpha=.2) +
  labs(x="Date", y="Percentage", color = "Candidates")+
  geom_smooth(method = lm, se = FALSE,size=.4)+
  ## to not show the plot -> y gets the neg value on the plot
  facet_wrap(~ candidate_name, nrow=2)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1, size=7)) 
graph

##   

# Q2
library(dplyr)
library(tidyverse)
primaryPolls2 <- read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls2$start_date<-as.Date(primaryPolls2$start_date, "%m/%d/%y")
primaryPolls2 %>%
  select(state, candidate_name, pct,sample_size)  %>% 
  filter(candidate_name %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"))

#Q3
library(fivethirtyeight)
library(tidyverse)
polls <- read_csv('https://jmontgomery.github.io/PDS/Datasets/president
_primary_polls_feb2020.csv')
Endorsements <- endorsements_2020
  


