# Danny Kim : Problem set #3
rm(list=ls())

## calling the ggplot2 and dplyr
library(ggplot2)
library(dplyr)

# Q1
primaryPolls <- read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%y")
# by States
primaryPolls<-primaryPolls[primaryPolls$state%in%c("Alabama", "Arkansas", "California", "Colorado", "Maine", 
                                                   "Massachusetts", "Minnesota", "North Carolina", "Oklahoma", "Tennessee", "Texas", "Utah", "Vermont", "Virginia"),]
# by Candidate names
primaryPolls<-primaryPolls[primaryPolls$candidate_name%in% c("Amy Klobuchar","Bernard Sanders","Elizabeth Warren",
                                                             "Joseph R. Biden Jr.",  "Michael Bloomberg", "Pete Buttigieg"),]
