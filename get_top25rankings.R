
library(rvest)
library(dplyr)
library(lubridate)
library(stringr)
# The following code scrapes ESPN for the top 25 rankings and also fringe rankings (26+)


Season1Ranks <- data.frame()

# Ok, in week 11, there are 26 teams...
for(i in 1:19){
  html <- read_html(paste0('http://espn.go.com/mens-college-basketball/rankings/_/poll/1/year/2014/week/',i,'/seasontype/2'))
  datestring <- html  %>% html_nodes('h1')  %>% html_text  %>% .[str_detect(.,'\\(')]  %>% str_extract(.,'(?<=\\().*(?=\\))')
  teams <- html %>% html_nodes('.school') %>% html_text %>% str_extract('(\\w|\\s)*') %>% str_trim()
  teamsoutside <- html  %>% html_nodes('.foot-content')  %>% html_text  %>% str_extract('(?<=: ).*(?=Dropped)')  %>% 
    str_split(., ',')  %>% unlist()  %>% str_trim()  %>% str_extract(., '[\\D]*')  %>% str_trim()
  newdf <- data.frame(date = datestring, teams = c(teams[1:25],teamsoutside), ranks = 1:(25+length(teamsoutside)))
  Season1Ranks <- rbind(Season1Ranks,newdf)
}


# Now season 2 ranks
Season2Ranks <- data.frame()
for(i in 1:18){
  html <- read_html(paste0('http://espn.go.com/mens-college-basketball/rankings/_/poll/1/year/2015/week/',i,'/seasontype/2'))
  datestring <- html  %>% html_nodes('h1')  %>% html_text  %>% .[str_detect(.,'\\(')]  %>% str_extract(.,'(?<=\\().*(?=\\))')
  teams <- html %>% html_nodes('.school') %>% html_text %>% str_extract('(\\w|\\s)*') %>% str_trim()
  teamsoutside <- html  %>% html_nodes('.foot-content')  %>% html_text  %>% str_extract('(?<=: ).*(?=Dropped)')  %>% 
    str_split(., ',')  %>% unlist()  %>% str_trim()  %>% str_extract(., '[\\D]*')  %>% str_trim()
  newdf <- data.frame(date = datestring, teams = c(teams[1:25],teamsoutside), ranks = 1:(25+length(teamsoutside)))
  Season2Ranks <- rbind(Season2Ranks,newdf)
}


SeasonRanks <- rbind(Season1Ranks, Season2Ranks)

save(SeasonRanks, file="SeasonRanks.Rdata")
