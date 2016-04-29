# This script scrapes sportsmediawatch.com in order to obtain 
# the tv rankings for each game that occurred in the 2013-2014 
# and 2014-2015 NCAA Men's Basketball Seasons

library(rvest)
library(dplyr)
library(lubridate)
library(stringr)


# Begin scraping 
Season1part1 <- read_html('http://www.sportsmediawatch.com/2014/03/college-basketball-tv-ratings-numbers-for-every-game-of-the-2013-14-season/2/') %>% html_nodes('table') %>% html_table() %>% as.data.frame()


# Indices where the date is listed instead of data
indices <- which(is.na(Season1part1$Game))

# Put the date as a new column and take out notes column
Season1part1 <- Season1part1 %>% select(-Notes)
Season1part1$Date <- NA

for(i in 1:length(indices)){
  if(i != length(indices)){
    inbetween <- (indices[i]+1):(indices[i+1]-1)
    Season1part1$Date[inbetween] <- mdy(Season1part1$Rtg.[indices[i]])
  }
  else {
    inbetween <- (indices[length(indices)]+1):nrow(Season1part1)
    Season1part1$Date[inbetween] <- mdy(Season1part1$Rtg.[indices[i]])
  }
}

# Filter out the unneeded dates
Season1part1 <- Season1part1 %>% filter(!is.na(Game))

# Create Names of colleges
Season1part1$College1 <- NA
Season1part1$College1 <- lapply(Season1part1$Game, function(x) str_split(x," at ")[[1]][1])

Season1part1$College2 <- NA
Season1part1$College2 <- lapply(Season1part1$Game, function(x) str_split(x, " at ")[[1]][2])



## Season 1 part 2
Season1part2 <- read_html('http://www.sportsmediawatch.com/2014/03/college-basketball-tv-ratings-numbers-for-every-game-of-the-2013-14-season/3/') %>% html_nodes('table') %>% html_table() %>% as.data.frame()


# Indices where the date is listed instead of data
indices <- which(is.na(Season1part2$Game))

# Put the date as a new column and take out notes column
Season1part2 <- Season1part2 %>% select(-Notes)
Season1part2$Date <- NA

for(i in 1:length(indices)){
  if(i != length(indices)){
    inbetween <- (indices[i]+1):(indices[i+1]-1)
    Season1part2$Date[inbetween] <- mdy(Season1part2$Rating[indices[i]])
  }
  else {
    inbetween <- (indices[length(indices)]+1):nrow(Season1part2)
    Season1part2$Date[inbetween] <- mdy(Season1part2$Rating[indices[i]])
  }
}

# Filter out the unneeded dates
Season1part2 <- Season1part2 %>% filter(!is.na(Game))

# Create Names of colleges
Season1part2$College1 <- NA

# Not well-behaved #

getcollegename1 <- function(x){
  if(length(str_split(x, ' at ')[[1]]) > 1){
    str_split(x, ' at ')[[1]][1]
  }
  else{
    str_split(x, ' vs. ')[[1]][1]
  }
}

getcollegename2 <- function(x){
  if(length(str_split(x, ' at ')[[1]]) > 1){
    str_split(x, ' at ')[[1]][2]
  }
  else {
    str_split(x, ' vs. ')[[1]][2]
  }
}
Season1part2$College1 <- lapply(Season1part2$Game, getcollegename1)

Season1part2$College2 <- NA
Season1part2$College2 <- lapply(Season1part2$Game, getcollegename2)



Season2part1 <-read_html('http://www.sportsmediawatch.com/2015/03/college-basketball-ratings-viewership-every-game-2014-2015-regular-season-espn-cbs-fox-fs1-nbcsn/2/') %>% html_nodes('table') %>% html_table() %>% as.data.frame()


# Indices where the date is listed instead of data
indices <- which(is.na(Season2part1$Game))

# Put the date as a new column 
Season2part1$Date <- NA

for(i in 1:length(indices)){
  if(i != length(indices)){
    inbetween <- (indices[i]+1):(indices[i+1]-1)
    Season2part1$Date[inbetween] <- mdy(Season2part1$Rtg.[indices[i]])
  }
  else {
    inbetween <- (indices[length(indices)]+1):nrow(Season2part1)
    Season2part1$Date[inbetween] <- mdy(Season2part1$Rtg.[indices[i]])
  }
}

# Filter out the unneeded dates
Season2part1 <- Season2part1 %>% filter(!is.na(Game))

# Create Names of colleges
Season2part1$College1 <- NA
Season2part1$College1 <- lapply(Season2part1$Game, function(x) str_split(x,"/")[[1]][1])

Season2part1$College2 <- NA
Season2part1$College2 <- lapply(Season2part1$Game, function(x) str_split(x, "/")[[1]][2])


Season2part2 <-read_html('http://www.sportsmediawatch.com/2015/03/college-basketball-ratings-viewership-every-game-2014-2015-regular-season-espn-cbs-fox-fs1-nbcsn/3/') %>% html_nodes('table') %>% html_table() %>% as.data.frame()


# Indices where the date is listed instead of data
indices <- which(is.na(Season2part2$Game))

# Put the date as a new column 
Season2part2$Date <- NA

for(i in 1:length(indices)){
  if(i != length(indices)){
    inbetween <- (indices[i]+1):(indices[i+1]-1)
    Season2part2$Date[inbetween] <- mdy(Season2part2$Rtg.[indices[i]])
  }
  else {
    inbetween <- (indices[length(indices)]+1):nrow(Season2part2)
    Season2part2$Date[inbetween] <- mdy(Season2part2$Rtg.[indices[i]])
  }
}

# Filter out the unneeded dates
Season2part2 <- Season2part2 %>% filter(!is.na(Game))

# Create Names of colleges
Season2part2$College1 <- NA
Season2part2$College1 <- lapply(Season2part2$Game, function(x) str_split(x,"/")[[1]][1])

Season2part2$College2 <- NA
Season2part2$College2 <- lapply(Season2part2$Game, function(x) str_split(x, "/")[[1]][2])


# For this particular dataset, the data is not well formatted for some of the later games, so we simply remove those (html table is not well behaved)

# Empirically, we note that many entires after Seton Hall v. Georgetown on March 7, 2015 are poorly behaved.

well_behaved <- which(Season2part2$College1 == 'Seton Hall' & Season2part2$College2 == 'Georgetown')

Season2part2 <- Season2part2[1:well_behaved,]


### Combine datasets

names(Season1part1) <- c("Rtg.", "Vwrs.", "Game", "Time", "Network", "Date", "College1", "College2")
# Reorder to end up like all other datasets

Season1part1 <- Season1part1 %>% select(Rtg., Vwrs., Time, Game, Network, Date, College1, College2)

#Season1 part 2
names(Season1part2) <- c("Rtg.", "Vwrs.", "Game", "Time", "Network", "Date", "College1", "College2")

# Reorder
Season1part2 <- Season1part2 %>% select(Rtg., Vwrs., Time, Game, Network, Date, College1, College2)

AllSeasons <- data.frame(rbind(Season1part1,Season1part2, Season2part1, Season2part2))

# Save the file
save(AllSeasons, file = "AllSeasons.Rdata")
