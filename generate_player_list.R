library(baseballr)
library(lubridate)
library(purrr)
library(dplyr)

#players container
players_list <- data.frame(matrix(vector(), 0, 2,
                                  dimnames=list(c(), c("player_name", "batter"))),
                           stringsAsFactors=F)


#get date list
dates <- seq(as.Date("2022-04-07"), as.Date("2022-11-05"), by = "days")


for (i in seq_along(dates)) {
  
  #Select and import CSV
  date <- dates[i]
  d <- scrape_statcast_savant(date, date, player_type = 'batter')
  d <- d[,c(6,7)]
  d <- unique(d)
  players_list <- merge(players_list, d, by.x =(c("player_name", "batter")), by.y =(c("player_name", "batter")), all=T)
  cat("Downloaded games on:", as.character(date))
}


