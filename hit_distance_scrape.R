library(baseballr)

####INFO####
#This script takes a list of dates and
#pulls the total hit distance for every 
#player for that day. It merges together
#each consecutive day and then creates
#another data frame of just the top 10.

####steps for product####
#set to run once a day with Sys.Date()
#save data frame after each day in case of crash
#connect to chart producer
#tweet out chart


#get date list
dates <- seq(as.Date("2022-04-07"), as.Date("2022-4-30"), by = "days")

#loop for pulling
for (i in seq_along(dates)) {
  
  #Select and import CSV
  date <- dates[i]
  
  #get month and day characters
  month_char <- strftime(date, format = "%m")
  day_char <- strftime(date, format = "%d")
  
  #scrape loop
  d <- scrape_statcast_savant(date, date, player_type = 'batter')
  d <- d[,c(6,7,53)]
  d <- unique(d)
  
  #aggregate by player
  d.dist <- aggregate(x = d$hit_distance_sc,          
                      by = list(d$player_name, d$batter),              
                      FUN = sum,
                      na.rm = TRUE)
  rm(d)
  
  colnames(d.dist) <- c("player_name","batter", "hit_distance")
  
  #create total.dist or merge d.dist with total.dist
  if(!exists('total.dist')){
    total.dist <- d.dist
    total.dist$total <- total.dist$hit_distance
    total.dist <- total.dist[c(1,2,4,3)]
    names(total.dist)[ncol(total.dist)] <- paste0("h",month_char,"_",day_char) # rename last column to date code
  } else{
    total.dist <- merge(total.dist, d.dist, by.x =(c("player_name", "batter")), by.y =(c("player_name", "batter")), all=T)
    total.dist[is.na(total.dist)] <- 0 #convert NAs to 0
    names(total.dist)[ncol(total.dist)] <- paste0("h",month_char,"_",day_char) # rename last column to 
    total.dist$total = total.dist$total + total.dist[,ncol(total.dist)] #add last column to total
    total.dist <- total.dist[order(-total.dist$total),] #re-order dataframe by total hit distance
    top10 <- total.dist[1:10,1:3] #generate top 10 list
  }
  rm(d.dist)
  cat("Synced games on:", as.character(date))
}

