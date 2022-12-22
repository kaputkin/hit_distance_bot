library(baseballr)
library(plyr)


#get yesterdays date
date <- Sys.Date()-200

##pull live ball distance
d <- scrape_statcast_savant(date, date, player_type = 'batter')
d <- d[which(d$description == 'hit_into_play'),]
d <- d[,c(6,7,53)]
s
#aggregate by player
d.dist <- aggregate(x = d$hit_distance_sc,          
          by = list(d$player_name, d$batter),              
          FUN = sum,
          na.rm = TRUE)
rm(d)

#figure out join. Needs to be able to add new names and easily sum new distance everyday
d.joined <- merge(total.dist, d.dist, by.x =(c("Group.1", "Group.2")), by.y =(c("Group.1", "Group.2")), all.y=T)

colnames(d.joined) <- c("Player Name", "PlayerID", "Dist_old", "Dist_new")

data.dist.joined$Dist <- data.dist.joined$Dist_old + data.dist.joined$Dist_new
