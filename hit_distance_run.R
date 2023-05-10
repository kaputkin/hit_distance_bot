library(scales)
library(ggplot2)
library(ggimage)
library(ggtext)
library(tidyverse) 
library(magick)
library(dplyr)
library(showtext)
library(lubridate)



############
#PULL STATS#
############

#get date list from 4/7/2022 to 4/30/2022
#yesterday <- seq(as.Date("2023-04-30"), as.Date("2023-05-6"), by = "days")
#yesterday <- as.Date("2023-04-29")
#yesterday

#get today's date
yesterday <- Sys.Date()
yesterday


for (y in seq_along(yesterday)) {
  require(baseballr)
  skip <- FALSE
  #try to run the code, if there is an error, skip the date and move on to the next date
  tryCatch({
    #Select and import CSV for each date
    date <- yesterday[y] - 1
    #date <- yesterday
    #get month and day characters
    month_char <- strftime(date, format = "%m")
    day_char <- strftime(date, format = "%d")
    
    #scrape statcast data for each date
    d <- scrape_statcast_savant(date, date, player_type = 'batter')
    d <- d[d$description == 'hit_into_play',] #only look at balls that are in play
    d <- d[,c(6,7,53)] 
    #d <- unique(d)
    
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
      names(total.dist)[ncol(total.dist)] <- paste0("h",month_char,"_",day_char) # rename last column (daily hit distance) to date code
      total.dist <- total.dist[order(-total.dist$total),] #reorder df by total hit distance
      total.dist <- total.dist %>% mutate(row_num = row_number())
      names(total.dist)[ncol(total.dist)] <- paste0("rank",month_char,"_",day_char) # rename last column (rank) to date code
      rank_column <- paste0("rank", month_char, "_", day_char)
      total.dist$last_rank <- total.dist[[rank_column]]
      total.dist$change <- 0
      total.dist <- total.dist[c(1,2,3,6,7,4,5)]
      top10 <- total.dist[1:10,1:5] #generate top 10 list
    } else{
      total.dist <- merge(total.dist, d.dist, by.x =(c("player_name", "batter")), by.y =(c("player_name", "batter")), all=T)
      total.dist[is.na(total.dist)] <- 0 #convert NAs to 0
      names(total.dist)[ncol(total.dist)] <- paste0("h",month_char,"_",day_char) # rename last column to 
      total.dist$total = total.dist$total + total.dist[,ncol(total.dist)] #add last column to total
      total.dist <- total.dist[order(-total.dist$total),] #re-order dataframe by total hit distance
      total.dist <- total.dist %>% mutate(row_num = row_number()) #add rank
      names(total.dist)[ncol(total.dist)] <- paste0("rank",month_char,"_",day_char) # rename last column (rank) to date code
      rank_column <- paste0("rank", month_char, "_", day_char)
      total.dist$change <- total.dist$last_rank - total.dist[[rank_column]]
      top10 <- total.dist[1:10,1:5] #generate top 10 list
      total.dist$last_rank <- total.dist[[rank_column]]
    }
    rm(d.dist)
    cat("Synced games on:", as.character(date))
  }, error = function(e) {
    # Code to handle the error
    cat("Error occurred on:", as.character(date), "- ")
    e
    skip <<- TRUE 
  })
  if(skip) { next } 
  
  
  
  
  ##############
  #CREATE CHART#
  ##############
  
  # Create a list of ids
  batters <- top10$batter
  
  
  # Loop through the URLs and download each image
  for (i in seq_along(batters)) {
    url <-
      paste0(
        'https://img.mlbstatic.com/mlb-photos/image/upload/w_180,q_100/v1/people/',
        batters[i],
        '/headshot/silo/current'
      )
    filename <- paste0(batters[i], ".png")
    destfile <- paste0(tempdir(), '/', filename)
    download.file(url, destfile)
  }
  
  ################
  ################
  
  for (i in 1:length(batters)) {
    #load packages
    require(magick)
    require(cropcircles)
    #Select and import CSV
    player_id = batters[i]
    image_write(
      image_composite(
        image_composite(
          image_read(
            "https://raw.githubusercontent.com/kaputkin/hit_distance_bot/main/assets/circle_bg_2.png"
          ),
          image_read(circle_crop(paste0(
            tempdir(), '/', batters[i], '.png'
          ))),
          operator = "atop",
          offset = "+0+0",
          gravity = "northwest",
          compose_args = ""
        ),
        image_read(
          "https://raw.githubusercontent.com/kaputkin/hit_distance_bot/main/assets/circle_border.png"
        ),
        operator = "atop",
        offset = "+0+0",
        gravity = "northwest",
        compose_args = ""
      ),
      path = paste0(tempdir(), "/", batters[i], ".png"),
      #path = paste0('/Users/mac/Desktop/waywiser/Baseball Hit Distance/test/', batters[i], ".png"),
      format = "png"
    )
  }
  
  ################
  ################
  #add fonts
  font_add_google("bevan", family = "bevan")
  font_add_google("roboto mono", family = "roboto mono")
  
  showtext_auto()
  
  #find headshot file path
  top10$image <- paste0(tempdir(), '/', top10$batter, '.png')
  
  #reformat name
  top10$player_name <-
    sub("(\\w+),\\s(\\w+)", "\\2 \\1", top10$player_name)

  
  #add arrrows
  top10$arrow <- ifelse(top10$change > 0, "/Users/ari/Documents/GitHub/hit_distance_bot/assets/up_arrow.png", 
                        #ifelse(top10$change < 0, "/Users/ari/Documents/GitHub/hit_distance_bot/assets/down_arrow.png", NA))
                        ifelse(top10$change < 0, NA, NA))
  
  #arrange from most to least for chart
  df = top10
  df <- df %>% arrange(desc(total))
  
  #get top three player info
  player_name1 <- df[1, 1]
  player_name2 <- df[2, 1]
  player_name3 <- df[3, 1]
  player1_dist <- df[1, 3]
  player2_dist <- df[2, 3]
  player3_dist <- df[3, 3]
  
  
  # Define colors
  #column_color = "#9364DF"
  column_color = "#7743cc"
  text_color = "#FFFFFF"
  title_color = "#2E1E0F"
  bg_color = "#EFF2F1"
  axis_color = "#282828"
  handle_color = "#FB5012"
  
  max_total <- max(df$total)
  arrow_offset <- max_total * 0.064
  
  
  ######### PLOT #########
  ########################
  g <- ggplot(df, aes(x = reorder(player_name, total), y = total)) +
    geom_col(width = .55,
             position = position_dodge(0.1),
             fill = column_color) +
    geom_image(aes(image = image, x = player_name, y = total), size = .085) +
    geom_image(aes(image = arrow, x = player_name, y = total + arrow_offset), size = .055)+
    #distance text
    geom_text(
      aes(
        label = paste(scales::comma(round(total), accuracy = 1), "Feet"),
        y = total,
        hjust = 1.3,
        vjust = .5
      ),
      colour = text_color,
      family = "roboto mono",
      size = 10.5,
      fontface = "bold"
    ) +
    #player name text
    geom_text(
      aes(
        label = player_name,
        y = .01 * (max(total)),
        hjust = -.05,
        vjust = .5
      ),
      colour = text_color,
      family = "roboto mono",
      fontface = "bold",
      size = 10.8
    ) +
    #title and caption text
    labs(
      title = paste0(
        "<span style='font-size:38pt;font-family:Calibri;'>**Who&#39;s hit the ball furthest in 2023?**",
        "<br/>",
        "<span style='font-size:31pt;font-style:bold;'>as of ",
        format(yesterday[y]-1, "%B"),
        " ",
        format(yesterday[y]-1, "%d"),
        ", ",
        format(yesterday[y]-1, "%Y"),
        "      ",
        "<span style='font-size:29pt;color:",
        handle_color,
        ";'>@howfartheball</span></span>"
      ),
      caption = "Total cumulative distance in feet for all fair batted balls this season"
    ) +
    
    #set correct plot margins
    scale_y_continuous(limits = c(0, max(df$total * 1.1)),
                       expand = c(0, 0),
                       labels = function(x) paste0(formatC(x, format="f", big.mark=",", digits=0), " ft")) +
    #flip the whole thing
    coord_flip() +
    #theme
    theme(
      # Set background color
      plot.background = element_rect(fill = bg_color),
      panel.background = element_rect(fill = bg_color),
      #set margins
      plot.margin = margin(1.2, 1.9, 1.2, 1.9, "cm"),
      # Set the color and the width of the grid lines for the horizontal axis
      panel.grid.major.x = element_line(size = 0.0),
      panel.grid.major.y = element_line(size = 0.0),
      # Set the color and the width of the grid lines for the horizontal axis
      panel.grid.minor.x = element_line(size = 0.0),
      panel.grid.minor.y = element_line(size = 0.0),
      # Remove tick marks by setting their length to 0
      axis.ticks.length = unit(0, "mm"),
      #add axis line
      axis.line.x = element_line(
        size = .7,
        colour = axis_color,
        linetype = 1
      ),
      # Remove the title for both axes
      axis.title = element_blank(),
      # Remove labels from the vertical axis
      axis.text.y = element_blank(),
      # But customize labels for the horizontal axis
      axis.text.x = element_text(
        family = "calibiri",
        size = 30,
        face = "bold",
        color = axis_color,
        margin = margin(t = 8),
        hjust = -.1
      ),
      #title
      plot.title = element_markdown(lineheight = 1.1,),
      #caption
      plot.caption = element_text(
        color = title_color,
        size = 30,
        family = "calibiri",
        hjust = 0,
        vjust = -1.5
      ),
    )
  
  
  #set and save
  setwd('/Users/ari/Desktop/Baseball Hit Distance')
  ggsave(
    "plot.png",
    g,
    width = 8 * .75,
    height = 8.58 * .75,
    dpi = 300
  )
  
  rm(g)
  
  #save total distance df
  export_df <- total.dist %>% 
    select(-starts_with("rank"))
  
  write.csv(export_df, "Hit_distance_totals.csv", row.names=F)

  #########################################
  library(rtweet)
  
  #post tweet
  plot = "/Users/ari/Desktop/Baseball Hit Distance/plot.png"
  
  post_tweet(
   status = paste0("So far, ", player_name1, " has hit the ball the furthest #mlb #baseball"),
   media = plot,
   media_alt_text = paste("A chart showing the top ten MLB players who have hit the ball the furtherst in 2023 by cumulative total. In first place is",player_name1, "with", player1_dist, "feet of cumulative hit distance followed by", player_name2, "with", player2_dist,"feet of cumulative hit distance, and then", player_name3, "with", player3_dist, "feet in third place."),
   token = auth
  )
#print("...sleeping for 24 hours...")
#Sys.sleep(86400)
}

colnames(total.dist)


