library(scales)
library(ggplot2)
library(ggimage)
library(ggtext)
library(tidyverse) 
library(magick)
library(dplyr)
library(showtext)

############
#PULL STATS#
############

#get date list from 4/7/2022 to 4/30/2022
dates <- seq(as.Date("2022-08-24"), as.Date("2022-08-26"), by = "days")

#loop through each date in the date list
for (i in seq_along(dates)) {
  require(baseballr)
  skip <- FALSE
  #try to run the code, if there is an error, skip the date and move on to the next date
  tryCatch({
    #Select and import CSV for each date
    date <- dates[i]
    
    #get month and day characters
    month_char <- strftime(date, format = "%m")
    day_char <- strftime(date, format = "%d")
    
    #scrape statcast data for each date
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
      total.dist <- total.dist[order(-total.dist$total),]
      top10 <- total.dist[1:10,1:3] #generate top 10 list
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
  df = top10
  
  #get date
  today <- Sys.Date()
  month <- format(today, format = "%B")
  day <- format(today, format = "%d")
  year <- format(today, format = "%Y")
  
  #arrange from most to least for chart
  df <- df %>% arrange(desc(total))
  
  
  # Define colors
  column_color = "#9364DF"
  text_color = "#FFFFFF"
  title_color = "#2E1E0F"
  bg_color = "#EFF2F1"
  axis_color = "#282828"
  handle_color = "#FB5012"
  
  
  ######### PLOT #########
  ########################
  g <- ggplot(df, aes(x = reorder(player_name, total), y = total)) +
    geom_col(width = .55,
             position = position_dodge(0.1),
             fill = column_color) +
    geom_image(aes(image = image, x = player_name, y = total), size = .085) +
    #scale_y_continuous(labels = comma)+
    geom_text(
      aes(
        label = paste(scales::comma(round(total), accuracy = 1), "Feet"),
        y = total,
        hjust = 1.3,
        vjust = .5
      ),
      colour = text_color,
      family = "roboto mono",
      size = 3.5,
      fontface = "bold"
    ) +
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
      size = 3.8
    ) +
    #title and caption text
    labs(
      title = paste0(
        "<span style='font-size:18pt;font-family:Calibri;'>**Who's Hit the Furthest in 2023?**",
        "<br/>",
        "<span style='font-size:11pt;font-style:bold;'>as of ",
        month,
        " ",
        day,
        ", ",
        year,
        "      ",
        "<span style='font-size:9pt;color:",
        handle_color,
        ";'>@howfartheball</span></span>"
      ),
      caption = "Total distance in feet for all batted balls this season"
    ) +
    
    #set correct plot margins
    scale_y_continuous(limits = c(0, max(df$total * 1.1)),
                       expand = c(0, 0),
                       labels = comma) +
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
        size = 10,
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
        size = 10,
        family = "calibiri",
        hjust = 0,
        vjust = -1.5
      ),
    )
  
  #set and save
  setwd('/Users/mac/Desktop/waywiser/Baseball Hit Distance')
  ggsave(
    "plot.png",
    g,
    width = 8 * .75,
    height = 8.58 * .75,
    dpi = 300
  )
  
  
  #########################################3
  library(rtweet)
  
  # Create a token containing your Twitter keys
  #auth <- rtweet_bot()
  #auth_save(auth, "baseballbot")
  auth = auth_as("baseballbot")
  
  #post tweet
  plot = "/Users/mac/Desktop/waywiser/Baseball Hit Distance/plot.png"
  
  post_tweet(
    status = "",
    media = plot,
    media_alt_text = "alt_text_test",
    token = auth
  )  
}



