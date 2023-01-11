playerlist <- read.csv('/Users/mac/Desktop/waywiser/Baseball Hit Distance/player_list_2022.csv')

# Create a list of ids
batters <- playerlist$batter


# Loop through the URLs and download each image
for (i in seq_along(batters)) {
  url <- paste0('https://img.mlbstatic.com/mlb-photos/image/upload/w_180,q_100/v1/people/', 
                 batters[i],
                '/headshot/silo/current'
                )
  filename <- paste0(batters[i], ".jpg")
  destfile <- paste0('/Users/mac/Desktop/waywiser/Baseball Hit Distance/headshots/', filename)
  download.file(url, destfile)
}

