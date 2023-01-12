players <- read.csv('https://raw.githubusercontent.com/kaputkin/hit_distance_bot/main/player_list_2022.csv')
ids <- players$batter


for (i in 1:length(ids)) {
  #load packages
  require(magick)
  require(cropcircles)
  #Select and import CSV
  player_id = ids[i]

  image_write(
    image_composite(
        image_read("C:/Users/kaputka/Downloads/ball_players/assets/circle_bg.png"),
        image_read(circle_crop(paste0("C:/Users/kaputka/Downloads/ball_players/assets/headshots/", ids[i], ".jpg"))),
        operator = "atop",
        offset = "+0+0",
        gravity = "northwest",
        compose_args = ""
      ),
    path = paste0("C:/Users/kaputka/Downloads/ball_players/crops/", ids[i], ".png"),
    format = "png"
  )
}

