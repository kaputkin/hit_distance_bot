library(magick)
library(cropcircles)

names = c("Judge","Schwarber","Trout","Alonso","Riley", "Alvarez", "Walker", "Betts", "Tellez", "Goldschmidt")

for (i in 1:length(names)) {
  
  #Select and import CSV
  player_name = names[i]

  image_write(
    image_composite(
        image_read("C:/Users/kaputka/Downloads/ball_players/circle_bg.png"),
        image_read(circle_crop(paste0("C:/Users/kaputka/Downloads/ball_players/", player_name, ".png"))),
        operator = "atop",
        offset = "+0+0",
        gravity = "northwest",
        compose_args = ""
      ),
    path = paste0("C:/Users/kaputka/Downloads/ball_players/crops/", player_name, ".png"),
    format = "png"
  )
}

