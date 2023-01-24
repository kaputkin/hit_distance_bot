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
      image_read("https://raw.githubusercontent.com/kaputkin/hit_distance_bot/f0234e766d1001fb4e2d72e1c2d470db118d1ad8/assets/circle_bg.png"),
      image_read(circle_crop(paste0(tempdir(),'/',batters[i],'.png'))),
      operator = "atop",
      offset = "+0+0",
      gravity = "northwest",
      compose_args = ""
    ),
    path = paste0(tempdir(),"/", batters[i], ".png"),
    format = "png"
  )
}


 
