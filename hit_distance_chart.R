library(tidyverse) 
library(ggimage)
library(magick)

top10$image <- paste0('./crops/',top10$batter,'.png')

df = data.frame(
  name = c("Judge","Schwarber","Trout","Alonso","Riley", "Alvarez", "Walker", "Betts", "Tellez", "Goldschmidt"),  
  value = c(62,46,40,40,38,37,36,35,35,35),
  image = c("C:/Users/kaputka/Downloads/ball_players/crops/Judge.png", 
            "C:/Users/kaputka/Downloads/ball_players/crops/Schwarber.png", 
            "C:/Users/kaputka/Downloads/ball_players/crops/Trout.png",
            "C:/Users/kaputka/Downloads/ball_players/crops/Alonso.png",
            "C:/Users/kaputka/Downloads/ball_players/crops/Riley.png",
            "C:/Users/kaputka/Downloads/ball_players/crops/Alvarez.png",
            "C:/Users/kaputka/Downloads/ball_players/crops/Walker.png",
            "C:/Users/kaputka/Downloads/ball_players/crops/Betts.png",
            "C:/Users/kaputka/Downloads/ball_players/crops/Tellez.png",
            "C:/Users/kaputka/Downloads/ball_players/crops/no_hs.png")
) 

df=top10

df %>% 
  mutate(value1 = total) %>%
  transform(name = reorder(player_name, total))

ggplot(df, aes(player_name, total)) + 
  geom_col(width = .35,
           position = position_dodge(0.1),
           fill = '#173753') + 
  geom_image(aes(image=image, y = total), size=.07) +
  coord_flip()+
  geom_text(
        aes(label = paste(total, "Distance"),
            y = total - 3,
            hjust = 1,
            vjust= .4),
        colour = "#FFFFFF",
        family = "Econ Sans Cnd",
        size = 4,
        fontface = "bold")+
  geom_text(
        aes(label = player_name,
            y = 1,
            hjust = 0,
            vjust = .38),
        colour = "#C5D8E4",
        family = "Econ Sans Cnd",
        fontface = "bold",
        size = 5)+
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "#C5D8E4"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#EEEBD3", size = 0.1),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.minor.x = element_line(color = "#EEEBD3", size = 0.0),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(family = "Econ Sans Cnd", size = 10, face="bold")
  )




