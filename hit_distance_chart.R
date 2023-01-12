library(tidyverse) 
library(ggimage)
library(magick)
library(dplyr)

####IMPROVEMENTS####
#Need to add link to twitter
#work on colors and design

top10$image <- paste0('https://raw.githubusercontent.com/kaputkin/hit_distance_bot/main/crops/',top10$batter,'.png')
df=top10

df <- df %>% arrange(desc(total))

df <- df[1:4,]

g <- ggplot(df, aes(x = reorder(player_name, total), y = total)) + 
  geom_col(width = .35,
           position = position_dodge(0.1),
           fill = '#173753') + 
  geom_image(aes(image=image, y = total), size=.07) +
  coord_flip()+
  geom_text(
        aes(label = paste(scales::comma(round(total), accuracy=1), "Feet"),
            y = total,
            hjust = 1.31,
            vjust= .4),
        colour = "#FFFFFF",
        family = "Arial Black",
        size = 4,
        fontface = "bold")+
  geom_text(
        aes(label = player_name,
            y = 1,
            hjust = 0,
            vjust = .38),
        colour = "#C5D8E4",
        family = "Arial Black",
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
    axis.text.x = element_text(family = "Arial Black", size = 10, face="bold")
  )

ggsave("plot.png", g)

setwd('/Users/mac/Desktop/waywiser/Baseball Hit Distance')


