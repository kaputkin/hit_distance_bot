library(tidyverse) 
library(ggimage)
library(magick)
library(dplyr)
#library(extrafont)
library(showtext)

font_add_google("Bevan", family = "bevan")
showtext_auto()


top10 <- top10[1:3]
top10$batter[4] <- "999999"
list.files(tempdir())


#find headshot file path
top10$image <- paste0(tempdir(),'/',top10$batter,'.png')

#reformat name
top10$player_name <-  sub("(\\w+),\\s(\\w+)","\\2 \\1", top10$player_name)

#put in df
df=top10

#arrange from most to least for chart
df <- df %>% arrange(desc(total))

#make chart
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
        colour = "#C5D8E4",
        family = "Arial",
        size = 4,
        fontface = "plain")+
  geom_text(
        aes(label = player_name,
            y = .01*(max(total)),
            hjust = 0,
            vjust = .38),
        colour = "#FFFFFF",
        family = "Arial",
        fontface = "plain",
        size = 4)+
  ggtitle("Who's Hit The Furthers?")+
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
    axis.text.x = element_text(family = "Arial Black", size = 10, face="bold"),
    #title
    plot.title = element_text(color="red", size=14, face="bold.italic")
  )

ggsave("plot.png", g)

setwd('/Users/mac/Desktop/waywiser/Baseball Hit Distance')






