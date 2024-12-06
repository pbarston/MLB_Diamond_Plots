#INSPIRED FROM: https://thef5.substack.com/p/how-to-diamond-plots-in-r?utm_source=post-email-title&publication_id=47430&post_id=150563272&utm_campaign=email-post-title&isFreemail=true&r=tvc8&triedRedirect=true

# need for data wrangling
library(tidyverse)
# need for plotting mlb team logos
library(mlbplotR)
# need this for rotation and saving
library(grid)
# need this for labels
library(ggtext)
#need this to read the xcl
library(readxl)

#download the data from: https://www.fangraphs.com/prospects/farm-system-rankings/2024-prospect-list
#or from the Git
system_rankings=read_xlsx("/Diamond Plots - FG System Rankings.xlsx")

#make sure the data types are right: 
str(system_rankings)

#to start, let's make it really easy and over-simplified: # of pitchers and # of hitters
pitcher_count = system_rankings %>% select(-contains("Bat")) #easier to drop the columns I don't want rather than selecting for the things I do
#now for pitcher_count, let's create a column that sums up to get the number of pitchers
pitcher_count = pitcher_count %>% mutate(pitcher_count = rowSums(select(.,contains("Pit"))))

#let's do same for batters
batter_count = system_rankings %>% select(-contains("Pit")) ##easier to drop the columns I don't want rather than selecting for the things I do
batter_count = batter_count %>% mutate(batter_count = rowSums(select(.,contains("Bat"))))

#and let's start by filtering on just one year, 2024, to demonstrate proof of concept
pitcher_count_24 = pitcher_count %>% filter(Year == '2024')
batter_count_24 = batter_count %>% filter(Year == '2024')

#now let's join!
combined_24 = pitcher_count_24 %>% inner_join(batter_count_24,by="Org")
#better data practices would organize the table better but not needed here

#For Diamond Plots to work effectively, they need to be symmetric. 
#Meaning, the X-axis has to mirror the Y-axis. 
#For our purposes, that means finding the maximum and minimum for hitter and pitcher count because those are upper bounds plots. 
# find the min and max value of players for plot
sort(combined_24$batter_count, decreasing = T) 
sort(combined_24$pitcher_count, decreasing = T) 
#so let's just go with 32, 32 as the upper and 8, 8 and the lower for a little buffer

# set max and min, want them to be symmetric for chart
batter_min <- 8
batter_max <- 32
pitcher_min = 8
pitcher_max = 32

# set rotation to 45 degrees
rotation <- 45

#Now we can start plotting our data. 
#Start by plotting hitters  on the x-axis and pitchers on the y-axis.
p_pb1 <- combined_24 %>%
  ggplot(aes(x = batter_count, y = pitcher_count))
p_pb1 
#as you can see, the dimensions of the plot are based on the underlying data even though no points are plotted

#Next, we’re going to draw four squares in the corners of each plot that serve as our four quadrants.

p_pb1 <- p_pb1 +
  # add color blocking
  #annotate: This function adds geoms to a plot, 
  #but unlike a typical geom function, the properties of the geoms are not mapped from variables of a data frame, but are instead passed in as vectors.
  annotate("rect", xmin = (batter_max + batter_min) / 2, xmax = batter_max, 
           ymin = pitcher_min, ymax = (pitcher_max + pitcher_min) / 2, fill= "#f7f7f7", alpha = .5, color = 'transparent') + 
  annotate("rect", xmin = batter_min, xmax = (batter_max + batter_min) / 2, 
           ymin = (pitcher_max + pitcher_min) / 2, ymax = pitcher_max, fill= "#f7f7f7", alpha = .5, color = 'transparent') +
  annotate("rect", xmin = (batter_max + batter_min) / 2, xmax = batter_max,
           ymin = (pitcher_max + pitcher_min) / 2, ymax = pitcher_max, fill= "#a6dba0", alpha = .5, color = 'transparent')  +
  annotate("rect", xmin = batter_min, xmax = (batter_max + batter_min) / 2,
           ymin = pitcher_min, ymax = (pitcher_max + pitcher_min) / 2, fill= "#c2a5cf", alpha = .5, color = 'transparent')
p_pb1
#what we've also done by creating the min and max is push the graph to the boundaries we want
#aka 8,8 and 30,30 (see values)
#and we've essentially just hand-drawn on these rectangles with annotate

#Then we’re going to add labels to each quadrant.

#I don’t have a good programmatic way of doing this step so I just play around with the x/y location of each label until I think it looks good enough.
p_pb1 <- p_pb1 +
  suppressWarnings(geom_richtext(aes(x = 30, y = 10, label = "+Bat, -Pit"), angle = -1 * rotation, size = 2, fontface = 'bold', color = 'black', fill = "#f7f7f7"))  +
  suppressWarnings(geom_richtext(aes(x = 10, y = 30, label = "-Bat, +Pit"), angle = -1 * rotation,  size = 2, fontface = 'bold', color = 'black', fill = "#f7f7f7"))   +
  suppressWarnings(geom_richtext(aes(x = 10, y = 10, label = "-Bat, -Pit"), angle = -1 * rotation, size = 2, fontface = 'bold', color = 'white',  fill = "#762a83", label.colour = 'black'))  +
  suppressWarnings(geom_richtext(aes(x = 30, y = 30, label = "+Bat, +Pit"), angle = -1 * rotation,  size = 2, fontface = 'bold', color = 'white', fill = "#1b7837", label.colour = 'black'))

p_pb1

#let's add logos
p_pb1 = p_pb1 +
  #add team logos
  geom_mlb_logos(aes(team_abbr = Org),width = 0.05, alpha = 0.75, angle = -1*rotation)
show(p_pb1)
#lucky for us, the team abbreviations are matched by the geom_mlb_logos call function automatically
#we get to play around with width, alpha, etc.

#Now we add labels!
p_pb1 <- p_pb1 +
  # add axis labels
  labs(
    x = "# of Hitters Ranked in Farm System",
    y = "# of Pitchers Ranked in Farm System"
  ) 
print(p_pb1)



#we forced the plot to be symmetric with our values, so we'll play around with widths of logos and placement of text

#Then we’re going to make a bunch of thematic tweaks that include:
#using theme_minimal()
#rotating each axis text and adjusting their positions
#removing some gridlines
#changing the background color of the plot (floralwhite is an F5 trademark)
p_pb1 <- p_pb1 +
  # thematic stuff 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=(-1 * rotation), hjust = 0.5, margin = margin(t = -9.5)),
        axis.text.y = element_text(angle=(-1 * rotation), hjust = 0.5, margin = margin(r = -5)),
        axis.title.x = element_text(size = 8,
                                    vjust = 0.5, 
                                    margin = margin(t = 10),
                                    face = 'bold',
                                    color = "black"),
        axis.title.y = element_text(size = 8,
                                    angle=(-1 * rotation - 45),
                                    hjust = 0.5,
                                    margin = margin(r = 10),
                                    color = "black", 
                                    face = 'bold'), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"))

p_pb1

#Lastly, we’ll put together a custom title and subtitle. 
#Again, you’ll have to play with the x/y location of the text depending on your data. 
#Don’t worry if the title/subtitle is spilling off the page. It’ll right itself when we rotate the plot.

p_pb1 <- p_pb1 +
  # hack together a title and subtitle
  annotate(geom = 'text', x = 31, y = 31, label = "2024 Farm System Builds", angle = -1 * rotation, vjust = -1.5, fontface = 'bold', size = 4) +
  annotate(geom = 'text', x = 31, y = 31, label = "Per FG 2024 Report", angle = -1 * rotation, vjust = -0.5, size = 3)

p_pb1 

#now, to actually rotate the plot
#That’s what the next bit of code does. 
#It creates a file called “2024systems_diamond_plot.png” and then uses the print() function to rotate it by 45 degrees so that it looks like a diamond.
# save plot
# save plot
png("2024systems_diamond_plot15.png", res = 300, width = 10, height = 10, units = "in", bg = 'floralwhite')

print(p_pb1, vp=viewport(angle=rotation,  
                        width = unit(6, "in"), 
                        height = unit(6, "in")))

dev.off()

#you may have to play around with this last step, particularly the width and height of the PNG. 
