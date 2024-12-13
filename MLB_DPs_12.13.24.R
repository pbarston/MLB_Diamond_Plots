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
#need this for scaling
library(scales)

#download the data from: https://www.fangraphs.com/prospects/farm-system-rankings/2024-prospect-list
#or from the Git
system_rankings=read_xlsx("/Diamond Plots - FG System Rankings.xlsx")

#make sure the data types are right: 
str(system_rankings)

#now, we need to a table for value by prospect type and level so that we can back into individual values
#download from git
prospect_values = read_xlsx("/FG_Prospect_Values.xlsx")
#this is from
#https://blogs.fangraphs.com/an-update-to-prospect-valuation/
#https://blogs.fangraphs.com/putting-a-dollar-value-on-prospects-outside-the-top-100/
str(prospect_values)

#let's take just 2024 report 
system_rankings_2024 = system_rankings %>% filter(Year == 2024)

#let's grab 2024 pitchers
pitcher_count = system_rankings_2024 %>% select(-contains("Bat")) #easier to drop the columns I don't want rather than selecting for the things I do
batter_count = system_rankings_2024 %>% select(-contains("Pit")) #easier to drop the columns I don't want rather than selecting for the things I do

#let's gather
pitcher_count_gather = pitcher_count %>% gather(.,category,num_at_this_level,'80 Pit':'35+ Pit')
#key (first variable) is name of new "naming" variable ("category")
#   aka, what will the new column that has all the old column names be called
#value (second variable) is  the name of the new “result” variable (num_at_this_level)
#   aka, what will the new column that has all the values be called
#then you tell it what columns, note that I had to put them in quotes because they have space
#https://rpubs.com/mm-c/gather-and-spread

#join to get values
pitcher_count_gather = pitcher_count_gather %>% left_join(prospect_values, by = c("category"="Prospect Type")) %>%
  #let's clean up some naming
  rename(per_player_value=Value.y) %>% 
  rename(total_system_value=Value.x) %>%
  rename(total_system_count=Ct) %>%
  rename(total_system_rank=Rk)

#now let's add the column for pitcher value
pitcher_count_gather = pitcher_count_gather %>% mutate(value_at_this_level = num_at_this_level*per_player_value)
#replace NAs with 0
pitcher_count_gather = pitcher_count_gather %>% replace(is.na(.),0)

#now create a summary table by team
pitcher_value_summarised = pitcher_count_gather %>% group_by(Org) %>% 
  summarise(total_pitcher_value = sum(value_at_this_level))
#group by doesn't change how it's displayed, it changes how it interacts with other verbs! 
# To removing grouping, use ungroup
# https://dplyr.tidyverse.org/reference/group_by.html


#now, let's do hitters
#let's gather
batter_count_gather = batter_count %>% gather(.,category,num_at_this_level,'80 Bat':'35+ Bat')
#join to get values
batter_count_gather = batter_count_gather %>% left_join(prospect_values, by = c("category"="Prospect Type")) %>%
  #let's clean up some naming
  rename(per_player_value=Value.y) %>% 
  rename(total_system_value=Value.x) %>%
  rename(total_system_count=Ct) %>%
  rename(total_system_rank=Rk)
#now let's add the column for batter  value
batter_count_gather = batter_count_gather %>% mutate(value_at_this_level = num_at_this_level*per_player_value)
#replace NAs with 0
batter_count_gather = batter_count_gather %>% replace(is.na(.),0)
#now create a summary table by team
batter_value_summarised = batter_count_gather %>% group_by(Org) %>% 
  summarise(total_batter_value = sum(value_at_this_level))

#now, let's join them together
cpv_24 = inner_join(batter_value_summarised,pitcher_value_summarised,by = 'Org')
#let's add the total value
cpv_24 = cpv_24 %>% mutate(total_value_PB = total_batter_value+total_pitcher_value)
#matches online by total value! 
#let's also make them into $M so we can use more easily
scale_m = function(x,na.rm = FALSE)(x/1000000)
cpv_24m=cpv_24 %>% mutate(across(c("total_batter_value","total_pitcher_value","total_value_PB"),scale_m))

#For Diamond Plots to work effectively, they need to be symmetric. 
#Meaning, the X-axis has to mirror the Y-axis. 
#However, in our case the pitcher values are just too small to be symmetric with the hitting values (makes sense)
#so let's try and make this more amenable by calculating Z Score
z_score = function(x,na.rm = FALSE)((x-mean(x))/sd(x)) #z_score(cpv_24m$total_batter_value)
cpv_24_m_z = cpv_24m %>% mutate(across(c("total_batter_value","total_pitcher_value","total_value_PB"),z_score))


#now this is much easier with Z Score
sort(cpv_24_m_z$total_batter_value, decreasing = T) 
sort(cpv_24_m_z$total_pitcher_value, decreasing = T) 
#so let's just go with 4,4 as the upper and -4,-4 and the lower for a little buffer

# set max and min, want them to be symmetric for chart
batter_min <- -4
batter_max <- 4
pitcher_min = -4
pitcher_max = 4

# set rotation to 45 degrees
rotation <- 45

#Now we can start plotting our data. 
#Start by plotting hitters  on the x-axis and pitchers on the y-axis.
p_pb1 <- cpv_24_m_z %>%
  ggplot(aes(x = total_batter_value, y = total_pitcher_value))
p_pb1 
#as you can see, the dimensions of the plot RIGHT NOW are based on the underlying data even though no points are plotted


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
#aka 4, 4 and -4,-4 (see values)
#and we've essentially just hand-drawn on these rectangles with annotate

#Then we’re going to add labels to each quadrant.

#I don’t have a good programmatic way of doing this step so I just play around with the x/y location of each label until I think it looks good enough.
#so make sure you just test, then assign it

#to test
p_pb1 + 
  suppressWarnings(geom_richtext(aes(x = 3.75, y = -3.75, label = "+Bat, -Pit"), angle = -1 * rotation, size = 2, fontface = 'bold', color = 'black', fill = "#f7f7f7"))  +
  suppressWarnings(geom_richtext(aes(x = -3.75, y = 3.75, label = "-Bat, +Pit"), angle = -1 * rotation,  size = 2, fontface = 'bold', color = 'black', fill = "#f7f7f7"))   +
  suppressWarnings(geom_richtext(aes(x = -3.75, y = -3.75, label = "-Bat, -Pit"), angle = -1 * rotation, size = 2, fontface = 'bold', color = 'white',  fill = "#762a83", label.colour = 'black'))  +
  suppressWarnings(geom_richtext(aes(x = 3.75, y = 3.75, label = "+Bat, +Pit"), angle = -1 * rotation,  size = 2, fontface = 'bold', color = 'white', fill = "#1b7837", label.colour = 'black'))

#now you can assign
p_pb1 <-p_pb1 + 
  suppressWarnings(geom_richtext(aes(x = 3.75, y = -3.75, label = "+Bat, -Pit"), angle = -1 * rotation, size = 2, fontface = 'bold', color = 'black', fill = "#f7f7f7"))  +
  suppressWarnings(geom_richtext(aes(x = -3.75, y = 3.75, label = "-Bat, +Pit"), angle = -1 * rotation,  size = 2, fontface = 'bold', color = 'black', fill = "#f7f7f7"))   +
  suppressWarnings(geom_richtext(aes(x = -3.75, y = -3.75, label = "-Bat, -Pit"), angle = -1 * rotation, size = 2, fontface = 'bold', color = 'white',  fill = "#762a83", label.colour = 'black'))  +
  suppressWarnings(geom_richtext(aes(x = 3.75, y = 3.75, label = "+Bat, +Pit"), angle = -1 * rotation,  size = 2, fontface = 'bold', color = 'white', fill = "#1b7837", label.colour = 'black'))



#let's add logos
#but first, make a test GG so that you can make edits without having all the logos have to load
test_gg = p_pb1

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
    x = "Value of Hitters Ranked in Farm System (Z Score)",
    y = "Value of Pitchers Ranked in Farm System (Z Score)"
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

test_gg + annotate(geom = 'text', x = 4.15, y = 4.15, label = "2024 Farm System Values", angle = -1 * rotation, vjust = -1.5, fontface = 'bold', size = 4) +
  annotate(geom = 'text', x = 4.15, y = 4.15, label = "Per FG 2024 Report", angle = -1 * rotation, vjust = -0.5, size = 3)

p_pb1 <- p_pb1 +
  # hack together a title and subtitle
  annotate(geom = 'text', x = 4.15, y = 4.15, label = "2024 Farm System Values", angle = -1 * rotation, vjust = -1.5, fontface = 'bold', size = 4) +
  annotate(geom = 'text', x = 4.15, y = 4.15, label = "Per FG 2024 Report", angle = -1 * rotation, vjust = -0.5, size = 3)
p_pb1 

#now, to actually rotate the plot
#That’s what the next bit of code does. 
#It creates a file called “2024systems_value_diamond_plot1.png” and then uses the print() function to rotate it by 45 degrees so that it looks like a diamond.
# save plot
png("2024systems_value_diamond_plot1.png", res = 300, width = 10, height = 10, units = "in", bg = 'floralwhite')

print(p_pb1, vp=viewport(angle=rotation,  
                        width = unit(6, "in"), 
                        height = unit(6, "in")))

dev.off()

#you may have to play around with this last step, particularly the width and height of the PNG. 
