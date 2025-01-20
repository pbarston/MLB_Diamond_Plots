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
#for themes
library(ggthemes)
#for the table
library(gt)
#for exporting
library(webshot2)
#for working with data
library(forcats)



#download the data from: https://www.fangraphs.com/prospects/farm-system-rankings/2024-prospect-list
#or from the Git
system_rankings=read_xlsx("/Diamond Plots - FG System Rankings.xlsx")

#make sure the data types are right: 
str(system_rankings)

#now, we need to a table for value by prospect type and level so that we can back into individual values
prospect_values = read_xlsx("/FG_Prospect_Values.xlsx")
#this is from
#https://blogs.fangraphs.com/an-update-to-prospect-valuation/
#https://blogs.fangraphs.com/putting-a-dollar-value-on-prospects-outside-the-top-100/
str(prospect_values)

#now we want to see z scores each year
#first, let's split
pitcher_count = system_rankings %>% select(-contains("Bat")) #easier to drop the columns I don't want rather than selecting for the things I do
batter_count = system_rankings %>% select(-contains("Pit")) #easier to drop the columns I don't want rather than selecting for the things I do

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
  rename(total_system_rank=Rk) %>%
  rename(system_wide_average=Average)

#now let's add the column for pitcher value
pitcher_count_gather = pitcher_count_gather %>% mutate(value_at_this_level = num_at_this_level*per_player_value)
#replace NAs with 0
pitcher_count_gather = pitcher_count_gather %>% replace(is.na(.),0)

#now create a summary table by team
pitcher_value_summarised = pitcher_count_gather %>% group_by(Org,Year) %>% 
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
  rename(total_system_rank=Rk) %>%
  rename(system_wide_average=Average)

#now let's add the column for batter  value
batter_count_gather = batter_count_gather %>% mutate(value_at_this_level = num_at_this_level*per_player_value)
#replace NAs with 0
batter_count_gather = batter_count_gather %>% replace(is.na(.),0)
#now create a summary table by team
batter_value_summarised = batter_count_gather %>% group_by(Org,Year) %>% 
  summarise(total_batter_value = sum(value_at_this_level))

#now, let's join them together by year
hpvalue = inner_join(batter_value_summarised,pitcher_value_summarised,by = c('Org','Year'))
#let's add the total value
hpvalue = hpvalue %>% mutate(total_value_PB = total_batter_value+total_pitcher_value)

#let's also make them into $M so we can use more easily
scale_m = function(x,na.rm = FALSE)(x/1000000)
hpvalue_m=hpvalue %>% mutate(across(c("total_batter_value","total_pitcher_value","total_value_PB"),scale_m))

#For Diamond Plots to work effectively, they need to be symmetric. 
#Meaning, the X-axis has to mirror the Y-axis. 
#However, in our case the pitcher values are just too small to be symmetric with the hitting values (makes sense)
#so let's try and make this more amenable by calculating Z Score
#let's go z scores by year
mean_zs = hpvalue_m %>% group_by(Year) %>% summarise(mean_hitter_value = mean(total_batter_value),
                                                     mean_pitcher_value = mean(total_pitcher_value),
                                                     mean_total_value = mean(total_value_PB),
                                                     sd_hitter_value = sd(total_batter_value),
                                                     sd_pitcher_value = sd(total_pitcher_value),
                                                     sd_total_value = sd(total_value_PB))
#join by year
hpvalue_m_join = hpvalue_m %>% inner_join(mean_zs,"Year")
#so now we can z score across year
hp_value_m_z = hpvalue_m_join %>% 
  mutate(hitter_z_score = ((total_batter_value-mean_hitter_value)/(sd_hitter_value))) %>%
  mutate(pitcher_z_score = ((total_pitcher_value-mean_pitcher_value)/(sd_pitcher_value))) %>%
  mutate(total_z_score = ((total_value_PB-mean_total_value)/(sd_total_value)))

#now scale it down for readability
hp_value_m_z_slim = hp_value_m_z %>% select(Org, Year,hitter_z_score,pitcher_z_score,total_z_score)
#write it for the GitHub
write_csv(hp_value_m_z_slim,"Org_Z_Scores_By_Year.csv")

#make a summary
hp_value_m_z_slim_org = hp_value_m_z_slim %>% group_by(Org) %>% summarise(Total_Zs = sum(total_z_score))

#make a GG plot
#first, change up the team abbrevs to match
#in the other example this was done automatically
#now, we need to do call it manually
hp_value_m_z_slim_org = hp_value_m_z_slim_org %>% mutate(Org = clean_team_abbrs(Org))

#next, let's create the plot
table_plot=hp_value_m_z_slim_org %>% ggplot(aes(x=Org,y=Total_Zs)) + #first, set table_plot as the ggplot object with X and Y values
  geom_col(aes (fill = Org,color=Org))+ # tell it fill and color it by Org
  scale_color_mlb(type = "secondary") + #and then say that the color should match the mlbplotr color
  scale_fill_mlb(alpha = 0.75) + #and then say that the fill should match the mlbplotr color, with a transparency of 0.75
  theme_bw() + #from ggthemes, apply the bw theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size=9)) + # make the text slightly angled and offset
  labs( #give it some labels
    x = "Organization",
    y = "Sum of Farm System Z Scores (2019 - 2024)"
  )
table_plot
#need to label the values! 

#build a fun little table
table_gt = hp_value_m_z_slim_org %>% arrange(desc(Total_Zs)) %>% gt() %>% tab_header(
  title = "Total Prospect Z Scores") %>%
  tab_footnote("From Fangraphs Prospect Rankings, 2019 - 2024") %>%
  fmt_number(columns = 'Total_Zs',
             decimals = 1) %>%
  cols_label(Total_Zs = "Sum of Z Scores",
             Org = "Organization")  %>% opt_stylize(style = 6, color = "cyan") %>%
  gt_fmt_mlb_scoreboard_logo(columns = "Org")
table_gt


#gtsave(table_gt,"Total_Prospect_Zscores.png", expand = 100) # add some extra room


# for this week, let's just show points
point_plot = hp_value_m_z_slim %>% mutate(ID = paste(Org,Year,sep = "_")) %>%
  ggplot(aes(x  = hitter_z_score, y = pitcher_z_score)) +
  geom_point() + theme_bw() + labs ( x = 'Hitter Z Score', y = 'Pitcher Z Score') + 
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10), limits = c(-2,4)) + #All you have to do is insert the number of ticks wanted for n.
  scale_y_continuous(breaks = scales::breaks_pretty(n = 10), limits = c(-2,4)) +
  geom_vline(color = "cyan", linewidth = .5, linetype = 2, xintercept = 0) + 
  geom_hline(color = "cyan", linewidth = .5, linetype = 2, yintercept = 0)

point_plot_smooth = hp_value_m_z_slim %>% mutate(ID = paste(Org,Year,sep = "_")) %>%
  ggplot(aes(x  = hitter_z_score, y = pitcher_z_score)) +
  geom_point() + theme_bw() + labs ( x = 'Hitter Z Score', y = 'Pitcher Z Score') + 
  geom_smooth() + 
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10), limits = c(-2,4)) + #All you have to do is insert the number of ticks wanted for n.
  scale_y_continuous(breaks = scales::breaks_pretty(n = 10), limits = c(-2,4)) +
  geom_vline(color = "cyan", linewidth = .5, linetype = 2, xintercept = 0) + 
  geom_hline(color = "cyan", linewidth = .5, linetype = 2, yintercept = 0)

point_plot_alph = hp_value_m_z_slim %>% mutate(ID = paste(Org,Year,sep = "_")) %>%
  ggplot(aes(x  = total_z_score, y = forcats::fct_reorder(Org,desc(Org)))) +
  geom_point(aes()) + theme_bw() + labs ( x = 'Total Z Scores (By Season)', y = 'Organization') +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10), limits = c(-2,4)) +
  geom_vline(color = "black", linewidth = .5, linetype = 2, xintercept = 0)


point_plot_alph_window = hp_value_m_z_slim %>% mutate(ID = paste(Org,Year,sep = "_")) %>%
  ggplot(aes(x  = total_z_score, y = forcats::fct_reorder(Org,desc(Org)))) +
  geom_point(aes()) + theme_bw() + labs ( x = 'Organization Total Z Scores (By Season)', y = 'Organization') +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10), limits = c(-2,4)) +
  geom_vline(color = "black", linewidth = .5, linetype = 2, xintercept = 0) +
  annotate("rect", xmin = -1, xmax = 1, 
           ymin = 'WSN', ymax = 'ARI', fill= 'chocolate3', alpha = .5,vjust = 1)


point_plot
point_plot_smooth
point_plot_alph
point_plot_alph_window
