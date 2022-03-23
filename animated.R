
#################################################################
##      Data Visualization: Gender Gap in Weightlifiting       ##
##                    Author: Leah Nguyen                      ##
#################################################################

#################################################################
##                         Task 1 - EDA                        ##
#################################################################
##----------------------------------------------------------------
##  Load the Libraries                                          --
##----------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(ggtext)
library(gifski)
library(gganimate)
library(cr)

theme_set(theme_minimal())


##----------------------------------------------------------------
##  Dataset Overview                                            --
##----------------------------------------------------------------
# Load the dataset
df <- read_csv(here("data/lift.csv")) %>%
  mutate(year = year(date))

##---------------------------------------------------------------
##  Data Transformation                                        --
##---------------------------------------------------------------
# Reshape the data
data <- df %>%
  # Reshape the three lifts into one column
  pivot_longer(
    # specify 3 columns need to merge into 1
    cols = c("best3squat_kg", "best3bench_kg", "best3deadlift_kg"), 
    # create new combined column as "lift 
    names_to = "lift") 


# Select top heaviest lifts for each year
max_lift <- data %>%
  # group the df by necessary values
  group_by(year, sex, lift) %>%
  # select top N=1 highest value by group
  top_n(1, value) %>% 
  ungroup %>%
  distinct(year, lift, value, .keep_all = TRUE)

# Split sex column into 2 new columns with each gender lifting record
max_pivot <- max_lift %>%
  spread(sex, value)

# Construct a dataframe for each gender
male_lifts <- max_pivot %>%
  # remove unnecessary column
  select(-name) %>%
  # subset rows where not contain null value in Male column  
  filter(!is.na(M)) %>%
  group_by(year, lift) %>%
  # calculate averge lift
  summarise(male = mean(M))

female_lifts <- max_pivot %>%
  # remove unnecessary column
  select(-name) %>%
  # subset rows where not contain null value in Female column
  filter(!is.na(F)) %>%
  group_by(year, lift) %>%
  # calculate averge lift
  summarise(female = mean(F))

# Merge them together
total_max_lift <- merge(male_lifts, female_lifts) %>%
  group_by(year, lift) %>% 
  mutate(diff = male - female)

##---------------------------------------------------------------
##  Data Visualization                                         --
##---------------------------------------------------------------
# Weightlifting Gender Gap Overtime
animation <- total_max_lift %>%
  ggplot() +
  # plot dumbbell plot
  ggalt::geom_dumbbell(aes(y = lift,
                           x = female, xend = male),
                       colour = "grey", size = 5,
                       colour_x = "#D6604C", colour_xend = "#395B74") +
  labs(y = element_blank(),
       # add y label
       x = "Top Lift Recorded (kg)",
       # add visual title
       title =  "How <span style='color:#D6604C'>Women</span> and <span style='color:#395B74'>Men</span> Differ in Top Lifts",
       # display desciption as subtitle for storytelling
       subtitle='\nThis plot depicts the difference between the heaviest lifts for each sex at International Powerlifting Federation\nevents over time. \n \n{closest_state}') +
  theme(plot.title = element_markdown(lineheight = 1.1, size = 25, margin=margin(0,0,0,0)),
        plot.subtitle = element_text(size = 15, margin=margin(8,0,-30,0))) +
  scale_y_discrete(labels = c("Bench", "Deadlift", "Squat")) +
  geom_text(aes(x = female, y = lift, label = paste(female, "kg")),
            color = "#D6604C", size = 4, vjust = -2) +
  geom_text(aes(x = male, y = lift, label = paste(male, "kg")),
            color = "#395B74", size = 4, vjust = -2) +
  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

# animate the rendered plot
a_gif <- animate(animation,
                 fps = 10,
                 duration = 25,
                 width = 800, height = 400,
                 renderer = gifski_renderer("outputs/animation.gif"))

a_gif


# Differences in Weightlifting Gender Gap Overtime
animation2 <- total_max_lift %>%
  ungroup %>%
  mutate(lift = case_when(lift == "best3bench_kg" ~ "Bench",
                          lift == "best3squat_kg" ~ "Squat",
                          lift == "best3deadlift_kg" ~ "Deadlift")) %>%
  ggplot(aes(year, diff, group = lift, color = lift)) +
  geom_line(show.legend = FALSE) +
  geom_segment(aes(xend = 2019.1, yend = diff), linetype = 2, colour = 'grey', show.legend = FALSE) +
  geom_point(size = 2, show.legend = FALSE) +
  geom_text(aes(x = 2019.1, label = lift, color = "#000000"), hjust = 0, show.legend = FALSE) +
  transition_reveal(year) +
  coord_cartesian(clip = 'off') +
  theme(plot.title = element_text(size = 20)) +
  labs(title = 'Difference over time',
       y = 'Difference (kg)',
       x = element_blank()) +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))
animation2

# animate the rendered plot
b_gif <- animate(animation2,
                 fps = 10,
                 duration = 25,
                 width = 800, height = 200,
                 renderer = gifski_renderer("outputs/animation2.gif"))

b_gif

# combine 2 animated plots tgt
library(magick)
a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = TRUE)
for(i in 2:250){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = TRUE)
  new_gif <- c(new_gif, combined)
}
