# Luke Nigro
#
# This script analyzes several aspects of fitness activity data.
# These data are my own activities, collected from 4/28/2019 til May 2023.
# I have ownership of this data, therefore they can easily be used by me as an 
# example of my skills in R. 
#
# Data were collected with a Garmin Forerunner 35 watch. The watch has GPS 
# tracking and an optical wrist-mounted heart rate monitor. 
# The spreadsheet includes data such as date, total time, distance, average pace 
# (or speed), min/max elevation, elevation gain/loss, fastest split, etc. 
#
# This is messy data. Sometimes the HR monitor mfunctions, the GPS malcutions,
# and Garmin often overestimates elevation gain for certain locations/routes.
# I do not have any data for some of the variables that Garmin trackes, such 
# as power data from a bike power meter. It also includes large periods before 
# and after moving from Newark, Delaware (sea level) to Boulder, Colorado 
# (high-altitude).
#
# Activities include: running, cycling, "cardio", walking, and skiing.
#
# Running: Pretty self-explanatory. This includes runs on any type of terrain 
# (paved, track, trails of variable technicality). 
# 
# Cycling: Pretty self-explanatory. This includes bike rides on any type of 
# terrain. 
#
# The rest: I will not use these activities.  

################################################################################
################################################################################
################################################################################

# import libraries
library(tidyverse)
library(gapminder)
library(lubridate)
library(car)

# import and clean data

df = read.csv("C:/Users/luken/Documents/R projects/Fitness Data project/Fitness data.csv")

# First, let's just see what's in the dataset 

unique(df$Activity.Type)
table(df$Activity.Type)

# 9 different types of activities, of which I'll only use Running, Cycling, and 
# "Cardio", as described above. 
# Let's clear out those other activities right off the bat.

df = df %>% 
  filter(Activity.Type=="Running" | Activity.Type=="Cycling")

table(df$Activity.Type)

# Now, we filtered it down to 336 bike rides and 537 runs. 

n = 100

df %>% 
  ggplot()+
  geom_point(aes(Date,Distance,color=Activity.Type))+
  labs(title = "All Activites by Date",
       x = "Date",
       y = "Distance (mi)")+
  scale_x_discrete(breaks = df$Date[seq(1, nrow(df), by = n)],
                     labels = df$Date[seq(1, nrow(df), by = n)]) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  theme_bw()

# Clear out bad HR data - if the HR monitor malfucntions, it will display a 
# constant value between 70-90bpm. I'll delete any activities with a max HR less
# than 100bpm. 

df = df %>% 
  filter(Max.HR>100)

table(df$Activity.Type)

# That only got rid of a handful of cycling activities, but no runs.

# I'll also clear out any data with over-estimated elevation gain. This only
# seems to happen in mountainous areas with long, consistent ascents. However, 
# I can't just get rid of all runs or bike rides over an absolute threshold of 
# total ascent, because longer activities will almost always have more ascent. 
# I'll do it based on a feet-of-ascent-per-mile metric, with running and cycling
# having different thresholds. Just based on my own experience, a run usually
# will not have more than 300ft/mi overall, and a bike ride will not have more
# than 150ft/mi overall. There are also activities with no ascent data ("--") 
# that we need to delete.

df = df %>% 
  filter(!Total.Ascent == "--")

# We have to create a new variable for ascent per mile, but Total. Ascent is a 
# "character" type so I have to convert it to numeric. Distance is already 
# numeric. However, lots of other variables are "characters" or "integers" that 
# you would expect to be "numerical" so I need to convert a lot.

class(df$Total.Ascent)
class(df$Distance)

# Values over 1,000 have commas (hence the data type being a "character") so I
# need to get rid of them before I can convert to numeric 

df$Total.Ascent = gsub(",", "", df$Total.Ascent, fixed = TRUE)
df$Total.Descent = gsub(",", "", df$Total.Descent, fixed = TRUE)
df$Min.Elevation = gsub(",", "", df$Min.Elevation, fixed = TRUE)
df$Max.Elevation = gsub(",", "", df$Max.Elevation, fixed = TRUE)
df$Calories = gsub(",", "", df$Calories, fixed = TRUE)

# Avg.Pace is listed in min per mile for running activities, but mph for
# cycling activities. Let's create two new variables with just min/mi and mph
# separately for all activities. The best way to do this is to break up the 
# dataset into two separate ones (just running and just cycling), do the
# conversions, then merge them back together. 

running = df %>% 
  filter(Activity.Type == "Running")
running$Avg.Pace.spm = period_to_seconds((ms(running$Avg.Pace)))
running$Avg.Pace.mph = 3600/running$Avg.Pace.spm

cycling = df %>% 
  filter(Activity.Type == "Cycling")
cycling$Avg.Pace.mph = as.numeric(cycling$Avg.Pace)
cycling$Avg.Pace.spm = 1/(cycling$Avg.Pace.mph/3600)

df = rbind(running,cycling)
df = arrange(df,desc(df$Date))

# A handful of activities were done in places that are actually below sea level
# (beach towns), so Min.Elevation might be negative. This is recorded as "--", 
# which I will correct to 0 since I don't have any values for that.

df$Min.Elevation = gsub("--", 0, df$Min.Elevation, fixed = TRUE)

# Now we can convert 

df$Activity.Type = factor(df$Activity.Type)
df$Total.Ascent = as.numeric(df$Total.Ascent)
df$Total.Descent = as.numeric(df$Total.Descent)
df$Min.Elevation = as.numeric(df$Min.Elevation)
df$Max.Elevation = as.numeric(df$Max.Elevation)
df$Avg.HR = as.numeric(df$Avg.HR)
df$Calories = as.numeric(df$Calories)
df$Time_seconds = period_to_seconds((hms(df$Time)))
df$Moving.Time = period_to_seconds((hms(df$Moving.Time)))

df$ascent_per_mi = df$Total.Ascent/df$Distance

df = df %>% 
  filter((Activity.Type == "Running" & ascent_per_mi < 300) | 
  (Activity.Type == "Cycling" & ascent_per_mi < 150))

# Get rid of one more wonky activity that is likely a hike mis-categorized as a 
# run. It's so slow that it must be walking. 

df = df %>% 
  filter(Avg.Pace.mph > 2)

table(df$Activity.Type)

# Now we're down to 297 bike rides and 519 runs, but I know that nearly all of
# them have good data.

# Next, I'll classify the activities as high or low altitude.

df$high_alt = if_else(df$Min.Elevation > 5000, 1,0)
df$high_alt = as.factor(df$high_alt)


# End of data wrangling
################################################################################
################################################################################
################################################################################

# Basic two-way ANOVA to see if my average heart rate is higher at altitude for 
# a given running pace or cycling speed

df %>% 
  filter(Activity.Type == "Cycling") %>% 
  ggplot(aes(Avg.Pace.mph,Avg.HR,color=high_alt))+
  geom_point(size=3,alpha=0.8)+
  labs(title = "Cycling Activities by High Altitude",
       x = "Speed (mph)",
       y = "HR (bpm)")+
  geom_smooth()+
  scale_x_continuous(breaks = c(0,5,10,15,20)) +
  scale_y_continuous(breaks = c(60,80,100,120,140,160)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_bw()

model = aov(Avg.HR ~ high_alt * Avg.Pace.mph, df[df$Activity.Type == "Cycling",])
summary(model)

# Based on the output of this model, there is no significant difference in avg.
# HR with altitude, there is a significant difference in HR with avg. speed,
# and there could potentially be a weak interaction between altitude and speed
# that affect avg HR. 

# This is surprising! I expected that HR would be affected by altitude, but it
# only seems to be affected by speed. This may be an indication that cycling 
# speed is more affected by environmental conditions like wind and incline/decline
# gradient. These data are also confounded by the fact that some activities are
# mountain bike activities, which can be much slower due to trail conditions &
# technicality, while still being just as taxing HR-wise. 



# Now let's try the same thing with running activities. 

# Plotting

df %>% 
  filter(Activity.Type == "Running") %>% 
  ggplot(aes(Avg.Pace.spm,Avg.HR,color=high_alt))+
  geom_point(size=3,alpha=0.8)+
  labs(title = "Running Activities by High Altitude",
       x = "Pace (seconds per mile)",
       y = "HR (bpm)")+
  geom_smooth()+
  scale_x_continuous(breaks = c(100,200,300,400,500,600,1100,1200)) +
  scale_y_continuous(breaks = c(60,80,100,120,140,160)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_bw()

# Modelling

model = aov(Avg.HR ~ high_alt * Avg.Pace.spm, df[df$Activity.Type == "Running",])
summary(model)

# Based on the output of this model, there is no significant difference in HR 
# with altitude OR with pace on their own, but there is a very strong 
# interaction between altitude and pace that affects HR. 

# These results are surprising as well, but in a different way compared to the
# cycling model. I would have expected both altitude and pace to affect HR, but
# only the interaction between the two significantly affects HR for running
# activities. 

################################################################################

# Next, let's look at how total distance and Calories compare to each other

plot = df %>% 
  filter(Activity.Type == "Cycling") %>% 
  ggplot(aes(Distance,Calories,color=high_alt))+
  geom_point(size=3,alpha=0.8)+
  geom_smooth(data = . %>% filter(high_alt == 0), method = "lm", se = FALSE, color = "red") +
  geom_smooth(data = . %>% filter(high_alt == 1), method = "lm", se = FALSE, color = "blue") +
  labs(title = "Calories burned vs Distance by Altitude (Cycling)")

model0 = lm(Calories ~ Distance, df[df$Activity.Type == "Cycling" & df$high_alt == 0,])
model1 = lm(Calories ~ Distance, df[df$Activity.Type == "Cycling" & df$high_alt == 1,])

intercept0 = coef(model0)[1]
slope0 = coef(model0)[2]
intercept1 = coef(model1)[1]
slope1 = coef(model1)[2]

plot + annotate(
  "text",
  x = max(df$Distance) * 0.7,
  y = max(df$Calories) * 0.7,
  label = paste0("y = ", round(slope0, 2), "x + ", round(intercept0, 2)),
  color = "red",
  size = 5,
  hjust = 0
) + 

annotate(
  "text",
  x = max(df$Distance) * 0.4,
  y = max(df$Calories) * 0.85,
  label = paste0("y = ", round(slope1, 2), "x + ", round(intercept1, 2)),
  color = "blue",
  size = 5,
  hjust = 0
)

# It appears that I burn Calories at a faster rate at altitude than at sea level.
# This makes sense!

# I'll compare these two models to see if they are significantly different, but
# first I need to make them the same number of data points. I'll do that by
# sampling with replacement (bootstrapping) the samller data set until I have
# the same number of points as the larger dataset. 

n = nrow(df[df$Activity.Type == "Running" & df$high_alt == 0,])

bootstrapped_data = vector("list",n)

for (i in 1:n) {
  bootstrap_indices <- sample(1:length(df[df$Activity.Type == "Running" & df$high_alt == 0,]), replace = TRUE)
  bootstrapped_sample <- df[df$Activity.Type == "Running" & df$high_alt == 0,][bootstrap_indices]
  bootstrapped_data[[i]] <- bootstrapped_sample
}

View(bootstrapped_data) #????????????????????????????????????????????????????????????????????????????????????????????????????????????????????/

# Now we'll do the same thing for running

plot = df %>% 
  filter(Activity.Type == "Running") %>% 
  ggplot(aes(Distance,Calories,color=high_alt))+
  geom_point(size=3,alpha=0.8)+
  geom_smooth(data = . %>% filter(high_alt == 0), method = "lm", se = FALSE, color = "red") +
  geom_smooth(data = . %>% filter(high_alt == 1), method = "lm", se = FALSE, color = "blue") +
  labs(title = "Calories burned vs Distance by Altitude (Running)")

model0 = lm(Calories ~ Distance, df[df$Activity.Type == "Running" & df$high_alt == 0,])
model1 = lm(Calories ~ Distance, df[df$Activity.Type == "Running" & df$high_alt == 1,])

intercept0 = coef(model0)[1]
slope0 = coef(model0)[2]
intercept1 = coef(model1)[1]
slope1 = coef(model1)[2]

plot + annotate(
  "text",
  x = 10,
  y = 600,
  label = paste0("y = ", round(slope0, 2), "x + ", round(intercept0, 2)),
  color = "red",
  size = 5,
  hjust = 0
) + 
  
  annotate(
    "text",
    x = 5,
    y = 1000,
    label = paste0("y = ", round(slope1, 2), "x + ", round(intercept1, 2)),
    color = "blue",
    size = 5,
    hjust = 0
  )

# It appears that I burn Calories at a faster rate at altitude than at sea level.
# This makes sense!




