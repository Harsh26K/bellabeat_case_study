# install required packages

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("gghighlight")
install.packages("dplyr")
install.packages("ggcorrplot")

# load installed packages

library(tidyverse)
library(ggplot2)
library(gghighlight)
library(dplyr)
library(ggcorrplot)

# read required datasets

# data about summarization of daily activity of users using pivot table
DailyActivity <- read_csv('summerization_DailyActivity.csv')
head(DailyActivity)

# data about summarization of hourly avtivity of users using pivot table
HourlyActivity <- read_csv('summerization_HourlyActivity.csv')
head(HourlyActivity)

# the data taken above is preprocessed using google sheets, so we will directly 
# analyze the data

# let's look at the data at more depth using summary()/ summarize() functions.

DailyActivity %>% summarize(MinStepsOverall = min(AvgSteps),MinDistanceOverall = min(`AvgDistanceKm`),
                  MinHrsOverall = min(`AvgHrs.`),MinCaloriesBurntOverall= min(`TotalCaloriesKCal`))
DailyActivity %>% summarize(AvgStepsOverall = mean(AvgSteps),AvgDistanceOverallKm = mean(`AvgDistanceKm`),
                  AvgHrsOverall = mean(`AvgHrs.`),AvgCaloriesBurnt = mean(`TotalCaloriesKCal`))
DailyActivity %>% summarize(MaxStepsOverall = max(AvgSteps),MaxDistanceOverall = max(`AvgDistanceKm`),
                  MaxHrsOverall = max(`AvgHrs.`), MaxCaloriesBurntOverall = max(`TotalCaloriesKCal`))


DailyActivity %>% select(AvgSteps, AvgHrs., TotalCaloriesKCal) %>% summary()
DailyActivity %>% select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes,SedentaryMinutes) %>% summary() 

HourlyActivity %>% summary()


# let's try to visualize to get better understanding

# first check the correlation between the features using heatmap() and 
# ggcorrplot()

data <- HourlyActivity %>%
  select(AvgSteps, AvgIntensity, AvgCalories) %>%
  as.matrix()

print(data)

heatmap(data,scale = "column")
data2 <- round(cor(data, method="pearson"),2)
heatmap(data2, Colv=NA, Rowv=NA)
ggcorrplot(data2, lab=TRUE)

# let's plot scatter plot and area plot for activityHour Vs Intensity

ggplot(data = HourlyActivity, aes(x= ActivityHour, y= AvgIntensity)) + geom_point() + geom_smooth()

ggplot(data = HourlyActivity, aes(x=ActivityHour, y= AvgIntensity)) + 
  geom_area(fill = "blue", alpha = 0.4) +
  labs(title="Avg. Intensity Each Hour", subtitle = "Intensity recorded for sample of 30 each hour") + 
  gghighlight(AvgIntensity > 15) 

# scatter plot for ActivityHour Vs Calories

ggplot(data = HourlyActivity, aes(x=ActivityHour, y= AvgCalories)) + 
  geom_point()

# line plots between ActivityHour Vs (Intensity, Steps, Calories)

ggplot(data = HourlyActivity,aes(x=ActivityHour, y=AvgIntensity)) + 
  geom_line(color = "red")

ggplot(data = HourlyActivity,aes(x=ActivityHour, y=AvgSteps)) + 
  geom_line(color = "blue") 

ggplot(data = HourlyActivity,aes(x=ActivityHour, y=AvgCalories)) + 
  geom_line(color = "green") + 
  scale_y_continuous(expand=c(0,0), limits=c(0,125))

# line plots between Intensity, Steps and Calories

ggplot(data = HourlyActivity , aes(x=AvgSteps , y=AvgIntensity)) + 
  geom_line() + 
  labs(title = "AvgSteps Vs AvgIntensity")
ggplot(data = HourlyActivity, aes(x=AvgSteps, y = AvgCalories)) + 
  geom_line() + 
  labs(title = "AvgSteps Vs AvgCaloriesBurnt")
ggplot(data = HourlyActivity, aes(x=AvgIntensity,y=AvgCalories)) + 
         geom_line() + 
  labs(title = "AvgIntensity Vs AvgCaloriesBurnt")


