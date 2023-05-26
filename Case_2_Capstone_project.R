#Case Study 2: How Can a Wellness TechnologyCompany Play It Smart?

#Bellabeat, a high-tech manufacturer of health-focused
#products for women. Bellabeat is a successful small company, but they have the potential to become a larger player in the
#global smart device market.

# Products
#○ Bellabeat app: The Bellabeat app provides users with health data related to their activity, sleep, stress,
#menstrual cycle, and mindfulness habits. This data can help users better understand their current habits and
#make healthy decisions. The Bellabeat app connects to their line of smart wellness products.
#○ Leaf: Bellabeat’s classic wellness tracker can be worn as a bracelet, necklace, or clip. The Leaf tracker connects
#to the Bellabeat app to track activity, sleep, and stress.
#○ Time: This wellness watch combines the timeless look of a classic timepiece with smart technology to track user
#activity, sleep, and stress. The Time watch connects to the Bellabeat app to provide you with insights into your
#daily wellness.
#○ Spring: This is a water bottle that tracks daily water intake using smart technology to ensure that you are
#appropriately hydrated throughout the day. The Spring bottle connects to the Bellabeat app to track your
#hydration levels.
#○ Bellabeat membership: Bellabeat also offers a subscription-based membership program for users.
# Membership gives users 24/7 access to fully personalized guidance on nutrition, activity, sleep, health and
# beauty, and mindfulness based on their lifestyle and goals.


#Sršen, Bellabeat’s cofounder and Chief Creative Officer, knows that an analysis of Bellabeat’s available consumer data would reveal more opportunities for growth. She has
#asked the marketing analytics team to focus on a Bellabeat product and analyze smart device usage data in order to gain
#insight into how people are already using their smart devices. Then, using this information, she would like high-level
#recommendations for how these trends can inform Bellabeat marketing strategy.

#Sršen asks you to analyze smart device usage data in order to gain insight into how consumers use non-Bellabeat smart
#devices. She then wants you to select one Bellabeat product to apply these insights to in your presentation. 

# These questions will guide your analysis:
#   
#   1.	What are some trends in smart device usage?
#   2. How could these trends apply to Bellabeat customers?
#   3. How could these trends help influence Bellabeat marketing strategy
# 
# Sršen encourages you to use public data that explores smart device users’ daily habits. She points you to a specific data set:
#   ● FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius): This Kaggle data set
# contains personal fitness tracker from thirty fitbit users. Thirty eligible Fitbit users consented to the submission of
# personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. It includes
# information about daily activity, steps, and heart rate that can be used to explore users’ habits.
# Sršen tells you that this data set might have some limitations, and encourages you to consider adding another data to help
# address those limitations as you begin to work more with this data.

#Installing packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("dplyr")
install.packages("gridExtra")
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(gridExtra)

#Importing csv files from Fitabase Data
#Importing dailyActivity_merged.csv
dailyActivity_merged <- read_csv("C:\\Users\\y.v.kucherenko\\Downloads\\archive(1)\\Fitabase Data 4.12.16-5.12.16\\dailyActivity_merged.csv")
glimpse(dailyActivity_merged)
#Formating date in ActivityDate
dailyActivity_merged$ActivityDate <- as.Date(dailyActivity_merged$ActivityDate, "%m/%d/%Y")
#Looking for duplicates in dailyActivity_merged
duplicated(dailyActivity_merged)
sum(duplicated(dailyActivity_merged))
#Looking for N/A data in dailyActivity_merged
is.na(dailyActivity_merged)
sum(is.na(dailyActivity_merged))

View(dailyActivity_merged)

#Sorting data in dailyActivity_merged by Date in ascending order
dailyActivity_time_sorted <- dailyActivity_merged %>%
  arrange(ActivityDate)
View(dailyActivity_time_sorted)
colnames(dailyActivity_time_sorted)
#Saving weightLog
#write.csv(dailyActivity_time_sorted, "C:\\Users\\y.v.kucherenko\\Documents\\R_projects\\dailyActivity_time_sorted_Google_capstone_project.csv")

#plotting dailyActivity_time_sorted (TotalSteps vs. Calories)
#plotting dailyActivity_time_sorted (TotalDistance vs. Calories)
#plotting dailyActivity_time_sorted (SedentaryMinutes vs. Calories) 
plot_1 <- ggplot(data = dailyActivity_time_sorted) + geom_point(mapping = aes(x= TotalSteps, y = Calories), color = "blue") + stat_smooth(mapping = aes(x= TotalSteps, y = Calories, color = "black"), data = dailyActivity_time_sorted, geom = "smooth", position = "identity",method = "lm", formula = y ~ x) + labs(title = "Total Steps vs. Calories") +  scale_colour_identity()
plot_2 <- ggplot(data = dailyActivity_time_sorted) + geom_point(mapping = aes(x= TotalDistance, y = Calories), color = "green") + stat_smooth(mapping = aes(x= TotalDistance, y = Calories, color = "black"), data = dailyActivity_time_sorted, geom = "smooth", position = "identity",method = "lm", formula = y ~ x) + labs(title = "Total Distance vs. Calories") + scale_colour_identity()
grid.arrange(plot_1, plot_2, ncol=2, widths=c(1,1), nrow=1)

#plotting dailyActivity_time_sorted (SedentaryMinutes vs. Calories) 
plt_1 <- ggplot(data = dailyActivity_time_sorted) + geom_point(mapping = aes(x= SedentaryMinutes/60, y = Calories), color = "purple") + stat_smooth(mapping = aes(x= SedentaryMinutes/60, y = Calories, color = "black"), data = dailyActivity_time_sorted, geom = "smooth", position = "identity",method = "lm", formula = y ~ x) + labs(title = "Sedentary Hours vs. Calories") + scale_colour_identity()
plt_2 <- ggplot(data = dailyActivity_time_sorted) + geom_point(mapping = aes(x= VeryActiveMinutes/60, y = Calories), color = "green") + stat_smooth(mapping = aes(x= VeryActiveMinutes/60, y = Calories, color = "black"), data = dailyActivity_time_sorted, geom = "smooth", position = "identity",method = "lm", formula = y ~ x) + labs(title = "VeryActive Hours vs. Calories") + scale_colour_identity()
plt_3 <- ggplot(data = dailyActivity_time_sorted) + geom_point(mapping = aes(x= FairlyActiveMinutes/60, y = Calories), color = "blue") + stat_smooth(mapping = aes(x= FairlyActiveMinutes/60, y = Calories, color = "black"), data = dailyActivity_time_sorted, geom = "smooth", position = "identity",method = "lm", formula = y ~ x) + labs(title = "FairlyActive Hours vs. Calories") + scale_colour_identity()
plt_4 <- ggplot(data = dailyActivity_time_sorted) + geom_point(mapping = aes(x= LightlyActiveMinutes/60, y = Calories), color = "yellow") + stat_smooth(mapping = aes(x= LightlyActiveMinutes/60, y = Calories, color = "black"), data = dailyActivity_time_sorted, geom = "smooth", position = "identity",method = "lm", formula = y ~ x) + labs(title = "LightlyActive Hours vs. Calories") + scale_colour_identity()
grid.arrange(plt_2, plt_3, ncol=2, widths=c(1,1), plt_4, plt_1, nrow=2)


#plotting dailyActivity_time_sorted (VeryActiveDistance vs. VeryActiveMinutes)
ggplot(data = dailyActivity_time_sorted) + geom_point(mapping = aes(x= VeryActiveDistance, y = VeryActiveMinutes, color = "blue", size = 0, shape = "square")) + 
  geom_point(mapping = aes(x= ModeratelyActiveDistance, y = FairlyActiveMinutes, color = "green", size = 0, shape = "circle")) +
  geom_point(mapping = aes(x= LightActiveDistance, y = LightlyActiveMinutes, color = "yellow", size = 0, shape = "diamond")) +
  geom_point(mapping = aes(x= SedentaryActiveDistance, y = SedentaryMinutes, color = "red", size = 0, shape = "triangle")) +
  scale_colour_identity() + 
  labs(title = "ACTIVITY DISTANCE vs. ACTIVITY TIME", subtitle = "VERY ACTIVE is shown in BLUE,\nMODERATELY ACTIVE is shown in GREEN, \nLIGHTLY ACTIVE is shown in YELLOW,\nSEDENTARY ACTIVE is shown in RED", x = "DISTANCE", y = "TIME, minutes")


plot_4 <- ggplot(data = dailyActivity_time_sorted) + geom_point(mapping = aes(x= VeryActiveDistance, y = VeryActiveMinutes / 60, color = "red")) + scale_colour_identity() + labs(title = "Very Active Distance\n vs. Very Active Hours") 
plot_5 <- ggplot(data = dailyActivity_time_sorted) + geom_point(mapping = aes(x= ModeratelyActiveDistance, y = FairlyActiveMinutes / 60, color = "blue")) + scale_colour_identity() + labs(title = "Moderately Active Distance\n vs. Fairly Active Hours") 
plot_6 <- ggplot(data = dailyActivity_time_sorted) + geom_point(mapping = aes(x= LightActiveDistance, y = LightlyActiveMinutes / 60, color = "green")) + scale_colour_identity() + labs(title = "Light Active Distance\n vs. Lightly Active Hours") 
plot_7 <- ggplot(data = dailyActivity_time_sorted) + geom_point(mapping = aes(x= SedentaryActiveDistance, y = SedentaryMinutes / 60)) + scale_colour_identity() + labs(title = "Sedentary Active Distance\n vs. Sedentary Hours") 

grid.arrange(plot_4, plot_5, ncol=2, widths=c(1,1), plot_6, plot_7, nrow=2) 
  
par(mfrow = c(2, 2))
plot(dailyActivity_time_sorted$VeryActiveDistance, dailyActivity_time_sorted$VeryActiveMinutes, xlab = "VeryActiveDistance", ylab = "VeryActiveMinutes")
plot(dailyActivity_time_sorted$ModeratelyActiveDistance, dailyActivity_time_sorted$FairlyActiveMinutes, xlab = "ModeratelyActiveDistance", ylab = "FairlyActiveMinutes")
plot(dailyActivity_time_sorted$LightActiveDistance, dailyActivity_time_sorted$LightlyActiveMinutes, xlab = "LightActiveDistance", ylab = "LightlyActiveMinutes")
plot(dailyActivity_time_sorted$SedentaryActiveDistance, dailyActivity_time_sorted$SedentaryMinutes, xlab = "SedentaryActiveDistance", ylab = "SedentaryMinutes")


#Calculating mean and sd in VeryActiveDistance, ModeratelyActiveDistance and lightActiveDistance
veryActiveDistance <- dailyActivity_time_sorted %>%
  summarize(mean_VeryActiveDistance = mean(VeryActiveDistance), sd_VeryActiveDistance = sd(VeryActiveDistance))
print(veryActiveDistance)

moderatelyActiveDistance <- dailyActivity_time_sorted %>%
  summarize(mean_ModeratelyActiveDistance = mean(ModeratelyActiveDistance), sd_ModeratelyActiveDistance = sd(ModeratelyActiveDistance))
print(moderatelyActiveDistance)

lightActiveDistance <- dailyActivity_time_sorted %>%
  summarize(mean_LightActiveDistance = mean(LightActiveDistance), sd_LightActiveDistance = sd(LightActiveDistance))
print(lightActiveDistance)

sedentaryActiveDistance <- dailyActivity_time_sorted %>%
  summarize(mean_SedentaryActiveDistance = mean(SedentaryActiveDistance), sd_SedentaryActiveDistance = sd(SedentaryActiveDistance))
print(sedentaryActiveDistance)

# Creating a dataframe for 3 types of activity_Distance
activity_Dictance = data.frame(c(veryActiveDistance[1], moderatelyActiveDistance[1], lightActiveDistance[1], sedentaryActiveDistance[1]), c(veryActiveDistance[2], moderatelyActiveDistance[2], lightActiveDistance[2], sedentaryActiveDistance[2]))

activity_Distance_df = data.frame(distance = c("Very Active", "Fairly Active", "Lightly Active", "Sedentary Active"),
                                  mean = c(activity_Dictance$mean_VeryActiveDistance, activity_Dictance$mean_ModeratelyActiveDistance, activity_Dictance$mean_LightActiveDistance, activity_Dictance$mean_SedentaryActiveDistance),
                                  sd = c(activity_Dictance$sd_VeryActiveDistance, activity_Dictance$sd_ModeratelyActiveDistance, activity_Dictance$sd_LightActiveDistance, activity_Dictance$sd_SedentaryActiveDistance))

View(activity_Distance_df)
total_distance <- sum(activity_Distance_df$mean)
print(total_distance)
total_distance_1 <- mean(dailyActivity_merged$TotalDistance)
print(total_distance_1)
total_steps  <- mean(dailyActivity_merged$TotalSteps)
print(total_steps)

# Creating a bar plot for activity distance
plot_8 <- ggplot(data = activity_Distance_df) +
  geom_bar( aes(x = distance, y = mean, fill = distance, alpha = 0.5), stat = "identity") +
#  geom_errorbar(aes(x = distance, ymin = mean - sd, ymax = mean + sd), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
  labs(title = "AVERAGE DISTANCE FOR \n4 TYPES OF ACTIVITY", x = "TYPE OF ACTIVITY", y = "AVERAGE DISTANCE") +
  theme(axis.text.x = element_text(angle = 30))

#Calculating mean and sd in VeryActiveMinutes, ModeratelyActiveMinutes, lightActiveMinutes and SedentaryMinutes
veryActiveMinutes <- dailyActivity_time_sorted %>%
  summarize(mean_VeryActiveMinutes = mean(VeryActiveMinutes), sd_VeryActiveMinutes = sd(VeryActiveMinutes))
print(veryActiveMinutes)

fairlyActiveMinutes <- dailyActivity_time_sorted %>%
  summarize(mean_FairlyActiveMinutes = mean(FairlyActiveMinutes), sd_FairlyActiveMinutes = sd(FairlyActiveMinutes))
print(fairlyActiveMinutes)

lightlyActiveMinutes <- dailyActivity_time_sorted %>%
  summarize(mean_LightlyActiveMinutes = mean(LightlyActiveMinutes), sd_LightlyActiveMinutes = sd(LightlyActiveMinutes))
print(lightlyActiveMinutes)

sedentaryMinutes <- dailyActivity_time_sorted %>%
  summarize(mean_SedentaryMinutes = mean(SedentaryMinutes), sd_SedentaryMinutes = sd(SedentaryMinutes))
print(sedentaryMinutes)

# Creating a dataframe for 4 types of activity_Minutes
activity_Minutes = data.frame(c(veryActiveMinutes[1], fairlyActiveMinutes[1], lightlyActiveMinutes[1], sedentaryMinutes[1]), c(veryActiveMinutes[2], fairlyActiveMinutes[2], lightlyActiveMinutes[2], sedentaryMinutes[2]))

activity_Minutes_df = data.frame(activity = c("Very Active", "Fairly Active", "Lightly Active", "Sedentary Active"),
                                  mean = c(activity_Minutes$mean_VeryActiveMinutes/60, activity_Minutes$mean_FairlyActiveMinutes/60, activity_Minutes$mean_LightlyActiveMinutes/60, activity_Minutes$mean_SedentaryMinutes/60),
                                  sd = c(activity_Minutes$sd_VeryActiveMinutes/60, activity_Minutes$sd_FairlyActiveMinutes/60, activity_Minutes$sd_LightlyActiveMinutes/60, activity_Minutes$sd_SedentaryMinutes/60))

View(activity_Minutes_df)

# Creating a bar plot for activity distance
plot_9 <- ggplot(data = activity_Minutes_df) +
  geom_bar( aes(x=activity, y=mean, fill=activity, alpha=0.5), stat="identity") +
#  geom_errorbar( aes(x=activity, ymin = mean-sd, ymax=mean+sd), width=0.4, colour="black", alpha=0.9, size=1.3) +
  labs(title = "AVERAGE ACTIVITY (in hours) FOR \n4 TYPES OF ACTIVITY", x = "TYPE OF ACTIVITY", y = "AVERAGE ACTIVITY, hours") +
  theme(axis.text.x = element_text(angle = 30))
grid.arrange(plot_8, plot_9, ncol=2, widths=c(1,1), nrow=1) 

#Importing sleepDay_merged.csv
sleepDay_merged <- read_csv("C:\\Users\\y.v.kucherenko\\Downloads\\archive(1)\\Fitabase Data 4.12.16-5.12.16\\sleepDay_merged.csv")
glimpse(sleepDay_merged)
#Formating date in SleepDay
sleepDay_merged$SleepDay <- as.Date(sleepDay_merged$SleepDay, "%m/%d/%Y")
#Lokking for duplicates in sleepDay_merged
duplicated(sleepDay_merged)
sum(duplicated(sleepDay_merged))
#Removing duplicates
sleepDay <- unique(sleepDay_merged)
sum(duplicated(sleepDay))
#Looking for N/A data in sleepDay
is.na(sleepDay)
sum(is.na(sleepDay))
#Columns names in sleepDay
colnames(sleepDay)
View(sleepDay)
#Counting unique values in sleepDay
counts_sleepRecords <- sleepDay %>%
  count(TotalSleepRecords)
print(counts_sleepRecords)
#Saving weightLog
#write.csv(sleepDay, "C:\\Users\\y.v.kucherenko\\Documents\\R_projects\\sleepDay_Google_capstone_project.csv")

#Calculating min and max TotalSleepRecords
min_Sleep_Records <- sleepDay %>%
  filter(TotalSleepRecords == min(TotalSleepRecords)) 
View(min_Sleep_Records)
min_Sleep_Records %>%
  count(TotalSleepRecords)
print(min_Sleep_Records)

max_Sleep_Records <- sleepDay %>%
  filter(TotalSleepRecords == max(TotalSleepRecords))
print(max_Sleep_Records)
max_Sleep_Records %>%
  count(TotalSleepRecords)

moderate_Sleep_Records <- sleepDay %>%
  filter(TotalSleepRecords != max(TotalSleepRecords) & TotalSleepRecords != min(TotalSleepRecords))
View(moderate_Sleep_Records)
moderate_Sleep_Records %>%
  count(TotalSleepRecords)

#Calculating mean and sd for moderate_Sleep_Records, min_Sleep_Records  and max_Sleep_Records
moderate_TotalHoursAsleep <- moderate_Sleep_Records %>%
  summarize(mean_TotalHoursAsleep = mean(TotalMinutesAsleep)/60, sd_TotalHoursAsleep = sd(TotalMinutesAsleep)/60)
print(moderate_TotalHoursAsleep)

min_TotalHoursAsleep <- min_Sleep_Records %>%
  summarize(mean_TotalHoursAsleep = mean(TotalMinutesAsleep)/60, sd_TotalHoursAsleep = sd(TotalMinutesAsleep)/60)
print(min_TotalHoursAsleep)

max_TotalHoursAsleep <- max_Sleep_Records %>%
  summarize(mean_TotalHoursAsleep = mean(TotalMinutesAsleep)/60, sd_TotalHoursAsleep = sd(TotalMinutesAsleep)/60)
print(max_TotalHoursAsleep)


# Creating a dataframe for 3 types of Total Sleeps
hours_Asleep = data.frame(c(min_TotalHoursAsleep[1], moderate_TotalHoursAsleep[1], max_TotalHoursAsleep[1]), c(min_TotalHoursAsleep[2], moderate_TotalHoursAsleep[2], max_TotalHoursAsleep[2]))

hours_Asleep_df = data.frame(number_sleeps = c("1", "2", "3"),
                                 mean = c(min_TotalHoursAsleep$mean_TotalHoursAsleep, moderate_TotalHoursAsleep$mean_TotalHoursAsleep, max_TotalHoursAsleep$mean_TotalHoursAsleep),
                                 sd = c(min_TotalHoursAsleep$sd_TotalHoursAsleep, moderate_TotalHoursAsleep$sd_TotalHoursAsleep, max_TotalHoursAsleep$sd_TotalHoursAsleep))
View(hours_Asleep_df)

# Creating a bar plot for sleep types
plot_10 <- ggplot(data = hours_Asleep_df) +
  geom_bar( aes(x=number_sleeps, y=mean, fill=number_sleeps, alpha=0.5), stat="identity") +
  geom_errorbar( aes(x=number_sleeps, ymin = mean-sd, ymax=mean+sd), width=0.4, colour="black", alpha=0.9, size=1.3) +
  labs(title = "AVERAGE TIME ASLEEP (hours)\n FOR 3 TYPES OF SLEEP", x = "NUMBER OF SLEEPS PER DAY", y = "AVERAGE SLEEP TIME, hour") + ylim(0, 13)

#Calculating mean and sd for moderate_Sleep_Records, min_Sleep_Records  and max_Sleep_Records
moderate_TotalTimeInBed <- moderate_Sleep_Records %>%
  summarize(mean_TotalTimeInBed = mean(TotalTimeInBed)/60, sd_TotalTimeInBed = sd(TotalTimeInBed)/60)
print(moderate_TotalTimeInBed)

min_TotalTimeInBed <- min_Sleep_Records %>%
  summarize(mean_TotalTimeInBed = mean(TotalTimeInBed)/60, sd_TotalTimeInBed = sd(TotalTimeInBed)/60)
print(min_TotalTimeInBed)

max_TotalTimeInBed <- max_Sleep_Records %>%
  summarize(mean_TotalTimeInBed = mean(TotalTimeInBed)/60, sd_TotalTimeInBed = sd(TotalTimeInBed)/60)
print(max_TotalTimeInBed)


# Creating a dataframe for 3 types of Total Sleeps
hours_InBed = data.frame(c(min_TotalTimeInBed[1], moderate_TotalTimeInBed[1], max_TotalTimeInBed[1]), c(min_TotalTimeInBed[2], moderate_TotalTimeInBed[2], max_TotalTimeInBed[2]))

hours_InBed_df = data.frame(number_sleeps = c("1", "2", "3"),
                               mean = c(min_TotalTimeInBed$mean_TotalTimeInBed, moderate_TotalTimeInBed$mean_TotalTimeInBed, max_TotalTimeInBed$mean_TotalTimeInBed),
                               sd = c(min_TotalTimeInBed$sd_TotalTimeInBed, moderate_TotalTimeInBed$sd_TotalTimeInBed, max_TotalTimeInBed$sd_TotalTimeInBed))
View(hours_InBed_df)

# Creating a bar plot for sleep types
plot_11 <- ggplot(data = hours_InBed_df) +
  geom_bar( aes(x=number_sleeps, y=mean, fill=number_sleeps, alpha=0.5), stat="identity") +
  geom_errorbar( aes(x=number_sleeps, ymin = mean-sd, ymax=mean+sd), width=0.4, colour="black", alpha=0.9, size=1.3) +
  labs(title = "AVERAGE TIME IN BED (hours)\n FOR 3 TYPES OF SLEEP", x = "Number OF SLEEPs PER DAY", y = "AVERAGE TIME IN BED, hour") + ylim(0, 13)


#Calculating the difference between total time in bed and total sleep minutes
min_difference <- min_Sleep_Records %>%
  summarize(mean_difference = (mean(TotalTimeInBed)/60 - mean(TotalMinutesAsleep)/60), sd_difference = (sd(TotalTimeInBed)/60 - sd(TotalMinutesAsleep)/60))
print(min_difference)
moderate_difference <- moderate_Sleep_Records %>%
  summarize(mean_difference = (mean(TotalTimeInBed)/60 - mean(TotalMinutesAsleep)/60), sd_difference = (sd(TotalTimeInBed)/60 - sd(TotalMinutesAsleep)/60))
print(moderate_difference)
max_difference <- max_Sleep_Records %>%
  summarize(mean_difference = (mean(TotalTimeInBed)/60 - mean(TotalMinutesAsleep)/60), sd_difference = (sd(TotalTimeInBed)/60 - sd(TotalMinutesAsleep)/60))
print(max_difference)

# Creating a dataframe for 3 types of diferences (TotalTimeInBed - TotalMinutesAsleep)
difference = data.frame(c(min_difference[1], moderate_difference[1], max_difference[1]), c(min_difference[2], moderate_difference[2], max_difference[2]))

difference_df = data.frame(number_sleeps = c("1", "2", "3"),
                               mean = c(min_difference$mean_difference, moderate_difference$mean_difference, max_difference$mean_difference),
                               sd = c(min_difference$sd_difference, moderate_difference$sd_difference, max_difference$sd_difference))


# Creating a bar plot for sleep types
plot_12 <- ggplot(data = difference_df) +
  geom_bar( aes(x=number_sleeps, y=mean, fill=number_sleeps, alpha=0.5), stat="identity") +
  geom_errorbar( aes(x=number_sleeps, ymin = mean-sd, ymax=mean+sd), width=0.4, colour="black", alpha=0.9, size=1.3) +
  labs(title = "AVERAGE DIFFERENCE BETWEEN \nTOTAL TIME IN BED AND \nTIME ASLEEP (hours) FOR 3 TYPES OF SLEEP", x = "NUMBER OF SLEEPs PER DAY", y = "AVERAGE DIFFERENCE, hour")
grid.arrange(plot_10, plot_11, ncol=2, widths=c(1,1), plot_12, nrow=2) 


# Creating a pointplot and boxplot for TotalTimeInBed and TotalTimeAsleep using sleepDay dataframe
#ggplot(data = sleepDay) + geom_point(mapping = aes(x = TotalSleepRecords, y = TotalTimeInBed/60, color = TotalSleepRecords, size = 1))
#ggplot(data = sleepDay) + geom_point(mapping = aes(x = TotalSleepRecords, y = TotalMinutesAsleep/60, color = TotalSleepRecords, size = 1))
plot_13 <- ggplot(data = sleepDay) + geom_boxplot(mapping = aes(x = TotalSleepRecords, y = TotalMinutesAsleep/60, group = TotalSleepRecords), color = "black", fill = "green") + ylim(0, 18) + labs(title = "SLEEP RECORDS vs.\n TOTAL HOURS ASLEEP", x = "NUMBER OF SLEEPS A DAY", y = "TOTAL HOURS ASLEEP")
plot_14 <- ggplot(data = sleepDay) + geom_boxplot(mapping = aes(x = TotalSleepRecords, y = TotalTimeInBed/60, group = TotalSleepRecords), color = "black", fill = "blue") + ylim(0, 18) + labs(title = "SLEEP RECORDS vs.\n TOTAL HOURS IN BED", x = "NUMBER OF SLEEPS A DAY", y = "TOTAL HOURS IN BED ")
grid.arrange(plot_13, plot_14, ncol=2, widths=c(1,1), nrow=1)  


#Importing weightLogInfo_merged.csv
weightLogInfo_merged <- read_csv("C:\\Users\\y.v.kucherenko\\Downloads\\archive(1)\\Fitabase Data 4.12.16-5.12.16\\weightLogInfo_merged.csv")
glimpse(weightLogInfo_merged)
#Formating date in Date
weightLogInfo_merged$Date <- as.Date(weightLogInfo_merged$Date, "%m/%d/%Y")
colnames(weightLogInfo_merged)
#Looking for duplicates in weightLogInfo_merged
duplicated(weightLogInfo_merged)
sum(duplicated(weightLogInfo_merged))
#Removing duplicates
weightLogInfo <- unique(weightLogInfo_merged)
sum(duplicated(weightLogInfo))
#Looking for N/A data in weightLogInfo
is.na(weightLogInfo)
sum(is.na(weightLogInfo))
#Removing Fat and IsManualReport from weightLogInfo
weightLog <- select(weightLogInfo, -"Fat", -"IsManualReport", -"LogId")
#Columns names in weightLog
colnames(weightLog)
View(weightLog)
#Saving weightLog
#write.csv(weightLog, "C:\\Users\\y.v.kucherenko\\Documents\\R_projects\\weightLog_Google_capstone_project.csv")

#Plotting WeightKg vs. BMI
ggplot(data = weightLog) + geom_point(mapping = aes(x = WeightKg, y = BMI, size = 0, shape = "diamond")) + stat_smooth(mapping = aes(x= WeightKg, y = BMI), data = weightLog, geom = "smooth", position = "identity", method = "lm", formula = y ~ poly(x, 2)) + labs(title = "WEIGHT (in kilogram) vs. BMI") + scale_colour_identity() + scale_shape_identity()
#Filtering overweight group BMI > 25 and obese group BMI > 30
obese_group <- weightLog %>% 
  filter(BMI >= 30)
View(obese_group)
mean_obese <- mean(obese_group$BMI)
sd_obese <- sd(obese_group$BMI)
print(mean_obese)
overweight_group <- weightLog %>% 
  filter(BMI >= 25 & BMI < 30)
View(overweight_group)
mean_overweight <- mean(overweight_group$BMI)
sd_overweight <- sd(overweight_group$BMI)
print(sd_overweight)
normal_group <- weightLog %>% 
  filter(BMI >= 18.5 & BMI < 25)
View(normal_group)
mean_normal <- mean(normal_group$BMI)
sd_normal <- sd(normal_group$BMI)
print(sd_normal)
#Creating dataframe of BMI for 3 different groups
data_BMI <- data.frame(BMI = c("Normal\nBMI>=18.5 & BMI<25", "Overweight\nBMI>=25 & BMI<30", "Obese\nBMI>=30"),
                       mean = c(mean_normal, mean_overweight, mean_obese),
                       sd = c(sd_normal, sd_overweight, sd_obese))
View(data_BMI)  

#Plotting boxplots for normal, overweight and obese groups
ggplot(data = data_BMI) +
  geom_bar( aes(x=BMI, y=mean, fill= BMI, alpha=0.5), stat="identity") +
  geom_errorbar( aes(x=BMI, ymin = mean-sd, ymax=mean+sd), width=0.4, colour="black", alpha=0.9, size=1.3) +
  labs(title = "AVERAGE BMI FOR NORMAL, OVERWEIGHT AND OBESE GROUPS", x = "BMI GROUP", y = "AVERAGE BMI")


#merging the data (inner_join) by Id and ActivityDate, SleepDay and Date
two_df_merged <- dailyActivity_merged %>% inner_join(sleepDay_merged, by = c("ActivityDate" = "SleepDay", "Id" = "Id"))
View(two_df_merged)  
all_data_merged <- two_df_merged %>% inner_join(weightLogInfo_merged, by = c("Id" = "Id", "ActivityDate" = "Date"))
View(all_data_merged) 

#Finding and removing duplicated data in all_data_merged
duplicated(all_data_merged)
sum(duplicated(all_data_merged))

#Finding N/A data in all_data_merged
is.na(all_data_merged)
sum(is.na(all_data_merged))

#Fat column in all_data_merged contained N/A data

# na.fail − It terminates the execution if any of the missing values are found
# na.omit − It simply rules out any rows that contain any missing value and forgets those rows forever.
# na.exclude − This agument ignores rows having at least one missing value.

#Removing "Fat" column from all_data_merged and assigning to all_data
all_data <- select(all_data_merged, -"Fat", -"IsManualReport", -"LogId")
glimpse(all_data)
is.na(all_data)
sum(is.na(all_data))
duplicated(all_data)
sum(duplicated(all_data))

colnames(all_data)
View(all_data)

#Finding unique values in Id column in all_data
unique(all_data$Id)

#Saving all_data as csv file
#write.csv(all_data, "C:\\Users\\y.v.kucherenko\\Documents\\R_projects\\all_data_merged_Google_capstone_project.csv")

all_data <- arrange(all_data, ActivityDate)
View(all_data)

#Filtering all_data for 3 types of BMI
obese_data <- all_data %>%
  filter(BMI >=30)
View(obese_data)

overweight_data <- all_data %>%
  filter(BMI >= 25 & BMI < 30)
View(overweight_data)

normal_data <- all_data %>% 
  filter(BMI >= 18.5 & BMI < 25)
View(normal_data)

colnames(normal_data)

#Plotting Activity Distance vs. Activity Minutes
plot_15 <- ggplot(data = normal_data) + geom_point(mapping = aes(x= VeryActiveDistance, y = VeryActiveMinutes / 60, color = "red")) + scale_colour_identity() + labs(title = "Very Active Distance\n vs. Very Active Hours") + stat_smooth(mapping = aes(x= VeryActiveDistance, y = VeryActiveMinutes / 60), data = normal_data, geom = "smooth", position = "identity", method = "lm", formula = y ~ x, color = "black")
plot_16 <- ggplot(data = normal_data) + geom_point(mapping = aes(x= ModeratelyActiveDistance, y = FairlyActiveMinutes / 60, color = "blue")) + scale_colour_identity() + labs(title = "Moderately Active Distance\n vs. Fairly Active Hours") +  stat_smooth(mapping = aes(x= ModeratelyActiveDistance, y = FairlyActiveMinutes / 60), data = normal_data, geom = "smooth", position = "identity", method = "lm", formula = y ~ x, color = "black")
plot_17 <- ggplot(data = normal_data) + geom_point(mapping = aes(x= LightActiveDistance, y = LightlyActiveMinutes / 60, color = "green")) + scale_colour_identity() + labs(title = "Light Active Distance\n vs. Lightly Active Hours") + stat_smooth(mapping = aes(x= LightActiveDistance, y = LightlyActiveMinutes / 60), data = normal_data, geom = "smooth", position = "identity", method = "lm", formula = y ~ x, color = "black")
plot_18 <- ggplot(data = normal_data) + geom_point(mapping = aes(x= SedentaryActiveDistance, y = SedentaryMinutes / 60)) + scale_colour_identity() + labs(title = "Sedentary Active Distance\n vs. Sedentary Hours") + stat_smooth(mapping = aes(x= SedentaryActiveDistance, y = SedentaryMinutes / 60), data = normal_data, geom = "smooth", position = "identity", method = "lm", formula = y ~ x, color = "black")
grid.arrange(plot_15, plot_16, ncol=2, widths=c(1,1), plot_17, plot_18, nrow=2) 

mean_normal_asleep = mean(normal_data$TotalMinutesAsleep/60)
print(mean_normal_asleep)
mean_overweight_asleep = mean(overweight_data$TotalMinutesAsleep/60)
print(mean_overweight_asleep)
mean_obese_asleep = mean(obese_data$TotalMinutesAsleep/60)
print(mean_obese_asleep)

#Plotting normal_data, overweight_data and obese_data for TotalSleepRecords vs. TotalMinutesAsleep/60
plot_19 <- ggplot(data = normal_data) + geom_boxplot(mapping = aes(x = TotalSleepRecords, y = TotalMinutesAsleep/60, group = TotalSleepRecords), color = "black", fill = "green") + ylim(0, 11) + xlim(0, 3.5) + labs(title = "NORMAL GROUP\nSLEEP RECORDS vs.\nTOTAL HOURS ASLEEP", x = "NUMBER OF SLEEPS A DAY", y = "TOTAL HOURS ASLEEP")
plot_20 <- ggplot(data = overweight_data) + geom_boxplot(mapping = aes(x = TotalSleepRecords, y = TotalMinutesAsleep/60, group = TotalSleepRecords), color = "black", fill = "blue") + ylim(0, 11) + xlim(0, 3.5) +labs(title = "OVERWEIGHT GROUP\nSLEEP RECORDS vs.\nTOTAL HOURS IN BED", x = "NUMBER OF SLEEPS A DAY", y = "TOTAL HOURS IN BED ")
plot_21 <- ggplot(data = obese_data) + geom_boxplot(mapping = aes(x = TotalSleepRecords, y = TotalMinutesAsleep/60, group = TotalSleepRecords), color = "black", fill = "yellow") + ylim(0, 11) + xlim(0, 3.5) +labs(title = "OBESE GROUP\nSLEEP RECORDS vs.\nTOTAL HOURS IN BED", x = "NUMBER OF SLEEPS A DAY", y = "TOTAL HOURS IN BED ")
grid.arrange(plot_19, plot_20, ncol=2, widths=c(1,1), plot_21, nrow=2)  

#Importing heartrate_seconds_merged dataset
heartrate_seconds_merged <- read_csv("C:\\Users\\y.v.kucherenko\\Downloads\\archive(1)\\Fitabase Data 4.12.16-5.12.16\\heartrate_seconds_merged.csv")
glimpse(heartrate_seconds_merged)
View(heartrate_seconds_merged)
#Looking for unique heartrate_seconds_merged Id and Time column records
unique(heartrate_seconds_merged$Id)

#Sorting Time column and looking for unique values of Time columns
heartrate_seconds <- arrange(heartrate_seconds_merged, Time)
unique(heartrate_seconds$Time)

#Filtering heartrate_seconds for Id=2022484408 aand sorting Time column in ascending order
heartrate_seconds_2022484408 <- heartrate_seconds_merged %>%
  filter(Id == 2022484408)
heartrate_seconds_2022484408 <- arrange(heartrate_seconds_2022484408, Time)
View(heartrate_seconds_2022484408)

#Filtering heartrate_seconds dataset for the date 4/12/2016 8 AM to 5/12/2016 8 AM 
heartrate_seconds_4122016_8_9_AM <- heartrate_seconds %>%
  filter(Time >= "4/12/2016 8:05:05 AM" & Time <= "4/12/2016 9:05:05 AM") %>%
  summarize(mean = mean(Value), max = max(Value), min = min(Value), sd = sd(Value))
View(heartrate_seconds_4122016_8_9_AM)
heartrate_seconds_4122016_10_11_AM <- heartrate_seconds %>%
  filter(Time >= "4/12/2016 10:05:05 AM" & Time <= "4/12/2016 11:05:05 AM") %>%
  summarize(mean = mean(Value), max = max(Value), min = min(Value), sd = sd(Value))
View(heartrate_seconds_4122016_10_11_AM)
heartrate_seconds_4122016_1_2_PM <- heartrate_seconds %>%
  filter(Time >= "4/12/2016 1:05:05 PM" & Time <= "4/12/2016 2:05:05 PM") %>%
  summarize(mean = mean(Value), max = max(Value), min = min(Value), sd = sd(Value))
View(heartrate_seconds_4122016_1_2_PM)
heartrate_seconds_4122016_3_4_PM <- heartrate_seconds %>%
  filter(Time >= "4/12/2016 3:05:05 PM" & Time <= "4/12/2016 4:05:05 PM") %>%
  summarize(mean = mean(Value), max = max(Value), min = min(Value), sd = sd(Value))
View(heartrate_seconds_4122016_3_4_PM)
heartrate_seconds_4122016_5_6_PM <- heartrate_seconds %>%
  filter(Time >= "4/12/2016 5:05:05 PM" & Time <= "4/12/2016 6:05:05 PM") %>%
  summarize(mean = mean(Value), max = max(Value), min = min(Value), sd = sd(Value))
View(heartrate_seconds_4122016_5_6_PM)
heartrate_seconds_4122016_7_8_PM <- heartrate_seconds %>%
  filter(Time >= "4/12/2016 7:05:05 PM" & Time <= "4/12/2016 8:05:05 PM") %>%
  summarize(mean = mean(Value), max = max(Value), min = min(Value), sd = sd(Value))
View(heartrate_seconds_4122016_7_8_PM)
heartrate_seconds_4122016_10_11_PM <- heartrate_seconds %>%
  filter(Time >= "4/12/2016 10:05:05 PM" & Time <= "4/12/2016 11:05:05 PM") %>%
  summarize(mean = mean(Value), max = max(Value), min = min(Value), sd = sd(Value))
View(heartrate_seconds_4122016_10_11_PM)
heartrate_seconds_5122016_1_2_AM <- heartrate_seconds %>%
  filter(Time >= "5/12/2016 1:05:05 AM" & Time <= "5/12/2016 2:05:05 AM") %>%
  summarize(mean = mean(Value), max = max(Value), min = min(Value), sd = sd(Value))
View(heartrate_seconds_5122016_1_2_AM)
heartrate_seconds_5122016_3_4_AM <- heartrate_seconds %>%
  filter(Time >= "5/12/2016 3:05:05 AM" & Time <= "5/12/2016 4:05:05 AM") %>%
  summarize(mean = mean(Value), max = max(Value), min = min(Value), sd = sd(Value))
View(heartrate_seconds_5122016_3_4_AM)
heartrate_seconds_5122016_5_6_AM <- heartrate_seconds %>%
  filter(Time >= "5/12/2016 5:05:05 AM" & Time <= "5/12/2016 6:05:05 AM") %>%
  summarize(mean = mean(Value), max = max(Value), min = min(Value), sd = sd(Value))
View(heartrate_seconds_5122016_5_6_AM)
heartrate_seconds_5122016_7_8_AM <- heartrate_seconds %>%
  filter(Time >= "5/12/2016 7:05:05 AM" & Time <= "5/12/2016 8:05:05 AM") %>%
  summarize(mean = mean(Value), max = max(Value), min = min(Value), sd = sd(Value))
View(heartrate_seconds_5122016_7_8_AM)

#merging dataframes to build a heartrate_seconds_4122026_5122016
total <- rbind(heartrate_seconds_4122016_8_9_AM, heartrate_seconds_4122016_10_11_AM, 
               heartrate_seconds_4122016_1_2_PM, heartrate_seconds_4122016_3_4_PM,
               heartrate_seconds_4122016_5_6_PM, heartrate_seconds_4122016_7_8_PM,
               heartrate_seconds_4122016_10_11_PM,
               heartrate_seconds_5122016_1_2_AM, heartrate_seconds_5122016_3_4_AM,
               heartrate_seconds_5122016_5_6_AM, heartrate_seconds_5122016_7_8_AM)
View(total)

#Adding a column Time to total dataset
total_df <- total %>%
  add_column("DateTime" = c("4/12/2016_8_9_AM", "4/12/2016_10_11_AM", "4/12/2016_1_2_PM", "4/12/2016_3_4_PM", 
               "4/12/2016_5_6_PM", "4/12/2016_7_8_PM", "4/12/2016_10_11_PM", "5/12/2016_1_2_AM", "5/12/2016_3_4_AM", 
               "5/12/2016_5_6_AM", "5/12/2016_7_8_AM"))
View(total_df)

#Plotting total_df data
ggplot(data = total_df) +
  geom_bar( aes(x=DateTime, y=mean, fill= DateTime), stat="identity") +
  geom_errorbar( aes(x=DateTime, ymin = mean-sd, ymax=mean+sd), width=0.4, colour="black", alpha=0.9, size=1.3) +
  labs(title = "AVERAGE HEARTRATE FOR TIME RANGE 4/12/2016 TO 5/12/2016", x = "DATETIME", y = "AVERAGE HEARTRATE") +
  theme(axis.text.x = element_text(angle = 30))

# Plot the bar chart 
barplot(total_df$mean,names.arg=total_df$DateTime,xlab="DATETIME",
        ylab="AvVERAGE HEARTRATE",col="blue",
        main="AVERAGE HEARTRATE FOR TIME RANGE 4/12/2016 TO 5/12/2016",border="black")

colnames(heartrate_seconds)
#Splitting datetime column Time into 2 columns Date and Time
heartrate_seconds_df <- tidyr::separate(heartrate_seconds, Time, c("Date", "Time"), sep = " ")
View(heartrate_seconds_df)
