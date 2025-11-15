# Analysis file

#STEP 1. INSTALL
install.packages(c("rmarkdown", "readr", "lubridate", "skimr","tidyverse", "ggrepel"))
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(skimr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)

#STEP 2. Import de data set 
# File clean and filter, ready for analysis
Daily_Activity_df <- read.csv("C:/Proyecto Google/Prepare/Analysis/Daily_Activity_ready_for_analysis.csv")

# Analysis 1: Number of registers per ID

# I made another data frame for the number of records per user.
records_count_ID <- Daily_Activity_df %>%
  group_by(Id) %>%    #group every value ID.
  summarise(Num_Entradas = n())  # Count how many time the Id shows up
print(records_count_ID, n=33)               # Shows the number of registers of each Id.

#### Obtain the statistic values for the count of registers per ID. Resumen estadístico

summary(records_count_ID$Num_Entradas)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.00    8.00   11.00   10.42   11.00   28.00 

#The values show us that 5 is the minimum value of records and 28 is the maximum value, but the most common count of registers is 11.
# The 50% of the users have between 8 and 11 records.

#Esto te muestra qué tan consistente fue cada usuario registrando actividad.
ggplot(records_count_ID, aes(x = reorder(Id, Num_Entradas), y = Num_Entradas, fill = Num_Entradas)) +
  geom_col() +
  scale_fill_gradient(low = "#A6CEE3", high = "#1F78B4", name = "Recorded days") +
  labs(
    title = "Number of days register per ID",
    x = "ID of the user",
    y = " Count of active days"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

#reorde() -> arrange by bar height
#theme(axis.text.x = element_text(angle = 90)) rotate the labels so they don't pile up

#Save the graph
ggsave("C:/Proyecto Google/Prepare/Analysis/Plots/Register_days_per_ID.png",
       width = 8, height = 5, dpi = 300)

# Analysis 2: Records per day of week and type of day (Weekday or Weekend).

# Fix the format of ActivityDate from chr to date. Obtain each day of week in a new variable named DayOfWeek and in other variable (WeekType) put if it is Weekend or Weekday. 

Daily_Activity_df <- Daily_Activity_df %>%
  mutate(ActivityDate = ymd(ActivityDate),
         DayOfWeek = wday(ActivityDate, label = TRUE, abbr = FALSE, locale = "en_US"),
         WeekType = ifelse(DayOfWeek %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

# Count the records per week type (Weekday vs Weekend)
week_summary <- Daily_Activity_df %>%
  group_by(WeekType) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
print(week_summary)

#WeekType count
#Weekday    223
#Weekend    121

#It can be seen that the number of records on weekdays (223) is almost double that on weekends (121). 
#Therefore, it is used more during the week. However, it is important to consider that there are more weekdays 
#than weekends. So we must look at its frequency per day.

# Get the number of records per day of the week.
weekday_summary <- Daily_Activity_df %>%
  group_by(DayOfWeek) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
print(weekday_summary)

#DayOfWeek count
# domingo      61 Day with more activity
# sábado       60
# lunes        56
# viernes      54
# martes       38
# miércoles    38
# jueves       37 Day with less activity

#Sunday is the day with the most records and therefore the most activity, while Thursday has the least activity.
#Saturday and Sunday seem to be the days with the most activity compared to the others, suggesting greater interest 
#in monitoring activity during free time. 

#The average activity was calculated for weekends and workdays.

promedio_weekend = week_summary$count[week_summary$WeekType=="Weekend"] / 2
promedio_weekday = week_summary$count[week_summary$WeekType=="Weekday"] / 5

promedio_weekend # 60.5 de biggest mean is in the weekend. 
promedio_weekday # 44.6

#Although users recorded more days of activity during the week, this is because there are more 
#working days than weekend days. Further analysis shows that, per day, users tend to use the device 
#more on weekends.

# Bar graph 
ggplot(weekday_summary, aes(x = reorder(DayOfWeek, count), y = count, fill = count)) +
  geom_col() +
  scale_fill_gradient(low = "#ffeda0", high = "#f03b20") +
  labs(title = "Records per Day of the Week", x = "Day", y = "Number of Records") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("C:/Proyecto Google/Prepare/Analysis/Plots/registro_dias_semana.png",
       width = 7, height = 5, dpi = 300, bg = "transparent")

# Analysis 3: Adherence analysis by minute

# Import the data with out the last filter.
Daily_Activity_df_adherence <- read.csv("C:/Proyecto Google/Prepare/Analysis/Daily_Activity_without_filter_of_minutes.csv")

head (Daily_Activity_df_adherence)
str(Daily_Activity_df_adherence) #366 obs
print(length(unique(Daily_Activity_df_adherence$Id))) #34

#Classify usage of the device by category using the minutes.
Daily_Activity_df_adherence <- Daily_Activity_df_adherence %>%
  mutate(Total_Minutes = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes + SedentaryMinutes,
         Use_category = case_when(
           Total_Minutes < 300 ~ "Low use",
           Total_Minutes >= 300 & Total_Minutes < 720 ~ "Moderate use",
           Total_Minutes >= 720 & Total_Minutes < 1080 ~ "High",
           Total_Minutes >= 1080 ~ "Very High"
         ))

head(Daily_Activity_df_adherence) # It work
str(Daily_Activity_df_adherence)


# Summary of usage categories :
usage_summary <- Daily_Activity_df_adherence %>%
  group_by(Use_category) %>%
  summarise(Days = n(),
            Percentage = (n()/nrow(Daily_Activity_df_adherence))*100) %>%
  mutate(Label = paste0(Use_category, " (", round(Percentage, 1), "%)"))

print(usage_summary)

#Use_category  Days Percentage
#High           131      35.8 
#Low use         10       2.73
#Moderate use    17       4.64
#Very High      208      56.8 

# Adherence pie chart
ggplot(usage_summary, aes(x = "", y = Days, fill = Label)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() + #makes the background transparent
  labs(fill = "Usage Category") + #legend
  ggtitle("Daily Activity Usage") +  # Title
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), # Centered and larger title
    legend.title = element_text(size = 14, face = "bold"),  # Title of the legend 
    legend.text = element_text(size = 12) # Text of the legend
  ) +
  scale_fill_brewer(palette = "Set2") 

ggsave("C:/Proyecto Google/Prepare/Analysis/Plots/Distribution_usage_categories.png",
       width = 7, height = 5, dpi = 300, bg = "transparent")

# Analysis 4. Compare adherence vs. number of records

# Obtain adherence per user (percentage of active days)
adherencia_df <- Daily_Activity_df_adherence %>%
  group_by(Id) %>%
  summarise(Adherencia = n()/nrow(Daily_Activity_df_adherence)*100)
print(adherencia_df) # Gives the percentage of adherence per ID 

# Join the number of registers with the percentage of adherence from each ID in the same data frame.
comparison_df <- records_count_ID %>%
  left_join(adherencia_df, by = "Id")
print (comparison_df) 

# Scatter plot
ggplot(comparison_df, aes(x = Num_Entradas, y = Adherencia)) +
  geom_jitter(width = 0.2, height = 0.2, color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Adherence vs Number of Entries per User",
       x = "Number of Entries",
       y = "Adherence (%)") +
  theme_minimal(base_size = 14)

ggsave("C:/Proyecto Google/Prepare/Analysis/Plots/Adherence_vs_Entries.png",
       width = 7, height = 5, dpi = 300, bg = "transparent")

#comparison_df has a row per Id.
#x = Num_Entradas → number of registers per user..
#y = Adherence (%) → percentage of the active days per ID.
#geom_point(color = "steelblue", alpha = 0.7) → All the points in this color “steelblue”.
#Each point represents a user.
#Some points appear darker than others because there are overlapping points at those values of (Num_Entries, Adherence). 
#Red line: general trend. If it rises from left to right, it means that more records are associated with greater adherence.

# Correlation analysis 

cor(comparison_df$Num_Entradas, comparison_df$Adherencia, use = "complete.obs") # 0.9942141

# more registers more adherence to the device.

# Analysis 5: Mobility profile of the users

#For each user, the mobility profile is calculated based on the value of the total distance for each day. 
#The profiles are as follows: sedentary (less than 0.9 miles), light (0.9 to less than 3 miles), moderate 
#(3 to less than 4.7 miles), and high (more than 4.7 miles).

#The distance ranges were obtained from the literature (Tudor-Locke et al., 2011; Oja et al., 2018) 
#and were converted from kilometers to miles, as our distances are in miles.

daily_profile <- Daily_Activity_df %>% # This generates a new df with a new column, in which all the registers are classified as the mobility profiles. 
  mutate( # distance in miles
    DistanceCategory = case_when(
      TotalDistance < 0.9 ~ "Sedentary",
      TotalDistance >= 0.9 & TotalDistance < 3.0 ~ "Light",
      TotalDistance >= 3.0 & TotalDistance < 4.7 ~ "Moderate",
      TotalDistance >= 4.7 ~ "High",
      TRUE ~ "Unknown"
    )
  )

# Generate a data frame that contains each user's profile by summarizing its registered profiles and the number of days.
user_profile <- daily_profile %>%
  group_by(Id, DistanceCategory) %>% 
  summarise(days = n(), .groups = "drop") %>%
  group_by(Id) %>%
  slice_max(days, n = 1, with_ties = FALSE) %>%
  ungroup()

print(user_profile, n= 33)

# Obtains how many users have each profile and the percentage that each profile represents.
profile_counts <- user_profile %>%
  count(DistanceCategory) %>%
  mutate(percentage = round(n / sum(n) * 100, 2))

print(profile_counts)

#DistanceCategory     n percentage
#High                15       45.4
#Light                7       21.2
#Moderate             5       15.2
#Sedentary            6       18.2

#pie chart

ggplot(profile_counts, aes(x = "", y = percentage, fill = DistanceCategory)) +
  geom_col(width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5), size = 5) +
  labs(title = "User Activity Profiles Based on Daily Distance") +
  theme_void() +
  theme(legend.title = element_blank())

ggsave("User_Activity_Profiles_PieChart.png",
       width = 7, height = 6, dpi = 300, bg = "transparent")


#Analysis 6: Steps vs Calories 

# Check that the variables does not have NA.

skim(Daily_Activity_df$Calories) # No NA
skim(Daily_Activity_df$TotalSteps) # No NA

# Scatter plot to see the relationship between both variables.
ggplot(Daily_Activity_df, aes(x = TotalSteps, y = Calories)) +
  geom_point(color = "#1f77b4", alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "#d62728", se = TRUE) +
  labs(
    title = " Relationship between daily steps and burned calories",
    x = "Daily Total steps",
    y = "Burned calories"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("C:/Proyecto Google/Prepare/Analysis/Plots/Relationship_dailysteps_vs_burnedcalories.png",
       width = 7, height = 5, dpi = 300, bg = "transparent")

# Blue points represent the registers.
# The red line is the linear trend line (linear model). It summarizes the average relationship between steps and calories.
# Gray shadow around the red line (the “confidence interval”). - Represents the 95% confidence level of the linear model.

# Pearson correlation

cor(Daily_Activity_df$TotalSteps, Daily_Activity_df$Calories, use = "complete.obs") #0.5396948

#Analysis 7: Comparison between average steps on weekends vs. weekdays

# Obtain the mean of total steps by user and type of week (weekend or weekday).  
avg_steps_user <- Daily_Activity_df %>%
  group_by(Id, WeekType) %>%
  summarise(avg_steps = mean(TotalSteps, na.rm = TRUE)) %>%
  ungroup()

print(avg_steps_user, n=100) 

#The result shows 66 rows because for each ID that we have, we obtain the average of total steps by type of week (weekend or weekday).
#By averaging the number of steps per user, each person has the same weight in the final result, avoiding bias due to the number of records.

# Obtain the overall average by type of week (average of averages) 
avg_steps_summary <- avg_steps_user %>%
  group_by(WeekType) %>%
  summarise(
    mean_steps = mean(avg_steps, na.rm = TRUE),
    sd_steps = sd(avg_steps, na.rm = TRUE),
    n = n(),
    se_steps = sd_steps / sqrt(n)  # standard error
  )

print(avg_steps_summary) 
# Here we obtain the average number of steps per weekday and weekend, SD, number of users 33, and their standard errors.
#WeekType mean_steps sd_steps     n se_steps
#Weekday       6910    4344    33     756
#Weekend       6536    4735    33     824


#Graph comparison: Weekday vs. Weekend

ggplot(avg_steps_summary, aes(x = WeekType, y = mean_steps, fill = WeekType)) +
  geom_col(width = 0.6, color = "black", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_steps - se_steps, ymax = mean_steps + se_steps),
                width = 0.2, color = "gray20", linewidth = 0.8) +
  geom_text(aes(label = round(mean_steps, 0)), vjust = -0.5, size = 4.2) +
  labs(
    title = "Average Steps per User on Weekdays vs. Weekends",
    x = "",
    y = "Average Steps per User"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave("C:/Proyecto Google/Prepare/Analysis/Plots/Average_Steps_per_User_Weekdays_vs_Weekends.png",
       width = 7, height = 5, dpi = 300, bg = "transparent")


#Analysis 8: Classification by type of day and activity profile   

# Count number of records in each distance category per type of day
activity_by_daytype <- daily_profile %>%
  group_by(WeekType, DistanceCategory) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(WeekType) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

print(activity_by_daytype)

# The result is the number of registers in each type of week, categorized by the activity level in distance.

#WeekType DistanceCategory count percentage
# Weekday  High               108       48.4
# Weekday  Light               38       17  
# Weekday  Moderate            32       14.3
# Weekday  Sedentary           45       20.2
# Weekend  High                49       40.5
# Weekend  Light               29       24  
# Weekend  Moderate            22       18.2
# Weekend  Sedentary           21       17.4


# Stacked bar chart to visualize the proportion of each activity level per type of day
ggplot(activity_by_daytype, aes(x = WeekType, y = percentage, fill = DistanceCategory)) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Activity Profiles by Type of Day",
    x = "",
    y = "Percentage of Records (%)",
    fill = "Activity Profile"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("C:/Proyecto Google/Prepare/Analysis/Plots/Activity_Profiles_by_Type_of_Day.png",
       width = 7, height = 5, dpi = 300, bg = "transparent")

