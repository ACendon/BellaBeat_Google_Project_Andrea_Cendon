#Activity merged 

#STEP 1. INSTALL
install.packages(c("rmarkdown", "readr", "lubridate", "skimr","tidyverse"))
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(skimr)
library(tidyverse)
library(ggplot2)

#STEP 2. Import de data sets and check the structure

##2.1 Main file: Daily_Activity_merged.csv
## 2.1.1 Import the data of the main file and analyze the structure of the file.
Daily_Activity_df <- read.csv("C:/Proyecto Google/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/dailyActivity_merged.csv")

head(Daily_Activity_df, 3) #This part gives me the first 6 rows and the column titles.
str(Daily_Activity_df) # It tells me what kind of objects are in each column: The data includes 457 observations and 15 variables. Long format: Each row = one individual measurement or record; the same entity may appear in several rows.

### 2.1.2 See if there are duplicates or NA. (The data of distance is in miles)
sum(duplicated(Daily_Activity_df)) #0 duplicates

skim(Daily_Activity_df) #No NA, complete rate 1, for all the variables. It shows all the columns values.

### 2.1.3 Analyze which and how many unique IDs they are.

unique(Daily_Activity_df$Id) #Gives the list of IDs
print(length(unique(Daily_Activity_df$Id))) #count the number of unique IDs =35

### 2.1.4 For the final analysis I need to check how many users logged their distance manually at this point of the process.

Logged_df <- Daily_Activity_df %>% 
  filter(LoggedActivitiesDistance > 0)
View(Logged_df) # 6 different IDs, 24 obs. 

## 2.2 The other data sets

### 2.2.1 Import the data
Heart_r_df <- read.csv("C:/Proyecto Google/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/heartrate_seconds_merged.csv")
hourlyCalories_df <- read.csv("C:/Proyecto Google/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/hourlyCalories_merged.csv")
hourlyIntensities_df <- read.csv("C:/Proyecto Google/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/hourlyIntensities_merged.csv")
hourlySteps_df <- read.csv("C:/Proyecto Google/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/hourlySteps_merged.csv")
minuteCaloriesNarrow_df <- read.csv("C:/Proyecto Google/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/minuteCaloriesNarrow_merged.csv")
minuteIntensitiesNarrow_df <- read.csv("C:/Proyecto Google/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/minuteIntensitiesNarrow_merged.csv")
minuteMETsNarrow_df <- read.csv("C:/Proyecto Google/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/minuteMETsNarrow_merged.csv")
minuteSleep_df <- read.csv("C:/Proyecto Google/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/minuteSleep_merged.csv")
minuteStepsNarrow_df <- read.csv("C:/Proyecto Google/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/minuteStepsNarrow_merged.csv")
weightLogInfo_df <- read.csv("C:/Proyecto Google/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/weightLogInfo_merged.csv")

### 2.2.2 Analyze the structure of each one, if they have duplicates, the unique IDs, and how many of them exist.

#### 2.2.2.1 Heart_r_df

# Analize the structure
head(Heart_r_df, 3) 
str(Heart_r_df) 

#The duplicates 
sum(duplicated(Heart_r_df)) 

# IDs
unique(Heart_r_df$Id) 
print(length(unique(Heart_r_df$Id))) 

#### 2.2.2.2 hourlyCalories_df

# Analize the structure
head(hourlyCalories_df, 3) # 3 columns, has IDs  
str(hourlyCalories_df) 

#The duplicates 
sum(duplicated(hourlyCalories_df)) #0 duplicates

# IDs
unique(hourlyCalories_df$Id) 
print(length(unique(hourlyCalories_df$Id))) #IDs = 34

#### 2.2.2.3 hourlyIntensities_df

# Analize the structure
head(hourlyIntensities_df, 3) # 4 columns, has IDs  
str(hourlyIntensities_df) 

#The duplicates 
sum(duplicated(hourlyIntensities_df)) #0 duplicates

# IDs
unique(hourlyIntensities_df$Id) 
print(length(unique(hourlyIntensities_df$Id))) #IDs = 34

#### 2.2.2.4 hourlySteps_df

# Analize the structure
head(hourlySteps_df, 3) # 3 columns, has IDs  
str(hourlySteps_df) 

#The duplicates 
sum(duplicated(hourlySteps_df)) #0 duplicates

# IDs
unique(hourlySteps_df$Id) 
print(length(unique(hourlySteps_df$Id))) #IDs = 34

#### 2.2.2.5 minuteCaloriesNarrow_df

# Analize the structure
head(minuteCaloriesNarrow_df, 3) # 3 columns, has IDs  
str(minuteCaloriesNarrow_df) 

#The duplicates 
sum(duplicated(minuteCaloriesNarrow_df)) #0 duplicates

# IDs
unique(minuteCaloriesNarrow_df$Id) 
print(length(unique(minuteCaloriesNarrow_df$Id))) #IDs = 34

#### 2.2.2.6 minuteIntensitiesNarrow_df

# Analize the structure
head(minuteIntensitiesNarrow_df, 3) # 3 columns, has IDs  
str(minuteIntensitiesNarrow_df) 

#The duplicates 
sum(duplicated(minuteIntensitiesNarrow_df)) #0 duplicates

# IDs
unique(minuteIntensitiesNarrow_df$Id) 
print(length(unique(minuteIntensitiesNarrow_df$Id))) #IDs = 34

#### 2.2.2.7 minuteMETsNarrow_df

# Analize the structure
head(minuteMETsNarrow_df, 3) # 3 columns, has IDs  
str(minuteMETsNarrow_df) 

#The duplicates 
sum(duplicated(minuteMETsNarrow_df)) #0 duplicates

# IDs
unique(minuteMETsNarrow_df$Id) 
print(length(unique(minuteMETsNarrow_df$Id))) #IDs = 34

#### 2.2.2.8 minuteSleep_df

# Analize the structure
head(minuteSleep_df, 3) # 4 columns, has IDs  
str(minuteSleep_df) 

#The duplicates 
sum(duplicated(minuteSleep_df)) #525 duplicates

# IDs
unique(minuteSleep_df$Id) 
print(length(unique(minuteSleep_df$Id))) #IDs = 23

##### 2.2.2.8.1 Analyze the duplicates and remove them from the file.

#To see if there are registers from the same ID in the same date duplicated:
minuteSleep_df %>%
  group_by(Id, date) %>%
  filter(n() > 1) %>%
  arrange(Id,date)

#We see the duplicates so it is time to remove them
minuteSleep_df <- minuteSleep_df %>% distinct()

# To confirm the number of duplicates  
sum(duplicated(minuteSleep_df)) #0

#### 2.2.2.9 minuteStepsNarrow_df

# Analize the structure
head(minuteStepsNarrow_df, 3) # 3 columns, has IDs  
str(minuteStepsNarrow_df) 

#The duplicates 
sum(duplicated(minuteStepsNarrow_df)) #0 duplicates

# IDs
unique(minuteStepsNarrow_df$Id) 
print(length(unique(minuteStepsNarrow_df$Id))) #IDs = 34

#### 2.2.2.9 weightLogInfo_df

# Analize the structure
head(weightLogInfo_df, 3) # 8 columns, has IDs  
str(weightLogInfo_df) 

#The duplicates 
sum(duplicated(weightLogInfo_df)) #0 duplicates

# IDs
unique(weightLogInfo_df$Id) 
print(length(unique(weightLogInfo_df$Id))) #IDs = 11

#STEP 3. Compare and analyse the IDs from each file in Excel 

#Most of the sample size of the other data set is 34. So I will analyze the extra ID user (4388161847) 
#from Daily_Activity_df to check if it was an error, and I can remove it.

## 3.1 Filter the lines of this ID user.

filas_seleccionadas <- Daily_Activity_df %>%
  filter(Id == "4388161847")
print(filas_seleccionadas) # Shows the 8 rows

#It shows that the only minimum registered information is on one row and is on sedentary time and calories.
#I need the same sample size in both data sets. The user 4388161847 has only 8 observations and doesn't have a lot of information, 
#so I will remove it.

## 3.2 Remove the Id from the data and confirm the action.
Daily_Activity_df <- Daily_Activity_df %>%
  filter(Id != "4388161847")

str(Daily_Activity_df) # 499 obs.
# Confirm
print(length(unique(Daily_Activity_df$Id))) #34


#STEP 4.Change the ActivityDate format (chr) to format date and confirm.
Daily_Activity_df <- Daily_Activity_df %>% 
  mutate(ActivityDate = as.Date(ActivityDate, format = "%m/%d/%Y")) # It fixed the format but it appears as 2016-03-25
str(Daily_Activity_df) #to confirm the format

# STEP 5. Make the operations to see if the values in each column are right.

## 5.1 Distances 

### 5.1.1 Tracker distance 
# Tracker distance = Very Active distance + Moderate Active distance + Light Active distance

Daily_Activity_df <- Daily_Activity_df %>%
  mutate(Resultado_T_V_M_L = TrackerDistance - (VeryActiveDistance + ModeratelyActiveDistance + LightActiveDistance) )

Daily_Activity_df$Resultado_T_V_M_L # Analyze the differences because we need the result to around 0

summary(Daily_Activity_df$Resultado_T_V_M_L) # we can see that the difference are strange so we have a max of 7.41 and min -6.41

#### 5.1.1.1  Analyze the outliers shown in the formula applied above, T-(V-M-L)=0, using Tukey's rule. With the result, we create another dataframe to enter those records that give the outlier value and analyze them.
Q1 <- quantile(Daily_Activity_df$Resultado_T_V_M_L, 0.25, na.rm = TRUE)
Q3 <- quantile(Daily_Activity_df$Resultado_T_V_M_L, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1 #0.0099

outliers_df <- Daily_Activity_df %>%
  filter(Resultado_T_V_M_L < (Q1 - 1.5 * IQR) |
           Resultado_T_V_M_L > (Q3 + 1.5 * IQR))
View(outliers_df) # 70 atypical values

#### 5.1.1.2 Graph to see how the data is distributed.

#Create a histogram to see if the outliers are very isolated.

ggplot(Daily_Activity_df, aes(x = Resultado_T_V_M_L)) +
  geom_histogram(bins = 50) +
  geom_vline(aes(xintercept = Q1), color = "blue", linetype = "dashed") +
  geom_vline(aes(xintercept = Q3), color = "blue", linetype = "dashed") +
  geom_vline(aes(xintercept = Q1 - 1.5*IQR), color = "red") +
  geom_vline(aes(xintercept = Q3 + 1.5*IQR), color = "red") +
  labs(title =  "Distribution of de diference of Tracker distance",size= 1.5, subtitle = "T-(V+M+L)")+
  labs(x = "Results of the diference", y = "Amount of registers") 

#### 5.1.1.3 Comparison of means between df.

mean(Daily_Activity_df$Resultado_T_V_M_L) #0.06118
mean(outliers_df$Resultado_T_V_M_L) #0.38342

#### 5.1.1.4 Analyze registers of the users that have the biggest or lowest values in the outliers_df.
print(outliers_df %>%
  group_by(Id) %>%
  summarise(
    media = mean(Resultado_T_V_M_L),
    maximo = max(Resultado_T_V_M_L),
    minimo = min(Resultado_T_V_M_L),
    n = n()
  ) %>%
  arrange(desc(abs(media))), n=Inf) #Hay 17 usuarios con registros atipicos
# Los que muestran valores mas atipicos
#1 6391747486 -4.38   -2.03   -6.73       2
#2 2891001357 -2.42   -1.61   -3.22       2
#3 4020332650  1.97    7.41    0.0300    25
#4 7007744171 -1.11   -0.820  -1.25       7
#5 6962181067 -0.895  -0.620  -1.36       4

#### 5.1.1.4.1 View the registers of the 5 principal users that generate atypical values. 
outliers_df %>%
  filter(Id %in% c(6391747486, 2891001357, 4020332650, 6962181067,7007744171)) %>%
  select(Id, ActivityDate, TrackerDistance, VeryActiveDistance,
         ModeratelyActiveDistance, LightActiveDistance, Resultado_T_V_M_L) %>%
  arrange(Id, ActivityDate, n= Inf) # hay 40 datos con valores atipicos de estos usuarios 

##### 5.1.1.4.2 Analyze the registers of the first ID (6391747486).

#6391747486
Filas_seleccionadas<- outliers_df %>%
  filter(Id == "6391747486")
print(Filas_seleccionadas)

#Two records are observed on different dates for this ID; the first shows that the Tracker distance is zero but has a Very Active distance of 2.03 miles. These may mean that there is a device synchronization error or a recording failure. It should be deleted because it breaks the logic of the variables, distorts the average and range, and may generate errors in subsequent analyses.

### 5.1.1.4.3. Search both data frames for how many records have TrackerDistance equal to zero and activity levels measured in distance greater than zero.
# Trd = 0 & (V >0 | M >0 |L > 0 | S > 0)

Daily_Activity_df %>%
  filter(TrackerDistance == 0 & (VeryActiveDistance > 0 | ModeratelyActiveDistance > 0 | LightActiveDistance > 0 | SedentaryActiveDistance > 0))
#It was obtained 3 registers, two from the ID 2891001357 and one from the ID 6391747486.

#### 5.1.1.4.4. Remove the registers because it is impossible that they mark and confirm the action.

Daily_Activity_df <- Daily_Activity_df %>%
  filter(!(TrackerDistance == 0 & 
             (VeryActiveDistance > 0 | ModeratelyActiveDistance > 0 | LightActiveDistance > 0 | SedentaryActiveDistance > 0)))

outliers_df <- outliers_df %>%
  filter(!(TrackerDistance == 0 & 
             (VeryActiveDistance > 0 | ModeratelyActiveDistance > 0 | LightActiveDistance > 0 | SedentaryActiveDistance > 0)))
# confirm 
Daily_Activity_df %>%
  filter(TrackerDistance == 0 & (VeryActiveDistance > 0 | ModeratelyActiveDistance > 0 | LightActiveDistance > 0 | SedentaryActiveDistance > 0)) 
# 0 obs.

str(Daily_Activity_df) # now 446 obs.
print(length(unique(Daily_Activity_df$Id))) # 34 IDs

str(outliers_df) # now 67 obs.
print(length(unique(outliers_df$Id))) # 16 IDs it was remove one user.

### 5.1.1.4.5 Analyze the other register from the ID (6391747486)

Filas_seleccionadas<- outliers_df %>%
  filter(Id == "6391747486")
print(Filas_seleccionadas)

#The tracker value is much smaller than the distance of each activity at different levels. 
#Also, it shows a value of 7.46 miles in fairly distance, which is physically impossible. 
#This result can be attributed to a Fitbit synchronization error, a recording error caused 
#by using two devices at the same time, causing the information to overlap, or an error in 
#the export and transformation of data. 

#So, to remedy this, we will look at how many records have a tracker value less than the sum
#of the activity levels. It is important to consider the IQR of 0.009 that we calculated above 
#with Tukey's rule to get the upper and lower limits.

#### 5.1.1.4.6 Calculate the upper and lower limits, see how many records have that error, and analyze a row to check if it has the error.

#upper and lower limits
lower_bound <- Q1 - 1.5 * IQR # 25% -0.014999
upper_bound <- Q3 + 1.5 * IQR # 75% 0.024999

#Se how many registers have this error.
outliers_detected <- outliers_df %>%
  filter(Resultado_T_V_M_L < lower_bound | Resultado_T_V_M_L > upper_bound) 

# There are 67 registers.

View(outliers_detected) # these are the registers in df outliners

# confirm with an example
Filas_seleccionadas<- outliers_df %>%
  filter(Id == "1503960366")
print(Filas_seleccionadas) 

#Remove outliers that exceed the limits of the dataframe outliers_df to see how many would remain and what problem they have.

outliers_df <- outliers_df[outliers_df$Resultado_T_V_M_L >= lower_bound & outliers_df$Resultado_T_V_M_L <= upper_bound, ]

str(outliers_df) # now 0 obs.

Daily_Activity_df <- Daily_Activity_df[Daily_Activity_df$Resultado_T_V_M_L >= lower_bound & Daily_Activity_df$Resultado_T_V_M_L <= upper_bound, ]
str(Daily_Activity_df) #379 obs.
print(length(unique(Daily_Activity_df$Id))) #34 IDs

##### 5.1.1.4.7 Make a histogram with the results.
ggplot(Daily_Activity_df, aes(x = Resultado_T_V_M_L)) +
  geom_histogram(bins = 10) +
  geom_vline(aes(xintercept = Q1), color = "blue", linetype = "dashed") +
  geom_vline(aes(xintercept = Q3), color = "blue", linetype = "dashed") +
  geom_vline(aes(xintercept = Q1 - 1.5*IQR), color = "red") +
  geom_vline(aes(xintercept = Q3 + 1.5*IQR), color = "red") +
  labs(title =  "Distribution of de diference of Tracker distance", subtitle = "T-(V+M+L)")+
  labs(x = "Results of the diference", y = "Amount of registers") +
  theme(axis.title.x = element_text(size = 8),axis.title.y =element_text(size = 8),axis.title = element_text(size=10))

### 5.1.2 Total distance

#### 5.1.2.1 Total distance - (Tracker distance + Logged distance) ~ 0
# Total distance= Tracker distance + Logged distance 
# T-(Tr+L)=0 

Daily_Activity_df <- Daily_Activity_df %>%
  mutate(Resultado_T_T_L = TotalDistance -(TrackerDistance + LoggedActivitiesDistance) )

# Daily_Activity_df$Resultado_T_T_L # Analyze the differences because we need the result to around 0

summary(Daily_Activity_df$Resultado_T_T_L)  #Min -4.82803, Max 0

#### 5.1.2.2 Use the Tukey rule to obtain the atypical values, analyze them, and remove them if it is necessary.

#Not all the results of the difference give 0 that´s why we use the Tukey rule

Q1 <- quantile(Daily_Activity_df$Resultado_T_T_L, 0.25, na.rm = TRUE) # 25% 0
Q3 <- quantile(Daily_Activity_df$Resultado_T_T_L, 0.75, na.rm = TRUE) # 75% 0
IQR <- Q3 - Q1 #Tukey' Rule 0 

lower_bound <- Q1 - 1.5 * IQR # 25% 0
upper_bound <- Q3 + 1.5 * IQR # 75% 0

outliers_df <- Daily_Activity_df %>%
  filter(Resultado_T_T_L < (Q1 - 1.5 * IQR) |
           Resultado_T_T_L> (Q3 + 1.5 * IQR))

View(outliers_df) 

#8 atypical values; 7 from the same user and 1 from another user.

# Remove 

outliers_df <- outliers_df[outliers_df$Resultado_T_T_L >= lower_bound & outliers_df$Resultado_T_T_L <= upper_bound, ]
Daily_Activity_df <- Daily_Activity_df[Daily_Activity_df$Resultado_T_T_L >= lower_bound & Daily_Activity_df$Resultado_T_T_L <= upper_bound, ]

# confirm
View(outliers_df)  # there is nothing
str(Daily_Activity_df) #371 obs
print(length(unique(Daily_Activity_df$Id))) #34 users

## 5.2 Zero calories is an error

#How many registers have zero calories.
sum(Daily_Activity_df$Calories == 0) #5

# Get the percentage of these cases against all the values in the data frame.

mean(Daily_Activity_df$Calories == 0) * 100 #1.37741%

Daily_Activity_df %>%
  filter(Calories == 0) # In this 5 registers we can see that the only value is 1440 sedentary minutes, the other variables are in zero.

# Remove this cases 
Daily_Activity_df <- Daily_Activity_df %>%
  filter(!(Calories == 0))
# confirm
str(Daily_Activity_df) #366 obs. 
print(length(unique(Daily_Activity_df$Id))) #34 users

# EXTRA:  I checked the ones with 0 total steps because it seemed strange to me that some would have registered minutes of activity. After searching, I found out that the smart devices of Fitbit register the activity minutes with the heart rate of the client, not with the steps. So it is possible to have zero steps and minutes of activity. 
Daily_Activity_df %>%
  filter(TotalSteps == 0) 
#The two cases that showed very active minutes (33 and 20), but zero total steps or distance, were kept because they could be exercises such as Pilates or weight training.

## 5.4 Analyze the relation between total distance and steps when one of them is bigger than zero and the other is zero.

Daily_Activity_df %>%
  filter((TotalSteps == 0 & TotalDistance > 0) | (TotalSteps > 0 & TotalDistance == 0) ) # maintain the 2 rows

## 5.5 Analyze the statistics values of each column at this point.

summary(Daily_Activity_df$TotalSteps)
summary(Daily_Activity_df$TotalDistance)
summary(Daily_Activity_df$TrackerDistance)
summary(Daily_Activity_df$LoggedActivitiesDistance)

Daily_Activity_df %>%
  filter(LoggedActivitiesDistance >0) # gives zero as value

summary(Daily_Activity_df$VeryActiveDistance)
summary(Daily_Activity_df$ModeratelyActiveDistance)
summary(Daily_Activity_df$LightActiveDistance)
summary(Daily_Activity_df$SedentaryActiveDistance)
summary(Daily_Activity_df$VeryActiveMinutes)
summary(Daily_Activity_df$FairlyActiveMinutes)
summary(Daily_Activity_df$LightlyActiveMinutes)
summary(Daily_Activity_df$SedentaryMinutes)
summary(Daily_Activity_df$Calories) # Here we have problems that we will solve after checking the minutes values.

## 5.6 Minutes 
# It is known that the sum of VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes + SedentaryMinutes = 1440 minutes
# If we apply 1440 - (VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes + SedentaryMinutes), it must be around zero.


Daily_Activity_df_raw <- Daily_Activity_df %>%
  mutate(
    Minutos_registrados = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes + SedentaryMinutes,
    Minutos_no_registrados = 1440 - Minutos_registrados
  )

# Tukey rule to remove outliers in total minutes
Q1 <- quantile(Daily_Activity_df_raw$Minutos_no_registrados, 0.25, na.rm = TRUE)
Q3 <- quantile(Daily_Activity_df_raw$Minutos_no_registrados, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR

Daily_Activity_df_raw <- Daily_Activity_df_raw %>% 
  filter(Minutos_no_registrados >= lower & Minutos_no_registrados <= upper)


str(Daily_Activity_df_raw) #358 obs
print(length(unique(Daily_Activity_df_raw$Id))) #34
str(Daily_Activity_df) #366
print(length(unique(Daily_Activity_df$Id))) #34

# Save in csv file the data that does not filter the observations without use for later analysis.

write.csv(Daily_Activity_df, "C:/Proyecto Google/Prepare/Analysis/Daily_Activity_without_filter_of_minutes.csv", row.names = FALSE)
# change the data from Daily_Activity_df_raw to Daily_Activity_df so that we have all the filters.

Daily_Activity_df <- Daily_Activity_df_raw

## 5.7 Calories 

# Analyze the statistical values of the calories

summary(Daily_Activity_df$Calories)  
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#399    1776    2035    2106    2443    4430 


outliers_df <- Daily_Activity_df %>%
  filter(Calories <= 1000)

View(outliers_df) #14 obs.
print(length(unique(outliers_df$Id))) # 14 users

summary(outliers_df$Calories)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#399.0   553.5   725.0   666.0   776.5   938.0 

mean(Daily_Activity_df$Calories) #2106.047

#Remove the atypical values

outliers_df <- outliers_df %>%
  filter(!Calories <= 1000)

Daily_Activity_df <- Daily_Activity_df %>%
  filter(!Calories <= 1000) 

#Confirm
View(outliers_df) #0 obs.
str(Daily_Activity_df) #344 obs. 
print(length(unique(Daily_Activity_df$Id))) #33 users

#STEP 6. Remove extra columns that I make and maintaining the correct values. Then make a new CSV ready to process.

Daily_Activity_df <- Daily_Activity_df %>% #remove extra columns
  select(- Resultado_T_V_M_L) %>%
  select(- Resultado_T_T_L) %>%
  select(- Minutos_no_registrados ) %>%
  select(- Minutos_registrados)

head(Daily_Activity_df) #Check the values
str(Daily_Activity_df) #Check all the columns 344 obs.

# Create a new document with the changes.
Daily_Activity_ready_for_analysis <- Daily_Activity_df
write.csv(Daily_Activity_ready_for_analysis, "C:/Proyecto Google/Prepare/Analysis/Daily_Activity_ready_for_analysis.csv", row.names = FALSE)




