## importing data
daily_activity   <- read.csv("C:/Users/Vamshi/OneDrive/Desktop/2 casestudy/dailyActivity_merged.csv")
hour_calories    <- read.csv("C:/Users/Vamshi/OneDrive/Desktop/2 casestudy/hourlyCalories_merged.csv")
hour_steps       <- read.csv("C:/Users/Vamshi/OneDrive/Desktop/2 casestudy/hourlySteps_merged.csv")
daily_sleep      <- read.csv("C:/Users/Vamshi/OneDrive/Desktop/2 casestudy/sleepDay_merged.csv")
second_heartrate <- read.csv("C:/Users/Vamshi/OneDrive/Desktop/2 casestudy/heartrate_seconds_merged.csv")





## verifying number of users
n_distinct(daily_activity$Id)
n_distinct(daily_sleep$Id)
n_distinct(hour_steps$Id)
n_distinct(hour_calories$Id)
n_distinct(second_heartrate$Id)

##  checking for duplicates
sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
sum(duplicated(hour_steps))
sum(duplicated(hour_calories))
sum(duplicated(second_heartrate))

## Removing duplicates
daily_sleep <- daily_sleep %>%
  distinct() %>%
  drop_na()
sum(duplicated(daily_sleep))





## changing data types
daily_activity <- daily_activity %>%
  rename(date = ActivityDate) %>%
     mutate(date = as_date(date, format = "%m/%d/%Y"))

daily_sleep <- daily_sleep %>% 
  rename(date = SleepDay) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))

hour_calories <- hour_calories %>% 
  rename(Time = ActivityHour) %>% 
  mutate(Time = as_datetime(Time, format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))

second_heartrate <- second_heartrate %>% 
  mutate(Time = as_datetime(Time, format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))

hour_steps <- hour_steps %>% 
  rename(Time = ActivityHour) %>% 
  mutate(Time = as_datetime(Time, format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))





## Summarizing data
## activity
daily_activity %>%  
  select(TotalSteps,
         TotalDistance,
         Calories) %>%
  summary()

## explore number of active minutes per category
daily_activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, 
         LightlyActiveMinutes, SedentaryMinutes) %>%
  summary()

## calories
hour_calories %>%
  select(Calories) %>%
  summary()

## sleep
daily_sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()

## steps
hour_steps %>% 
  select(StepTotal) %>% 
  summary()

## heartrate
second_heartrate %>% 
  select(Value) %>% 
  summary()
