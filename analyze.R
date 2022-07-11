## merging data

## daily activity & daily sleep
daily_activity_sleep <- merge(daily_activity, daily_sleep, by=c("Id", "date"))
glimpse(daily_activity_sleep)

## hour calories & hour sleep
hour_calories_steps <- merge(hour_calories, hour_steps, by=c("Id","Time"))
glimpse(hour_calories_steps)




## analysis of daily activity averages of users 
daily_average <- daily_activity_sleep %>%
  group_by(Id) %>%
  summarise (mean_daily_steps = mean(TotalSteps), 
             mean_daily_calories = mean(Calories), 
             mean_daily_sleep = mean(TotalMinutesAsleep)) %>% 
  mutate(user_type = case_when(mean_daily_steps < 5000 ~ "sedentary",
                               mean_daily_steps >= 5000 & 
                                 mean_daily_steps < 7499 ~ "lightly active",
                               mean_daily_steps >= 7500 & 
                                 mean_daily_steps < 9999 ~ "fairly active", 
                               mean_daily_steps >= 10000 ~ "very active" )) %>% 
  mutate(sleep_type = case_when(mean_daily_sleep < 360 ~ "Bad Sleep",
                                mean_daily_sleep >=360 & 
                                  mean_daily_sleep <=480 ~ "Normal Sleep",
                                mean_daily_sleep > 480 ~ "Over Sleep")) %>% 
  mutate(calories_type = case_when(mean_daily_calories < 1900 ~ "sedentary",
                                   mean_daily_calories >=1900 & 
                                     mean_daily_calories <2400 ~ "lightly active",
                                   mean_daily_calories >=2400 &
                                     mean_daily_calories <3000 ~ "fairly active",
                                   mean_daily_calories >=3000 ~ "very active"))

daily_average$user_type <- factor(daily_average$user_type ,
                                  levels = c("very active", "fairly active",
                                             "lightly active", "sedentary"))
daily_average$sleep_type <- factor(daily_average$sleep_type ,
                                  levels = c("Over Sleep", "Normal Sleep",
                                             "Bad Sleep"))
daily_average$calories_type <- factor(daily_average$calories_type ,
                                  levels = c("very active", "fairly active",
                                             "lightly active", "sedentary"))





## plots

##
ggarrange(
  ggplot(daily_average)+
    geom_bar(aes(x= user_type), fill="#006699") +
    labs(title = "User type", x=NULL, y=NULL) +
    theme(legend.position ="none"),
  ggplot(daily_average)+
    geom_bar(aes(x= sleep_type), fill="#85e0e0") +
    labs(title = "Sleep type", x=NULL, y=NULL) +
    theme(legend.position ="none"),
  ggplot(daily_average)+
    geom_bar(aes(x= calories_type), fill="#00abff") +
    labs(title = "Calories type", x=NULL, y=NULL) +
    theme(legend.position ="none")
)
  

ggplot(daily_average) + 
  geom_bar(aes(user_type, fill=user_type)) +
  facet_wrap(~sleep_type) +
  labs(title = "Correlation Between sleep type and steps type", x=NULL, y= NULL)+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1))

ggplot(daily_average) + 
  geom_bar(aes(calories_type, fill=calories_type)) + 
  facet_wrap(~sleep_type) +
  labs(title = "Correlation Between sleep type and calories type", x=NULL, y= NULL)+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1))

ggplot(data=daily_activity, aes(x=TotalSteps, y = Calories))+ 
  geom_point()+ 
  stat_smooth(method=lm)+
  labs(title = "Relation between total steps and calories burned")

ggplot(data=daily_activity_sleep, aes(x=TotalMinutesAsleep, y=SedentaryMinutes)) + 
  geom_point(color='darkblue') + geom_smooth() +
  labs(title="Relation between sleep time and sedentary minutes")





## adding new rows
daily_activity_sleep <- daily_activity_sleep %>% 
  mutate(weekday = weekdays(date))


second_heartrate <- second_heartrate %>% 
  mutate(weekday = weekdays(Time)) %>% 
  mutate(time = strftime(Time, format ="%H" ))


daily_activity_sleep$weekday <-
  ordered(daily_activity_sleep$weekday,
          levels=c("Monday", "Tuesday",
                   "Wednesday", "Thursday",
                   "Friday", "Saturday", "Sunday"))


summarized_weekday <- daily_activity_sleep %>% 
  group_by(weekday) %>% 
  summarise(average_steps = mean(TotalSteps),
            average_distance = mean(TotalDistance),
            sedentary_minutes = mean(SedentaryMinutes),
            average_calories = mean(Calories),
            average_sleep = mean(TotalMinutesAsleep) ) %>% 
  arrange(weekday)
 

summary(summarized_weekday) 

## plots
ggarrange(
  ggplot(summarized_weekday) +
    geom_col(aes(weekday, average_steps), fill = "#006699") +
    geom_hline(yintercept = 10000) +
    labs(title = "Daily steps per weekday", x= "", y = "") +
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1)),
  ggplot(summarized_weekday) +
    geom_col(aes(weekday, average_calories), fill = "#85e0e0") +
    geom_hline(yintercept = 2500) +
    labs(title = "Daily calories per weekday", x= "", y = "") +
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1))
)
ggarrange(
  ggplot(summarized_weekday) +
    geom_col(aes(weekday, average_sleep), fill = "#85e0e0") +
    geom_hline(yintercept = 500) +
    labs(title = "Minutes asleep per weekday", x= "", y = "") +
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1)),
  ggplot(summarized_weekday) +
    geom_col(aes(weekday, sedentary_minutes), fill = "#006699") +
    geom_hline(yintercept = 800) +
    labs(title = "Sendentary Minutes", x= "", y = "") +
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1))
)



hour_calories_steps <- hour_calories_steps %>% 
  mutate(time = strftime(Time, format ="%H" ))

summarized_hour <- hour_calories_steps %>% 
  group_by(time) %>% 
  summarise(steps = mean(StepTotal), calories = mean(Calories))


## plots
ggarrange(
  ggplot(summarized_hour) +
    geom_col(mapping=aes(x=time, y=steps, fill=steps)) +
    labs(title = "Hourly steps throughout the day") + 
    scale_fill_gradient(low = "green", high = "red")+
    theme(legend.position = "none", axis.text.x = element_text(angle = 45)),
  ggplot(summarized_hour) +
    geom_col(mapping=aes(x=time, y=calories, fill=calories)) +
    labs(title = "Hourly steps throughout the day") + 
    scale_fill_gradient(low = "green", high = "red")+
    theme(legend.position = "none", axis.text.x = element_text(angle = 45))
)










