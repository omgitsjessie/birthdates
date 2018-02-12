library(ggplot2)
library(dplyr)
library(plyr)
library(lubridate)

#Read in birthdate data
US_births_1994.2003_CDC_NCHS <- read.csv("US_births_1994-2003_CDC_NCHS.csv")
US_births_2000.2014_SSA <- read.csv("US_births_2000-2014_SSA.csv")

#Create date type field in each set
US_births_1994.2003_CDC_NCHS$date <- paste0(US_births_1994.2003_CDC_NCHS$month, "-", 
                                            US_births_1994.2003_CDC_NCHS$date_of_month, "-",
                                            US_births_1994.2003_CDC_NCHS$year) %>%
                                                as.Date("%m-%d-%Y")
US_births_1994.2003_CDC_NCHS$month_day <- paste0(US_births_1994.2003_CDC_NCHS$month, "-", 
                                          US_births_1994.2003_CDC_NCHS$date_of_month) %>%
                                              as.Date("%m-%d")

#tbh I don't use this data.  recreate with these data next?
US_births_2000.2014_SSA$date <- paste0(US_births_2000.2014_SSA$month, "-", 
                                       US_births_2000.2014_SSA$date_of_month, "-",
                                       US_births_2000.2014_SSA$year)
US_births_2000.2014_SSA$date <- as.Date(US_births_2000.2014_SSA$date, "%m-%d-%Y")


#Look at time series over the ten years
ggplot(US_births_1994.2003_CDC_NCHS, aes(date, births)) + geom_point() +
  xlab("Time") + ylab("Births") + ggtitle("US Births per day, from 1994 to 2004") + 
  theme_bw()

#rename factor levels for day of week.
US_births_1994.2003_CDC_NCHS$day_of_week <- as.factor(US_births_1994.2003_CDC_NCHS$day_of_week)
levels(US_births_1994.2003_CDC_NCHS$day_of_week) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

#collapse to be over a year - scatter looks goofy; look at days of week!
ggplot(US_births_1994.2003_CDC_NCHS, aes(month_day, births)) + geom_point(aes(color = day_of_week)) +
  xlab("Month") + ylab("Births") + ggtitle("Ten years of births, by month") + 
  scale_x_date(date_labels = "%B", date_breaks = "1 month") + 
  theme_bw()

#Even over ten years, there's a clear day-of-week trend where births aren't 
#scheduled on weekends.  And probably some non-weekend holidays.  
avg_births <- US_births_1994.2003_CDC_NCHS %>%
  group_by(month_day) %>%
  summarise(avg_births = mean(births))

#Plot showing estimated conception dates collapsed to be over a year
#without removing weekly & monthly scheduling artifacts
ggplot(avg_births, aes(month_day, avg_births)) + geom_line() + 
  xlab("") + ylab("conceptions") + ggtitle("Avg Birth Dates, collapsed over a year") + 
  scale_x_date(date_labels = "%B", date_breaks = "1 month") + 
  theme_bw()

#Look at time series decomposition effects due to different seasonality.
#remove weekday trend.
births_ts2 <- ts(US_births_1994.2003_CDC_NCHS$births, frequency = 7, start = c(1,1))
plot.ts(births_ts2)
birthscomp2 <- decompose(births_ts2)
plot(birthscomp2)
#remove weekday trend, look at month trend.
births_ts3 <- births_ts2 - birthscomp2$seasonal
plot(births_ts3)
births_ts4 <- ts(births_ts3, frequency = 30.4, start = c(1,1))
births_ts4_decomp <- decompose(births_ts4)
plot(births_ts4_decomp)
#Separate out annual trend, that's what you want (but just one cycle).
births_ts5 <- births_ts4 - births_ts4_decomp$seasonal
births_ts6 <- ts(births_ts5, frequency = 365.25, start = c(1,1))
births_ts6_decomp <- decompose(births_ts6)
plot(births_ts6_decomp)

#trend in birthrate from 1994 to 2004
plot(births_ts6_decomp$trend)

#annual trend in birthrate, each year
annual_birthrates <- births_ts6_decomp$seasonal[1:365] %>% data.frame()
names(annual_birthrates) <- c("effect")
annual_birthrates$date <- seq(as.Date("2018/1/1"), by = "day", length.out = 365)
ggplot(annual_birthrates, aes(date, effect)) + geom_smooth(span = 0.1, se = FALSE) + 
  xlab("") + ylab("Birth Trend") + ggtitle("Annual Birthrate Trend, 1994-2004") + 
  scale_x_date(date_labels = "%B", date_breaks = "1 month") + 
  theme_bw() + 
  #Annotate holidays in US between 1994-2004
  #Christmas 12/24 - 12/26
  #Thanksgiving 11/22 - 11/29
  #Labor Day 9/1 - 9/7
  #4th of July 7/4
  #Memorial Day 5/24 - 5/30
  annotate("rect", xmin = as.Date("2018-01-01"), xmax = as.Date("2018-01-03"), ymin = -1500, ymax = -750, alpha = 0.2) +
  annotate("text", x=as.Date("2018-01-02"), y=-1500, label="New Year's") +
  annotate("rect", xmin = as.Date("2018-05-24"), xmax = as.Date("2018-05-30"), ymin = -250, ymax = 0, alpha = 0.2) +
  annotate("text", x=as.Date("2018-05-27"), y=-375, label="Memorial Day") +
  annotate("rect", xmin = as.Date("2018-07-04"), xmax = as.Date("2018-07-06"), ymin = 125, ymax = 375, alpha = 0.2) + 
  annotate("text", x=as.Date("2018-07-05"), y=50, label="4th of July") +
  annotate("rect", xmin = as.Date("2018-09-01"), xmax = as.Date("2018-09-07"), ymin = 200, ymax = 450, alpha = 0.2) + 
  annotate("text", x=as.Date("2018-09-05"), y=125, label="Labor Day") +
  annotate("rect", xmin = as.Date("2018-11-22"), xmax = as.Date("2018-11-29"), ymin = -625, ymax = -375, alpha = 0.2) +  
  annotate("text", x=as.Date("2018-11-27"), y=-700, label="Thanksgiving") +
  annotate("rect", xmin = as.Date("2018-12-24"), xmax = as.Date("2018-12-26"), ymin = -500, ymax = -250, alpha = 0.2) +  
  annotate("text", x=as.Date("2018-12-25"), y=-550, label="Christmas")
  
#monthly trends in birthrate, each month
monthly_birthrates <- births_ts4_decomp$seasonal[1:30] %>% data.frame()
names(monthly_birthrates) <- c("effect")
monthly_birthrates$date <- seq(1, by = 1, length.out = 30)
ggplot(monthly_birthrates, aes(date, effect)) + geom_smooth(span = 0.5) + 
  xlab("") + ylab("Birth Trend") + ggtitle("Monthly Birthrate Trend, 1994-2004") + 
  scale_x_continuous(minor_breaks = c(5, 15, 25)) + 
  theme_bw()
#TODO - does this make sense?  valleys @ 4th & 26th of each month?


#weekly trends in birthrate, each week
weekly_birthrates <- birthscomp2$seasonal[1:7] %>% data.frame()
names(weekly_birthrates) <- c("effect")
weekly_birthrates$date <- seq(as.Date("1994/1/1"), by = "day", length.out = 7)
ggplot(weekly_birthrates, aes(date, effect)) + geom_smooth(se = FALSE) + 
  xlab("Day of Week") + ylab("Birth Trend") + ggtitle("Weekly Birthrate Trend, 1994-2004") + 
  scale_x_date(date_labels = "%A", date_breaks = "1 day") + 
  theme_bw()

#Specifically looking at annual trend in birthdate, shift the plot so that it
#becomes annual trends in "conception date".  

#Smooth over Holiday bands that may impact birthdate by avoiding long weekends.
#Create a table indicating holiday intervals that we will smooth over.
holidays <- data.frame("holiday" = c("New Year", "Memorial Day", "July 4th", "Labor Day", "Thanksgiving", "Christmas"), 
                       "StartDate" = c("2018-01-01", "2018-05-24", "2018-07-04", "2018-09-01", "2018-11-22", "2018-12-24"), 
                       "EndDate" = c("2018-01-03", "2018-05-30", "2018-07-06", "2018-09-07", "2018-11-29", "2018-12-26"))

#Make a copy of annual_birthrates so you don't throw away data you want later.
annual_birthrates_noscheduling <- annual_birthrates

#For each holiday segment, remove effect data during that holiday. 
for (i in 1:nrow(holidays)) {
  for (j in 1:nrow(annual_birthrates_noscheduling)) {
    if (annual_birthrates_noscheduling[j, "date"] >= as.Date(holidays[i, "StartDate"]) && 
        annual_birthrates_noscheduling[j, "date"] <= as.Date(holidays[i, "EndDate"])) {
          annual_birthrates_noscheduling[j, "effect"] <- NA
    }
  }
}

#Plot annual_birthrate trend again, using set with removed holiday scheduling effect.
ggplot(annual_birthrates_noscheduling, aes(date, effect)) + geom_smooth(span = 0.1, se = FALSE) + 
  xlab("Month") + ylab("Birth Trend") + ggtitle("Annual Birthrate Trend, 1994-2004.  Holiday Birth Scheduling Effects Removed.") + 
  scale_x_date(date_labels = "%B", date_breaks = "1 month") + 
  theme_bw()


#Shift everything back 280 days to look at conception trends.
#Split data into columns
conception_date <- annual_birthrates_noscheduling$date
birth_effect <- annual_birthrates_noscheduling$effect

#Split effect column at the 280 day mark.
conception_effect_pt1 <- birth_effect[1:280]
conception_effect_pt2 <- birth_effect[281:365]

#Reorder the halves of the effect col.
conception_effect <- c(conception_effect_pt2, conception_effect_pt1) %>% data.frame()
#cbind effect back to the ordered date vector
annual_conception <- cbind(conception_date, conception_effect)
names(annual_conception) <- c("date", "effect")

#Plot again. Now you have conception trends!
ggplot(annual_conception, aes(date, effect)) + geom_smooth(span = 0.1, se = FALSE) + 
  xlab("Month") + ylab("Conception Trend") + ggtitle("Annual Conception Trend, using birth data from 1994-2004.") + 
  scale_x_date(date_labels = "%B", date_breaks = "1 month") + 
  theme_bw()

