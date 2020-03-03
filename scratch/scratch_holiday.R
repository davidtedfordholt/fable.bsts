library(dplyr)
library(fable)
library(tsibble)
library(tsibbledata)
library(ggplot2)
library(bsts)
source("./R/model.R")

# future::plan(multisession)

## Use pedestrian because we also have easy to access holiday_aus() function
.data <- pedestrian %>%
  index_by(Date) %>%
  group_by(Sensor) %>%
  summarize(Count = sum(Count))

## Plot .data
.data %>%
  ggplot() +
  geom_line(aes(Date, Count), color = 'dodgerblue', alpha = 0.6) +
  facet_wrap(~Sensor, scales = 'free_y') +
  theme_bw()

## View holiday_aus()
holiday_aus(2015:2016)

## Is the idea passing a holiday tbl, or requiring bsts implementation for a holiday list?
## I like a tbl, but seems tricky

july4 <- FixedDateHoliday("July4", "July", 4)
memorial.day <- LastWeekdayInMonthHoliday("MemorialDay", "May", "Monday")
labor.day <- NthWeekdayInMonthHoliday("LaborDay", "September", "Monday", 1)
another.way.to.get.memorial.day <- NamedHoliday("MemorialDay")
easter <- NamedHoliday("Easter")
winter.olympics <- DateRangeHoliday("WinterOlympicsSince2000",
                                    start = as.Date(c("2002-02-08",
                                                      "2006-02-10",
                                                      "2010-02-12",
                                                      "2014-02-07",
                                                      "2018-02-07")),
                                    end = as.Date(c("2002-02-24",
                                                    "2006-02-26",
                                                    "2010-02-28",
                                                    "2014-02-23",
                                                    "2018-02-25")))

## Independence Day (fixed)
holiday.date <- as.Date('2020-07-04')

## Presidents day (nth)
holiday.date <- as.Date('2020-02-17')

## Arbor Day (last)
holiday.date <- as.Date('2020-04-24')

## Ideally, we pass a holiday and a known instance (date) of the holiday, and the rest is parsed for us
## Still must pass days before/after, and whether it's a last weekday holiday, nth weekday, fixed, etc


add_holiday <- function(.list = list(), holiday.name, holiday.type = c('fixed', 'nth', 'last'), holiday.date, days.before = 1, days.after = 1){
  holiday.date <- as.Date(holiday.date)
  if(holiday.type == 'fixed'){
    .list[[length(.list)+1]] <- FixedDateHoliday(holiday.name,
                     month = format(holiday.date, '%B'),
                     day = as.integer(format(holiday.date, '%d')),
                     days.before = days.before,
                     days.after = days.after)
  }
  if(holiday.type == 'nth'){
    .list[[length(.list)+1]] <- NthWeekdayInMonthHoliday(holiday.name,
                             month = format(holiday.date, '%B'),
                             day.of.week = weekdays(holiday.date),
                             week.number = (as.integer(format(holiday.date, '%d')) %/% 7) + 1,
                             days.before = days.before,
                             days.after = days.after)
  }
  if(holiday.type == 'last'){
    .list[[length(.list)+1]] <- LastWeekdayInMonthHoliday(holiday.name,
                              month = format(holiday.date, '%B'),
                              day.of.week = weekdays(holiday.date),
                              days.before = days.before,
                              days.after = days.after)
  }
  .list
}

holiday_list <- list()

holiday_list <- holiday_list %>%
  add_holiday(holiday.name = 'independence_day', holiday.type = 'fixed', holiday.date = '2020-07-04', days.before = 2, days.after = 1) %>%
  add_holiday('presidents_day', 'nth', '2020-02-17', 1, 3) %>%
  add_holiday('arbor_day', 'last', '2020-04-24')

## Regression Holiday Models and Hierarchical Regression Holiday Models work with a list
## Random Walk Holiday Models do not

