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


add_holiday <- function(.list = list(), holiday.name, holiday.type = c('fixed', 'nth', 'last'), holiday.date, days.before = rep(1, length(holiday.name)), days.after = rep(1, length(holiday.name))){

  holiday.date <- as.Date(holiday.date)

  if(any(length(holiday.name) != length(holiday.type),
         length(holiday.name) != length(holiday.date),
         length(holiday.name) != length(days.before),
         length(holiday.name) != length(days.after))){
    stop('all holiday arguments must have same length')
  }

  for(i in 1:length(holiday.name)){
    holiday.name_i <- holiday.name[i]
    holiday.type_i <- holiday.type[i]
    holiday.date_i <- holiday.date[i]
    days.before_i <- days.before[i]
    days.after_i <- days.after[i]

    if(holiday.type_i == 'fixed'){
      .list[[length(.list)+1]] <- FixedDateHoliday(holiday.name_i,
                                                   month = format(holiday.date_i, '%B'),
                                                   day = as.integer(format(holiday.date_i, '%d')),
                                                   days.before = days.before_i,
                                                   days.after = days.after_i)
    }
    if(holiday.type_i == 'nth'){
      .list[[length(.list)+1]] <- NthWeekdayInMonthHoliday(holiday.name_i,
                                                           month = format(holiday.date_i, '%B'),
                                                           day.of.week = weekdays(holiday.date_i),
                                                           week.number = (as.integer(format(holiday.date_i, '%d')) %/% 7) + 1,
                                                           days.before = days.before_i,
                                                           days.after = days.after_i)
    }
    if(holiday.type_i == 'last'){
      .list[[length(.list)+1]] <- LastWeekdayInMonthHoliday(holiday.name_i,
                                                            month = format(holiday.date_i, '%B'),
                                                            day.of.week = weekdays(holiday.date_i),
                                                            days.before = days.before_i,
                                                            days.after = days.after_i)
    }
  }
  .list
}

holiday_list <- list()

holiday_list <- holiday_list %>%
  add_holiday(holiday.name = 'independence_day', holiday.type = 'fixed', holiday.date = '2020-07-04', days.before = 2, days.after = 1) %>%
  add_holiday('presidents_day', 'nth', '2020-02-17', 1, 3) %>%
  add_holiday('arbor_day', 'last', '2020-04-24')

holiday_list <- list()

holiday.name <- c('ind_day', 'pres_day', 'arbor_day')
holiday.type <- c('fixed', 'nth', 'last')
holiday.date <- c('2020-07-04', '2020-02-17', '2020-04-24')
days.before <- c(1, 1, 1)
days.after <- c(2, 2, 1)

holiday_list %>%
  add_holiday(holiday.name, holiday.type, holiday.date)


## Regression Holiday Models and Hierarchical Regression Holiday Models work with a list
## Random Walk Holiday Models do not

