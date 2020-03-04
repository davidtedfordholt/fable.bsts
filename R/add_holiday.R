#' Create a holiday list object for bsts from a data frame or explicit values
#'
#' @param .list a holiday list object to append to (defaults to list())
#' @param holiday.name a string or character vector of holiday names
#' @param holiday.type a string or character vector of holiday types (must be 'fixed', 'nth', or 'last')
#' @param holiday.date a date or date vector of holiday dates
#' @param days.before an integer or integer vector of days of effect before the holiday
#' @param days.after an integer or integer vector of days of effect after the holiday
#' @param holiday.df a holiday dataframe including the above columns (defaults to NULL)
#'
#' @return a holiday list object for use in bsts
#'
#' @import tidyr
#' @import purrrlyr by_row
#' @export
#'
#' @examples
#' # Pass it individual values
#'
#' holiday_list <- list()
#'
#' holiday_list %>%
#'   add_holiday(holiday.name = 'independence_day', holiday.type = 'fixed', holiday.date = '2020-07-04', days.before = 2, days.after = 1) %>%
#'   add_holiday('presidents_day', 'nth', '2020-02-17', 1, 3) %>%
#'   add_holiday('arbor_day', 'last', '2020-04-24')
#'
#' # Pass it vectors
#'
#' holiday_list <- list()
#'
#' holiday.name <- c('ind_day', 'pres_day', 'arbor_day')
#' holiday.type <- c('fixed', 'nth', 'last')
#' holiday.date <- c('2020-07-04', '2020-02-17', '2020-04-24')
#' days.before <- c(1, 1, 1)
#' days.after <- c(2, 2, 1)
#'
#' holiday_list %>%
#'   add_holiday(holiday.name, holiday.type, holiday.date)
#'
#' ## Pass it a df
#'
#' holiday_list <- list()
#'
#' holidf <- tibble(holiday.name, holiday.type, holiday.date, days.before, days.after)
#'
#' holiday_list %>%
#'   add_holiday(holiday.df = holidf)
#'
#'
#'
#'
add_holiday <- function(.list = list(),
                        holiday.name,
                        holiday.type = c('fixed', 'nth', 'last'),
                        holiday.date,
                        days.before = rep(1, length(holiday.name)),
                        days.after = rep(1, length(holiday.name)),
                        holiday.df = NULL){

  # coerce args as df if holiday.df is null
  if(is.null(holiday.df)){
    # kick out if different lengths
    if(any(length(holiday.name) != length(holiday.type),
           length(holiday.name) != length(holiday.date),
           length(holiday.name) != length(days.before),
           length(holiday.name) != length(days.after))){
      stop('all holiday arguments must have same length')
    }

    holiday.df <- tibble(holiday.name, holiday.type, holiday.date, days.before, days.after)
  }

  # check for NAs in days.before and days.after
  if(any(is.na(holiday.df$days.before))){
    warning(sprintf('days.before is NA in row %s, defaulting to 1', paste0(which(is.na(holiday.df$days.before)), sep = ', ')))
    holiday.df$days.before <- tidyr::replace_na(holiday.df$days.before, 1)
  }

  if(any(is.na(holiday.df$days.after))){
    warning(sprintf('days.after is NA in row %s, defaulting to 1',
                    paste0(which(is.na(holiday.df$days.after)), sep = ', ')))
    holiday.df$days.after <- tidyr::replace_na(holiday.df$days.after, 1)
  }

  # Kick out if any rows are incomplete
  if(any(!complete.cases(holiday.df))){
    stop(sprintf('add_holiday requires complete cases: row %s incomplete',
                 paste0(which(!complete.cases(holiday.df)), sep = ', ')))
  }

  # convert holiday.date to date column
  holiday.df$holiday.date <- as.Date(holiday.df$holiday.date)


  int_func <- function(holiday.df){
    holiday.name <- holiday.df$holiday.name
    holiday.type <- holiday.df$holiday.type
    holiday.date <- holiday.df$holiday.date
    days.before <- holiday.df$days.before
    days.after <- holiday.df$days.after
    if(holiday.type == 'fixed'){
      res <- FixedDateHoliday(holiday.name,
                              month = format(holiday.date, '%B'),
                              day = as.integer(format(holiday.date, '%d')),
                              days.before = days.before,
                              days.after = days.after)
    }
    if(holiday.type == 'nth'){
      res <- NthWeekdayInMonthHoliday(holiday.name,
                                      month = format(holiday.date, '%B'),
                                      day.of.week = weekdays(holiday.date),
                                      week.number = (as.integer(format(holiday.date, '%d')) %/% 7) + 1,
                                      days.before = days.before,
                                      days.after = days.after)
    }
    if(holiday.type == 'last'){
      res <- LastWeekdayInMonthHoliday(holiday.name,
                                       month = format(holiday.date, '%B'),
                                       day.of.week = weekdays(holiday.date),
                                       days.before = days.before,
                                       days.after = days.after)
    }
    res
  }

  .list <- append(.list, purrrlyr::by_row(holiday.df, int_func)$.out)
  .list
}

