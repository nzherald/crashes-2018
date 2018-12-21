library(tidyverse)
library(ggthemes)
library(RPostgreSQL)
library(zoo)
library(lubridate)
library(here)
library(knitr)
library(stats)

db <- dbConnect(PostgreSQL(), dbname='crashes-18')


prev_xmas <- function(day) { ymd(paste(year(day) - 1, "12-24", sep='-'))}
xmas <- function(day) { ymd(paste(year(day), "12-24", sep='-')) }
jan_season <- function(day, hour, end) {
  case_when(
    yday(day) < end ~ prev_xmas(day),
    yday(day) == end & hour < 6 ~ prev_xmas(day),
    TRUE ~ xmas(day)
  )
}

crashes <- tbl(db, "crashes") %>%
  collect() %>%
  mutate(xmasEve = case_when(
    month(day) != 1 ~ xmas(day),
    month(day) == 1 ~ case_when(
      wday(prev_xmas(day)) %in% c(1,2,7) ~ jan_season(day, hour, 3),
      wday(prev_xmas(day)) == 2 ~ jan_season(day, hour, 4),
      TRUE ~ jan_season(day, hour, 5)
    )
  ),
  xmas = case_when(
    month(day) == 1 & year(day) != year(xmasEve) ~ TRUE,
    month(day) == 12 & mday(day) > 24 ~ TRUE,
    month(day) == 12 & mday(day) == 24 & hour >= 16 ~ TRUE,
    TRUE ~ FALSE
  ),
  xmasYear = case_when(
    month(day) <= 6 ~ year(day) - 1,
    TRUE ~ year(day)
  ),
  period = case_when(
    hour < 6 ~ 0,
    hour >= 6 & hour < 12 ~ 1,
    hour >= 12 & hour < 18 ~ 2,
    hour >= 16 ~ 3,
    TRUE ~ -1
  ),
  md = paste(month(day), mday(day), sep='-'),
  fakeTime = case_when(
    period == -1 ~ ymd_h(paste(paste('1900', month(day), mday(day), sep='-'), 1)),
    month(day) == 12 ~ ymd_h(paste(paste('2019', month(day), mday(day), sep='-'), period * 6 + 3)),
    TRUE ~ ymd_h(paste(paste('2020', month(day), mday(day), sep='-'), period * 6 + 3))
  ),
  fakeDay = ymd(paste('2020', month(day), mday(day), sep='-'))
  )

christmas <- crashes %>%
  group_by(xmasEve) %>%
  summarise(fatal = sum(count[severity == 'Fatal' & !xmas])/sum(count[!xmas]),
            xmas_fatal = sum(count[severity == 'Fatal' & xmas])/sum(count[xmas]),
            fatal_severe = sum(count[severity %in% c('Fatal','Serious') & !xmas])/sum(count[!xmas]),
            xmas_fatal_severe = sum(count[severity %in% c('Fatal','Serious') & xmas])/sum(count[xmas]),
            ratio = xmas_fatal/fatal,
            ratio_sev = xmas_fatal_severe/fatal_severe
  )


christmas2 <- crashes %>%
  group_by(xmasYear) %>%
  summarise(fatal = sum(count[severity == 'Fatal' & !xmas])/sum(count[!xmas]),
            xmas_fatal = sum(count[severity == 'Fatal' & xmas])/sum(count[xmas]),
            fatal_severe = sum(count[severity %in% c('Fatal','Serious') & !xmas])/sum(count[!xmas]),
            xmas_fatal_severe = sum(count[severity %in% c('Fatal','Serious') & xmas])/sum(count[xmas]),
            ratio = xmas_fatal/fatal,
            ratio_sev = xmas_fatal_severe/fatal_severe
  )


newyears <- crashes %>%
  filter(md == '1-1')

nym_yearly <- newyears %>% 
  filter(period == 0) %>%
  group_by(Year = ymd(paste(year(day),1,1,sep='-')), Severity=severity) %>%
  summarise(Count=sum(count,na.rm=T))

nym_yearly %>%
  spread(Severity, Count, fill=0) %>%
  rename_all(tolower) %>%
  rename(nonInjury=`non-injury`) %>%
  write_csv(here('data/nym_yearly.csv'))

kable(newyears %>% filter(period==0) %>% group_by(severity) %>% summarise(count=sum(count)))

crashes %>% filter(xmas & day > '2000-02-02') %>%
  group_by(xmasYear,severity) %>%
  summarise(count=sum(count)) %>%
  spread(severity, count, fill=0) %>%
  rename_all(tolower) %>%
  rename(nonInjury=`non-injury`) %>%
  write_csv(here('data/yearly.csv'))

crashes %>% filter(xmas & period != -1) %>%
  group_by(fakeTime,severity) %>%
  summarise(count=sum(count)) %>%
  spread(severity, count, fill=0) %>%
  rename_all(tolower) %>%
  rename(nonInjury=`non-injury`) %>%
  write_csv(here('data/xmas_periods.csv'))

# predictions

daily.fatal <- crashes %>% filter(year(day) != 2018 & severity=='Fatal') %>% group_by(fakeDay) %>% summarise(count=sum(count)/18)
daily.serious<- crashes %>% filter(year(day) != 2018 & severity=='Serious') %>% group_by(fakeDay) %>% summarise(count=sum(count)/18)
daily.minor <- crashes %>% filter(year(day) != 2018 & severity=='Minor') %>% group_by(fakeDay) %>% summarise(count=sum(count)/18)
daily.non <- crashes %>% filter(year(day) != 2018 & severity=='Non-injury') %>% group_by(fakeDay) %>% summarise(count=sum(count)/18)

# There is a real discontinuity in the data between December and Janurary
# My guess is that it is a data collection issue - but I'd like to explore it more later ...
pred_daily <- function(daily1) {
  daily.lo <- loess(count ~ as.numeric(fakeDay), daily1)
  round(predict(daily.lo),4)
}

pred.fatal <- pred_daily(daily.fatal)
pred.serious <- pred_daily(daily.serious)
pred.minor <- pred_daily(daily.minor)
pred.non <- pred_daily(daily.non)

trends <- tibble(faketime=seq.Date(as.Date('2020-01-01'), as.Date('2020-12-31'), 1),
       fatal=pred.fatal,
       minor=pred.minor,
       nonInjury=pred.non,
       serious=pred.serious) 

trends %>% write_csv(here("data/daily_trend.csv"))
