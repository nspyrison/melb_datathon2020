
## Setup -----
require(tidyverse); require(lubridate);
require(stringr); require("ggplot2"); ## Updated versions as of 20/09/2020
set.seed(20200920)

## VIC covid restrictions timeline
timeline <- data.frame(
  period_name  = c("firstcase-stage1", "stages1-3", "stage4-step2"),
  description  = c("first AUS case thru stage 1",
                   "stage 1 thru end of stage 3",
                   "stage 4 thru road map step 2 (first loosening for melb metro)"),
  start_ymd    = lubridate::ymd(c("2020-01-19", "2020-03-23", "2020-08-02")),
  end_ymd      = lubridate::ymd(c("2020-03-23", "2020-08-02", "2020-09-28")),
  duration_day = c(64L, 132L, 57L),
  start_source = c("https://www.health.gov.au/ministers/the-hon-greg-hunt-mp/media/first-confirmed-case-of-novel-coronavirus-in-australia#:~:text=The%20patient%2C%20a%20man%20from,these%20processes%20have%20been%20activated.%E2%80%9D",
                   "https://www.minterellison.com/articles/covid-19-stage-1-restrictions",
                   "https://7news.com.au/lifestyle/health-wellbeing/stage-four-restrictions-officially-announced-in-victoria-as-coronavirus-crisis-continues-c-1210947")
)
## Read agg data -----
fp <- "./data/Data_20152020_Vic.csv"
raw <- read.csv2(fp, header = T, sep = ",", check.names = T)
dat <- dplyr::mutate(
  raw,
  .keep = "none",
  region      = as.factor(`ï..REGION`),
  datetime    = as_datetime(dmy_hm(SETTLEMENTDATE), tz = "Australia/Melbourne"),
  period_name = 
    case_when(datetime < timeline$start_ymd[1] ~ "before first AUS case (2020-01-19)",
              datetime >= timeline$start_ymd[1] &
                datetime < timeline$end_ymd[1] ~ timeline$period_name[1],
              datetime >= timeline$start_ymd[2] &
                datetime < timeline$end_ymd[2] ~ timeline$period_name[2],
              datetime >= timeline$start_ymd[3] &
                datetime < timeline$end_ymd[3] ~ timeline$period_name[3],
              datetime > timeline$end_ymd[3] ~ "after stage4, step2 (2020-09-28)"
    ),
  yr_mo       = tsibble::yearmonth(datetime),
  yr          = year(datetime),
  mo          = month(datetime),
  ww          = week(datetime),
  dd          = day(datetime),
  yr_mo_dd    = as_datetime(paste(yr, mo, dd, sep = "-"), tz = "Australia/Melbourne"),
  hm          = unlist(lapply(strsplit(as.character(raw$SETTLEMENTDATE), " "), FUN =  function(L) L[2])),
  demand_MWHR = as.numeric(TOTALDEMAND),
  price_AUD   = as.numeric(RRP),
  compkey_rp = paste0(region, ".", period_name),
  compkey_rpd = paste0(region, ".", datetime, ".", period_name)
)
str(dat)
skimr::skim(dat)
# d <- 48*365.25 *6 ## Half hours in 6 years, but 2020, not done
# round(100*nrow(dat)/d, 1) ## 95.1 pct of 6 years of data.


#### 1 day example
tmp <- dat[dat$yr_mo_dd > "2019-08-01" & dat$yr_mo_dd < "2019-08-08", ]
# str(tmp)
ggplot(data = tmp, aes(x = datetime, y = demand_MWHR, group = period_name)) +
  geom_line()



### SEARCH FOR polynomial fit -----
if(F){
  browseURL("http://www.sthda.com/english/articles/40-regression-analysis/162-nonlinear-regression-essentials-in-r-polynomial-and-spline-regression-models/#spline-regression")
  ?smooth.spline
}
## With knots
# knots <- quantile(tmp$datetime, p = 1:7/8)
# model <- lm(tmp$demand_MWHR ~ bs(tmp$datetime, knots = knots), data = tmp)
## Without knots
# model <- lm(tmp$demand_MWHR ~ bs(tmp$datetime, df = 7), data = tmp)
# model$coefficients

#### TMP dat smooth.spline
library(splines)
set.seed(123)
## Build the spline model

## View as plot, with split (7 df per day)
ndays <- length(unique(tmp$yr_mo_dd))
ggplot(tmp, aes(datetime, demand_MWHR, group = period_name) ) +
  geom_line() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 6 * ndays))
 
## Add the poly nomial eq
# library(ggpmisc)
# stat_poly_eq(parse = T, aes(label = ..eq.label..), formula = y ~ splines::bs(x, df = 7))


## Reduce to df containing prime and prime2
my_spline <- smooth.spline(x = tmp$datetime, y=tmp$demand_MWHR, df = 6 * ndays)
prime <-  predict(my_spline, deriv = 1)
prime2 <- predict(my_spline, deriv = 2)
df_spline <- data.frame(
  datetime = tmp$datetime,
  demand_MWHR = tmp$demand_MWHR,
  x = model.prime$x,
  y_prm  = prime$y,
  y_prm2 = prime2$y
)
# plot(x = df_spline$datetime, y = df_spline$demand_MWHR)
# plot(x = df_spline$datetime, y = df_spline$y.prm, type = "l")
# abline(h=0)
# plot(x = df_spline$datetime, y = df_spline$y.prm2, type = "l")
# abline(h=0)

## Solve for the zero crossing (roots)
library(rootSolve)
POSIXct2AEST <- function(POSIXct){
  as_datetime(
    as.POSIXct(POSIXct, origin = "1970-01-01", tz = "Australia/Melbourne"),
    tz = "Australia/Melbourne")
}

roots_y_prm <- approxfun(df_spline$x, df_spline$y_prm) %>% 
  ## find roots for out approximate function
  uniroot.all(interval = range(df_spline$x)) #%>%
  ## Convert POSIXct int to datetime [AEST]
  # POSIXct2AEST()

roots_y_prm2 <- approxfun(df_spline$x, df_spline$y_prm2) %>% 
  ## find roots for out approximate function
  uniroot.all(interval = range(df_spline$x))
  #%>%
  # ## Convert POSIXct int to datetime [AEST]
  # POSIXct2AEST()
## Remove first and last pts
roots_y_prm2 <- roots_y_prm2[2:(length(roots_y_prm2)-1)]
## Estimate the demand at the roots of y prime^2
#### TODO I DON'T THINK THIS IS RIGHT.
roots_est_demand <- predict(my_spline, x = roots_y_prm2)

roots_y_prm
roots_y_prm2
roots_est_demand


## Plot the peaks and valleies.
ggplot(data.frame(roots_est_demand), aes(POSIXct2AEST(x), y) ) +
  geom_line() +
  geom_vline(xintercept = roots_y_prm2)

## Find the indices for morning onset and evening offset
evening_srt_ind <- seq(from = 1, to = nroots, by = 4)
## moring set is 2 to 3, 6 to 7, 10 to 11, (4 apart)
## evening offset is 5 to 6, 9 to 10, (4 apart)
## Evening offset is to this +1,
## Morning onset is this+1 to this+2
full_ind <- sort(c(evening_srt_ind, evening_srt_ind + 1, evening_srt_ind + 2))

## All idecies needed
df_roots_prm2 <- data.frame(datetime = POSIXct2AEST(roots_est_demand$x[full_ind]),
                            est_demand = roots_est_demand$y[full_ind])
nr <- nrow(df_roots_prm2)
name <- rep(c("Evening high", "Nightly low", "Morning high"), length.out = nr)
name <-  c("Evening high", "Nightly low", "Morning high")
name[.ind]
df_roots_prm2 %>% 
  mutate(yr = year(datetime),
         mo = month(datetime),
         dd = day(datetime),
         yr_mo_dd = as_datetime(paste(yr, mo, dd, sep = "-"), 
                                tz = "Australia/Melbourne"),
         name = rep(c("Evening high", "Nightly low", "Morning high"), length.out = nr)
  ) %>% select(yr_mo_dd, name, est_demand) %>%
  pivot_wider(names_from = name, values_from = est_demand) %>% 
  mutate(evening_diff = `Evening high` - `Nightly low`,
         morning_diff = `Morning high` - `Nightly low`)
  
# u_yrmodd <- data.frame(yrmodd = unique(tmp$yr_mo_dd))
# .df_name <-data.frame(
#   name = rep(c("Evening high", "Nightly low", "Morning high"), 
#              lenght.out = nrow(fj)))
# fj <- full_join(u_yrmodd, .df_name, by = character())
# fj$est_demand_MWHR <- roots_est_demand$y
# tib_demand <- select(fj, -ind) %>% 
#   pivot_wider(names_from = name, values_from = est_demand_MWHR, values_fn = {mean}) %>% 
#   mutate(evening_diff = `Evening high` - `Nightly low`
#          morning_diff = 

## Find the duration

data.frame(u_yrmodd, )


