
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
tmp <- dat[dat$yr_mo_dd == "2020-08-01", ]
str(tmp)
ggplot(data = tmp, aes(x = datetime, y = demand_MWHR, group = period_name)) + 
  geom_line()


### SEARCH FOR polynomial fit -----
if(F)
  browseURL("http://www.sthda.com/english/articles/40-regression-analysis/162-nonlinear-regression-essentials-in-r-polynomial-and-spline-regression-models/")
?poly

