## Inalial exploreation for electricity data for Melbourne Datathon 2020.
#### Trying to make Tsne of every hh across yr_mo.

## Setup -----
require(tidyverse); require(tsfeatures); require(tsibble); require(lubridate);
require(stringr); require(Rtsne); require("ggplot2"); ## Updated versions as of 20/09/2020
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

dat_wide_hh <- dat %>% ## AGG OVER
  select(compkey_rp, yr_mo_dd, hm, demand_MWHR) %>%
  group_by(compkey_rp, yr_mo_dd, hm) %>%
  summarise(.groups = "drop", demand_MWHR = sum(demand_MWHR, na.rm = TRUE)) %>%
  pivot_wider(names_from = hm, values_from = demand_MWHR)
## REMOVE NAs
#sum(is.na(dat_wide_hh)) ## 60
#sum(!complete.cases(dat_wide_hh)) ## 7
dat_wide_hh <- dat_wide_hh[complete.cases(dat_wide_hh), ]
#sum(is.na(dat_wide_hh)) ## 0
#sum(!complete.cases(dat_wide_hh)) ## 0

#### CREATE TSIBBLE -----
tsib_dat <- dat_wide_hh %>%
  as_tsibble(index = yr_mo_dd, key = compkey_rp) ## need to use compkey, tSK can't handle multiple key columns
# tsib_wlky_agg <- as_tsibble(df_hh_agg, index = yr_mo, key = region)

## tsfeatures vignette:
if (F){
  browseURL("https://cran.r-project.org/web/packages/tsfeatures/vignettes/tsfeatures.html")
  mylist <- list(sunspot.year, WWWusage, AirPassengers, USAccDeaths)
  tsfeatures(mylist)
  ## 1 ts, freq30 min, 2 variables: (demand_MWHR, price_AUD)
}

## FORMAT AS LIST, with index, key, and 1 hh, FOR TS FEATURES
ls_tsib_dat <- list() ## Init
offset <- 3 ## 1 less than the first measure column
i_s <- 1:(ncol(tsib_dat) - offset) ## columns with hh measure
colnms <- colnames(tsib_dat)
for(i in i_s){
  ls_tsib_dat[[i]] <- tsib_dat %>% 
    select(compkey_rp, yr_mo_dd, colnms[i + offset])
}

## CREATE TSF -----
tsf_dat <- tsfeatures::tsfeatures(ls_tsib_dat)
beepr::beep()
## ERR 
# Error: Result 1 must be a single double, not a double vector of length 16
# Run `rlang::last_error()` to see where the error occurred.

# str(raw_tsf)
# skimr::skim(raw_tsf)
## Note that frequency, nperiods, seasonal_period
#### are all the same values
tmp_tsf <- select(raw_tsf, -frequency, -nperiods, -seasonal_period)

#### DECODE TSIBBLE
nms <- str_split(colnames(tsib_hh)[i_s], pattern = "__")
my_tsf <- tibble()
for(i in i_s - 2){
  j <- ceiling(i/2)
  split_vect <- nms[[i]]
  row <- tibble(name = colnames(tsib_hh)[i + 2],
                measure = split_vect[1],
                hh = split_vect[2],
                tmp_tsf[i, ])
  my_tsf <- rbind(my_tsf, row)
}
str(my_tsf)

#### T-SNE -----
num_dat <- my_tsf[, 3:ncol(my_tsf)]
tsne_obj <- Rtsne::Rtsne(num_dat, perplexity = nrow(num_dat)^(1/3),
                         theta = .5, max_iter = 1000)
# str(tsne_out)
proj <- as_tibble(tsne_obj$Y)
df_tsne <- tibble(my_tsf[, 1:3], proj)

## PLOT
ggplot(df_tsne, aes(V1, V2, shape = measure, color = hh)) + geom_point() + theme_minimal()

## REMEMBER WE CANNOT INTERPRET DISTANCES.
#### HOLES TOUR??