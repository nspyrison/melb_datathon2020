## Inalial exploreation for electricity data for Melbourne Datathon 2020.
#### Trying to make Tsne of every hh across yr_mo.

## Setup
require(tidyverse); require(tsfeatures); require(tsibble); require(lubridate);
require(stringr); require(Rtsne); require("ggplot2"); ## Updated versions as of 20/09/2020
set.seed(20200920)

## Read agg data
fp <- "./data/Data_20152020_Vic.csv"
raw <- read.csv2(fp, header = T, sep = ",", check.names = T)
dat <- dplyr::mutate(
  raw,
  .keep = "none",
  region      = as.factor(`ï..REGION`),
  datetime    = as_datetime(dmy_hm(SETTLEMENTDATE), tz = "Australia/Melbourne"),
  yr_mo       = tsibble::yearmonth(datetime),
  yr          = year(datetime),
  mo          = month(datetime),
  hm          = unlist(lapply(strsplit(as.character(raw$SETTLEMENTDATE), " "), FUN =  function(L) L[2])),
  demand_MWHR = as.numeric(TOTALDEMAND),
  price_AUD   = as.numeric(RRP)
)

## tsfeatures vignette:
if (F)
  browseURL("https://cran.r-project.org/web/packages/tsfeatures/vignettes/tsfeatures.html")
## 1 ts, freq30 min, 2 variables: (demand_MWHR, price_AUD)

#### AGG DFS
## GROUP BY REGION*YR*MO*HH, AGGREGATE OVER DAYS (avg of hh of the day within each month) FOR DEMAND AND PRICE.
df_hh_agg <- select(dat, region, yr_mo, hm, demand_MWHR, price_AUD) %>%
  group_by(region, yr_mo, hm) %>%
  summarise(.groups = "drop",
            demand_MWHR = mean(demand_MWHR), price_AUD = mean(price_AUD)) %>%
  pivot_wider(names_from = hm, names_sep = "__", values_from = c(demand_MWHR, price_AUD))

#### CREATE TSIBBLE
tsib_hh_agg <- as_tsibble(df_hh_agg, index = yr_mo, key = region)


### CREATE LIST FOR TSF
ls_tsib_hh <- list()
measure <- NA
hh <- NA
i_s <- 3:(ncol(tsib_hh_agg))
for(i in i_s){
  ls_tsib_hh[[i - 2]] <- select(tsib_hh_agg, region, yr_mo, colnames(tsib_hh_agg)[i])
}
raw_tsf <- tsfeatures::tsfeatures(ls_tsib_hh)
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

#### T-SNE
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