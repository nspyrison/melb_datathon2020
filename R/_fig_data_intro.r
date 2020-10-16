
## Setup -----
require(tidyverse); require(lubridate); 
require(stringr); require(ggplot2); ## Updated versions as of 20/09/2020
require(ggpmisc)   ## Call outs, annotations, and overlays
require(ggformula) ## geom_spline
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
  datetime    = as.POSIXct(dmy_hm(SETTLEMENTDATE)), #, tz = "Australia/Melbourne"),
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
  POSIXt_seconds = as.integer(datetime),
  yr_mo       = tsibble::yearmonth(datetime),
  yr          = year(datetime),
  mo          = month(datetime),
  ww          = week(datetime),
  dd          = day(datetime),
  yr_mo_dd    = as_datetime(paste(yr, mo, dd, sep = "-")), #tz = "Australia/Melbourne"),
  hm          = unlist(lapply(strsplit(as.character(raw$SETTLEMENTDATE), " "), FUN =  function(L) L[2])),
  wday        = wday(datetime),
  is_weekend  = case_when(wday %in% 1:2 ~ 1,
                          wday %in% 3:7 ~ 0),
  demand_GWh = as.numeric(TOTALDEMAND) / 1000,
  price_AUD   = as.numeric(RRP),
  compkey_rp = paste0(region, ".", period_name),
  compkey_rpd = paste0(region, ".", datetime, ".", period_name)
)
#str(dat)
sub <- dat[dat$yr_mo_dd >= "2020-08-03" & dat$yr_mo_dd < "2020-08-06", ]

##### PLOTTING -----
## see ggpmisc for call outs!
if(F)
  browseURL("https://cran.r-project.org/web/packages/ggpmisc/vignettes/user-guide.html#geom_table-and-stat_fmt_table")

### CREATE THE INLAID PLOT
inlay <-
  ggplot(data = sub, aes(datetime, demand_GWh, group = 1)) +
  geom_line() +
  # stat_peaks(geom = "text", colour = "red", hjust = -0.2, vjust = 0.5,
  #            angle = 90, check_overlap = TRUE) +
  # stat_valleys(geom = "text", colour = "blue", hjust = 1.2, vjust = 0.5,
  #              angle = 90, check_overlap = TRUE) +
  #ggtitle("3 day close up") +
  xlab("") + ylab("") +
  scale_x_datetime(date_breaks = "24 hours",
                   date_minor_breaks = "24 hours",
                   date_labels = "%a"
                   ) +
  theme_minimal() #+
  #theme(aspect.ratio = .5) +
  #theme(axis.text.x = element_text(hjust = 1.85))

inlay_tib <- tibble(x = as.POSIXct(ymd("2020-03-15")),
                    y = 11, ## TOP RIGHT CORNER
                    plot = list(inlay +
                                  theme_bw()
                    )
)

##### CREATE THE FULL plot with annotate line.
n_years <- length(unique(dat$yr)) ## ~6
ind <- which(dat$datetime == "2020-08-03 00:00:00 UTC")
xinter <- as.numeric(dat$datetime[ind])
ggplot(data = dat, aes(datetime, demand_GWh, group = 1)) +
  geom_spline(df = 3 * n_years, color = "blue", size = 1) +
  geom_line(color = "grey40", alpha = .7) +
  theme_minimal() +
  # theme(#axis.text.x = element_text(hjust = 2.15),
  #       panel.grid.minor.x = element_line(colour = "grey80", size = 0.8)) +
  scale_x_datetime(date_breaks = "12 months",
                   date_minor_breaks = "6 months",
                   date_labels = "%Y") +
  #ggtitle("VIC electricity demand") +
  xlab("Time") +
  ylab("Demand (GWh)") +
  geom_plot(data = inlay_tib, aes(x, y, label = plot)) +
  geom_vline(xintercept = xinter, linetype = "dotted", colour = "black") +
  geom_segment(x = as.POSIXct(ymd("2020-08-03")),
               y = 5.2,
               xend = as.POSIXct(ymd("2020-02-15")),
               yend = 8.3,
               linetype = "dashed")

ggsave("figure_data_intro.png", width = 6, height = 3.73) 
