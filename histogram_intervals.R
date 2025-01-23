# read long format data
dat <- read.csv("dat_long.csv")

# get right timezone
library(dplyr)
library(lubridate)
dat <- dplyr::mutate(dat, dplyr::across(
  .cols = dplyr::starts_with("time"), 
  .fns = \(x) as.POSIXct(x, tz = "Europe/Brussels",
                         format = "%Y-%m-%d %H:%M:%S")
  )
)

# participants and days
participants <- unique(dat$participant_id)
days <- 1:21

# get begin time of schedule for participant
df_pp <- data.frame(participant_id = participants)
dat_begin <- read.csv("intake.csv")
begin_i <- merge(df_pp,
                 subset(dat_begin, select = c("participant_id", "daily_start")))

# compute interval lengths
intervalLengths <- function(dat, i, d, begin_i, phase = "day", n_missing = 20) {
  # get subset of participant and day
  dat_i_d <- subset(dat, participant_id == i & study_day == d)
  # remove rows with no start time
  dat_i_d_na_rm <- dat_i_d[!is.na(dat_i_d$time_start), ]
  # extract start times
  t_start <- dat_i_d_na_rm$time_start
  
  # for original daytime intervals
  if (phase == "day") {
    # compute interval lengths including begin and end time
    interval_length <- difftime(t_start[-1],
                                t_start[-length(t_start)],
                                units = "sec")
  }
  
  # for nighttime or new intervals
  if (phase != "day") {
    # add date to begin time
    begin_datetime <- lubridate::parse_date_time(
      paste(lubridate::date(t_start[1]), begin_i),
      "ymd HMS", tz = "Europe/Brussels"
    )
    
    # get end datetime 12.5 hours later
    end_datetime <- begin_datetime + lubridate::hours(12) + lubridate::minutes(30)
  }
  
  # for nighttime intervals
  if (phase == "night") {
    # compute interval lengths including begin and end time
    interval_length <- difftime(c(t_start[1], end_datetime),
                                c(begin_datetime, t_start[length(t_start)]),
                                units = "sec")
  }
  
  # for new intervals
  if (phase == "day_new" | phase == "night_new") {
    # compute number of missing values to add
    n_na <- n_missing - length(t_start)
    
    # compute interval lengths including begin and end time
    interval_length <- difftime(c(t_start, end_datetime),
                                c(begin_datetime, t_start),
                                units = "sec")
    
    # compute where to add missing values
    if (length(t_start) > 0) {
      # create new vectors
      new_interval_length <- as.numeric(interval_length)
      
      # transform to list to easily add multiple values in place
      list_interval_length <- as.list(new_interval_length)
      
      # repeated for every NA to add
      for (r in 1:n_na) {
        # compute the largest interval in the current iteration
        largest <- which.max(sapply(1:length(interval_length),
                                    function(i) list_interval_length[[i]][1]))
        
        # extract largest interval(s)
        largest_interval <- list_interval_length[[largest]]
        
        # new number of intervals
        n_interval <- length(largest_interval) + 1
        
        # divide original interval by new number of intervals
        new_interval <- sum(largest_interval) / n_interval
        
        # replace old interval(s) with new intervals
        list_interval_length[[largest]] <- rep(new_interval, n_interval)
      }
      # save new interval lengths
      new_interval_length <- unlist(list_interval_length)
    }
  }
  
  # for new daytime intervals
  if (phase == "day_new") {
    if (length(t_start) > 0) {
      interval_length <- new_interval_length[-c(1, length(new_interval_length))]
    } else {
      interval_length <- NA
    }
  }
  
  # for new nighttime intervals
  if (phase == "night_new") {
    if (length(t_start) > 0) {
      interval_length <- new_interval_length[c(1, length(new_interval_length))]
    } else {
      interval_length <- NA
    }
  }
  
  # return vector of interval lengths
  return(as.numeric(interval_length))
}

# get data frame for daytime interval lengths
## get list interval lengths in a day per participant
list_day_il <- lapply(1:103,
  function(i) lapply(1:21,
    function(d) intervalLengths(dat, participants[i], days[d],
                                begin_i$daily_start[i], phase = "day")
  )
)
## unlist per participant
unlist_day_il <- lapply(1:103, function(i) unlist(list_day_il[[i]]))
## add participant id
list_df_day_il_i <- lapply(1:103,
  function(i) data.frame(participant_id = participants[i],
                         interval = unlist_day_il[[i]])
)
## unlist to data frame with columns participant_id and interval
## contains all daytime interval lengths per participant
df_day_il <- data.table::rbindlist(list_df_day_il_i)

# get data frame for nighttime interval lengths
list_night_il <- lapply(1:103,
  function(i) sapply(1:21,
      function(d) intervalLengths(dat, participants[i], days[d],
                                  begin_i$daily_start[i], phase = "night")
  )
)
## function to help compute nighttime intervals
computeNightInterval <- function(x) {
  morning <- x[1, ]
  evening <- x[2, ]
  night_interval_sec <- morning[-1] + evening[-length(evening)] + 11.5 * 60 * 60
  return(night_interval_sec)
}
night_il <- sapply(1:103, function(i) computeNightInterval(list_night_il[[i]]))
list_df_night_il_i <- lapply(1:103,
  function(i) data.frame(participant_id = participants[i], 
                         interval = night_il[, i])
)
df_night_il <- data.table::rbindlist(list_df_night_il_i)

# get data frame for new daytime interval lengths
list_day_il_new <- lapply(1:103,
  function(i) lapply(1:21,
      function(d) intervalLengths(dat, participants[i], days[d],
                                  begin_i$daily_start[i], phase = "day_new")
  )
)
unlist_day_il_new <- lapply(1:103, function(i) unlist(list_day_il_new[[i]]))
list_df_day_il_i_new <- lapply(1:103,
  function(i) data.frame(participant_id = participants[i],
                         interval = unlist_day_il_new[[i]])
)
df_day_il_new <- data.table::rbindlist(list_df_day_il_i_new)

# get data frame for new nighttime interval lengths
list_night_il_new <- lapply(1:103,
  function(i) lapply(1:21,
      function(d) intervalLengths(dat, participants[i], days[d],
                                  begin_i$daily_start[i], phase = "night_new")
  )
)
computeNightIntervalNew <- function(x) {
  morning <- sapply(1:21, function(d) x[[d]][1])
  evening <- sapply(1:21, function(d) x[[d]][2])
  night_interval_sec <- morning[-1] + evening[-length(evening)] + 11.5 * 60 * 60
  return(night_interval_sec)
}
night_il_new <- sapply(1:103,
  function(i) computeNightIntervalNew(list_night_il_new[[i]])
)
list_df_night_il_i_new <- lapply(1:103,
  function(i) data.frame(participant_id = participants[i],
                         interval = night_il_new[, i])
)
df_night_il_new <- data.table::rbindlist(list_df_night_il_i_new)

# compute delta t (on average)
average_night_il_i_new <- apply(night_il_new, 2, mean, na.rm = TRUE)
average_day_il_i_new <- sapply(1:103, function(i) mean(unlist_day_il_new[[i]]))
mean(average_night_il_i_new) / mean(average_day_il_i_new, na.rm = TRUE)

# create data frames for plotting
## daytime intervals original and new
df_day_il$correction <- "No"
df_day_il_new$correction <- "Yes"
df_long_day <- rbind(df_day_il, df_day_il_new)
df_long_day$period <- "Daytime"
## nighttime intervals original and new
df_night_il$correction <- "No"
df_night_il_new$correction <- "Yes"
df_long_night <- rbind(df_night_il, df_night_il_new)
df_long_night$period <- "Nighttime"
## merge all in one data frame
df_long_all <- rbind(df_long_day, df_long_night)
## interval lengths in hours
df_long_all$interval <- df_long_all$interval / 60 / 60

# x-axis ticks per facet
xBreaks <- function(x) {
  if (max(x) < 10) seq(0, 9, 1) else seq(10, 35, 5)
}
# x-axis limits per facet
xLimits <- function(x) {
  if (max(x) < 10) c(0, 9) else c(10, 35)
}
# y-axis ticks per facet
yBreaks <- function(x) {
  if (max(x) > 20000) seq(0, 40000, 10000) else seq(0, 1250, 250)
}
# y-axis limits per facet
yLimits <- function(x) {
  if (max(x) > 20000) c(0, 40000) else c(0, 1250)
}

# axes lines (segments) per facet
df_axis_x <- data.frame(x = c(0, 10), xend = c(9, 35),
                        y = c(-Inf, -Inf), yend = c(-Inf, -Inf),
                      period = c("Daytime", "Nighttime"))
df_axis_y <- data.frame(x = c(-Inf, -Inf), xend = c(-Inf, -Inf),
                        y = c(0, 10), yend = c(40000, 1250),
                      period = c("Daytime", "Nighttime"))

# plot histograms
library(ggplot2)
ggplot(df_long_all) +
  geom_histogram(aes(x = interval, fill = correction, alpha = correction),
                 bins = 30, position = "identity") +
  scale_x_continuous(breaks = xBreaks, limits = xLimits) +
  scale_y_continuous(breaks = yBreaks, limits = yLimits) +
  scale_fill_manual(values = c("black", "grey"),
                    labels = c("Original", "Corrected")) +
  scale_alpha_manual(values = c(1, 0.75), labels = c("Original", "Corrected")) +
  facet_wrap(~ period, scales = "free") +
  labs(y = "Frequency", x = "Interval Length in Hours", period = c("A", "B")) +
  theme_void() +
  theme(text = element_text(family = "sans", size = 16),
        strip.text = element_text(family = "sans", size = 16,
                                  margin = margin(5, 5, 5, 5)),
        axis.text = element_text(margin = margin(5, 5, 5, 5)),
        axis.title.y = element_text(angle = 90),
        axis.title = element_text(margin = margin(5, 5, 5, 5)),
        axis.ticks = element_line(lineend = "butt",
                                  linewidth = 0.3),
        axis.ticks.length = unit(2.5, "pt"),
        legend.position = c(.3, .8),
        legend.title = element_blank()) +
  geom_segment(data = df_axis_x, aes(x = x, xend = xend, y = y, yend = yend),
               linewidth = 0.3, lineend = "square") +
  geom_segment(data = df_axis_y, aes(x = x, xend = xend, y = y, yend = yend),
               linewidth = 0.3, lineend = "square")

# save plot
ggsave("histogram_intervals.pdf", height = 4, width = 8)
