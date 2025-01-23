# read raw data
dat_raw <- read.csv2("esm_clean.csv")

# get relevant variables
dat <- subset(dat_raw, select = c("participant_id",
                                  "time_sent", "time_start", "time_stop",
                                  "study_day", "beep_nr", "first_beep",
                                  "positive", "negative",
                                  "sleep_quality"))


# get right timezone
library(dplyr)
library(lubridate)
dat <- dplyr::mutate(dat, dplyr::across(
  .cols = dplyr::starts_with("time"), 
  .fns = \(x) as.POSIXct(x, tz = "Europe/Brussels",
                         format = "%Y-%m-%dT%H:%M:%SZ")
  )
)

# remove observations recorded within 15 minutes after the previous observation
## difference in time between start and end of previous questionnaire
diff_in_sec <- difftime(dat$time_start, c(NA, dat$time_stop[-nrow(dat)]))
## check if shorter than 15 minutes and remove differences between participants
dummy_beep_nr_without_na <- dat$beep_nr
dummy_beep_nr_without_na[is.na(dummy_beep_nr_without_na)] <- 999
too_short <- which(
  diff_in_sec < 15 * 60 & !(dat$study_day == 1 & dummy_beep_nr_without_na == 1)
)
## remove too short intervals
dat <- dat[-too_short, ]

# remove participant 56224
dat <- subset(dat, participant_id != "56224")

# remove sleep_quality for second first beep
dat[dat$participant_id == 57637 & dat$study_day == 13 & dat$beep_nr == 4, "sleep_quality"] <- NA
dat[dat$participant_id == 57637 & dat$study_day == 17 & dat$beep_nr == 8, "sleep_quality"] <- NA

addMissings <- function(dat, i, d, begin_i, n_missing) {
  # create empty df to fill for every day
  empty_df <- dat[0, ]
  empty_df[1:n_missing, ] <- NA
  
  # get subset of participant and day
  dat_i_d <- subset(dat, participant_id == i & study_day == d)
  # remove rows with no start time
  dat_i_d_na_rm <- dat_i_d[!is.na(dat_i_d$time_start), ]
  # extract start times
  t_start <- dat_i_d_na_rm$time_start
  # compute number of missing values to add
  n_na <- n_missing - length(t_start)

  # add date to begin time
  begin_datetime <- lubridate::parse_date_time(
    paste(lubridate::date(t_start[1]), begin_i),
    "ymd HMS", tz = "Europe/Brussels"
  )
  
  # get end datetime 12.5 hours later
  end_datetime <- begin_datetime + lubridate::hours(12) + lubridate::minutes(30)
  
  # compute interval lengths including begin and end time
  interval_length <- difftime(c(t_start, end_datetime),
                              c(begin_datetime, t_start),
                              units = "sec")
  
  # fill empty df with participant_id and study_day
  new_dat_i_d <- empty_df
  new_dat_i_d$participant_id <- i
  new_dat_i_d$study_day <- d
  
  # compute where to add missing values
  if (length(t_start) > 0) {
    # original beep order
    beep_nr <- 1:length(t_start)
    
    # create new vectors
    new_interval_length <- as.numeric(interval_length)
    new_beep_nr <- beep_nr

    # transform to list to easily add multiple values in place
    list_interval_length <- as.list(new_interval_length)
    list_beep_nr <- as.list(new_beep_nr)
    
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
        
      # get new beep order
      if (largest > length(list_beep_nr)) {
        # if missing value is added after last beep only add NA
        list_beep_nr[[largest]] <- NA
      } else {
        # if missing value is added between beeps, replace the latest beep
        # with a missing value and the beep itself
        list_beep_nr[[largest]] <- c(NA, list_beep_nr[[largest]])
      }
    }
    
    # unlist to create vector of new beep numbers
    new_beep_nr <- unlist(list_beep_nr)
    # add original observations to new data frame in new order
    new_dat_i_d[which(!is.na(new_beep_nr)), ] <- dat_i_d_na_rm
  }
  
  # add sleep_quality observation to all rows if observed
  sleep_quality <- unique(na.omit(dat_i_d$sleep_quality))
  if (length(sleep_quality > 0)) {
    new_dat_i_d$sleep_quality <- unique(na.omit(dat_i_d$sleep_quality))
  }
  
  return(new_dat_i_d)
}

# get begin time of sampling schedule per participant
dat_begin <- read.csv("intake.csv")

# participants and days
participants <- unique(dat$participant_id)
days <- 1:21

# get begin time of schedule for participant
df_pp <- data.frame(participant_id = participants)
begin_i <- merge(df_pp,
                 subset(dat_begin, select = c("participant_id", "daily_start")))

# add missings per participant and per day
list_df <- lapply(1:103, function(i) lapply(1:21,
  function(d) addMissings(dat, participants[i], days[d], begin_i$daily_start[i],
                          n_missing = 20))
)
# 7 warnings because there are 7 days with all missing

# unlist to get data frame of 103 * 21 * 20 rows
unlist_df <- lapply(1:103, function(i) data.table::rbindlist(list_df[[i]]))
final_df <- data.table::rbindlist(unlist_df)

# get baseline between-level variables
dat_baseline <- read.csv2("personality_clean.csv")
dat_baseline$neuroticism <- as.numeric(dat_baseline$neuroticism)

# merge final_df with neuroticism and chronotype
dat <- merge(final_df, subset(dat_baseline, select = c("participant_id", "chronotype", "neuroticism")))

# add column with new beep numbers
dat$new_beep_nr <- rep(1:20, 21 * 103)

# save long format data
write.csv(dat, "dat_long.csv")

# save positive affect in long-and-wide format
## subset positive affect variable
dat_PA <- subset(dat, select = c("participant_id", "chronotype", "neuroticism", "sleep_quality", "positive", "new_beep_nr", "study_day"))
## make data frame wider for positive affect beeps (with 103 * 21 rows)
dat_PA_wider <- tidyr::pivot_wider(dat_PA,
  id_cols = c(participant_id, study_day, neuroticism, chronotype,
              sleep_quality),
  names_from = new_beep_nr, values_from = positive, names_prefix = "m"
)
## write data to dat_pa.dat file for mplus (missing values as periods)
write.table(dat_PA_wider, "dat_pa.dat",
            na = ".", col.names = FALSE, row.names = FALSE)

# save negative affect in long-and-wide format
## subset negative affect variable
dat_NA <- subset(dat, select = c("participant_id", "chronotype", "neuroticism", "sleep_quality", "negative", "new_beep_nr", "study_day"))
## make data frame wider for negative affect beeps (with 103 * 21 rows)
dat_NA_wider <- tidyr::pivot_wider(dat_NA,
  id_cols = c(participant_id, study_day, neuroticism, chronotype,
              sleep_quality),
  names_from = new_beep_nr, values_from = negative, names_prefix = "m"
)
## write data to dat_na.dat file for mplus (missing values as periods)
write.table(dat_NA_wider, "dat_na.dat",
            na = ".", col.names = FALSE, row.names = FALSE)
