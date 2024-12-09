dat_raw <- read.csv2("../../../OneDrive - Universiteit Utrecht/Projects/4. multilevel sleep/data/esm_clean.csv")

dat <- subset(dat_raw, select = c("participant_id",
                                  "time_sent", "time_start", "time_stop",
                                  "study_day", "beep_nr", "first_beep",
                                  "positive", "negative",
                                  "sleep_quality"))


# get right timezone
dat <- dplyr::mutate(dat, dplyr::across(
  .cols = dplyr::starts_with("time"), 
  .fns = \(x) as.POSIXct(x, tz = "Europe/Brussels",
                         format = "%Y-%m-%dT%H:%M:%SZ")
  )
)


# remove participant 56224
dat <- subset(dat, participant_id != "56224")

# remove observations recorded within 15 minutes after the previous observation
# difference in time between
diff_in_sec <- difftime(dat$time_start, c(NA, dat$time_stop[-nrow(dat)]))

# check if shorter than 15 minutes and remove differences between participants
too_short <- which(
  diff_in_sec < 15 * 60 & (dat$study_day != 1 & dat$beep_nr != 1)
)

# remove too short intervals
dat <- dat[-too_short, ]

# remove sleep_quality for second first beep
# participant 57637
# row 7650 and 7694
dat[dat$participant_id == 57637 & dat$study_day == 13 & dat$beep_nr == 4, "sleep_quality"] <- NA
dat[dat$participant_id == 57637 & dat$study_day == 17 & dat$beep_nr == 8, "sleep_quality"] <- NA

# for (i in dat$participant_id) {
#   for (d in 1:21) {
#     s <- subset(dat, participant_id == i & study_day == d, c("time_start", "first_beep"))
#     if (sum(first_beep) > 0) {
#       first_beep <- which(s$first_beep)
#       if (first_beep != 1) {
#         n_na <- sum(!is.na(s$time_start[1:(first_beep - 1)]))
#         if (n_na > 0) sprintf("pp %s day %s", i, d)
#       }
#     }
#   }
# }

addMissingsDay <- function(dat, i, d, begin_i, n_missing) {
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
    
    # repeated for every NA to add
    for (r in 1:n_na) {
      # transform to list to easily add multiple values in place
      list_interval_length <- as.list(new_interval_length)
      list_beep_nr <- as.list(new_beep_nr)
      
      # compute the largest interval in the current iteration
      largest <- which.max(list_interval_length)
      
      # replace largest interval with two intervals of half its length
      list_interval_length[[largest]] <- rep(list_interval_length[[largest]] / 2,
                                             2)
      # unlist to create new vector
      new_interval_length <- unlist(list_interval_length)
      
      # get new beep order
      if (largest > length(list_beep_nr)) {
        # if missing value is added after last beep only add NA
        list_beep_nr[[largest]] <- NA
      } else {
        # if missing value is added between beeps, replace the latest beep
        # with a missing value and the beep itself
        list_beep_nr[[largest]] <- c(NA, list_beep_nr[[largest]])
      }
      # unlist to create new vector
      new_beep_nr <- unlist(list_beep_nr)
    }
    
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
dat_begin <- read.csv("../../../OneDrive - Universiteit Utrecht/Projects/4. multilevel sleep/data/intake.csv")

# participants and days
participants <- unique(dat$participant_id)
days <- 1:21

# get begin time of schedule for participant
df_pp <- data.frame(participant_id = participants)
begin_i <- merge(df_pp,
                 subset(dat_begin, select = c("participant_id", "daily_start")))

# add missings per participant and per day
list_df <- lapply(1:103, function(i) lapply(1:21, function(d) addMissingsDay(dat, participants[i], days[d], begin_i$daily_start[i], n_missing = 20)))
# 7 warnings because there are 7 days with all missing
# but works out correctly
# which(unlist(list_df) == 20)

unlist_df <- lapply(1:103, function(i) data.table::rbindlist(list_df[[i]]))
final_df <- data.table::rbindlist(unlist_df)

dat_baseline <- read.csv2("../../../OneDrive - Universiteit Utrecht/Projects/4. multilevel sleep/data/personality_clean.csv")
dat_baseline$neuroticism <- as.numeric(dat_baseline$neuroticism)

dat <- merge(final_df, subset(dat_baseline, select = c("participant_id", "chronotype", "neuroticism")))

write.csv(dat, "../../../OneDrive - Universiteit Utrecht/Projects/4. multilevel sleep/data/dat_long.csv")

dat$new_beep_nr <- rep(1:20, 21 * 103)

dat_PA <- subset(dat, select = c("participant_id", "chronotype", "neuroticism", "sleep_quality", "positive", "new_beep_nr", "study_day"))
dat_PA_wider <- tidyr::pivot_wider(dat_PA, id_cols = c(participant_id, study_day, neuroticism, chronotype, sleep_quality), names_from = new_beep_nr, values_from = positive, names_prefix = "m")

write.table(dat_PA_wider, "../../../OneDrive - Universiteit Utrecht/Projects/4. multilevel sleep/data/dat_pa.dat",
            na = ".", col.names = FALSE, row.names = FALSE)

dat_NA <- subset(dat, select = c("participant_id", "chronotype", "neuroticism", "sleep_quality", "negative", "new_beep_nr", "study_day"))
dat_NA_wider <- tidyr::pivot_wider(dat_NA, id_cols = c(participant_id, study_day, neuroticism, chronotype, sleep_quality), names_from = new_beep_nr, values_from = negative, names_prefix = "m")

write.table(dat_NA_wider, "../../../OneDrive - Universiteit Utrecht/Projects/4. multilevel sleep/data/dat_na.dat",
            na = ".", col.names = FALSE, row.names = FALSE)

# think about plotting

