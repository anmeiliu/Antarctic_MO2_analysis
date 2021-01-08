# More flexible implementation of readclean (only reads necessary columns).
readclean2 <- function(path, delim = "\t", skiprows = 38, date_format = "%m/%d/%Y/%I:%M:%S %p") {
  
  default_spec <- readr::cols_only(
    X1 = readr::col_datetime(date_format),
    X2 = readr::col_character(),
    X4 = readr::col_double(), 
    X6 = readr::col_double(),
    X7 = readr::col_double(),
    X10 = readr::col_double(), 
    X13 = readr::col_double(),
    X16 = readr::col_double()
  )
  
  x <- readr::read_delim(path, delim, col_names = FALSE, col_types = default_spec,
                  locale = locale(encoding = "latin1"), skip = skiprows)

  names(x) <- c("DateTime", "Phase", "Salinity", "Temp", 
                "CH1O2.sat","CH2O2.sat", "CH3O2.sat", "CH4O2.sat")
  
  return(x)
}


# Performs augmentation and subset of data.
augment_measure_data <- function(data) {
  start_time <- data$DateTime[1]
  data <- data %>% extract_measure_period() %>% augment_time(start_time)
  return(data)
}

# Subsets the data to the measure period only.
extract_measure_period <- function(data) {
  data <- data[grep("M", data$Phase), ]
  return(data)
}

# Augments data with time since start.
augment_time <- function(data, start_time) {
  data <- data %>% dplyr::mutate(Time.m = as.numeric(difftime(DateTime, start_time, units = "mins")))
  return(data)
}

# Returns a dataframe containing start timestamps for each phase. 
phase_start_times <- function(data) {
  return(plyr::ddply(data, "Phase", head, 1)[,1:2])
}

# Converts the dataframe into long format (each fish is its own observation). 
melt_by_fish <- function(data, fish_ids) {
  names(data)[5:8] <- fish_ids
  data <- data %>% reshape2::melt(measure = fish_ids, value.name = "O2.sat")
  names(data)[6] <- "FishID"
  return(data)
}

# Plots the O2 sat over time for each phase, with separate points for each fish.
plot_all_fish <- function(data) {
  p <- ggplot(data = data, aes = aes(x = Time.m, y = O2.sat, group = Phase))
  p <- p + geom_point(aes(color = FishID)) + theme_bw()
  return(p)
}

# Returns a list of dataframes, one dataframe for each fish.
# This isn't currently used by my code but it's there?
split_by_fish <- function(data, fish_ids) {
  return(split(data, data$FishID))
}
