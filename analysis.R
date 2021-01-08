# Plots the O2 sat over time for each phase, with separate points for each fish.
plot_all_fish <- function(data) {
  p <- ggplot(data = data, aes(x = Time.m, y = O2.sat, group = Phase))
  p <- p + geom_point(aes(color = FishID)) + theme_bw()
  return(p)
}

# Plots each period of one fish separately.
plot_individual_period <- function(fish_data, fish_id, fit = FALSE) {
  p <- ggplot(data = fish_data, aes(x = Time.m, y = O2.sat))
  p <- p + geom_line(aes(color = Phase))
  if (fit) {
    p <- p + geom_smooth(method = "lm", color = "black")
  }
  p <- p + theme_bw() + facet_wrap(~ Phase, ncol = 8)
  p <- p + scale_y_continuous(name = "% air saturation", breaks = seq(90, 160, 50)) + scale_x_continuous(name="Time (min)") + labs(title = fish_id)
  p
}

# Returns the regression coefficients and R2 for this set of data, grouped by phase and fish. 
# Could be made more flexible/less cursed (most of this is formatting considerations).
slope_and_r2 <- function(data) {
  output <- data %>% group_by(Phase, FishID) %>% nest() %>%
    mutate(fit = map(data, ~ lm(O2.sat ~ Time.m, data = .))) %>%
    mutate(coefs = map(fit, coef)) %>% unnest_wider(col = coefs) %>%
    rename(Intercept = `(Intercept)`, Slope = Time.m) %>%
    mutate(Slope.hour = Slope * 60) %>%
    mutate(r2 = map(fit, ~ summary(.)$r.squared)) %>% unnest(col = r2) %>%
    select(-c(fit, data))
  output <- output %>% arrange(Phase)
  output <- output %>% ungroup() %>% mutate(Phase = as.factor(Phase))
  return(output)
}

# TODO refactor this and above into a more sensible order
rebind_output <- function(slope_r2, phase_start, mean_temp, start_time) {
  output <- merge(slope_r2, phase_start, by = c("Phase"))
  output$meanTemp <- mean_temp
  output$Time.m <- difftime(output$DateTime, start_time, units = "mins")
  return(output)
}

# Plots slope of each trial.
plot_slopes <- function(slope_r2) {
  p <- ggplot(slope_r2, aes(x = DateTime, y = (-1 * Slope))) +
    geom_point(aes(color = FishID)) + theme_bw()
  return(p)
}

# Averages the slope values across trials.
average_across_periods <- function(slope_r2) {
  averaged <- slope_r2 %>% filter(!grepl("M[12]", Phase)) %>% 
    group_by(FishID) %>% slice_max(Slope, n = 3) %>% 
    summarise(avg_slope = mean(Slope), avg_slope_hour = mean(Slope.hour), avg_r2 = mean(r2))
  return(averaged)
}