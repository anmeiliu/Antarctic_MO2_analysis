fish_ids <- c("D1_B1_C1", "D1_B1_C2", "D1_B1_C3", "D1_B1_C4")

raw_data <- readclean2("2018_burbot_resp_D1_B1_C_raw.txt")
data <- raw_data %>% augment_measure_data() %>% melt_by_fish(fish_ids)

plot_all_fish(data)

slope_r2 <- slope_and_r2(data) %>% rebind_output(phase_start_times(data), mean(raw_data$Temp), data$DateTime[1])

# Needs more workflow for processing multiple values

slope_r2 %>% plot_slopes()

slope_r2 %>% average_across_periods()

# Needs final processing for blanks correction