North_Duration <- ggplot(data = north_MHW_res, aes(x = duration, y = mean_CPUE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # geom_label(aes(label = NA, x = 20, y = 1.2)) +
  labs(x = "Duration (days)", y = "Mean CPUE")

North_intensity <- ggplot(data = north_MHW_res, aes(x = intensity_mean_relThresh, y = mean_CPUE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # geom_label(aes(label = NA, x = 1, y = 1.2)) +
  labs(x = "Mean Intensity (relative to 90th percentile)", y = "Mean CPUE")

North_Scatter <- grid.arrange(North_Duration, North_intensity)


South_Duration <- ggplot(data = south_MHW_res, aes(x = duration, y = mean_CPUE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # geom_label(aes(label = NA, x = 20, y = 1.2)) +
  labs(x = "Duration (days)", y = "Mean CPUE")

South_intensity <- ggplot(data = south_MHW_res, aes(x = intensity_mean_relThresh, y = mean_CPUE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # geom_label(aes(label = NA, x = 0.6, y = 1.2)) +
  labs(x = "Mean Intensity (relative to 90th percentile)", y = "Mean CPUE")

South_Scatter <- grid.arrange(South_Duration, South_intensity, ncol = 2, nrow = 1)


Fundy_Duration <- ggplot(data = fundy_MHW_res, aes(x = duration, y = mean_CPUE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # geom_label(aes(label = NA, x = 20, y = 1.2)) +
  labs(x = "Duration (days)", y = "Mean CPUE")

Fundy_intensity <- ggplot(data = fundy_MHW_res, aes(x = intensity_mean_relThresh, y = mean_CPUE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # geom_label(aes(label = NA, x = 1, y = 1.2)) +
  labs(x = "Mean Intensity (relative to 90th percentile)", y = "Mean CPUE")

Fundy_Scatter <- grid.arrange(Fundy_Duration, Fundy_intensity)
