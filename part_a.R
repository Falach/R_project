library(ggplot2)
library(ggdist)
library(dplyr)
library(readr)
library(tidyr)


# read the data
data <- read_csv("RCourse/project/combined_interictal_data.csv")

# Compute median for all spikes amplitude
all_median <- median(data$amplitude, na.rm = TRUE)

# Compute median for NREM spikes
nrem_data <- data |> filter(hypnogram_flag == "nrem")
nrem_median <- median(nrem_data$amplitude, na.rm = TRUE)

# Histogram for all spikes (with median)
ggplot(data, aes(x = amplitude)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.6) +
  geom_vline(xintercept = all_median, color = "black", linetype = "dotted", size = 1) +
  annotate("text", x = all_median, y = 5000, label = paste("Median:", round(all_median, 2)), color = "black", hjust = -0.1) +
  xlim(0, 20) +
  labs(title = "Distribution of Epileptic Spike Amplitudes",
       x = "Amplitude",
       y = "Count")

# Histogram for spikes during NREM sleep (with median)
ggplot(nrem_data, aes(x = amplitude)) +
  geom_histogram(bins = 30, fill = "red", alpha = 0.6) +
  geom_vline(xintercept = nrem_median, color = "black", linetype = "dotted", size = 1) +
  annotate("text", x = nrem_median, y = 4000, label = paste("Median:", round(nrem_median, 2)), color = "black", hjust = -0.1) +
  xlim(0, 20) +
  labs(title = "Distribution of Epileptic Spike Amplitudes (NREM Only)",
       x = "Amplitude",
       y = "Count")

# distribution of all spikes duration from all patients
ggplot(data, aes(x = duration)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.6) +
  labs(title = "Distribution of Epileptic Spike Duration",
       x = "Duration",
       y = "Count")

# distribution of all spikes angle from all patients
ggplot(data, aes(x = angle)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.6) +
  labs(title = "Distribution of Epileptic Spike Angle",
       x = "Angle",
       y = "Count")

# looking at amplitude across sleep stages
ggplot(data, aes(x = hypnogram_flag, y = amplitude, fill = hypnogram_flag)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(alpha=0.2, width=0.2)
labs(title = "Spike Amplitude Across Sleep Stages",
     x = "Sleep Stage",
     y = "Amplitude") +
  theme_minimal()

# amplitude distribution per patient
ggplot(data, aes(x = factor(subject_number), y = amplitude, fill = factor(subject_number))) +
  stat_dots(side = "both", dotsize = 0.3, binwidth = 0.06) +
  labs(title = "Distribution of Epileptic Spike Amplitudes per Subject",
       x = "Subject Number",
       y = "Amplitude") +
  theme_minimal() +
  ylim(0, 20) +
  theme(legend.position = "none")

# checking the effect of the stimuli on amplitude
ggplot(data, aes(x = stimuli_flag, y = amplitude, fill = stimuli_flag)) +
  stat_halfeye(alpha = 0.7, adjust = 0.5, width = 0.6, justification = -0.2) +
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), 
               vjust = -5, size = 5, color = "black") +
  labs(title = "Effect of Electrical Stimulation on Spike Amplitude",
       x = "Stimulation Condition",
       y = "Amplitude") +
  ylim(1, 20) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# checking the effect of the stimuli on duration
ggplot(data, aes(x = stimuli_flag, y = duration, fill = stimuli_flag)) +
  stat_halfeye(alpha = 0.7, adjust = 0.5, width = 0.6, justification = -0.2) +
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), 
               vjust = -1, size = 5, color = "black") +
  labs(title = "Effect of Electrical Stimulation on Spike Duration",
       x = "Stimulation Condition",
       y = "Spike Duration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# checking the effect of the stimuli on angle
ggplot(data, aes(x = stimuli_flag, y = angle, fill = stimuli_flag)) +
  stat_halfeye(alpha = 0.7, adjust = 0.5, width = 0.6, justification = -0.2) +
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), 
               vjust = -1, size = 5, color = "black") +
  labs(title = "Effect of Electrical Stimulation on Spike Angle",
       x = "Stimulation Condition",
       y = "Spike Angle") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 30)


# Handling groups - remove duplicate group entries (keep one representative per group)
group_data <- data |>
  group_by(group) |>
  slice(1)  # Keep only the first row per group

# Keep only spread > 0
group_data_filtered <- group_data |>
  filter(group_event_spatial_spread > 0)  

# checking the relation between the amplitude and the spread features
ggplot(group_data_filtered, aes(x = group_focal_amplitude, y = group_event_spatial_spread)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Spatial Spread vs. Spike Amplitude (Grouped Data)",
       x = "Spike Amplitude",
       y = "Spatial Spread (Per Group)") +
  theme_minimal()+
  ylim(0,100)

# checking the effect of the stimuli on spread
ggplot(group_data_filtered, aes(x = stimuli_flag, y = group_event_spatial_spread, fill = stimuli_flag)) +
  stat_halfeye(alpha = 0.7, adjust = 0.5, width = 0.6, justification = -0.2) +
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), 
               vjust = -1, size = 5, color = "black") + 
  labs(title = "Effect of Electrical Stimulation on Spatial Spread (Zero Values Removed)",
       x = "Stimulation Condition",
       y = "Spatial Spread") +
  theme_minimal() +
  ylim(0,100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Compute subject-level means for each parameter and stimulation condition
subject_means <- data |>
  filter(group_event_spatial_spread > 0) |> 
  group_by(subject_number, stimuli_flag) |>
  summarize(
    mean_spatial_spread = mean(group_event_spatial_spread, na.rm = TRUE),
    mean_duration = mean(duration, na.rm = TRUE),
    mean_angle = mean(angle, na.rm = TRUE),
    mean_amplitude = mean(amplitude, na.rm = TRUE),
    .groups = "drop"
  )

# Convert data to long format
subject_means_long <- subject_means |>
  pivot_longer(cols = starts_with("mean_"), names_to = "Parameter", values_to = "Mean_Value")

# Normalize each parameter using min-max scaling (0 to 1)
subject_means_long <- subject_means_long |>
  group_by(Parameter) |>
  mutate(Normalized_Value = (`Mean_Value` - min(`Mean_Value`, na.rm = TRUE)) / 
           (max(`Mean_Value`, na.rm = TRUE) - min(`Mean_Value`, na.rm = TRUE))) |>
  ungroup()

# plotting each subject as one sample 
ggplot(subject_means_long, aes(x = Parameter, y = Normalized_Value, fill = stimuli_flag)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  
  geom_point(position = position_dodge(width = 0.75), size = 1, color = "black") + 
  labs(title = "Effect of Electrical Stimulation on Normalized Parameters (Subject-Level Averages)",
       x = "Parameter",
       y = "Normalized Value (0-1)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# add lines between same patients
ggplot(subject_means_long, aes(x = stimuli_flag, y = Normalized_Value, fill = stimuli_flag)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  
  geom_line(aes(group = interaction(subject_number, Parameter)), color = "black", alpha = 0.5) +
  geom_point(size = 1, color = "black") +  
  facet_wrap(~Parameter, scales = "free_y") + 
  labs(title = "Effect of Electrical Stimulation on Normalized Parameters (Subject-Level Averages)",
       x = "Stimulation Condition",
       y = "Normalized Value (0-1)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
