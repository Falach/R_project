library(dplyr)

# read the data
data <- read_csv("RCourse/project/combined_interictal_data.csv")

median_amplitude <- median(data$group_focal_amplitude, na.rm = TRUE)

processed_data <- data |>
  mutate(
    # union of 3 stages that are after stimuli start to be "after"
    stimuli_flag = case_when(
      stimuli_flag == "before_first_stimuli_session" ~ "before_stimulation",
      stimuli_flag %in% c("after_last_stimuli_block", "pause_block", "stimuli_block") ~ "after_stimulation"
    ),
    # creating factors and normalize the numeric values
    stimuli_flag = factor(stimuli_flag, levels = c("before_stimulation", "after_stimulation")),
    hypnogram_flag = factor(hypnogram_flag, levels = c("nrem", "rem/wake")),
    duration_z = scale(duration),  
    spatial_spread_z = scale(group_event_spatial_spread),
    # new binary column for the logistic regression
    high_amplitude = ifelse(group_focal_amplitude >= median_amplitude, 1, 0)
  )  |> 
  select(stimuli_flag, hypnogram_flag, duration_z, spatial_spread_z, group_focal_amplitude, high_amplitude)

# make sure the factor level are as we want
contrasts(processed_data$stimuli_flag)
contrasts(processed_data$hypnogram_flag)


# same but using a function for ex 3
process_data <- function(data) {
  median_amplitude <- median(data$group_focal_amplitude, na.rm = TRUE)
  
  processed_data <- data |> 
    mutate(
      stimuli_flag = case_when(
        stimuli_flag == "before_first_stimuli_session" ~ "before_stimulation",
        stimuli_flag %in% c("after_last_stimuli_block", "pause_block", "stimuli_block") ~ "after_stimulation"
      ),
      stimuli_flag = factor(stimuli_flag, levels = c("before_stimulation", "after_stimulation")),
      hypnogram_flag = factor(hypnogram_flag, levels = c("nrem", "rem/wake")),
      duration_z = scale(duration),  
      spatial_spread_z = scale(group_event_spatial_spread),
      high_amplitude = ifelse(group_focal_amplitude >= median_amplitude, 1, 0)
    ) |> 
    select(stimuli_flag, hypnogram_flag, duration_z, spatial_spread_z, group_focal_amplitude, high_amplitude)
  
  return(processed_data)
}

# function call
processed_data <- process_data(data)


# bonus- using new library
library(recipes)

# first adding the new column
data_for_logistic <- data |> 
  mutate(
    high_amplitude = ifelse(group_focal_amplitude >= median(group_focal_amplitude, na.rm = TRUE), 1, 0)
  )

# create a recipe for preprocessing the dataset
data_recipe <- recipe(group_focal_amplitude ~ stimuli_flag + hypnogram_flag + duration + group_event_spatial_spread + high_amplitude, data = data_for_logistic) |> 
  step_mutate(
    stimuli_flag = case_when(
      stimuli_flag == "before_first_stimuli_session" ~ "0",
      stimuli_flag %in% c("after_last_stimuli_block", "pause_block", "stimuli_block") ~ "1"
    ),
    hypnogram_flag = case_when(
      hypnogram_flag == "nrem" ~ "0",
      TRUE ~ "1"
    )
  ) |> 
  step_mutate(
    stimuli_flag = factor(stimuli_flag, levels = c("0", "1")),
    hypnogram_flag = factor(hypnogram_flag, levels = c("0", "1")),
    high_amplitude = factor(high_amplitude, levels = c(0, 1), labels = c("low", "high"))
  ) |> 
  step_normalize(duration, group_event_spatial_spread)

processed_data <- prep(data_recipe) |> juice()
