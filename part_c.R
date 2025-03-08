library(ggplot2)
library(dplyr)
library(readr)
library(pROC)
library(broom)

# get function source
source("Rcourse/project/part_b.R")

# read the data
data <- read_csv("RCourse/project/combined_interictal_data.csv")
processed_data <- process_data(data)

# linear model
lm_model <- lm(group_focal_amplitude ~ stimuli_flag + hypnogram_flag + duration_z + spatial_spread_z, data = processed_data)
summary(lm_model)

# plotting effect size
lm_results <- tidy(lm_model, conf.int = TRUE)
lm_results <- lm_results |> filter(term != "(Intercept)")
ggplot(lm_results, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(size = 3, color = "blue") +  
  geom_errorbar(width = 0.3, color = "black") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +  
  labs(title = "Effect of Predictors on Spike Amplitude",
       x = "Predictors",
       y = "Estimate (Effect on Amplitude)") +
  theme_minimal()

# distributions according to stimuli flag
ggplot(processed_data, aes(x = stimuli_flag, y = group_focal_amplitude, fill = stimuli_flag)) +
  geom_violin(alpha = 0.5, trim = TRUE) +  
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(title = "Effect of Electrical Stimulation on Spike Amplitude",
       x = "Stimulation Condition",
       y = "Amplitude") +
  theme_minimal()

# logistic model
logit_model <- glm(high_amplitude ~ stimuli_flag + hypnogram_flag + duration_z + spatial_spread_z, 
                   data = processed_data, family = binomial)
summary(logit_model)

# plotting effect size
logit_results <- tidy(logit_model, conf.int = TRUE)
logit_results <- logit_results |> filter(term != "(Intercept)")
ggplot(logit_results, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(width = 0.3, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Effect of Predictors on Probability of High Amplitude",
       x = "Predictors",
       y = "Estimate (Log Odds)") +
  theme_minimal()

# plotting probability of high amplitude according to stimuli
logit_means <- processed_data |> 
  group_by(stimuli_flag) |> 
  summarize(mean_high_amplitude = mean(high_amplitude, na.rm = TRUE))
ggplot(logit_means, aes(x = stimuli_flag, y = mean_high_amplitude, fill = stimuli_flag)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_text(aes(label = round(mean_high_amplitude, 2)), vjust = -0.5) +
  labs(title = "Probability of High Amplitude Before and After Stimulation",
       x = "Stimulation Condition",
       y = "Probability of High Amplitude") +
  theme_minimal()

# get predictions and create ROC curve
predicted_probs <- predict(logit_model, type = "response")
roc_curve <- roc(processed_data$high_amplitude, predicted_probs)
auc_amp <- auc(roc_curve)
plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC =", round(auc_amp, 3), ")"))
