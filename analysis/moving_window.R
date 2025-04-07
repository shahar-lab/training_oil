library(dplyr)
library(zoo)  # For rolling functions
rm(list=ls())
load("data/empirical_data/data_analysis/df_all.rdata")

df=na.omit(df)
df <- df %>%
  mutate(global_trial = (as.numeric(trial) + 
                                50 * (as.numeric(block) - 1) + 
                                200 * (as.numeric(session) - 1)))

df <- df %>%
  arrange(subject, session, block, global_trial) %>%  # Ensure correct ordering
  group_by(subject, session, block) %>%
  mutate(
    rolling_stay_0 = rollapplyr(ifelse(reward_oneback == 0, stay_key, NA),
                                width = 10, FUN = mean, na.rm = TRUE, fill = NA, align = "right"),
    rolling_stay_1 = rollapplyr(ifelse(reward_oneback == 1, stay_key, NA),
                                width = 10, FUN = mean, na.rm = TRUE, fill = NA, align = "right"),
    rolling_stay_diff = rolling_stay_1 - rolling_stay_0
  ) %>%
  ungroup()


df_summary <- df %>%
  group_by(session, block, global_trial) %>%
  summarise(
    mean_diff = mean(rolling_stay_diff, na.rm = TRUE),
    se_diff = sd(rolling_stay_diff, na.rm = TRUE) / sqrt(n())
  ) %>%
  filter(!is.na(mean_diff))  # Remove remaining NAs
# Plot everything together with color representing blocks
ggplot(df_summary, aes(x = global_trial, y = mean_diff, group = interaction(session, block), color = as.factor(block))) +
  geom_smooth(se = TRUE, method = "loess", span = 0.2, linewidth = 1) +  # Smooth with LOESS
  geom_vline(xintercept = seq(0, max(df_summary$global_trial), by = 50), linetype = "dashed", color = "black", alpha = 0.5) +  
  geom_vline(xintercept = c(200, 400), linetype = "solid", color = "red", linewidth = 1) +  # Session markers
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 1) +  # Zero line
  theme_minimal(base_size = 14) +  # Increase font size for better readability
  scale_color_manual(values = c("red", "green", "blue", "purple")) +  # Distinct colors for blocks
  scale_x_continuous(breaks = seq(0, max(df_summary$global_trial), by = 50)) +  
  labs(title = "Mean Stay Probability Difference by Session and Block",
       subtitle = "Sessions separated by red vertical lines | Zero reference line added",
       y = "Stay Probability Difference",
       x = "Trial Number",
       color = "Block")

#accuracy
df <- df %>%
  arrange(subject, session, block, global_trial) %>%  # Ensure correct ordering
  group_by(subject, session, block) %>%
  mutate(
    rolling_accuracy = rollapplyr(accuracy, width = 10, FUN = mean, na.rm = TRUE, fill = NA, align = "right")
  ) %>%
  ungroup()

df_summary <- df %>%
  group_by(session, block, global_trial) %>%
  summarise(
    mean_accuracy = mean(rolling_accuracy, na.rm = TRUE),
    se_accuracy = sd(rolling_accuracy, na.rm = TRUE) / sqrt(n())
  ) %>%
  filter(!is.na(mean_accuracy))  # Remove remaining NAs


ggplot(df_summary, aes(x = global_trial, y = mean_accuracy, group = interaction(session, block), color = as.factor(block))) +
  geom_smooth(se = TRUE, method = "loess", span = 0.2, linewidth = 1) +  # Smooth trend
  geom_vline(xintercept = seq(0, max(df_summary$global_trial), by = 50), linetype = "dashed", color = "black", alpha = 0.5) +  
  geom_vline(xintercept = c(200, 400), linetype = "solid", color = "red", linewidth = 1) +  # Session markers
  geom_hline(yintercept = 0.5, linetype = "solid", color = "black", linewidth = 1) +  # Reference line at 50% accuracy
  theme_minimal(base_size = 14) +  # Increase font size for clarity
  scale_color_manual(values = c("red", "green", "blue", "purple")) +  # Distinct colors for blocks
  scale_x_continuous(breaks = seq(0, max(df_summary$global_trial), by = 50)) +  
  labs(title = "Moving Window Accuracy by Session and Block",
       subtitle = "Sessions separated by red vertical lines | Reference line at 50% accuracy",
       y = "Accuracy (Rolling Mean)",
       x = "Trial Number",
       color = "Block")


