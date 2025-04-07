# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)

#icc
var_subject <- 0.34^2  # Variance of intercepts
var_residual <- pi^2 / 3  # Residual variance in logistic models

icc_intercept <- var_subject / (var_subject + var_residual)
print(paste("ICC Intercept:", icc_intercept))
var_slope <- 0.23^2  # Variance of reward_oneback slopes

icc_slope <- var_slope / (var_slope + var_residual)
print(paste("ICC Slope:", icc_slope))


# Step 1: Compute Mean Stay Probability for Each Subject, Session, and Reward Condition
df_summary <- df %>%filter(reoffer_ch==F,reoffer_unch==F)%>%
  group_by(subject_id, session, reward_oneback) %>%
  summarise(stay_key_mean = mean(stay_key, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = reward_oneback, values_from = stay_key_mean, names_prefix = "stay_rw_") %>%
  mutate(diff_stay = stay_rw_1 - stay_rw_0)  # Compute Difference Score

# Step 2: Reshape to Wide Format (Each Subject Has One Row)
df_wide <- df_summary %>%
  select(subject_id, session, diff_stay) %>%
  pivot_wider(names_from = session, values_from = diff_stay, names_prefix = "diff_s")


# Step 4: Compute Correlations Between Sessions
cor_s1_s2 <- cor(df_wide$diff_s1, df_wide$diff_s2, use = "pairwise.complete.obs")
cor_s2_s3 <- cor(df_wide$diff_s2, df_wide$diff_s3, use = "pairwise.complete.obs")
cor_s1_s3 <- cor(df_wide$diff_s1, df_wide$diff_s3, use = "pairwise.complete.obs")

# Print Correlation Results
print(paste("Correlation between Session 1 and 2:", cor_s1_s2))
print(paste("Correlation between Session 2 and 3:", cor_s2_s3))
print(paste("Correlation between Session 1 and 3:", cor_s1_s3))

# Step 5: Generate Scatterplots to Visualize Correlations
ggplot(df_wide, aes(x = diff_s1, y = diff_s2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation: Session 1 vs. Session 2", x = "Diff Stay (S1)", y = "Diff Stay (S2)")

ggplot(df_wide, aes(x = diff_s2, y = diff_s3)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation: Session 2 vs. Session 3", x = "Diff Stay (S2)", y = "Diff Stay (S3)")

ggplot(df_wide, aes(x = diff_s1, y = diff_s3)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation: Session 1 vs. Session 3", x = "Diff Stay (S1)", y = "Diff Stay (S3)")

# Step 6: Compute Cronbach’s Alpha for Overall Reliability
alpha_value <- psych::alpha(df_wide_clean[, 2:4])$total$raw_alpha
print(paste("Cronbach’s Alpha:", alpha_value))  # Higher values (e.g., > 0.7) indicate good reliability

#UNCH

# Step 1: Compute Mean Stay Probability for Each Subject, Session, and Reward Condition
df_summary <- df %>%filter(reoffer_ch==F,reoffer_unch==T) %>%
  group_by(subject_id, session, reward_oneback) %>%
  summarise(stay_unch_card_mean = mean(stay_unch_card, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = reward_oneback, values_from = stay_unch_card_mean, names_prefix = "stay_rw_") %>%
  mutate(diff_stay = stay_rw_1 - stay_rw_0)  # Compute Difference Score

# Step 2: Reshape to Wide Format (Each Subject Has One Row)
df_wide <- df_summary %>%
  select(subject_id, session, diff_stay) %>%
  pivot_wider(names_from = session, values_from = diff_stay, names_prefix = "diff_s")

# Step 3: Filter Out Subjects Who Don't Have All 3 Sessions
df_wide_clean <- df_wide %>%
  filter(!is.na(diff_s1) & !is.na(diff_s2) & !is.na(diff_s3))

# Step 4: Compute Correlations Between Sessions
cor_s1_s2 <- cor(df_wide_clean$diff_s1, df_wide_clean$diff_s2, use = "pairwise.complete.obs")
cor_s2_s3 <- cor(df_wide_clean$diff_s2, df_wide_clean$diff_s3, use = "pairwise.complete.obs")
cor_s1_s3 <- cor(df_wide_clean$diff_s1, df_wide_clean$diff_s3, use = "pairwise.complete.obs")

# Print Correlation Results
print(paste("Correlation between Session 1 and 2:", cor_s1_s2))
print(paste("Correlation between Session 2 and 3:", cor_s2_s3))
print(paste("Correlation between Session 1 and 3:", cor_s1_s3))

# Step 5: Generate Scatterplots to Visualize Correlations
ggplot(df_wide_clean, aes(x = diff_s1, y = diff_s2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation: Session 1 vs. Session 2", x = "Diff Stay (S1)", y = "Diff Stay (S2)")

ggplot(df_wide_clean, aes(x = diff_s2, y = diff_s3)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation: Session 2 vs. Session 3", x = "Diff Stay (S2)", y = "Diff Stay (S3)")

ggplot(df_wide_clean, aes(x = diff_s1, y = diff_s3)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation: Session 1 vs. Session 3", x = "Diff Stay (S1)", y = "Diff Stay (S3)")

# Step 6: Compute Cronbach’s Alpha for Overall Reliability
alpha_value <- psych::alpha(df_wide_clean[, 2:4])$total$raw_alpha
print(paste("Cronbach’s Alpha:", alpha_value))  # Higher values (e.g., > 0.7) indicate good reliability
