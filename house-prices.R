library(tidyverse)
library(ggrepel)
library(lubridate)

# -----------------------
# 1. Load & Clean Data
# ----------------------

setwd("C:/Pyhton/PyCharm Community Edition 2024.2.1/Files/WEbScraping/Personal Projects/malaysia_property")

df_property <- read.csv("malaysia_house_price_data_2025.csv")
df_income <- read.csv("hh_income_state.csv")

df_income <- df_income %>%
  drop_na() %>%
  mutate(state = case_when(
    state == "Pulau Pinang" ~ "Penang",
    state == "W.P. Kuala Lumpur" ~ "Kuala Lumpur",
    state == "W.P. Labuan" ~ "Labuan",
    state == "W.P. Putrajaya" ~ "Putrajaya",
    TRUE ~ state
  )) %>%
  mutate(date = ymd(date)) %>%
  group_by(state) %>%
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup()

states_remaining <- df_property %>%
  group_by(State) %>%
  filter(n() > 50) %>%
  distinct(State) %>%
  pull(State)

df_income <- df_income %>%
  filter(state %in% states_remaining)

df_property <- df_property %>%
  group_by(State) %>%
  filter(n() > 50)

# --------------------------
# 2. Income Overview Plot
# -----------------------------

df_income %>%
  pivot_longer(cols = c(income_mean, income_median), 
               names_to = "Income_Type", 
               values_to = "Income_Value") %>%
  ggplot(aes(x = state, y = Income_Value, fill = Income_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "State", y = "Income", fill = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -------------------------------------
# 3. Transactions vs. PSF (Per State)
# --------------------------------------

df_property %>%
  ggplot(aes(x = Transactions, y = Median_PSF)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm") +
  facet_wrap(~State, scale = "free") +
  theme_minimal()

# ---------------------
# 4. Combined Summary
# ---------------------

df_combined <- df_property %>%
  group_by(State) %>%
  summarise(
    median_price = median(Median_Price, na.rm = TRUE),
    median_psf = median(Median_PSF, na.rm = TRUE),
    transactions = sum(Transactions, na.rm = TRUE)
  ) %>%
  left_join(df_income, by = c("State" = "state")) %>%
  mutate(affordability_ratio = median_price / income_median)

# ----------------------------------------------
# 5. Median Income vs. Property Price (Linear)
# ----------------------------------------------

ggplot(df_combined, aes(x = income_median, y = median_price, label = State)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_text_repel() +
  labs(
    title = "Median Income vs. Property Price by State",
    x = "Median Household Income (RM)",
    y = "Median Property Price (RM)"
  ) +
  theme_minimal()

# -----------------------------
# 6. Log-Log Income vs. Price
# -----------------------------

ggplot(df_combined, aes(x = log(income_median), y = log(median_price))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  geom_text_repel(aes(label = State)) +
  labs(
    x = "Log(Median Income)",
    y = "Log(Median Price)",
    title = "Log-Log Relationship Between Income and Property Price"
  ) +
  theme_minimal()

# ---------------------------------------
# 7. Affordability Index (Price/Income)
# ---------------------------------------

ggplot(df_combined, aes(x = reorder(State, -affordability_ratio), y = affordability_ratio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Housing Affordability by State (Lower is Better)",
    x = "State", y = "Median Price / Median Income"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------
# 8. Tenure Comparison
# -----------------------

df_property %>%
  group_by(State, Tenure) %>%
  summarise(median_psf = median(Median_PSF, na.rm = TRUE)) %>%
  ggplot(aes(x = State, y = median_psf, fill = Tenure)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Median PSF by Tenure Type per State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---------------------------
# 9. Property Type Comparison
# ---------------------------

df_property %>%
  group_by(Type) %>%
  summarise(median_psf = median(Median_PSF, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Type, median_psf), y = median_psf)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Median PSF by Property Type",
    x = "Property Type", y = "Median PSF"
  ) +
  theme_minimal()

# ---------------------------------
# 10. Volume vs. Price (Log Scale)
# --------------------------------

df_combined %>%
  ggplot(aes(x = transactions, y = median_psf, label = State)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Transaction Volume vs. Median PSF (Log Scale)",
    x = "Total Transactions", y = "Median PSF"
  ) +
  theme_minimal()

# --------------------------------
# 11. Linear Model: Income vs PSF
# --------------------------------

model <- lm(median_psf ~ income_median, data = df_combined)
summary(model)

df_combined <- df_combined %>%
  mutate(predicted_psf = predict(model))

ggplot(df_combined, aes(x = income_median)) +
  geom_point(aes(y = median_psf), color = "blue", size = 3) +
  geom_line(aes(y = predicted_psf), color = "red") +
  labs(
    title = "Linear Model: Median Income vs. Median PSF",
    x = "Median Income (RM)", y = "Median PSF (RM/sq ft)"
  ) +
  theme_minimal()

# Diagnostics
plot(model)
