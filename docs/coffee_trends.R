#| title: "Coffee Trends Analysis (Quarto)"
#| author: "Abbi Morgan"
#| format:
  #|   html:
  #|     toc: true
#|     theme: cosmo

#| ## Setup and packages
#| #Load required packages. Install any missing packages first.

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, janitor, lubridate, scales,
  gtsummary, broom, ggpubr, corrplot,
  cluster, factoextra, nnet, broom.mixed, ranger
)

# ## Load data
#Replace the filename below if needed.
csv_file <- "GACTT_RESULTS_ANONYMIZED_v2.csv"
data_raw <- read.csv("GACTT_RESULTS_ANONYMIZED_v2.csv", stringsAsFactors = FALSE)

# Quick peek at original column names (two examples shown)
# The CSV includes columns such as "What.is.your.age." and "How.many.cups.of.coffee.do.you.typically.drink.per.day.".

# Clean names to snake_case for easier coding

df <- data_raw %>%
  janitor::clean_names() %>%
  as_tibble()

# Print first rows for verification (Quarto will show this)

head(df, 3)

#| ## Data preparation
#| ##Convert obvious numeric columns and factorize categorical columns.
#| ###Adjust these conversions if your columns use different encodings.


df <- df %>%
  mutate(
    age = as.numeric(what_is_your_age),
    cups_per_day = as.numeric(how_many_cups_of_coffee_do_you_typically_drink_per_day),
    # Favorite overall coffee (clean short name)
    favorite_overall_coffee = coalesce(lastly_what_was_your_favorite_overall_coffee, lastly_what_was_your_favorite_overall_coffee),
    # Work location
    work_from = case_when(
      do_you_work_from_home_or_in_person %in% c("Work from home", "Home") ~ "home",
      do_you_work_from_home_or_in_person %in% c("In person", "Office") ~ "in_person",
      TRUE ~ as.character(do_you_work_from_home_or_in_person)
    ),
    # Spending numeric (attempt to parse numbers)

  monthly_spend = parse_number(in_total_much_money_do_you_typically_spend_on_coffee_in_a_month)
  )

# Convert many multi-choice columns to logical (TRUE/FALSE) where appropriate
# Example: brew methods (pour_over, french_press, espresso, etc.)

brew_cols <- df %>% select(starts_with("how_do_you_brew_coffee_at_home")) %>% names()

# If the dataset uses separate columns for each option (as appears), convert to logical 0/1 if needed
# (Assumes values like "Yes" or "Checked" or "1" indicate selection)

df <- df %>%
  mutate(across(all_of(brew_cols), ~ ifelse(. %in% c("Yes", "Checked", "1", "TRUE", "TRUE "), 1,
                                            ifelse(is.na(.) | . == "" , 0, 0))))

#| ## 1) High-level summaries

#| ##The following sections compute descriptive summaries for demographics, consumption, brew methods, add-ins, spending, and tasting results.

# 1a Demographics: age, gender, education, employment, political affiliation

demo_summary <- df %>%
  summarise(
    n = n(),
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    pct_missing_age = mean(is.na(age)) * 100
  )

demo_table <- df %>%
  select(age, gender, education_level, employment_status, political_affiliation) %>%
  tbl_summary(
    by = NULL,
    missing = "no"
  )

# 1b Consumption patterns: cups per day distribution

cups_summary <- df %>%
  summarise(
    mean_cups = mean(cups_per_day, na.rm = TRUE),
    median_cups = median(cups_per_day, na.rm = TRUE),
    pct_0_cups = mean(cups_per_day == 0, na.rm = TRUE) * 100
  )

# 1c Brew methods: frequency table for common methods

brew_methods <- df %>%
  select(starts_with("how_do_you_brew_coffee_at_home")) %>%
  pivot_longer(everything(), names_to = "method", values_to = "selected") %>%
  group_by(method) %>%
  summarise(count = sum(as.numeric(selected), na.rm = TRUE)) %>%
  arrange(desc(count))

# 1d Add-ins: milk, sugar, syrups

addins <- df %>%
  select(starts_with("do_you_usually_add_anything_to_your_coffee"),
         starts_with("what_kind_of_dairy_do_you_add"),
         starts_with("what_kind_of_sugar_or_sweetener_do_you_add"),
         starts_with("what_kind_of_flavorings_do_you_add")) %>%
  pivot_longer(everything(), names_to = "add_in", values_to = "selected") %>%
  group_by(add_in) %>%
  summarise(count = sum(!is.na(selected) & selected != "" & selected != "No" & selected != "No...just.black.", na.rm = TRUE)) %>%
  arrange(desc(count))

# 1e Spending and equipment

spend_summary <- df %>%
  summarise(
    mean_monthly_spend = mean(monthly_spend, na.rm = TRUE),
    median_monthly_spend = median(monthly_spend, na.rm = TRUE),
    mean_equipment_spend_5y = mean(approximately_how_much_have_you_spent_on_coffee_equipment_in_the_past_5_years, na.rm = TRUE)
  )

# 1f Tasting results: bitterness, acidity, personal preference for Coffee A-D

tasting_cols <- df %>% select(starts_with("coffee_a"), starts_with("coffee_b"), starts_with("coffee_c"), starts_with("coffee_d")) %>% names()

tasting_summary <- df %>%
  summarise(across(all_of(tasting_cols), ~ mean(as.numeric(.), na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "mean_value")

#| ## 2) Deeper statistical analyses
#| These analyses answer the deeper questions: predictors of preference, correlations, clustering, and relationships between spending/expertise.

# 2a Correlation matrix among numeric variables (cups_per_day, monthly_spend, tasting scores)
numeric_vars <- df %>%
  select(cups_per_day, monthly_spend, starts_with("coffee_a"), starts_with("coffee_b"), starts_with("coffee_c"), starts_with("coffee_d")) %>%
  mutate(across(everything(), ~ as.numeric(.)))

corr_mat <- cor(numeric_vars, use = "pairwise.complete.obs")

# 2b Predictors of choosing a particular coffee (example: favorite_overall_coffee == "Coffee A")
# We'll fit a multinomial model predicting favorite_overall_coffee from age, cups_per_day, roast preference, and expertise.
# Ensure favorite_overall_coffee is a factor with reasonable levels
df <- df %>% mutate(favorite_overall_coffee = as.factor(favorite_overall_coffee),
                    roast_pref = as.factor(what_roast_level_of_coffee_do_you_prefer),
                    expertise = as.numeric(lastly_how_would_you_rate_your_own_coffee_expertise))

# Fit multinomial logistic regression (nnet::multinom)
multinom_fit <- nnet::multinom(favorite_overall_coffee ~ age + cups_per_day + roast_pref + expertise + monthly_spend,
                               data = df, trace = FALSE)

multinom_tidy <- broom::tidy(multinom_fit, exponentiate = TRUE, conf.int = TRUE)

# 2c Classification / feature importance: random forest to predict favorite coffee
rf_df <- df %>%
  select(favorite_overall_coffee, age, cups_per_day, expertise, monthly_spend, roast_pref) %>%
  mutate(across(where(is.factor), ~ as.character(.))) %>%
  drop_na(favorite_overall_coffee)

rf_df$favorite_overall_coffee <- as.factor(rf_df$favorite_overall_coffee)

rf_fit <- ranger::ranger(favorite_overall_coffee ~ ., data = rf_df, importance = "impurity")
rf_importance <- ranger::importance(rf_fit) %>% enframe(name = "feature", value = "importance") %>% arrange(desc(importance))

# 2d Cluster analysis: create coffee personas using tasting scores and cups_per_day
cluster_data <- df %>%
  select(cups_per_day, starts_with("coffee_a"), starts_with("coffee_b"), starts_with("coffee_c"), starts_with("coffee_d")) %>%
  mutate(across(everything(), ~ as.numeric(.))) %>%
  drop_na()

# Scale and run k-means (k = 3 as a starting point)
cluster_scaled <- scale(cluster_data)
set.seed(123)
k3 <- kmeans(cluster_scaled, centers = 3, nstart = 25)
cluster_summary <- cluster_data %>% mutate(cluster = factor(k3$cluster)) %>% group_by(cluster) %>% summarise(across(everything(), ~ mean(., na.rm = TRUE)))

# 2e Simple hypothesis test example: Do people who add milk prefer sweeter coffees?
# Create a binary indicator for adding milk
df <- df %>% mutate(adds_milk = ifelse(grepl("milk|oat|almond|soy|half", tolower(what_kind_of_dairy_do_you_add %>% coalesce(""))), 1, 0),
                    prefers_sweet = ifelse(grepl("sugar|sweet|syrup|caramel|vanilla", tolower(what_kind_of_sugar_or_sweetener_do_you_add %>% coalesce(""))), 1, 0))

milk_pref_table <- table(df$adds_milk, df$prefers_sweet)
milk_pref_chisq <- chisq.test(milk_pref_table)

#| ## 3) Plots (ggplot2) — code to generate publication-ready figures
#| Each plot is saved to the Quarto output automatically when run in the document.

# Plot 1: Distribution of cups per day
p_cups <- ggplot(df, aes(x = cups_per_day)) +
  geom_histogram(binwidth = 1, fill = "#8B4513", color = "white") +
  labs(title = "Distribution of Cups per Day", x = "Cups per day", y = "Count") +
  theme_minimal(base_size = 14)

p_cups

# Plot 2: Age vs cups per day (scatter + smooth)
p_age_cups <- ggplot(df, aes(x = age, y = cups_per_day)) +
  geom_jitter(alpha = 0.4, width = 0.5, height = 0.1) +
  geom_smooth(method = "loess", color = "#2C3E50") +
  labs(title = "Age vs Cups per Day", x = "Age", y = "Cups per day") +
  theme_minimal(base_size = 14)

p_age_cups

# Plot 3: Brew method counts (bar)
p_brew <- brew_methods %>%
  mutate(method = str_replace_all(method, "how_do_you_brew_coffee_at_home...","")) %>%
  ggplot(aes(x = reorder(method, count), y = count)) +
  geom_col(fill = "#6B8E23") +
  coord_flip() +
  labs(title = "Home Brew Methods (counts)", x = "Method", y = "Count") +
  theme_minimal(base_size = 13)

p_brew

# Plot 4: Add-ins top 10
p_addins <- addins %>% slice_max(count, n = 10) %>%
  ggplot(aes(x = reorder(add_in, count), y = count)) +
  geom_col(fill = "#D2691E") +
  coord_flip() +
  labs(title = "Top Add-ins", x = "Add-in", y = "Count") +
  theme_minimal(base_size = 13)

p_addins

# Plot 5: Per-coffee tasting means (bitterness & acidity & preference)
tasting_long <- tasting_summary %>%
  separate(metric, into = c("coffee", "measure"), sep = "//.//.//.", extra = "merge", fill = "right") %>%
  mutate(coffee = str_to_upper(coffee))

p_tasting <- tasting_long %>%
  filter(measure %in% c("bitterness", "acidity", "personal_preference")) %>%
  ggplot(aes(x = coffee, y = mean_value, fill = measure)) +
  geom_col(position = position_dodge()) +
  labs(title = "Average Tasting Scores by Coffee (mean)", x = "Coffee", y = "Mean score") +
  theme_minimal(base_size = 13)

p_tasting

#| ## 4) Output tables and model summaries
# Show demographic table
demo_table



