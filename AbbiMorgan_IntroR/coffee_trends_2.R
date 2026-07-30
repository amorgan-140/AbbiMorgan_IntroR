# Load libraries
library(tidyverse)
library(ggplot2)
library(tidyr)
library(forcats)
library(dplyr)
library(janitor)
library(stringr)
library(tools)

data_path <- "C:/AbbiMorgan_IntroR/AbbiMorgan_IntroR/data"

# Get all CSVs in that folder
csv_files <- list.files(path = data_path, pattern = "\\.csv$", full.names = TRUE)

# Load coffee.csv specifically
coffee <- read_csv(file.path(data_path, "coffee.csv"))

# Clean names

df_clean <- coffee %>% clean_names()



# DETECT CHECKBOX COLUMNS (TRUE/FALSE or 0/1)

checkbox_cols <- names(df_clean)[sapply(df_clean, function(x) is.logical(x) || all(x %in% c(0,1,NA)))]

# EXTRACT PARENT QUESTIONS

parent_questions <- checkbox_cols %>%
  str_replace("_$", "") %>%                 # remove trailing underscore
  str_replace("_[^_]+$", "") %>%            # remove last segment (option)
  unique()

# CREATE HUMAN-READABLE LABELS

make_readable <- function(x) {
  x %>%
    str_replace_all("_", " ") %>%
    str_trim() %>%
    toTitleCase()
}

question_labels <- setNames(
  make_readable(parent_questions),
  parent_questions
)


checkbox_parent_summary_grouped <- function(df, checkbox_cols, label_dict) {
  df %>%
    select(submission_id, gender, what_is_your_age, political_affiliation, all_of(checkbox_cols)) %>%
    
    pivot_longer(
      cols = all_of(checkbox_cols),
      names_to = "full_question",
      values_to = "selected"
    ) %>%
    
    filter(selected == TRUE) %>%
    
    mutate(
      parent_raw = str_replace(full_question, "_[^_]+$", ""),
      method_raw = str_extract(full_question, "[^_]+$"),
      
      Question = label_dict[parent_raw],
      Method = make_readable(method_raw)
    ) %>%
    
    group_by(
      Question,
      Method,
      gender,
      political_affiliation
    ) %>%
    
    summarize(
      n = n(),
      .groups = "drop"
    ) %>%
    
    arrange(Question, Method, gender)
}

# RUN FINAL SUMMARY

final_summary <- checkbox_parent_summary_grouped(
  df_clean,
  checkbox_cols,
  question_labels
)

final_summary

library(gt)

final_summary %>%
  gt() %>%
  tab_header(
    title = "Coffee Preparation Methods by Demographics",
    subtitle = "Counts grouped by Question, Method, Gender, Age, and Political Affiliation"
  ) %>%
  cols_label(
    Question = "Question",
    Method = "Method",
    gender = "Gender",
    political_affiliation = "Political Affiliation",
    n = "Count"
  ) %>%
  fmt_number(
    columns = n,
    decimals = 0
  ) %>%
  tab_options(
    table.font.size = 14,
    heading.align = "center"
  )

#────────────────────────────────────────────────────────────
## Brew Method vs Gender 
#────────────────────────────────────────────────────────────

# Count responses for any categorical column (ignore NA)
count_by_category <- function(df, column) {
  df %>%
    drop_na({{ column }}) %>%
    group_by({{ column }}) %>%
    summarize(n = n()) %>%
    arrange(desc(n))
}

# Summarize checkbox-style logical columns (TRUE/FALSE)
summarize_checkboxes <- function(df, pattern) {
  df %>%
    summarize(across(matches(pattern), ~ sum(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "option", values_to = "count") %>%
    arrange(desc(count))
}

# Convert checkbox columns into long format for plotting (ignore NA)
checkbox_long <- function(df, pattern) {
  df %>%
    pivot_longer(
      cols = matches(pattern),
      names_to = "option",
      values_to = "selected"
    ) %>%
    filter(selected == TRUE) %>%
    count(option, sort = TRUE)
}


# ANALYSIS: Gender × Brew Method
# Identify ONLY logical brew checkbox columns (exclude parent questions)
brew_cols <- grep("^how_do_you_brew_coffee_at_home_", names(df_clean), value = TRUE)

# Brew method by gender
brew_by_gender <- df_clean %>%
  pivot_longer(
    cols = all_of(brew_cols),
    names_to = "brew_method",
    values_to = "selected"
  ) %>%
  filter(selected == TRUE) %>%
  drop_na(gender) %>%
  count(gender, brew_method, sort = TRUE) %>%
  mutate(
    method = brew_method %>%
      str_remove("^how_do_you_brew_coffee_at_home_") %>%
      str_replace_all("_", " ") %>%
      tools::toTitleCase()
  )


# Brew method by political affiliation
brew_pol <- df_clean %>%
  pivot_longer(
    cols = all_of(brew_cols),
    names_to = "brew_method",
    values_to = "selected"
  ) %>%
  filter(selected == TRUE) %>%
  drop_na(political_affiliation) %>%
  count(political_affiliation, brew_method, sort = TRUE)

# Reorder brew methods by total count
brew_pol <- brew_pol %>%
  group_by(brew_method) %>%
  mutate(total_method_n = sum(n)) %>%
  ungroup() %>%
  mutate(brew_method = fct_reorder(brew_method, total_method_n))


coffee <- coffee %>%
  mutate(
    Gender = case_when(
      Gender %in% c("Other (please specify)", "Prefer not to say") ~ "Other / Prefer not to say",
      TRUE ~ Gender
    )
  )


# PLOT
p <- brew_by_gender %>%
  ggplot(aes(x = method, y = n, fill = gender)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  theme_minimal() +
  labs(
    title = "Brew Method vs. Gender",
    x = "Brew Method",
    y = "Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)

# Political affiliation vs Brew Method 
#────────────────────────────────────────────────────────────

# Build summary table
brew_pol <- coffee %>%
  pivot_longer(
    cols = matches("^How do you brew coffee at home//? //("),
    names_to = "brew_method",
    values_to = "selected"
  ) %>%
  filter(selected == TRUE) %>%
  drop_na(`Political Affiliation`) %>%
  count(`Political Affiliation`, brew_method, sort = TRUE)

# Clean brew method labels
brew_pol <- brew_pol %>%
  mutate(
    brew_method = str_extract(brew_method, "//((.*)//)") %>%
      str_remove_all("[()]")
  )

# Reorder political affiliation 
brew_pol <- brew_pol %>%
  mutate(`Political Affiliation` = factor(
    `Political Affiliation`,
    levels = c("Democrat", "No Affiliation", "Independent", "Republican")
  ))

  # Sort brew methods by total count across all political groups
brew_pol <- brew_pol %>%
  group_by(brew_method) %>%
  mutate(total_method_n = sum(n)) %>%
  ungroup() %>%
  mutate(brew_method = fct_reorder(brew_method, total_method_n))

]# Horizontal bar chart 
ggplot(brew_pol, aes(x = n, y = brew_method, fill = `Political Affiliation`)) +
  geom_col(position = position_dodge(width = .75)) +
  geom_text(
    aes(label = n),
    position = position_dodge(width = .75),
    hjust = -.5,
    size = 3
  ) +
  theme_minimal() +
  labs(
    title = "Brew Method Distribution by Political Affiliation",
    x = "Count",
    y = "Brew Method",
    fill = "Political Affiliation"
  ) +
  theme(
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "right"
  ) +
  xlim(0, max(brew_pol$n) * 1.30)


## Gender + Why do you drink coffee

# List exact columns
why_cols <- c(
  "Why do you drink coffee? (It tastes good)",
  "Why do you drink coffee? (I need the caffeine)",
  "Why do you drink coffee? (I need the ritual)",
  "Why do you drink coffee? (It makes me go to the bathroom)",
  "Why do you drink coffee? (Other)"
)

# Pivot long: one row per selected reason
gender_why <- coffee %>%
  pivot_longer(
    cols = all_of(why_cols),
    names_to = "reason_raw",
    values_to = "selected"
  ) %>%
  filter(selected == TRUE) %>%     # logical TRUE only
  drop_na(Gender) %>%
  mutate(
    reason = str_extract(reason_raw, "//((.*)//)") %>% 
      str_remove_all("[()]")
  )

# Summary table
why_summary <- gender_why %>%
  count(Gender, reason) %>%
  group_by(Gender) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  ungroup()

#  bar chart
ggplot(why_summary, aes(x = reason, y = n, fill = Gender)) +
  geom_col(position = position_dodge(width = 1)) +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.75),
    vjust = -0.3,
    size = 2.5
  ) +
  theme_minimal() +
  labs(
    title = "Reasons for Drinking Coffee by Gender",
    x = "Reason",
    y = "Count",
    fill = "Gender"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
  )


names(df_clean)
