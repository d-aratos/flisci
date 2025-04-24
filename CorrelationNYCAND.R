

if (!require("dplyr")) install.packages("dplyr")
if (!require("corrplot")) install.packages("corrplot")
if (!require("magrittr")) install.packages("magrittr")
if (!require("ggplot2")) install.packages("ggplot2")


library(ggplot2)
library(dplyr)
library(magrittr)
library(corrplot)

X2021_YRBSS_with_states <- X2021_YRBSS_with_states %>%
  mutate(stfips = as.character(stfips))

generate_lgb_only_matrix <- function(data, fips_code, state_name) {
  state_data <- data %>% filter(stfips == fips_code)
  
  cleaned_data <- state_data %>%
    mutate(
      lgb = case_when(
        q65 %in% c(2, 3) ~ 1,
        q65 == 1 ~ 0,
        q65 %in% c(4, 5, 6) ~ NA_real_
      ),
      weapon_school = case_when(q12 == 1 ~ 0, q12 == 2 ~ 1, q12 == 3 ~ 2, q12 == 4 ~ 3, q12 == 5 ~ 4, TRUE ~ NA_real_),
      physical_fight = as.numeric(q16),
      forced_sex = case_when(q19 == 1 ~ 1, q19 == 2 ~ 0, TRUE ~ NA_real_),
      dating_violence = case_when(
        q22 == 1 ~ NA_real_, q22 == 2 ~ 0, q22 == 3 ~ 1, q22 == 4 ~ 2,
        q22 == 5 ~ 3, q22 == 6 ~ 4, TRUE ~ NA_real_
      ),
      bullied_school = case_when(q23 == 1 ~ 1, q23 == 2 ~ 0, TRUE ~ NA_real_),
      bullied_online = case_when(q24 == 1 ~ 1, q24 == 2 ~ 0, TRUE ~ NA_real_),
      sad_hopeless = case_when(q25 == 1 ~ 1, q25 == 2 ~ 0, TRUE ~ NA_real_),
      considered_suicide = case_when(q26 == 1 ~ 1, q26 == 2 ~ 0, TRUE ~ NA_real_),
      attempted_suicide = case_when(
        q28 == 1 ~ 0, q28 == 2 ~ 1, q28 == 3 ~ 2, q28 == 4 ~ 3, q28 == 5 ~ 4, TRUE ~ NA_real_
      ),
      first_drink_age = case_when(
        q40 == 1 ~ NA_real_, q40 == 2 ~ 8, q40 == 3 ~ 9.5, q40 == 4 ~ 11.5,
        q40 == 5 ~ 13.5, q40 == 6 ~ 15.5, q40 == 7 ~ 17, TRUE ~ NA_real_
      ),
      first_sex_age = case_when(
        q58 == 1 ~ NA_real_, q58 == 2 ~ 10, q58 == 3 ~ 12, q58 == 4 ~ 13,
        q58 == 5 ~ 14, q58 == 6 ~ 15, q58 == 7 ~ 16, q58 == 8 ~ 17, TRUE ~ NA_real_
      ),
      feel_close_school = case_when(
        q96 == 1 ~ 4, q96 == 2 ~ 3, q96 == 3 ~ 2, q96 == 4 ~ 1, q96 == 5 ~ 0, TRUE ~ NA_real_
      )
    ) %>%
    select(
      lgb,
      weapon_school,
      physical_fight,
      forced_sex,
      dating_violence,
      bullied_school,
      bullied_online,
      sad_hopeless,
      considered_suicide,
      attempted_suicide,
      first_drink_age,
      first_sex_age,
      feel_close_school
    ) %>%
    na.omit()
  
  if (nrow(cleaned_data) == 0) {
    message(paste("No data available for", state_name))
    return(NULL)
  }
  
  cor_matrix <- cor(cleaned_data, use = "pairwise.complete.obs")
  return(as.data.frame(t(cor_matrix["lgb", -1, drop = FALSE])))
  
}

california_lgb <- generate_lgb_only_matrix(X2021_YRBSS_with_states, "06", "California")
newyork_lgb <- generate_lgb_only_matrix(X2021_YRBSS_with_states, "36", "New York")
northdakota_lgb <- generate_lgb_only_matrix(X2021_YRBSS_with_states, "38", "North Dakota")


cal_df <- data.frame(Variable = rownames(california_lgb), California = california_lgb[, 1], stringsAsFactors = FALSE)
ny_df <- data.frame(Variable = rownames(newyork_lgb), New_York = newyork_lgb[, 1], stringsAsFactors = FALSE)
nd_df <- data.frame(Variable = rownames(northdakota_lgb), North_Dakota = northdakota_lgb[, 1], stringsAsFactors = FALSE)

combined_corrs <- full_join(cal_df, ny_df, by = "Variable") %>%
  full_join(nd_df, by = "Variable")

combined_corrs <- combined_corrs %>%
  filter(!(is.na(California) & is.na(New_York) & is.na(North_Dakota)))

combined_corrs <- combined_corrs %>%
  mutate(
    Max_Correlation = apply(select(., California, New_York, North_Dakota), 1, function(x) max(abs(x), na.rm = TRUE)),
    State_with_Max = apply(select(., California, New_York, North_Dakota), 1, function(x) {
      states <- c("California", "New York", "North Dakota")
      states[which.max(abs(x))]
    })
  )

summary_text <- apply(combined_corrs, 1, function(row) {
  var <- row["Variable"]
  state <- row["State_with_Max"]
  r <- round(as.numeric(row["Max_Correlation"]), 3)
  paste0(var, " has the highest correlation in ", state, " with r = ", r)
})

cat(paste(summary_text, collapse = "\n"))
combined_corrs <- combined_corrs %>%
  mutate(
    Label = ifelse(is.na(Max_Correlation), "NA", round(Max_Correlation, 3))
  )

p <- ggplot(combined_corrs, aes(x = Max_Correlation, y = reorder(Variable, Max_Correlation), fill = State_with_Max)) +
  geom_col(width = 0.7, na.rm = TRUE) +
  geom_text(aes(label = Label), hjust = -0.1, size = 3.5, na.rm = TRUE) +
  labs(
    title = "Highest Correlation with LGB per Variable (by State)",
    x = "Absolute Correlation (|r|)",
    y = "Survey Question",
    fill = "Top State"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom") +
  xlim(0, max(combined_corrs$Max_Correlation, na.rm = TRUE) + 0.1)

print(p)


##Use Quarto 
