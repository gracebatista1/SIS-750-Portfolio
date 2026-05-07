library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)
library(stringr)
library(forcats)

getwd()
setwd("/Users/gracebatista/Desktop/Data Analysis SP 26/PS4 Colombia")

## Open data file 

data <- read_csv("Colombia_2023.csv", skip = 1)

## Start selcting the variable we will be viewing
econ_vars <- c(
  "SEXO",
  "P5STGBS",
  "P6STGBS",
  "P7ST",
  "P8STGBS",
  "P11STGBS.B"
)

## COmbinging the data and the filtered variables together 
col_econ <- data %>%
  select(any_of(econ_vars))

## Begin recoding the Likert-Scale

col_econ_clean <- col_econ %>%
  transmute(
    gender = case_when(
      SEXO == 1 ~ "Man",
      SEXO == 2 ~ "Woman",
      TRUE ~ NA_character_
    ), 
    country_econ_now = case_when(
      P5STGBS == 1 ~ "Very good",
      P5STGBS == 2 ~ "Good",
      P5STGBS == 3 ~ "About average",
      P5STGBS == 4 ~ "Bad",
      P5STGBS == 5 ~ "Very bad",
      P5STGBS %in% c(-1, 0, 8) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    country_econ_next12 = case_when(
      P7ST == 1 ~ "Much better",
      P7ST == 2 ~ "A little better",
      P7ST == 3 ~ "The same",
      P7ST == 4 ~ "A little worse",
      P7ST == 5 ~ "Much worse",
      P7ST %in% c(-2, -1, 0, 8) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    family_econ_next12 = case_when(
      P8STGBS == 1 ~ "Much better",
      P8STGBS == 2 ~ "A little better",
      P8STGBS == 3 ~ "The same",
      P8STGBS == 4 ~ "A little worse",
      P8STGBS == 5 ~ "Much worse",
      P8STGBS %in% c(-2, -1, 0, 8) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    econ_satisfaction = case_when(
      `P11STGBS.B` == 1 ~ "Very satisfied",
      `P11STGBS.B` == 2 ~ "Quite satisfied",
      `P11STGBS.B` == 3 ~ "Not very satisfied",
      `P11STGBS.B` == 4 ~ "Not at all satisfied",
      `P11STGBS.B` %in% c(-2, -1, 0, 8) ~ NA_character_,
      TRUE ~ NA_character_
    ),

  )

### Making these ordered factors for use in tables and graphs 

col_econ_clean <- col_econ_clean %>%
  mutate(
    gender = factor(gender, levels = c("Man", "Woman")),
    
    country_econ_now = factor(
      country_econ_now,
      levels = c("Very good", "Good", "About average", "Bad", "Very bad"),
      ordered = TRUE
    ),
    
    country_econ_next12 = factor(
      country_econ_next12,
      levels = c("Much better", "A little better", "The same", "A little worse", "Much worse"),
      ordered = TRUE
    ),
    
    family_econ_next12 = factor(
      family_econ_next12,
      levels = c("Much better", "A little better", "The same", "A little worse", "Much worse"),
      ordered = TRUE
    ),
    
    econ_satisfaction = factor(
      econ_satisfaction,
      levels = c("Very satisfied", "Quite satisfied", "Not very satisfied", "Not at all satisfied"),
      ordered = TRUE
    ),
  
  )

### Double checking to make sure the NA and DNK are not read in the averages 
## I then made sure to exclude them from this data set 


count(col_econ, P5STGBS, sort = TRUE)
count(col_econ, P6STGBS, sort = TRUE)
count(col_econ, P7ST, sort = TRUE)
count(col_econ, P8STGBS, sort = TRUE)
count(col_econ, `P11STGBS.B`, sort = TRUE)
    


## Creating a table for the question "How Would You Descirbe the Country's CUrrent Economy"

# Step 1: Percent by gender
table_gender <- col_econ_clean %>%
  filter(!is.na(gender), !is.na(country_econ_now)) %>%
  count(gender, country_econ_now) %>%
  group_by(gender) %>%
  mutate(percent = n / sum(n) * 100) %>%
  select(-n)

# Step 2: Total percent
table_total <- col_econ_clean %>%
  filter(!is.na(country_econ_now)) %>%
  count(country_econ_now) %>%
  mutate(percent = n / sum(n) * 100) %>%
  mutate(gender = "Total") %>%
  select(-n)

# Step 3: Combine and reshape
final_table <- bind_rows(table_gender, table_total) %>%
  pivot_wider(
    names_from = gender,
    values_from = percent
  )

# Step 4: Clean formatting
final_table <- final_table %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  rename(Response = country_econ_now,
         Men = Man,
         Women = Woman)

# View table
final_table

kable(final_table, caption = "Perception of Colombia's Current Economy By Gender (Colombia 2023)")


## Making a Bar Graph on 
ggplot(
  col_econ_clean %>% filter(!is.na(gender), !is.na(country_econ_next12)),
  aes(x = gender, fill = country_econ_next12)
) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    y = "Percent",
    fill = "Economic perception"
  ) +
  theme_minimal()

