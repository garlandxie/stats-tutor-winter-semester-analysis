# libraries ----
library(janitor)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forcats)
library(stringr)
library(tidyr)

# import ----
df <- read.csv("stats-tutor-data.csv")

# check packaging: raw data ----
str(df)
head(df, n = 5)
tail(df, n = 5)

# clean ----
df_clean <- df %>%
  clean_names %>%
  rename(date = i_date) %>%
  mutate(date = as_date(date))

df_long <- df_clean %>%
  filter(name != "") %>%
  gather(key = data_field, value = counts, 
         -c(date, name, role, field, 
           ongoing_degree, remarks))

# check packaging: clean data ----
str(df_clean)

# data visualisation ----

# bar graph - ongoing degree
df_clean %>%
  filter(name != "") %>%
  group_by(name) %>%
  summarize(degree = unique(ongoing_degree)) %>%
  ggplot(aes(x = fct_infreq(degree))) + 
  geom_bar() + 
  coord_flip() + 
  labs(x = "Ongoing degree") + 
  theme_minimal()

# bar graph - field of study
df_clean %>%
  filter(name != "") %>%
  group_by(name) %>%
  summarize(field = unique(field)) %>%
  ggplot(aes(x = fct_infreq(field))) + 
    geom_bar() + 
    coord_flip() + 
    labs(x = "Field of Study") + 
    theme_minimal()

# bar graph - type of student 
df_clean %>%
  filter(name != "") %>%
  group_by(name) %>%
  summarize(role = unique(role)) %>%
  ggplot(aes(x = fct_infreq(role))) + 
   geom_bar() + 
   coord_flip() + 
   labs(x = "Type of Student") + 
   theme_minimal()

# bar graph - statistical software 
software <- c("R", "Excel", "Spss", "Smart Pls", "Costat")

df_long %>%
  mutate(data_field = str_replace(data_field, "_", " ") %>% str_to_title) %>%
  filter(counts > 0, data_field %in% software) %>%
  ggplot(aes(x = data_field)) + 
  geom_bar() + 
  facet_wrap(~name) +
  scale_y_discrete(limits = 1:10, breaks = 1:10) +
  labs(y = "", x = "") 

# bar graph: types of data problems
data_problems <- c("Data Manipulation", 
                   "Data Visualisation", 
                   "Statistical Modelling") 

df_long %>%
  mutate(data_field = str_replace(data_field, "_", " ") %>% str_to_title) %>%
  filter(counts > 0, data_field %in% data_problems)%>%
  ggplot(aes(x = data_field)) + 
    geom_bar() + 
    facet_wrap(~name) +
    scale_y_discrete(limits = 1:10, breaks = 1:10) +
    coord_flip() + 
    labs(y = "", x = "") 

# bar graph - how many visits per month?
df_clean %>%
  filter(name != "") %>%
  mutate(month = month(date, label = TRUE)) %>%
  ggplot(aes(x = month)) + 
    geom_bar() + 
    labs(x = "", y = "# Visits")

# bar graph - how many visiter per month for each STUDENT?
df_clean %>%
  filter(name != "") %>%
  mutate(month = month(date, label = TRUE)) %>%
  ggplot(aes(x = month)) + 
  geom_bar() + 
  facet_wrap(~name) + 
  labs(x = "", y = "# Visits")

# which weeks did they not show up?
df_clean %>%
  filter(name == "") %>%
  pull(date) 
  

  