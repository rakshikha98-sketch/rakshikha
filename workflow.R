# Exercise 3.1 - Reproducible Workflow

# Load required package
library(readr)

# Import Week 2 dataset
clinic_raw <- read_csv("data/clinic_raw.csv", skip = 2)

# Inspect the dataset
head(clinic_raw)
str(clinic_raw)
summary(clinic_raw)
# Load dplyr
library(dplyr)

# Clean the clinic dataset
clinic_clean <- clinic_raw %>%
  mutate(
    weight_kg = as.numeric(na_if(trimws(weight_kg), "-")),
    sex = toupper(trimws(sex)),
    site = gsub("_", " ", tolower(trimws(site))),
    blood_pressure = na_if(trimws(blood_pressure), "N/A"),
    blood_pressure = na_if(blood_pressure, "-")
  )

# Check the cleaned dataset
str(clinic_clean)
summary(clinic_clean)
sum(is.na(clinic_clean$weight_kg))
# Calculate mean age by clinic site

mean_age_by_site <- clinic_clean %>%
  group_by(site) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE)
  )

print(mean_age_by_site)
# Create a bar chart of mean age by clinic site
library(ggplot2)
# Create a bar chart of mean age by clinic site

ggplot(mean_age_by_site, aes(x = site, y = mean_age)) +
  geom_col() +
  labs(
    title = "Mean Age by Clinic Site",
    x = "Clinic Site",
    y = "Mean Age (years)"
  )
# Export cleaned dataset
write_csv(clinic_clean, "data/clinic_clean.csv")
# Updated workflow for Git version control practice