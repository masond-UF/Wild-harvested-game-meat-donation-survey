## --------------- HEADER ------------------------------------------------------
## Script name: 1_Prepare-raw-data.R
## Author: David S. Mason, Wild Harvest Institute
## Department: Fisheries and Wildlife
## Affiliation: Michigan State University
## Date Created: 2024-07-18
## Date Last Modified: 2024-07-18
## Copyright (c) David S. Mason, 2024
## Contact: masonda6@msu.edu, @EcoGraffito
## Purpose of script:

# Plan
# Clean date
# Carve out datasets
# Donations, funding, other, challenges/expansion, employees?

# All of Texas?


## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(lubridate)
library(tidylog)
library(styler)
library(janitor)

# Clear the decks
rm(list = ls())

# Bring in the data
main <- read.csv("Raw-data/1_Survey-Main.csv")

## --------------- COMBINE DATA ------------------------------------------------

# Get column names
main <- main |>
  janitor::row_to_names(row = 1)
main <- main[-1, ]

# Drop blank rows
main <- main |>
  filter(!is.na(Program_name) & Program_name != "")

# Combine data
comb <- main
rm(main)

# Drop unused rows
comb <- comb |>
  dplyr::select(
    -"Start Date", -"Response Type", -"Progress",
    -"Duration (in seconds)", -Finished, -"Recorded Date",
    -"Response ID", -"Distribution Channel", -"User Language",
    -"Date - Month", -"Date - Day", -"Date - Year"
  )


## --------------- CHANGE COLUMNS ----------------------------------------------

# Two places use the same name...
comb[26, 2] <- "Share the Harvest Columbia"
comb[27, 2] <- "Share the Harvest Jefferson City"

comb[37, 2] <- "Hunters Against Hunger Oklahoma City"
comb[21, 2] <- "Hunters Against Hunger Missoula"

comb[32, 2] <- "Hunters for the Hungry Texas"
comb[41, 2] <- "Hunters for the Hungry San Antonio"
comb[4, 2] <- "Hunters for the Hungry Covington"

comb[39, 2] <- "Farmers and Hunters Feeding the Hungry Fort Recovery"
comb[16, 2] <- "Farmers and Hunters Feeding the Hungry Donald"
comb[18, 2] <- "Farmers and Hunters Feeding the Hungry Sunbury"

comb <- comb |>
  mutate(Program_name = str_to_title(Program_name))

colnames(comb)[1] <- "DateTime"
colnames(comb)[2] <- "Program"
colnames(comb)[3] <- "City"
colnames(comb)[4] <- "State"
colnames(comb)[5] <- "Country"
colnames(comb)[5] <- "Postal"
colnames(comb)[13] <- "Network_name"
colnames(comb)[14] <- "Network_scale"
colnames(comb)[17] <- "Program_start"
colnames(comb)[18] <- "Program_length"
colnames(comb)[20] <- "Sample_start_month"
colnames(comb)[21] <- "Sample_start_year"
colnames(comb)[22] <- "Sample_end_month"
colnames(comb)[23] <- "Sample_end_year"
colnames(comb)[25] <- "Total_donations"
colnames(comb)[26] <- "Total_donations_units"
colnames(comb)[27] <- "White_tailed_deer"
colnames(comb)[28] <- "Mule_deer"
colnames(comb)[29] <- "Pronghorn"
colnames(comb)[30] <- "Elk"
colnames(comb)[31] <- "Turkey"
colnames(comb)[32] <- "Black_bear"
colnames(comb)[33] <- "Moose"
colnames(comb)[34] <- "Pigs"
colnames(comb)[35] <- "Sheep_goats"
colnames(comb)[36] <- "Bison"
colnames(comb)[37] <- "Alligator"
colnames(comb)[38] <- "Game_birds"
colnames(comb)[39] <- "Other_donations"
colnames(comb)[40] <- "White_tailed_deer_units"
colnames(comb)[41] <- "Mule_deer_units"
colnames(comb)[42] <- "Pronghorn_units"
colnames(comb)[43] <- "Elk_units"
colnames(comb)[44] <- "Turkey_units"
colnames(comb)[45] <- "Black_bear_units"
colnames(comb)[46] <- "Moose_units"
colnames(comb)[47] <- "Pigs_units"
colnames(comb)[48] <- "Sheep_goats_units"
colnames(comb)[49] <- "Bison_units"
colnames(comb)[50] <- "Alligator_units"
colnames(comb)[51] <- "Game_birds_units"
colnames(comb)[52] <- "Other_donations_units"
colnames(comb)[53] <- "Donations_comments"
colnames(comb)[54] <- "Average_donation_distance"
colnames(comb)[55] <- "Max_donation_distance"
colnames(comb)[56] <- "Average_donation_distance_units"
colnames(comb)[57] <- "Max_donation_distance_units"
colnames(comb)[58] <- "Program_scale"
colnames(comb)[60] <- "Challenges_USDA_inspection"
colnames(comb)[61] <- "Challenges_state_inspection"
colnames(comb)[62] <- "Challengea_hunters"
colnames(comb)[63] <- "Challenges_funding"
colnames(comb)[64] <- "Challenges_processors"
colnames(comb)[65] <- "Challenges_transportation"
colnames(comb)[66] <- "Challenges_storage"
colnames(comb)[67] <- "Challenges_Other"
colnames(comb)[87] <- "Employees_volunteers"
colnames(comb)[88] <- "Employees_volunteer_hrs"
colnames(comb)[89] <- "Employees_full_time"
colnames(comb)[90] <- "Employees_part_time"
colnames(comb)[91] <- "Employees_paid_hrs"
colnames(comb)[92] <- "Employees_units"
colnames(comb)[93] <- "Employees_units"
colnames(comb)[94] <- "Employees_units"
colnames(comb)[95] <- "Employees_units"
colnames(comb)[96] <- "Employees_units"
colnames(comb)[98] <- "Funding_total"
colnames(comb)[99] <- "Funding_total_units"
colnames(comb)[100] <- "Funding_public_donations"
colnames(comb)[101] <- "Funding_corporate_sponsors"
colnames(comb)[102] <- "Funding_nonprofit_sponsors"
colnames(comb)[103] <- "Funding_grants"
colnames(comb)[104] <- "Funding_state_agency"
colnames(comb)[105] <- "Funding_other"
colnames(comb)[106] <- "Funding_public_donations_units"
colnames(comb)[107] <- "Funding_corporate_sponsors_units"
colnames(comb)[108] <- "Funding_nonprofit_sponsors_units"
colnames(comb)[109] <- "Funding_grants_units"
colnames(comb)[110] <- "Funding_state_agency_units"
colnames(comb)[111] <- "Funding_other_units"

## --------------- CREATE DONATIONS DF -----------------------------------------

# Create a vector of column names
col.list <- as_tibble(colnames(comb))

# Create a vector of the columns we want
donations.col <- c(
  "Total_donations",
  "Total_donations_units", "White_tailed_deer", "White_tailed_deer_units",
  "Mule_deer", "Mule_deer_units", "Pronghorn", "Pronghorn_units", "Elk",
  "Elk_units", "Turkey", "Turkey_units", "Black_bear", "Black_bear_units",
  "Moose", "Moose_units", "Pigs", "Pigs_units", "Sheep_goats",
  "Sheep_goats_units", "Bison", "Bison_units", "Alligator", "Alligator_units",
  "Game_birds", "Game_birds_units", "Other_donations", "Other_donations_units"
)

donations <- comb |> dplyr::select(
  "Program", all_of(donations.col)
)

# Pivot the data to longer format
donations.lg <- donations %>%
  pivot_longer(
    cols = -Program,
    names_to = "Type_Units",
    values_to = "Value"
  ) %>%
  separate(Type_Units, into = c("Type", "Unit_Type"), sep = "_units", fill = "right") %>%
  mutate(Unit_Type = ifelse(is.na(Unit_Type), "value", "units")) %>%
  pivot_wider(
    names_from = Unit_Type,
    values_from = Value
  ) %>%
  rename(Units = units)

# Remove characters so the donation column can be numeric
colnames(donations.lg)[3] <- "Donations"
donations.lg$Donations <- gsub("[^0-9.]", "", donations.lg$Donations)


donations.lg[239, 3] <- "4000" # Manually get rid of the period
donations.lg[240, 3] <- "4000" # Manually get rid of the period
donations.lg[253, 3] <- "400" # Manually get rid of the period
donations.lg[254, 3] <- "400" # Manually get rid of the period

donations.lg[309, 3] <- "300000" # Added up the values
donations.lg[337, 3] <- "7610" # Added up the values
donations.lg[463, 3] <- "8059" # Added up the values

donations.lg[534, 3] <- "1000" # Fixed values

# Convert to numeric
donations.lg$Donations <- as.numeric(donations.lg$Donations)

# Round values
donations.lg$Donations <- round(donations.lg$Donations)

# Take a look at the total donations data
processed_data <- donations.lg |>
  filter(Type == "Total_donations") |>
  filter(Donations > 0) |>
  group_by(Program) |>
  summarise(Donations = sum(Donations, na.rm = TRUE)) |>
  arrange(desc(Donations)) |>
  mutate(
    Program_abbr = str_sub(Program, 1, 3),
    Program_abbr = make.unique(Program_abbr, sep = "_")
  ) |>
  mutate(Program_abbr = fct_inorder(Program_abbr))

ggplot(processed_data, aes(x = fct_rev(Program_abbr), y = Donations)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Total Donations by Program",
    x = "Program (Abbreviated)",
    y = "Donations"
  ) +
  scale_y_continuous(labels = scales::comma)

unique(processed_data$Program) # 37 values

library(fitdistrplus)
descdist(processed_data$Donations) # beta

library(EnvStats)
ebeta(processed_data$Donations, method = "mle")

scale_values <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
ebeta(scale_values(processed_data$Donations))

# Clear the decks
rm(donations, processed_data)

## --------------- CREATE FUNDS DF ---------------------------------------------

# Create a vector of the columns we need
funds.col <- c(
  "Funding_total", "Funding_total_units", "Funding_public_donations",
  "Funding_corporate_sponsors", "Funding_nonprofit_sponsors",
  "Funding_grants", "Funding_state_agency", "Funding_other",
  "Funding_public_donations_units", "Funding_corporate_sponsors_units",
  "Funding_nonprofit_sponsors_units", "Funding_grants_units",
  "Funding_state_agency_units", "Funding_other_units"
)

funds <- comb |> dplyr::select(Program, all_of(funds.col))

# Pivot the data to longer format
funds.lg <- funds %>%
  pivot_longer(
    cols = -Program,
    names_to = "Type_Units",
    values_to = "Value"
  ) %>%
  separate(Type_Units, into = c("Type", "Unit_Type"), sep = "_units", fill = "right") %>%
  mutate(Unit_Type = ifelse(is.na(Unit_Type), "value", "units")) %>%
  pivot_wider(
    names_from = Unit_Type,
    values_from = Value
  ) %>%
  rename(Units = units)

# Remove characters so the donation column can be numeric
colnames(funds.lg)[3] <- "Donations"
funds.lg$Donations <- gsub("[^0-9.]", "", funds.lg$Donations)

# Convert to numeric
funds.lg$Donations <- as.numeric(funds.lg$Donations)

# Round values
funds.lg$Donations <- round(funds.lg$Donations)

# Take a look at the total donations data
processed_data <- funds.lg |>
  filter(Type == "Funding_total") |>
  filter(Donations > 0) |>
  group_by(Program) |>
  summarise(Donations = sum(Donations, na.rm = TRUE)) |>
  arrange(desc(Donations)) |>
  mutate(
    Program_abbr = str_sub(Program, 1, 3),
    Program_abbr = make.unique(Program_abbr, sep = "_")
  ) |>
  mutate(Program_abbr = fct_inorder(Program_abbr))

ggplot(processed_data, aes(x = fct_rev(Program_abbr), y = Donations)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Total Donations by Program",
    x = "Program (Abbreviated)",
    y = "Donations"
  ) +
  scale_y_continuous(labels = scales::comma)

unique(processed_data$Program) # 28 values

library(fitdistrplus)
descdist(processed_data$Donations) # beta

# Clear the decks
rm(funds, processed_data)

## --------------- CREATE CHALLENGES/EXPANSION DF ------------------------------

expansion.col <- c(
  "Challenges_USDA_inspection",
  "Challenges_state_inspection",
  "Challengea_hunters",
  "Challenges_funding",
  "Challenges_processors",
  "Challenges_transportation",
  "Challenges_storage",
  "Challenges_Other",
  "Challenges_comments"
)

expansion <- comb |> dplyr::select(Program, all_of(expansion.col))

# 10 is a critical challenge, 0 is no challenge

expansion.lg <- expansion |>
  pivot_longer(cols = 2:9, names_to = "Type", values_to = "Score") |>
  dplyr::select(Program, Type, Score, Challenges_comments)

expansion.lg$Score <- as.numeric(expansion.lg$Score)

processed_data <- expansion.lg |>
  group_by(Type) |>
  summarize(Mean = mean(Score, na.rm = TRUE),
            std = sd(Score, na.rm = TRUE)) |>
  arrange(desc(Mean))

class(processed_data$Type)
processed_data$Type <- as_factor(processed_data$Type)

# new_levels <- gsub(".*_", "", levels(processed_data$Type))
# levels(processed_data$Type) <- new_levels
processed_data$Type <- factor(processed_data$Type, levels = processed_data$Type)


ggplot(processed_data, aes(x = fct_rev(Type), y = Mean)) +
  geom_errorbar(aes(ymin = Mean-std, ymax = Mean+std))+
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Biggest challengs",
    x = "Challenge type",
    y = "Mean score"
  ) +
  scale_y_continuous(labels = scales::comma)

## --------------- CREATE STRUCTURE DF -----------------------------------------
structure <- comb |> select()

## --------------- CREATE CHARACTERISTICS DF -----------------------------------

char <- comb |> select("Program", -all_of(donations.col))
