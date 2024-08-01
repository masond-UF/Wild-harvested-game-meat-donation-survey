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
comb.dat <- main
rm(main)

# Drop unused rows
comb.dat <- comb.dat |>
  dplyr::select(
    -"Start Date", -"Response Type", -"Progress",
    -"Duration (in seconds)", -Finished, -"Recorded Date",
    -"Response ID", -"Distribution Channel", -"User Language",
    -"Date - Month", -"Date - Day", -"Date - Year"
  )


## --------------- CHANGE COLUMNS ----------------------------------------------

# Two places use the same name...
comb.dat[26, 2] <- "Share the Harvest Columbia"
comb.dat[27, 2] <- "Share the Harvest Jefferson City"

comb.dat[37, 2] <- "Hunters Against Hunger Oklahoma City"
comb.dat[21, 2] <- "Hunters Against Hunger Missoula"

comb.dat[32, 2] <- "Hunters for the Hungry Texas"
comb.dat[41, 2] <- "Hunters for the Hungry San Antonio"
comb.dat[4, 2] <- "Hunters for the Hungry Covington"

comb.dat[39, 2] <- "Farmers and Hunters Feeding the Hungry Fort Recovery"
comb.dat[16, 2] <- "Farmers and Hunters Feeding the Hungry Donald"
comb.dat[18, 2] <- "Farmers and Hunters Feeding the Hungry Sunbury"

comb.dat <- comb.dat |>
  mutate(Program_name = str_to_title(Program_name))

colnames(comb.dat)[1] <- "DateTime"
colnames(comb.dat)[2] <- "Program"
colnames(comb.dat)[3] <- "City"
colnames(comb.dat)[4] <- "State"
colnames(comb.dat)[5] <- "Country"
colnames(comb.dat)[5] <- "Postal"
colnames(comb.dat)[13] <- "Network_name"
colnames(comb.dat)[14] <- "Network_scale"
colnames(comb.dat)[17] <- "Program_start"
colnames(comb.dat)[18] <- "Program_length"
colnames(comb.dat)[20] <- "Sample_start_month"
colnames(comb.dat)[21] <- "Sample_start_year"
colnames(comb.dat)[22] <- "Sample_end_month"
colnames(comb.dat)[23] <- "Sample_end_year"

colnames(comb.dat)[25] <- "Total_donations"
colnames(comb.dat)[26] <- "Total_donations_units"
colnames(comb.dat)[27] <- "White_tailed_deer"
colnames(comb.dat)[28] <- "Mule_deer"
colnames(comb.dat)[29] <- "Pronghorn"
colnames(comb.dat)[30] <- "Elk"
colnames(comb.dat)[31] <- "Turkey"
colnames(comb.dat)[32] <- "Black_bear"
colnames(comb.dat)[33] <- "Moose"
colnames(comb.dat)[34] <- "Pigs"
colnames(comb.dat)[35] <- "Sheep_goats"
colnames(comb.dat)[36] <- "Bison"
colnames(comb.dat)[37] <- "Alligator"
colnames(comb.dat)[38] <- "Game_birds"
colnames(comb.dat)[39] <- "Other_donations"
colnames(comb.dat)[40] <- "White_tailed_deer_units"
colnames(comb.dat)[41] <- "Mule_deer_units"
colnames(comb.dat)[42] <- "Pronghorn_units"
colnames(comb.dat)[43] <- "Elk_units"
colnames(comb.dat)[44] <- "Turkey_units"
colnames(comb.dat)[45] <- "Black_bear_units"
colnames(comb.dat)[46] <- "Moose_units"
colnames(comb.dat)[47] <- "Pigs_units"
colnames(comb.dat)[48] <- "Sheep_goats_units"
colnames(comb.dat)[49] <- "Bison_units"
colnames(comb.dat)[50] <- "Alligator_units"
colnames(comb.dat)[51] <- "Game_birds_units"
colnames(comb.dat)[52] <- "Other_donations_units"
colnames(comb.dat)[53] <- "Donations_comments"

colnames(comb.dat)[54] <- "Average_donation_distance"
colnames(comb.dat)[55] <- "Max_donation_distance"
colnames(comb.dat)[56] <- "Average_donation_distance_units"
colnames(comb.dat)[57] <- "Max_donation_distance_units"
colnames(comb.dat)[58] <- "Program_scale"

colnames(comb.dat)[60] <- "Challenges_USDA_inspection"
colnames(comb.dat)[61] <- "Challenges_state_inspection"
colnames(comb.dat)[62] <- "Challengea_hunters"
colnames(comb.dat)[63] <- "Challenges_funding"
colnames(comb.dat)[64] <- "Challenges_processors"
colnames(comb.dat)[65] <- "Challenges_transportation"
colnames(comb.dat)[66] <- "Challenges_storage"
colnames(comb.dat)[67] <- "Challenges_other"

colnames(comb.dat)[73] <- "Expansion_funding"
colnames(comb.dat)[74] <- "Expansion_hunter"
colnames(comb.dat)[75] <- "Expansion_storage"
colnames(comb.dat)[76] <- "Expansion_transportation"
colnames(comb.dat)[77] <- "Expansion_processor"
colnames(comb.dat)[78] <- "Expansion_other"

colnames(comb.dat)[87] <- "Employees_volunteers"
colnames(comb.dat)[88] <- "Employees_volunteer_hrs"
colnames(comb.dat)[89] <- "Employees_full_time"
colnames(comb.dat)[90] <- "Employees_part_time"
colnames(comb.dat)[91] <- "Employees_paid_hrs"
colnames(comb.dat)[92] <- "Employees_units_volunteers"
colnames(comb.dat)[93] <- "Employees_units_volunteer_hrs"
colnames(comb.dat)[94] <- "Employees_units_full_time"
colnames(comb.dat)[95] <- "Employees_units_part_time"
colnames(comb.dat)[96] <- "Employees_units_paid_hrs"

colnames(comb.dat)[98] <- "Funding_total"
colnames(comb.dat)[99] <- "Funding_total_units"
colnames(comb.dat)[100] <- "Funding_public_donations"
colnames(comb.dat)[101] <- "Funding_corporate_sponsors"
colnames(comb.dat)[102] <- "Funding_nonprofit_sponsors"
colnames(comb.dat)[103] <- "Funding_grants"
colnames(comb.dat)[104] <- "Funding_state_agency"
colnames(comb.dat)[105] <- "Funding_other"
colnames(comb.dat)[106] <- "Funding_public_donations_units"
colnames(comb.dat)[107] <- "Funding_corporate_sponsors_units"
colnames(comb.dat)[108] <- "Funding_nonprofit_sponsors_units"
colnames(comb.dat)[109] <- "Funding_grants_units"
colnames(comb.dat)[110] <- "Funding_state_agency_units"
colnames(comb.dat)[111] <- "Funding_other_units"

# Fix state names
class(comb.dat$State)
convert_to_abbreviation <- function(state) {
  if (state %in% state.abb) {
    return(state) # Already abbreviated
  } else {
    match_index <- match(state, state.name)
    if (!is.na(match_index)) {
      return(state.abb[match_index]) # Convert to abbreviation
    } else {
      return(state) # If not found, return the original
    }
  }
}

comb.dat <- comb.dat %>%
  mutate(State = sapply(State, convert_to_abbreviation))

# Fix the stragglers
comb.dat[1, 4] <- "YK"
comb.dat[16, 4] <- "SC"
comb.dat[19, 4] <- "KY"
comb.dat[22, 4] <- "NE"
comb.dat[28, 4] <- "AK"
comb.dat[30, 4] <- "NJ"

comb.dat[30, 3] <- "Lebanon"

# Create unique ID
library(stringr)
comb.dat <- comb.dat |>
  mutate(Program_ID = paste0(word(Program, 1), "_", City, "_", State)) |>
  dplyr::select(Program_ID, everything())

# Fix Lincoln, NE

## --------------- CREATE DONATIONS DF -----------------------------------------

# Create a vector of column names
col.list <- as_tibble(colnames(comb.dat))

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

donations <- comb.dat |> dplyr::select(
  "Program_ID", all_of(donations.col)
)

# Pivot the data to longer format [CHECK TO SEE IT WORKED]
donations.lg <- donations %>%
  pivot_longer(
    cols = -Program_ID,
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

# Make sure ID is a factor
donations.lg$Program_ID <- as_factor(donations.lg$Program_ID)

# Take a look at the total donations data
processed_data <- donations.lg |>
  filter(Type == "Total_donations") |>
  filter(Donations > 0) |>
  filter(Donations < 300001) |> # this drops programs potentially reporting all-time
  group_by(Program_ID) |>
  summarise(Donations = sum(Donations, na.rm = TRUE)) |>
  arrange(desc(Donations))

# Beg and plead to get the programs in the right order
processed_data$Program_ID <- factor(processed_data$Program_ID, levels = processed_data$Program_ID)

ggplot(processed_data, aes(x = fct_rev(Program_ID), y = Donations)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Total Meat Donations by Program",
    x = "Program",
    y = "Donations (g)"
  ) +
  scale_y_continuous(labels = scales::comma)


unique(processed_data$Program_ID) # 37 values

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

funds <- comb.dat |> dplyr::select(Program_ID, all_of(funds.col))

# Pivot the data to longer format
funds.lg <- funds %>%
  pivot_longer(
    cols = -Program_ID,
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
  group_by(Program_ID) |>
  summarise(Donations = sum(Donations, na.rm = TRUE)) |>
  arrange(desc(Donations))

# Beg and plead to get the programs in the right order
processed_data$Program_ID <- factor(processed_data$Program_ID, levels = processed_data$Program_ID)

ggplot(processed_data, aes(x = fct_rev(Program_ID), y = Donations)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Total Financial Donations by Program",
    x = "Program",
    y = "Donations ($)"
  ) +
  scale_y_continuous(labels = scales::comma)

unique(processed_data$Program_ID) # 28 values

library(fitdistrplus)
descdist(processed_data$Donations) # beta

# Clear the decks
rm(funds, processed_data, funds)

## --------------- CREATE CHALLENGES DF ----------------------------------------

challenges.col <- c(
  "Challenges_USDA_inspection",
  "Challenges_state_inspection",
  "Challengea_hunters",
  "Challenges_funding",
  "Challenges_processors",
  "Challenges_transportation",
  "Challenges_storage",
  "Challenges_other",
  "Challenges_comments"
)

challenges <- comb.dat |> dplyr::select(Program_ID, all_of(challenges.col))

# 10 is a critical challenge, 0 is no challenge

challenges.lg <- challenges |>
  pivot_longer(cols = 2:9, names_to = "Type", values_to = "Score") |>
  dplyr::select(Program_ID, Type, Score, Challenges_comments)

challenges.lg$Score <- as.numeric(challenges.lg$Score)

processed_data <- challenges.lg |>
  group_by(Type) |>
  summarize(
    Mean = mean(Score, na.rm = TRUE),
    std = sd(Score, na.rm = TRUE)
  ) |>
  arrange(desc(Mean))

class(processed_data$Type)
processed_data$Type <- as_factor(processed_data$Type)

new_levels <- sub("^[^_]*_([^_]*).*", "\\1", levels(processed_data$Type))
levels(processed_data$Type) <- new_levels
processed_data$Type <- factor(processed_data$Type, levels = processed_data$Type)

ggplot(processed_data, aes(x = fct_rev(Type), y = Mean)) +
  geom_errorbar(aes(ymin = Mean - std, ymax = Mean + std)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Biggest challenges",
    x = "",
    y = "Mean score"
  ) +
  scale_y_continuous(labels = scales::comma)

rm(processed_data, challenges)

## --------------- CREATE EXPANSION DF -----------------------------------------

expansion.col <- c(
  "Expansion", "Expansion_comments", "Expansion_funding",
  "Expansion_hunter", "Expansion_storage", "Expansion_transportation",
  "Expansion_processor", "Expansion_other", "Expansion_limitations_comments"
)

expansion.all <- comb.dat |> dplyr::select(Program_ID, all_of(expansion.col))

# 10 is a critical expansion limitation, 0 is no limitation

# Lets look at how many said yes/no to need for expansion
expansion <- expansion.all |> dplyr::select(Program_ID, Expansion,
                                               Expansion_comments)
expansion |>
  filter(Expansion != '') |>
  group_by(Expansion) |>
  summarize(Count = n()) |>
  ggplot(aes(x=Expansion, y = Count, fill = Expansion))+
  geom_bar(stat='identity')+
  coord_flip() +
  theme_minimal() +
  theme(legend.position = 'none') +
  labs(
    title = "Is there a need for expansion (n = 39), 77% answer yes",
    x = "",
    y = ""
  )


expansion.lim.lg <- expansion.all |>
  pivot_longer(cols = 4:9, names_to = "Type", values_to = "Score") |>
  dplyr::select(Program_ID, Type, Score, Expansion_limitations_comments)

expansion.lim.lg$Score <- as.numeric(expansion.lim.lg$Score)

processed_data <- expansion.lim.lg |>
  group_by(Type) |>
  summarize(
    Mean = mean(Score, na.rm = TRUE),
    std = sd(Score, na.rm = TRUE)
  ) |>
  arrange(desc(Mean))

class(processed_data$Type)
processed_data$Type <- as_factor(processed_data$Type)

new_levels <- sub("^[^_]*_([^_]*).*", "\\1", levels(processed_data$Type))
levels(processed_data$Type) <- new_levels
processed_data$Type <- factor(processed_data$Type, levels = processed_data$Type)

ggplot(processed_data, aes(x = fct_rev(Type), y = Mean)) +
  geom_errorbar(aes(ymin = Mean - std, ymax = Mean + std)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Expansion limitations",
    x = "",
    y = "Mean score"
  ) +
  scale_y_continuous(labels = scales::comma)

rm(processed_data, expansion.all)

## --------------- CREATE STRUCTURE DF -----------------------------------------

structure.col <- c(
  "Employees_volunteers", "Employees_volunteer_hrs",
  "Employees_full_time", "Employees_part_time",
  "Employees_paid_hrs", "Employees_units_volunteers",
  "Employees_units_volunteer_hrs", "Employees_units_full_time",
  "Employees_units_part_time", "Employees_units_paid_hrs",
  "Employees_comments"
)

structure <- comb.dat |> dplyr::select(Program_ID, all_of(structure.col))

# Pivot the data to longer format
structure.lg <- structure |>
  pivot_longer(
    cols = -c(Program_ID, Employees_comments),
    names_to = "Type_Units",
    values_to = "Value"
  )

structure.lg$Type_Units <- gsub("Employees_", "", as.character(structure.lg$Type_Units))

structure.lg <- structure.lg |>
  separate(Type_Units, into = c("Type", "Units"), sep = "units_", fill = "right")

# Separate the data into value and unit data frames
value_data <- structure.lg |>
  filter(is.na(Units)) |>
  dplyr::select(-Units)
unit_data <- structure.lg |>
  filter(!is.na(Units)) |>
  dplyr::select(-Type) |>
  dplyr::rename(Type = Units) |>
  rename(Units = Value)

# Join the value and unit data frames
structure.lg <- left_join(value_data, unit_data, by = c("Program_ID", "Type", "Employees_comments"))

# Arrange the columns as specified
structure.lg <- structure.lg %>%
  dplyr::select(Program_ID, Type, Value, Units, Employees_comments)

# Fix people inexplicably writing in the value column constantly
structure.lg[55, 3] <- "1644"
structure.lg[82, 3] <- "100"
structure.lg[185, 3] <- "2080"

# Convert to numeric
structure.lg$Value <- as.numeric(structure.lg$Value)

# Clear the decks
rm(unit_data, value_data, structure)

# Function to create a plot for a specific type
create_plot <- function(type, color, label) {
  plot_data <- structure.lg %>%
    filter(Type == type) %>%
    filter(Value > 0) |>
    arrange(Value) %>%
    mutate(Program_ID = factor(Program_ID, levels = unique(Program_ID)))

  ggplot(plot_data, aes(x = Program_ID, y = Value, fill = Type)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = color) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = paste("Values for", type), x = "Program ID", y = paste(label))
}

# Create individual plots
plot_volunteers <- create_plot("volunteers", "#F8766D", "# of Volunteers")
plot_volunteer_hrs <- create_plot("volunteer_hrs", "#00BFC4", "Volunteer Hours")
plot_full_time <- create_plot("full_time", "#00BA38", "# of Full-time Employees")
plot_part_time <- create_plot("part_time", "#C49A00", "# of Part-time Employees")
plot_paid_hrs <- create_plot("paid_hrs", "#FF61CC", "Paid Hours")

# Combine plots using patchwork
library(patchwork)
combined_plot <- plot_volunteers + plot_volunteer_hrs + plot_full_time + plot_part_time + plot_paid_hrs +
  plot_layout(ncol = 2)

# Print the combined plot
print(combined_plot)

## --------------- CREATE CHARACTERISTICS DF -----------------------------------

exclude.ls <- c(donations.col, expansion.col, structure.col, funds.col)
other <- comb.dat |> dplyr::select(-all_of(exclude.ls))

# Export zip codes for map
colnames(other)[7] <- "Zip"
zip <- other |> dplyr::select(Program_ID, Zip)
write.csv(zip, 'Output/Zip-codes.csv', row.names = FALSE)
