
install.packages("tidyverse")
install.packages("readxl")
install.packages("deaR")  # Only if you're proceeding to DEA later

library(tidyverse)
library(readxl)
library(deaR)


zomato_raw <- read.csv("C:\\Users\\jroch\\Downloads\\Zomato Dataset.csv\\Zomato Dataset.csv")


# "Low" = 1, "Medium" = 2, "High" = 3, "Jam" = 4
traffic_map <- c("Low" = 1, "Medium" = 2, "High" = 3, "Jam" = 4)
zomato_raw$Traffic_Score <- traffic_map[zomato_raw$Road_traffic_density]

#Remove NA or problematic rows
zomato_clean <- zomato_raw %>%
  filter(!is.na(Delivery_person_ID),
         !is.na(Time_taken..min.),
         !is.na(Delivery_person_Ratings),
         !is.na(Traffic_Score),
         !is.na(Delivery_person_Age),
         !is.na(multiple_deliveries))

# Rename columns for simplicity 
zomato_clean <- zomato_clean %>%
  rename(
    Agent_ID = Delivery_person_ID,
    Time_Taken = Time_taken..min.,
    Agent_Rating = Delivery_person_Ratings,
    Agent_Age = Delivery_person_Age,
    Multiple_Deliveries = multiple_deliveries
  )

#Aggregate per Delivery Agent
dea_df <- zomato_clean %>%
  group_by(Agent_ID) %>%
  summarise(
    Avg_Time_Taken_Min = mean(Time_Taken, na.rm = TRUE),
    Avg_Traffic_Score = mean(Traffic_Score, na.rm = TRUE),
    Avg_Multiple_Deliveries = mean(Multiple_Deliveries, na.rm = TRUE),
    Avg_Agent_Rating = mean(Agent_Rating, na.rm = TRUE),
    Avg_Agent_Age = mean(Agent_Age, na.rm = TRUE)
  ) %>%
  ungroup()

# Save Aggregated DEA Table
write.csv(dea_df, "DEA_Agent_Table.csv", row.names = FALSE)

#View the result 
View(dea_df)
