df <- read.csv("C:\\Users\\jroch\\Downloads\\DEA_Agent_Table2.csv")



library(deaR)
library(dplyr)

# Inputs: Time, Traffic, Age (cols 2,3,6), Output: Rating (col 5)
dea_data <- read_data(df,
                          ni = 3,
                          no = 1,
                          dmus = 1,
                          inputs = c(2, 3, 6),
                          outputs = 5)



#run dea 
dea_result <- model_basic(dea_data,
                          orientation = "io",
                          rts = "crs",
                          dmu_eval = 1:nrow(df),
                          dmu_ref = 1:nrow(df))

#effeciency scores
eff <- efficiencies(dea_result)
print(eff)
write.csv(eff, "agent_efficiencies2.csv")

# targets for improvement 

targets_df <- targets(dea_result)
write.csv(targets_df, "dea_targets.csv")

#plot and summary reprot 

plot(dea_result)
report <- summary(dea_result)
write.csv(report, "dea_summary_report2.csv")




# Agent age vs correlation 
cor.test(df$Avg_Agent_Age, eff, method = "pearson")

eff <- efficiencies(dea_result)
df$Efficiency <- eff                     

library(ggplot2)

ggplot(df, aes(x = Avg_Agent_Age, y = Efficiency)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Agent Age vs DEA Efficiency",
       x = "Average Agent Age",
       y = "Efficiency Score") +
  theme_minimal()




# Check correlation between age and rating
cor.test(df$Avg_Agent_Age, df$Avg_Agent_Rating, method = "pearson")

ggplot(df, aes(x = Avg_Agent_Age, y = Avg_Agent_Rating)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "Agent Age vs Customer Rating",
       x = "Average Agent Age",
       y = "Average Customer Rating") +
  theme_minimal()

