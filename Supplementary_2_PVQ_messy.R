# Load necessary library
library(dplyr)

# Read the data file
data <- read.csv('/Users/au708718/Library/CloudStorage/OneDrive-AarhusUniversitet/PhD/Projects/My Real Project/DATA/Analysis/PVQ/data - PVQ-RR.csv')

# Define the PVQ-RR scoring key as a list
pvq_rr_keys <- list(
  "Self_direction_Thought" = c(1, 23, 39),
  "Self_direction_Action" = c(16, 30, 56),
  "Stimulation" = c(10, 28, 43),
  "Hedonism" = c(3, 36, 46),
  "Achievement" = c(17, 32, 48),
  "Power_Dominance" = c(6, 29, 41),
  "Power_Resources" = c(12, 20, 44),
  "Face" = c(9, 24, 49),
  "Security_Personal" = c(13, 26, 53),
  "Security_Societal" = c(2, 35, 50),
  "Tradition" = c(18, 33, 40),
  "Conformity_Rules" = c(15, 31, 42),
  "Conformity_Interpersonal" = c(4, 22, 51),
  "Humility" = c(7, 38, 54),
  "Universalism_Nature" = c(8, 21, 45),
  "Universalism_Concern" = c(5, 37, 52),
  "Universalism_Tolerance" = c(14, 34, 57),
  "Benevolence_Care" = c(11, 25, 47),
  "Benevolence_Dependency" = c(19, 27, 55)
)

# Compute the means for each PVQ-RR value
results <- data %>%
  rowwise() %>%
  mutate(
    Self_direction_Thought = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Self_direction_Thought"]]))), na.rm = TRUE),
    Self_direction_Action = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Self_direction_Action"]]))), na.rm = TRUE),
    Stimulation = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Stimulation"]]))), na.rm = TRUE),
    Hedonism = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Hedonism"]]))), na.rm = TRUE),
    Achievement = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Achievement"]]))), na.rm = TRUE),
    Power_Dominance = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Power_Dominance"]]))), na.rm = TRUE),
    Power_Resources = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Power_Resources"]]))), na.rm = TRUE),
    Face = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Face"]]))), na.rm = TRUE),
    Security_Personal = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Security_Personal"]]))), na.rm = TRUE),
    Security_Societal = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Security_Societal"]]))), na.rm = TRUE),
    Tradition = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Tradition"]]))), na.rm = TRUE),
    Conformity_Rules = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Conformity_Rules"]]))), na.rm = TRUE),
    Conformity_Interpersonal = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Conformity_Interpersonal"]]))), na.rm = TRUE),
    Humility = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Humility"]]))), na.rm = TRUE),
    Universalism_Nature = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Universalism_Nature"]]))), na.rm = TRUE),
    Universalism_Concern = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Universalism_Concern"]]))), na.rm = TRUE),
    Universalism_Tolerance = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Universalism_Tolerance"]]))), na.rm = TRUE),
    Benevolence_Care = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Benevolence_Care"]]))), na.rm = TRUE),
    Benevolence_Dependency = mean(c_across(all_of(paste0("Q", pvq_rr_keys[["Benevolence_Dependency"]]))), na.rm = TRUE)
  ) %>%
  ungroup()

# Select only SUB, GENDER, and the newly calculated values
aggregated_data <- results %>%
  dplyr::select(SUB, GENDER, all_of(names(pvq_rr_keys)))

# View the resulting dataframe
head(aggregated_data)

# Further aggregation of the PVQ-RR values
final_results <- aggregated_data %>%
  rowwise() %>%
  mutate(
    # Self-Transcendence
    Self_Transcendence = mean(c(
      Universalism_Nature, Universalism_Concern, Universalism_Tolerance, 
      Benevolence_Care, Benevolence_Dependency
    ), na.rm = TRUE),
    # Self-Enhancement
    Self_Enhancement = mean(c(
      Achievement, Power_Dominance, Power_Resources
    ), na.rm = TRUE),
    # Openness to Change
    Openness_to_Change = mean(c(
      Self_direction_Thought, Self_direction_Action, Stimulation, Hedonism
    ), na.rm = TRUE),
    # Conservation
    Conservation = mean(c(
      Security_Personal, Security_Societal, Tradition, 
      Conformity_Rules, Conformity_Interpersonal
    ), na.rm = TRUE)
  ) %>%
  ungroup()

# Select only SUB, GENDER, and the newly aggregated categories
final_aggregated_data <- final_results %>%
  dplyr::select(SUB, GENDER, 
         Self_Transcendence, Self_Enhancement, Openness_to_Change, Conservation)

# View the final dataframe
head(final_aggregated_data)

# Add the Nationality column to the aggregated_data dataframe
aggregated_data <- aggregated_data %>%
  mutate(
    Nationality = ifelse(SUB <= 66, "Danish", "Chinese")
  )

# Add the Nationality column to the final_aggregated_data dataframe
final_aggregated_data <- final_aggregated_data %>%
  mutate(
    Nationality = ifelse(SUB <= 66, "Danish", "Chinese")
  )


##############################
##############################
##############################
##############################
# PLOTS #
##############################
##############################
##############################
##############################

# Load necessary library
library(ggplot2)
library(dplyr)
library(tidyr)

# Plot 1: Differences in the 4 aggregated values
# Reshape data for plotting
final_plot_data <- final_aggregated_data %>%
  pivot_longer(cols = c(Self_Transcendence, Self_Enhancement, Openness_to_Change, Conservation), 
               names_to = "Aggregated_Value", values_to = "Score")

# Create the first plot
ggplot(final_plot_data, aes(x = Aggregated_Value, y = Score, fill = Nationality)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(), alpha = 0.4, size = 1) +
  scale_fill_manual(values = c("Danish" = "blue", "Chinese" = "red")) +
  theme_minimal() +
  labs(title = "Differences in Aggregated Values by Nationality",
       x = "Aggregated Value",
       y = "Score",
       fill = "Nationality") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Differences in all 19 PVQ-RR values
# Reshape data for plotting
all_values_plot_data <- aggregated_data %>%
  pivot_longer(cols = -c(SUB, GENDER, Nationality), 
               names_to = "Value", values_to = "Score")

# Create the second plot
ggplot(all_values_plot_data, aes(x = Value, y = Score, fill = Nationality)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(), alpha = 0.4, size = 1) +
  scale_fill_manual(values = c("Danish" = "blue", "Chinese" = "red")) +
  theme_minimal() +
  labs(title = "Differences in All PVQ-RR Values by Nationality",
       x = "Value",
       y = "Score",
       fill = "Nationality") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



###########################
####
# CENTERED DATA #
####
###########################
# Step 1: Compute MRAT (Mean Rating) for each participant from the original data
data <- data %>%
  rowwise() %>%
  mutate(
    MRAT = mean(c_across(starts_with("Q")), na.rm = TRUE)  # Calculate mean of Q1-Q57
  ) %>%
  ungroup()

# Step 2: Add MRAT to `aggregated_data`
aggregated_data <- aggregated_data %>%
  left_join(data %>% dplyr::select(SUB, MRAT), by = "SUB")  # Add MRAT column

# Step 3: Center the 19 values using MRAT
aggregated_data <- aggregated_data %>%
  mutate(
    across(
      c(Self_direction_Thought, Self_direction_Action, Stimulation, Hedonism,
        Achievement, Power_Dominance, Power_Resources, Face, Security_Personal,
        Security_Societal, Tradition, Conformity_Rules, Conformity_Interpersonal,
        Humility, Universalism_Nature, Universalism_Concern, Universalism_Tolerance,
        Benevolence_Care, Benevolence_Dependency),
      ~ . - MRAT,  # Subtract MRAT
      .names = "centered_{.col}"  # Rename to indicate centered values
    )
  )

# Step 4: Recalculate the 4 aggregated categories using centered scores
final_centered_data <- aggregated_data %>%
  rowwise() %>%
  mutate(
    Self_Transcendence = mean(c(
      centered_Universalism_Nature, centered_Universalism_Concern,
      centered_Universalism_Tolerance, centered_Benevolence_Care, centered_Benevolence_Dependency
    ), na.rm = TRUE),
    Self_Enhancement = mean(c(
      centered_Achievement, centered_Power_Dominance, centered_Power_Resources
    ), na.rm = TRUE),
    Openness_to_Change = mean(c(
      centered_Self_direction_Thought, centered_Self_direction_Action,
      centered_Stimulation, centered_Hedonism
    ), na.rm = TRUE),
    Conservation = mean(c(
      centered_Security_Personal, centered_Security_Societal, centered_Tradition,
      centered_Conformity_Rules, centered_Conformity_Interpersonal
    ), na.rm = TRUE)
  ) %>%
  ungroup()

# Step 5: Prepare data for plotting the 4 aggregated categories
final_plot_data_centered <- final_centered_data %>%
  pivot_longer(
    cols = c(Self_Transcendence, Self_Enhancement, Openness_to_Change, Conservation),
    names_to = "Aggregated_Value",
    values_to = "Score"
  )

# Plot 1: Centered aggregated values by nationality
ggplot(final_plot_data_centered, aes(x = Aggregated_Value, y = Score, fill = Nationality)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(), alpha = 0.4, size = 1) +
  scale_fill_manual(values = c("Danish" = "blue", "Chinese" = "red")) +
  theme_minimal() +
  labs(title = "Differences in Centered Aggregated Values by Nationality",
       x = "Aggregated Value",
       y = "Centered Score",
       fill = "Nationality") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 6: Prepare data for plotting all 19 centered values
all_values_plot_data_centered <- aggregated_data %>%
  pivot_longer(
    cols = starts_with("centered_"),
    names_to = "Value",
    values_to = "Score"
  ) %>%
  mutate(Value = gsub("centered_", "", Value))  # Simplify names for plot

# Plot 2: Centered PVQ-RR values by nationality
ggplot(all_values_plot_data_centered, aes(x = Value, y = Score, fill = Nationality)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(), alpha = 0.4, size = 1) +
  scale_fill_manual(values = c("Danish" = "blue", "Chinese" = "red")) +
  theme_minimal() +
  labs(title = "Differences in Centered PVQ-RR Values by Nationality",
       x = "Value",
       y = "Centered Score",
       fill = "Nationality") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#######
#######
# CULTURAL FOCUS #
######
######

# Step 1: Compute Personal Focus and Social Focus
final_centered_data <- final_centered_data %>%
  rowwise() %>%
  mutate(
    Personal_Focus = mean(c(Openness_to_Change, Self_Enhancement), na.rm = TRUE),
    Social_Focus = mean(c(Self_Transcendence, Conservation), na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2: Prepare data for plotting
focus_plot_data <- final_centered_data %>%
  pivot_longer(
    cols = c(Personal_Focus, Social_Focus),
    names_to = "Focus_Type",
    values_to = "Score"
  )

# Step 3: Create the plot
ggplot(focus_plot_data, aes(x = Focus_Type, y = Score, fill = Nationality)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(), alpha = 0.4, size = 1) +
  scale_fill_manual(values = c("Danish" = "blue", "Chinese" = "red")) +
  theme_minimal() +
  labs(
    title = "Differences in Personal and Social Focus by Nationality",
    x = "Focus Type",
    y = "Centered Score",
    fill = "Nationality"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################################
###############################################
######## using Jan (stick figure) method ######
###############################################
###############################################

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Read the data file
data <- read.csv('/Users/au708718/Library/CloudStorage/OneDrive-AarhusUniversitet/PhD/Projects/My Real Project/DATA/Analysis/PVQ/data - PVQ-RR.csv')

# Step 1: Compute MRAT (Mean Rating for all PVQ items)
data <- data %>%
  rowwise() %>%
  mutate(
    pvq_mean = mean(c_across(starts_with("Q")), na.rm = TRUE)  # Mean across Q1-Q57
  ) %>%
  ungroup()

# Step 2: Compute Social and Personal Focus (Uncentered, Directly from PVQ Items)
data <- data %>%
  rowwise() %>%
  mutate(
    social_uc = mean(c(
      Q1, Q23, Q39, Q16, Q30, Q56,  # Self-Direction
      Q10, Q28, Q43, Q3, Q36, Q46,  # Stimulation & Hedonism
      Q17, Q32, Q48, Q6, Q29, Q41,  # Achievement & Power
      Q12, Q20, Q44, Q9, Q24, Q49,  # Face & Security
      Q13, Q26, Q53  # Additional Security
    ), na.rm = TRUE),
    
    personal_uc = mean(c(
      Q2, Q35, Q50, Q18, Q33, Q40,  # Security & Tradition
      Q15, Q31, Q42, Q4, Q22, Q51,  # Conformity
      Q7, Q38, Q54, Q11, Q25, Q47,  # Humility & Benevolence
      Q19, Q27, Q55, Q8, Q21, Q45,  # Universalism Nature
      Q5, Q37, Q52, Q14, Q34, Q57   # Universalism Concern & Tolerance
    ), na.rm = TRUE)
  ) %>%
  ungroup()

# Step 3: Center Social and Personal Focus (Subtract MRAT)
data <- data %>%
  mutate(
    social = social_uc - pvq_mean,
    personal = personal_uc - pvq_mean,
    social_minus_personal = social - personal  # Compute difference
  )

# Step 4: Assign Nationality (First 66 = Danish, Rest = Chinese)
data <- data %>%
  mutate(
    Nationality = ifelse(SUB <= 66, "Danish", "Chinese")
  )

# Step 5: Convert Data to Long Format for Visualization
data_long_focus <- data %>%
  pivot_longer(
    cols = c(social, personal),
    names_to = "Focus_Type",
    values_to = "Centered_Score"
  )

# **Plot 1: Boxplots for Social and Personal Focus**
ggplot(data_long_focus, aes(x = Focus_Type, y = Centered_Score, fill = Nationality)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  scale_fill_manual(values = c("Danish" = "blue", "Chinese" = "red")) +
  theme_minimal() +
  labs(
    title = "Centered Social and Personal Focus by Nationality (Direct PVQ Method)",
    x = "Focus Type",
    y = "Centered Score",
    fill = "Nationality"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# **Plot 2: Density Plot for Social Minus Personal Focus**
ggplot(data, aes(x = social_minus_personal, fill = Nationality)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Danish" = "blue", "Chinese" = "red")) +
  theme_minimal() +
  labs(
    title = "Density Plot: Social Minus Personal Focus (Direct PVQ Method)",
    x = "Social minus Personal focus",
    y = "Density",
    fill = "Nationality"
  )

ggplot(data, aes(x = social_minus_personal, fill = Nationality)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Danish" = "blue", "Chinese" = "red")) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Density Plot: Social Minus Personal Focus (Direct PVQ Method)",
    x = "Social minus Personal focus",
    y = "Density",
    fill = "Nationality"
  )

### THIS JAN WAY IS CORRECT. Now, lets accompany it with some statistics ###
hist(data$social_minus_personal, breaks = 30, main = "Histogram of x", xlab = "x")     # Histogram
qqnorm(data$social_minus_personal); qqline(data$social_minus_personal, col = "red")    
shapiro.test(data$social_minus_personal)

## It is normally distributed
t.test(social_minus_personal ~ Nationality, data = data, var.equal = FALSE)

# Install if needed
install.packages("effectsize")
install.packages("insight")

library(effectsize)

# Cohen's d
cohens_d(social_minus_personal ~ Nationality, data = data)
