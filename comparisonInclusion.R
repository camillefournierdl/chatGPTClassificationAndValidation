########## comparing inclusion only
library(tidyverse)

datasetHand <- read.csv("data/classificationByHand3.csv", sep = ";")

# datasetGPT <- read.csv("data/inclusionChatGPT1704.csv")
datasetGPT <- read.csv("data/inclusionChatGPT_testO4mini.csv")

getmode <- function(v) {
  v <- na.omit(v)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

datasetGPTsummary <- datasetGPT %>%
  group_by(custom_id) %>% 
  summarize(inclusionChatGPTCleanMode = getmode(inclusionChatGPTClean),
            countExclusion = sum(inclusionChatGPTClean == "excluded", na.rm = TRUE),
            countInclusion = sum(inclusionChatGPTClean == "included", na.rm = TRUE))

datasetGPTsimple <- datasetGPT %>%
  select(ID, inclusionChatGPT) %>% 
  rename(
    Inclusion        = inclusionChatGPT,
  ) %>% 
  mutate(reader = "ChatGPT")

datasetGPTsimple$Inclusion <- gsub(
  "\\[|\\]|'",         # pattern: any literal "[", "]" or "'"
  "",                  # replacement: nothing
  datasetGPTsimple$Inclusion
)

datasetGPTsimple$Inclusion <- ifelse(datasetGPTsimple$Inclusion == "included", 1, 0)

datasetHand_fit <- datasetHand %>% 
  select(ID, Inclusion, reader)

#### Merging classifications to make comparisons
dataCompare <- rbind(datasetHand_fit, datasetGPTsimple)

cols <- c("Inclusion")

class_cols <- cols

mismatch_summary <- dataCompare %>%
  group_by(ID) %>%
  summarize(across(all_of(class_cols), ~ n_distinct(.)), .groups = "drop") %>%
  # Identify IDs where at least one classification column has more than one unique value
  filter(if_any(all_of(class_cols), ~ . > 1))

# View the summary of IDs with mismatches
print(mismatch_summary)

####### reader specific 
# Convert classification columns to character and pivot to long format
long_data <- dataCompare %>%
  select(ID, reader, all_of(class_cols)) %>%
  mutate(across(all_of(class_cols), as.character)) %>%
  pivot_longer(cols = all_of(class_cols), 
               names_to = "variable", 
               values_to = "response")

# Self join to compare responses from different readers for the same ID and variable
pairwise <- long_data %>%
  inner_join(long_data, by = c("ID", "variable"), suffix = c("_1", "_2")) %>%
  filter(reader_1 < reader_2) %>%   # to avoid duplicate pairs
  mutate(agreement = ifelse(response_1 == response_2, "Agree", "Disagree"))

# Create a contingency table of overall agreement/disagreement
agreement_table <- table(pairwise$agreement)
print(agreement_table)

reader_pair_summary <- pairwise %>%
  group_by(reader_1, reader_2, agreement) %>%
  summarize(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = agreement, values_from = count, values_fill = 0)

print(reader_pair_summary)

##### comparing columns 
# Create a contingency table for each classification variable
agreement_tables <- pairwise %>%
  group_by(variable, agreement) %>%
  summarize(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = agreement, values_from = count, values_fill = 0)

print(agreement_tables)

### 
# Get list of unique raters
raters <- unique(dataCompare$reader)

# Create all unique pairs of raters
pairs <- combn(raters, 2, simplify = FALSE)

# Prepare a data frame to store percent agreement results
agreement_results <- data.frame(Pair = character(), 
                                Category = character(), 
                                PercentAgreement = numeric(),
                                PairType = character(),
                                stringsAsFactors = FALSE)
categories <- class_cols

# Loop over each pair and each category
for (pair in pairs) {
  rater1 <- pair[1]
  rater2 <- pair[2]
  
  # Subset df to rows for the two raters
  df_pair <- dataCompare %>% 
    filter(reader %in% c(rater1, rater2)) %>%
    group_by(ID) %>% 
    filter(n() == 2) %>%  # Keep only papers rated by both
    ungroup()
  
  for (cat in categories) {
    # For the given category, pivot to wide format so that each row is a paper 
    # with two columns for the two raters' responses.
    df_wide <- df_pair %>% 
      select(ID, reader, !!sym(cat)) %>% 
      pivot_wider(names_from = reader, values_from = !!sym(cat))
    
    # Only process if there is any data after pivoting
    if (nrow(df_wide) > 0) {
      # Calculate the raw percent agreement
      agreement <- mean(df_wide[[rater1]] == df_wide[[rater2]], na.rm = TRUE)
      
      # Label the pair type
      pair_type <- ifelse(grepl("ChatGPT", paste(rater1, rater2)), "Human-Model", "Human-Human")
      
      # Append the result
      agreement_results <- rbind(agreement_results,
                                 data.frame(Pair = paste(rater1, rater2, sep = "-"),
                                            Category = cat,
                                            PercentAgreement = agreement,
                                            PairType = pair_type,
                                            stringsAsFactors = FALSE))
      
    }
  }
}

# Display the results
print(agreement_results)

### visualise this

# Boxplot with jittered points to show distribution by Category and PairType
# ggplot(agreement_results, aes(x = Category, y = PercentAgreement, fill = PairType)) +
#   geom_boxplot(outlier.shape = NA, alpha = 0.7) +
#   geom_jitter(width = 0.05, alpha = 0.5, color = "black") +
#   labs(title = "Pairwise Agreement by Category and Pair Type",
#        x = "Category",
#        y = "Percent Agreement") +
#   theme_minimal()

ggplot(agreement_results, aes(x = Category, y = PercentAgreement, fill = PairType)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA, alpha = 0.7) +
  geom_jitter(aes(shape = PairType), 
              position = position_dodge(width = 0.8), alpha = 1) +
  labs(title = "Pairwise Agreement by Category and Pair Type",
       x = "Category",
       y = "Percent Agreement") +
  theme_minimal()

agg_data <- agreement_results %>%
  group_by(Category, PairType) %>%
  summarise(
    mean_agreement = mean(PercentAgreement, na.rm = TRUE),
    sd_agreement = sd(PercentAgreement, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(se = sd_agreement / sqrt(n))

# Plot bar chart with error bars (using standard error)
ggplot(agg_data, aes(x = Category, y = mean_agreement, fill = PairType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_agreement - se, ymax = mean_agreement + se),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  labs(title = "Average Agreement by Category and Pair Type",
       x = "Category",
       y = "Average Percent Agreement") +
  theme_minimal()

### getting more insights
# Calculate the prevalence (positive rate) for each category per reader

# Step 1: Calculate prevalence (positive rate) for each category per reader
prevalence_data <- dataCompare %>%
  pivot_longer(cols = categories, names_to = "Category", values_to = "Value") %>%
  group_by(reader, Category) %>%
  summarise(positive_rate = mean(Value, na.rm = TRUE),
            .groups = "drop")

# View the prevalence data
print(prevalence_data)

# Example plot: Compare positive rates per category for each reader
ggplot(prevalence_data, aes(x = Category, y = positive_rate, fill = reader)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Prevalence of Positive Class (1) by Category and reader",
       x = "Category",
       y = "Positive Rate") +
  theme_minimal()

# Step 2: For human raters, compute the mean positive rate and standard deviation per category.
human_prevalence <- prevalence_data %>%
  filter(reader != "ChatGPT") %>%
  group_by(Category) %>%
  summarise(mean_human_rate = mean(positive_rate, na.rm = TRUE),
            sd_human_rate = sd(positive_rate, na.rm = TRUE),
            .groups = "drop")

# Step 3: For the model, extract the positive rate for each category.
model_prevalence <- prevalence_data %>%
  filter(reader == "ChatGPT") %>%
  select(Category, model_rate = positive_rate)

# Step 4: Merge the human and model results by Category.
comparison_data <- left_join(model_prevalence, human_prevalence, by = "Category")

# Step 5: Reshape for plotting the two groups (Model and Humans).
comparison_long <- comparison_data %>%
  pivot_longer(cols = c(model_rate, mean_human_rate),
               names_to = "Rater", values_to = "Rate") %>%
  mutate(Rater = recode(Rater,
                        "model_rate" = "Model",
                        "mean_human_rate" = "Humans"))

comparison_long$sd_human_rate <- ifelse(comparison_long$Rater == "Model", 0, comparison_long$sd_human_rate)

# Step 6: Plot the comparison with error bars (only for the human group).
ggplot(comparison_long, aes(x = Category, y = Rate, fill = Rater)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
  # Add error bars only for Humans
  geom_errorbar(data = comparison_long,
                aes(x = Category,
                    ymin = Rate - sd_human_rate,
                    ymax = Rate + sd_human_rate),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  labs(title = "Mean Positive Rate by Category",
       x = "Category",
       y = "Mean Positive Rate") +
  theme_minimal()


### papers that see disagreements:
disagreements <- subset(datasetHand, ID %in% unique(subset(pairwise, agreement == "Disagree")$ID)) %>% select(ID, Title, Abstract) %>% group_by(ID) %>% 
  summarize(Title = first(Title),
            Abstract = first(Abstract))

inspectDisagreements <- merge(pairwise, disagreements, by = "ID", all.x=F, all.y = F)

