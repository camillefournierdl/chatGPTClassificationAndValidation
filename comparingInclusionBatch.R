library(tidyverse)

datasetGPTBatch1 <- read.csv("data/inclusionChatGPT_test2.csv", sep = ",") %>% 
  rename(ID = custom_id,
         messageBatch = message_content,
         inclusionBatch = inclusionChatGPTClean)

datasetGPT <- read.csv("data/inclusionChatGPT1704.csv")

datasetGPTmerge <- datasetGPT %>%
  rename(messageOld = response,
         inclusionOld = inclusionChatGPT) %>% 
  select(ID, messageOld, inclusionOld)

datasetGPTmerge$inclusionOld <- gsub(
  "\\[|\\]|'",         # pattern: any literal "[", "]" or "'"
  "",                  # replacement: nothing
  datasetGPTmerge$inclusionOld
)

test <- merge(datasetGPTBatch1, datasetGPTmerge)

#### Merging classifications to make comparisons

datasetHand <- read.csv("data/classificationByHand3.csv", sep = ";")

datasetGPTBatch1 <- read.csv("data/inclusionChatGPT_test2.csv", sep = ",") %>% 
  rename(ID = custom_id,
         messageBatch = message_content,
         Inclusion = inclusionChatGPTClean) %>% 
  select(ID, Inclusion) %>% 
  mutate(reader = "ChatBatch")

datasetGPT <- read.csv("data/inclusionChatGPT1704.csv")

datasetGPTmerge <- datasetGPT %>%
  rename(Inclusion = inclusionChatGPT) %>% 
  select(ID, Inclusion) %>% 
  mutate(reader = "ChatOld")

datasetGPTmerge$Inclusion <- gsub(
  "\\[|\\]|'",         # pattern: any literal "[", "]" or "'"
  "",                  # replacement: nothing
  datasetGPTmerge$Inclusion
)

GPTtogether <- rbind(datasetGPTBatch1, datasetGPTmerge)

GPTtogether$Inclusion <- ifelse(GPTtogether$Inclusion == "included", 1, 0)

datasetHand_fit <- datasetHand %>% 
  select(ID, Inclusion, reader)

dataCompare <- rbind(datasetHand_fit, GPTtogether)

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
