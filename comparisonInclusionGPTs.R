library(tidyverse)

datasetGPTinclusion <- read.csv("dataNew/inclusionChatGPT_O4mini_final.csv")

datasetGPTclassification <- read.csv("dataNew/classificationChatGPT_O4mini_final.csv")

cols <- c("Perceptions", "Behavior", "Policy", "Health", "Priority")

# datasetGPTclassification <- subset(datasetGPTclassification, message_content != "")

datasetGPTsimple <- datasetGPTclassification %>%
  select(custom_id, choice_index, classificationChatGPTClean)

datasetGPTsimple$classificationChatGPTClean <- ifelse(datasetGPTsimple$classificationChatGPTClean == "", "None", datasetGPTsimple$classificationChatGPTClean)

getmode <- function(v) {
  v <- na.omit(v)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

colsGPT <- c("Perceptions", "Behaviors", "Policy", "Health", "Priority", "None")

datasetGPTsimple_wide <- datasetGPTsimple %>%
  group_by(custom_id, choice_index) %>% 
  # Remove brackets and single quotes
  mutate(categories = gsub("\\[|\\]|'", "", classificationChatGPTClean)) %>%
  # Split the string into multiple rows
  separate_rows(categories, sep = ",\\s*") %>%
  mutate(categories = str_replace_all(categories, " ", "")) %>% 
  # Create a dummy column
  mutate(dummy = 1) %>%
  # Pivot to a wide format, filling missing values with 0
  tidyr::pivot_wider(names_from = categories, values_from = dummy, values_fill = 0) %>% 
  group_by(custom_id) %>% 
  summarise(
    across(
      all_of(colsGPT),   # or just `categories` if you’re unquoted
      getmode,              # the function to apply
      .names = "{.col}"     # leave the names unchanged; use "mode_{.col}" if you prefer
    )
  )

