library(tidyverse)

datasetGPTclassification <- read.csv("dataNew/classificationChatGPT_O4mini_final.csv")

cols <- c("Perceptions", "Behavior", "Policy", "Health", "Priority")

# datasetGPTclassification <- subset(datasetGPTclassification, message_content != "")

datasetGPTsimple <- datasetGPTclassification %>%
  select(custom_id, choice_index, classificationChatGPTClean)

datasetGPTsimple$classificationChatGPTClean <- ifelse(datasetGPTsimple$classificationChatGPTClean == "", "Blank", datasetGPTsimple$classificationChatGPTClean)

getmode <- function(v) {
  v <- na.omit(v)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

colsGPT <- c("Perceptions", "Behaviors", "Policy", "Health", "Priority", "None", "Blank")

# find a way to measure uncertainty for each category

# for sure, countBlanks, but then what, count the number for every category? Could work

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
      all_of(colsGPT),
      .fns = list(
        mode  = getmode,            # your existing mode function
        count = ~ sum(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

#IDs to check manually for inclusion from classification data
IDmanual <- unique(subset(datasetGPTsimple_wide, Blank_count > 1)$custom_id)

datasetGPTsimple_wide_certain <- subset(datasetGPTsimple_wide, Blank_count < 2)

datasetExclusionClassif <- subset(datasetGPTsimple_wide_certain, None_count > 3)

# Now set up the inclusion dataset to compare
datasetGPTinclusion <- read.csv("dataNew/inclusionChatGPT_O4mini_final.csv")

getmode <- function(v) {
  v <- na.omit(v)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# similarly for Gini
norm_gini <- function(n) {
  p <- n / sum(n)
  G <- 1 - sum(p^2)
  G / (1 - 1/length(n))
}

datasetGPTsummary <- datasetGPTinclusion %>%
  # filter(inclusionChatGPTClean %in% c("excluded", "included")) %>% 
  group_by(custom_id) %>% 
  summarize(inclusionChatGPTCleanMode = getmode(inclusionChatGPTClean),
            countExclusion = sum(inclusionChatGPTClean == "excluded", na.rm = TRUE),
            countInclusion = sum(inclusionChatGPTClean == "included", na.rm = TRUE),
            countBlank = sum(inclusionChatGPTClean == "", na.rm=T))

datasetGPTsummary$gini_norm <- mapply(
  function(a, b, c) norm_gini(c(a, b, c)),
  datasetGPTsummary$countExclusion,
  datasetGPTsummary$countInclusion,
  datasetGPTsummary$countBlank
)

#IDs to check manually for exclusion from inclusion data
IDmanual2 <- unique(subset(datasetGPTsummary, gini_norm > 0.5)$custom_id)
IDmanual3 <- subset(datasetGPTsummary, countBlank > 1)$custom_id

datasetGPTsummary <- subset(datasetGPTsummary, gini_norm < 0.5)
datasetGPTsummary <- subset(datasetGPTsummary, countBlank < 2)

datasetExclusionInclusion <- subset(datasetGPTsummary, countExclusion > 2)

#### comparing overlap
A <- unique(datasetExclusionClassif$custom_id)
B <- unique(datasetExclusionInclusion$custom_id)

compare_ids <- function(a, b) {
  # ensure we’re working with unique IDs
  a_u <- unique(a)
  b_u <- unique(b)
  
  tp <- intersect(a_u, b_u)         # in both
  fn <- setdiff(a_u, b_u)           # only in A
  fp <- setdiff(b_u, a_u)           # only in B
  uni <- union(a_u, b_u)            # union
  
  # metrics
  prec <- length(tp) / length(b_u)  # true positives / predicted
  rec  <- length(tp) / length(a_u)  # true positives / truth
  f1   <- if ((prec + rec) > 0) 2*prec*rec/(prec + rec) else 0
  jacc <- length(tp) / length(uni)  # intersection over union
  
  list(
    only_in_A      = fn,
    only_in_B      = fp,
    in_both        = tp,
    precision      = prec,
    recall         = rec,
    F1             = f1,
    Jaccard_index  = jacc
  )
}

# Run the comparison
res <- compare_ids(A, B)

# Inspect
res # only 6 IDs were excluded by the exclusion query and classified by the classification

