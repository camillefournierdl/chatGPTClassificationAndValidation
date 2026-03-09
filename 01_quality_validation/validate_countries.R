library(tidyverse)

if (!exists("modelName")) modelName <- "o4mini"
classificationGPT <- read.csv(paste0("data/llm_classifications/countries_", modelName, ".csv"))

classificationResearcher <- read.csv("data/raw/hand_classification_countries.csv")

classificationResearcher$countries <- ifelse(classificationResearcher$countries %in% "Shouldn't be included?" | is.na(classificationResearcher$countries), "None", classificationResearcher$countries)

# classificationResearcher$countries <- ifelse(classificationResearcher$countries %in% "South Korea", "Korea, Republic of", 
#                                              ifelse(classificationResearcher$countries %in% "Taiwan", "Taiwan, Province of China", classificationResearcher$countries))

getmode <- function(v) {
  v <- na.omit(v)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

classificationGPT_toMerge <- classificationGPT %>%
  rename(ID = custom_id) %>% 
  group_by(ID) %>% 
  summarize(countryGPTMode = getmode(countriesChatGPTClean))

# idMatching <-  read.csv("output/tableIDmatching.csv")
# 
# classificationGPT_toMatch <- merge(classificationGPT, idMatching %>% 
#                                      rename(TitleMatch = Title), by.x = "X", by.y = "IDgpt")

mergedClassifs <- merge(classificationGPT_toMerge, classificationResearcher %>% 
                          select(ID, countries) %>% 
                          rename(countryHand = countries)
                        , by = "ID")

split_norm <- function(x) {
  x %>% 
    str_split("\\s*[;,]\\s*") %>% 
    unlist() %>% 
    str_trim() %>% 
    purrr::discard(~ .x == "")
}

QAcountry <- mergedClassifs %>% 
  mutate(
    set1 = map(countryGPTMode, split_norm),
    set2 = map(countryHand, split_norm),
    inter = map2_int(set1, set2, ~ length(intersect(.x, .y))),
    uni   = map2_int(set1, set2, ~ length(union(.x, .y))),
    similarity = if_else(uni > 0, inter / uni, NA_real_)
    )

table(QAcountry$similarity) # almost perfect match

classificationGPT$countriesChatGPTClean <- ifelse(classificationGPT$countriesChatGPTClean %in% c("None", ""), "Unknown", classificationGPT$countriesChatGPTClean)

agreement_id_country <- classificationGPT %>%
  filter(countriesChatGPTClean != "Unknown") %>% 
  rename(ID = custom_id) %>%
  filter(!is.na(countriesChatGPTClean)) %>%
  mutate(rep_id = row_number()) %>%  # unique replicate row id
  separate_rows(countriesChatGPTClean, sep = "\\s*[,;|]\\s*") %>%  # adjust if needed
  mutate(country = str_trim(countriesChatGPTClean)) %>%
  filter(country != "") %>%
  distinct(ID, rep_id, country) %>%   # avoid double counting same country in a replicate
  count(ID, country, name = "n_included") %>%
  left_join(
    classificationGPT %>%
      rename(ID = custom_id) %>%
      group_by(ID) %>%
      summarise(n_reps = n_distinct(row_number()), .groups = "drop"),
    by = "ID"
  ) %>%
  mutate(agreement_rate = n_included / 3) %>%  # if it is truly always 3
  arrange(ID, desc(agreement_rate), country)

agreement_id_country

table(agreement_id_country$agreement_rate)

paper_agreement <- classificationGPT %>%
  filter(countriesChatGPTClean != "Unknown") %>% 
  rename(ID = custom_id) %>%
  filter(!is.na(countriesChatGPTClean)) %>%
  group_by(ID) %>%
  summarise(
    n_reps = n(),
    modal_agreement = max(table(countriesChatGPTClean)) / n_reps,
    full_agreement = n_distinct(countriesChatGPTClean) == 1,
    .groups = "drop"
  )

paper_agreement

table(paper_agreement$full_agreement)
table(paper_agreement$modal_agreement)
