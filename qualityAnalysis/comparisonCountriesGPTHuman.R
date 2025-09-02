library(tidyverse)

classificationGPT <- read.csv("LLMclassif/countriesChatGPT_5mini_final.csv")
classificationGPT <- read.csv("LLMclassif/countriesChatGPT_o4mini_final.csv")

classificationResearcher <- read.csv("dataNew/manualCodingCountry.csv")

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
    discard(~ .x == "")
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

