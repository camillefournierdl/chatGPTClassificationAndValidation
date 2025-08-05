library(tidyverse)

classificationGPT <- read.csv("dataNew/allgpt-lit_w_countries.csv")

classificationResearcher <- read.csv("dataNew/manualCodingCountry.csv")

classificationResearcher$countries <- ifelse(classificationResearcher$countries %in% "Shouldn't be included?" | is.na(classificationResearcher$countries), "None", classificationResearcher$countries)

classificationResearcher$countries <- ifelse(classificationResearcher$countries %in% "South Korea", "Korea, Republic of", 
                                             ifelse(classificationResearcher$countries %in% "Taiwan", "Taiwan, Province of China", classificationResearcher$countries))

idMatching <-  read.csv("output/tableIDmatching.csv")

classificationGPT_toMatch <- merge(classificationGPT, idMatching %>% 
                                     rename(TitleMatch = Title), by.x = "X", by.y = "IDgpt")

mergedClassifs <- merge(classificationGPT_toMatch, classificationResearcher %>% 
                          select(ID, countries) %>% 
                          rename(countryHand = countries)
                        , by.x = "IDhand", by.y = "ID")

split_norm <- function(x) {
  x %>% 
    str_split("\\s*[;,]\\s*") %>% 
    unlist() %>% 
    str_trim() %>% 
    discard(~ .x == "")
}

QAcountry <- mergedClassifs %>% 
  mutate(
    set1 = map(country, split_norm),
    set2 = map(countryHand, split_norm),
    inter = map2_int(set1, set2, ~ length(intersect(.x, .y))),
    uni   = map2_int(set1, set2, ~ length(union(.x, .y))),
    similarity = if_else(uni > 0, inter / uni, NA_real_)
    )

table(QAcountry$similarity)
