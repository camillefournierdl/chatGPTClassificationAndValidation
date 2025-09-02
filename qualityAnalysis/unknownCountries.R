library(tidyverse)
"%ni%" = Negate("%in%")

datasetClassification <- read.csv("output/dataClassifToPloto4mini.csv") # to know which countries are excluded

IDsubset <- unique(datasetClassification$ID)

# datasetCountries <- read.csv("LLMclassif/countriesChatGPT_5mini_final.csv")
datasetCountries <- read.csv("LLMclassif/countriesChatGPT_o4mini_final.csv")

getmode <- function(v) {
  v <- na.omit(v)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

datasetCountries_mode <- datasetCountries %>%
  rename(ID = custom_id) %>% 
  group_by(ID) %>% 
  summarize(countryGPTMode = getmode(countriesChatGPTClean))

datasetCountries_subset <- subset(datasetCountries_mode, ID %in% IDsubset)

datasetCountries_subset$countryGPTMode <- ifelse(datasetCountries_subset$countryGPTMode %in% c("None", ""), "Unknown", datasetCountries_subset$countryGPTMode)

table(datasetCountries_subset$countryGPTMode)

unknownCountry <- subset(datasetCountries_subset, countryGPTMode == "Unknown")

#### merge missing countries

# load manually classified countries
oldUnknownCountry <- read.csv("dataNew/unknownCountry_EHo4.csv", sep = ";") 

unique(oldUnknownCountry$X)
unique(unknownCountry$ID)

# merge to unknown countries (more recent)
oldUnknownCountry <- oldUnknownCountry %>%
  rename(ID = X)

## fix ID inconsistency by matching with title
# load title to partial match based on title
fullDataset <- read.csv("dataNew/datasetToClassifyGPT.csv")

unknownCountry_toMatch <- merge(unknownCountry, fullDataset %>%
                                  select(ID, Title), by = "ID", all.x = T, all.y = F)

clean_title <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%  # turn “é” → “e”
    tolower() %>%                          # lowercase
    gsub("[^[:alnum:] ]+", "", .) %>%      # drop punctuation
    trimws()                               # trim whitespace
}

library(stringi)
library(fuzzyjoin)

unknownCountry_toMatch <- unknownCountry_toMatch %>%
  mutate(title_clean = clean_title(Title))

oldUnknownCountry <- oldUnknownCountry %>%
  filter(!is.na(Title)) %>% 
  mutate(title_clean = clean_title(Title)) %>% 
  select(title_clean, country)

countryPartialManual <- stringdist_left_join(
  unknownCountry_toMatch, oldUnknownCountry,
  by = "title_clean",
  method = "lv",        # Levenshtein
  max_dist = 15,         # tweak this
  distance_col = "dist" # keep the distance
) %>%
  arrange(-dist)

# add these countries to the classif
datasetCountriesPartial <- merge(datasetCountries_subset, countryPartialManual %>% 
                                   select(ID, country), by = "ID", all.x = T, all.y = F)

datasetCountriesPartial$countryPartial <- ifelse(!is.na(datasetCountriesPartial$country),
                                                 datasetCountriesPartial$country,
                                                 datasetCountriesPartial$countryGPTMode)

IDunknown <- unique(datasetCountriesPartial[datasetCountriesPartial$countryPartial == "Unknown", "ID"])

# export remaining countries to manually code
unknownCountriesSave <- subset(fullDataset, ID %in% IDsubset & ID %in% IDunknown) # save data to manually classify papers that were not classified with chatgpt

write.csv(unknownCountriesSave, "output/unknownCountryo4mini_partial.csv", row.names = F)

# merge countries that would be classified by 5mini

datasetCountries5mini <- read.csv("LLMclassif/countriesChatGPT_5mini_final.csv")

getmode <- function(v) {
  v <- na.omit(v)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

datasetCountries_mode <- datasetCountries5mini %>%
  rename(ID = custom_id) %>% 
  group_by(ID) %>% 
  summarize(countryGPTMode = getmode(countriesChatGPTClean))

datasetCountries_subset <- subset(datasetCountries_mode, ID %in% IDunknown)

datasetCountries_subset$countryGPTMode <- ifelse(datasetCountries_subset$countryGPTMode %in% c("None", ""), "Unknown", datasetCountries_subset$countryGPTMode)

table(datasetCountries_subset$countryGPTMode)

datasetCountries_subset <- datasetCountries_subset %>% 
  rename(classif5mini = countryGPTMode)

unknownCountriesToCode <- merge(datasetCountriesPartial, datasetCountries_subset,
                              all.x = T, all.y = F, by = "ID")

unknownCountriesToCode$countryPartial <- ifelse(unknownCountriesToCode$countryPartial != "Unknown",
                                                 unknownCountriesToCode$countryPartial,
                                                 unknownCountriesToCode$classif5mini)

IDunknown <- unique(unknownCountriesToCode[unknownCountriesToCode$countryPartial == "Unknown", "ID"])

# export remaining countries to manually code
unknownCountriesSave <- subset(fullDataset, ID %in% IDsubset & ID %in% IDunknown) # save data to manually classify papers that were not classified with chatgpt

write.csv(unknownCountriesSave, "output/unknownCountryo4mini.csv", row.names = F)

######### after manually coding the countries:

# load manually coded remaining papers
manualCoded <- read.csv("dataNew/unknownCountryo4mini_CF.csv")

# merge and save for final classif

datasetCountriesPartial_f <- merge(datasetCountriesPartial, manualCoded %>%
                                     select(ID, manualCountry),
                                   all.x = T, all.y = F, by = "ID")

write.csv(datasetCountriesPartial_f, "output/fullClassifCountryo4mini.csv", row.names = F) # this should be used for final plotting





