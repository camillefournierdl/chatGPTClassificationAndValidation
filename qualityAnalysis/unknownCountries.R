library(tidyverse)
"%ni%" = Negate("%in%")

datasetClassification <- read.csv("output/dataClassifToPlot.csv")

IDsubset <- unique(datasetClassification$ID)

datasetCountries <- read.csv("dataNew/allgpt-lit_w_countries.csv")

datasetCountries <- subset(datasetCountries, X %in% IDsubset)

datasetCountries$country <- ifelse(datasetCountries$country %in% c("None", ""), "Unknown", datasetCountries$country)

table(datasetCountries$country)

unknownCountry <- subset(datasetCountries, country == "Unknown")

write.csv(unknownCountry, "output/unknownCountry.csv", row.names = F)
