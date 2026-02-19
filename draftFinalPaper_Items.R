
## Extract the final list of papers, and survey items as csv
papers <- read.csv2("output/paperClassifYear.csv", 
                    header = T, sep = ",", dec = ".")

surveys <- read.csv2("output/surveyClassifYear.csv", 
                     header = T, sep = ",", dec = ".")

## extract unique ids
ids_paper <- unique(papers$ID)
ids_surveys <- unique(surveys$id)

length(ids_paper) # 555 because 128 do not have a match with the country (not available in abstract)
length(ids_surveys) # 112

## papers (full text)
full_papers <- read.csv2("dataNew/combined_lit.csv", sep = ",")
subset_papers <- full_papers[which(full_papers$X %in% ids_paper),]  ## somehow I get 555 (which is not what we have in the paper I think?)

### nb of survey programs

length(unique(surveys$source))

### nb of countries covered by at least one paper

dataset <- read.csv("output/datasetPlots.csv")

# count how many countries see a paper
dataset %>% 
  filter(n_papers > 0) %>% 
  nrow()
  
### nb of countries covered by at least one survey item

length(unique(surveys$countries))

### identify nb of papers for different countries

dataset <- read.csv("output/datasetPlots.csv")

dataset$isDiff <- ifelse(dataset$isIndia == 1, "India",
                         ifelse(dataset$isChina == 1, "China", "Rest"))

datasetEur <- subset(dataset, continent == "Europe")

# total nb of papers 
datasetEur %>% 
  summarise(total_papers = sum(n_papers))
