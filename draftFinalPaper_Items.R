
## Extract the final list of papers, and survey items as csv
papers <- read.csv2("output/paperClassifYear.csv", 
                    header = T, sep = ",", dec = ".")

surveys <- read.csv2("output/surveyClassifYear.csv", 
                     header = T, sep = ",", dec = ".")

## extract unique ids
ids_paper <- unique(papers$ID)
ids_surveys <- unique(surveys$id)


## papers (full text)
full_papers <- read.csv2("dataNew/combined_lit.csv", sep = ",")
subset_papers <- full_papers[which(full_papers$X %in% ids_paper),]  ## somehow I get 555 (which is not what we have in the paper I think?)
