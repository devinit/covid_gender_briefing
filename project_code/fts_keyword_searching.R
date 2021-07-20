suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
setwd("C:/Users/jasminj/Documents/R/covid_gender_briefing-main")

#setwd("..")

fts <- fread("project_code/fts_flows.csv")

keep <- c(
  "id"
  ,
  "description"
  ,
  "year"
  ,
  "flowType"
  ,
  "keywords"
  ,
  "sector"
  ,
  "amountUSD"
)

fts <- fts[, ..keep]

fts <- fts[as.character(year) >= 2014]

major.keywords <- c(
  "girl.*education",
  "education.*girl",
  "gender.*education|education.*gender",
  "equitable.*education|education.*equitable",
  "equality.*education|education.*equality"
)

minor.keywords <- c(
  #"keyword3"
  #,
  #"keyword4"
)

disqualifying.keywords <- c(
  #"keyword5"
  #,
  #"keyword6"
)

disqualifying.sectors <- c(
 "promotion of development awareness",
 "material relief assistance and services"
)

fts$relevance <- "None"
fts[grepl(paste(minor.keywords, collapse = "|"), tolower(paste(fts$description)))]$relevance <- "Minor"
fts[grepl(paste(major.keywords, collapse = "|"), tolower(paste(fts$description)))]$relevance <- "Major"

fts$check <- "No"
fts[relevance == "Minor"]$check <- "potential false positive"
fts[relevance != "None"][PurposeName %in% disqualifying.sectors]$check <- "potential false negative"
fts[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(fts[relevance != "None"]$ProjectTitle, fts[relevance != "None"]$ShortDescription, fts[relevance != "None"]$LongDescription)))]$check <- "potential false negative"

fts[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(fts[relevance != "None"]$ProjectTitle, fts[relevance != "None"]$ShortDescription, fts[relevance != "None"]$LongDescription)))]$relevance <- "Nonefts[relevance != "None"][PurposeName %in% disqualifying.sectors]$relevance <- "None"

fts_output <- fts
rm(fts)

fts.years <- dcast.data.table(fts_output, year ~ relevance, value.var = "USD_Disbursement_Defl",fun.aggregate = function(x) sum(x, na.rm=T))