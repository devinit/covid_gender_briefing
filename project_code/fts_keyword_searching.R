suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
setwd("C:/Users/jasminj/Documents/R/covid_gender_briefing-main")

#setwd("..")

fts <- fread("project_code/fts_flows.csv")

names(fts)
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
  ,
  "sourceObjects_Organization.name"
  ,
  "destinationObjects_Location.name"
  ,
  "sourceObjects_Emergency.name"
  ,
  "destinationObjects_Emergency.name"
  ,
  "sourceObjects_GlobalCluster.name"  
  ,
  "destinationObjects_GlobalCluster.name"  
  ,
  
 
  
)

fts <- fts[, ..keep]

fts <- fts[as.character(year) >= 2014]

major.keywords <- c(
   "reproductive.",
   "contraceptive.",
  "birth control.",
  "gender.",
  "female empowerment.",
  "domestic violence.",
  "gender-based.* violence| violence.*gender.",
  "girl.*women. |women.* girl.",
  "GBV.",
  "women.* informal| informal.* women",
  "women",
  "woman",
  "girl.",
  "female",
  "maternal",
  "SRH"
  
  
)

minor.keywords <- c(
  #"keyword3"
  #,
  #"keyword4"
)

disqualifying.keywords <- c(
  "\\bmen\\b",
  "\\bman\\b",
  "boy.",
  "male."

)

disqualifying.sectors <- c(

)

fts$relevance <- "None"
fts[grepl(paste(minor.keywords, collapse = "|"), tolower(paste(fts$description)))]$relevance <- "Minor"
fts[grepl(paste(major.keywords, collapse = "|"), tolower(paste(fts$description)))]$relevance <- "Major"

fts$check <- "No"
fts[relevance == "Minor"]$check <- "potential false positive"
#fts[relevance != "None"][PurposeName %in% disqualifying.sectors]$check <- "potential false negative"
fts[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(fts[relevance != "None"]$ProjectTitle, fts[relevance != "None"]$description, fts[relevance != "None"]$description)))]$check <- "potential false negative"

#fts[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(fts[relevance != "None"]$ProjectTitle, fts[relevance != "None"]$ShortDescription, fts[relevance != "None"]$LongDescription)))]$relevance <- "Nonefts[relevance != "None"][PurposeName %in% disqualifying.sectors]$relevance <- "None"

fts_output <- fts
rm(fts)

#fts.years <- dcast.data.table(fts_output, year ~ relevance, value.var = "USD_Disbursement_Defl",fun.aggregate = function(x) sum(x, na.rm=T))