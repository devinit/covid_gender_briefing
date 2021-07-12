suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
lapply(c("functions/load_and_join.R"), source)

setwd("..")

crs <- load_crs()

keep <- c(
  "CrsID"
  ,
  "ProjectNumber"
  ,
  "Year"
  ,
  "Aid_t"
  ,
  "FlowName"
  ,
  "DonorName"
  ,
  "RecipientName"
  ,
  "USD_Commitment_Defl"
  ,
  "USD_Disbursement_Defl"
  ,
  "PurposeName"
  ,
  "ProjectTitle"
  ,
  "ShortDescription"
  ,
  "LongDescription"
  ,
  "Gender"
  ,
  "ChannelReportedName"
)

crs <- crs[, ..keep]
crs <- crs[
  FlowName == "ODA Loans" 
  |
    FlowName == "ODA Grants"
  | 
    FlowName == "Equity Investment"
  | 
    FlowName == "Private Development Finance"
  ]

crs <- crs[as.character(Year) >= 2014]

major.keywords <- c(
  "girl.*education",
  "education.*girl",
  "inclusive.*education"
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
  #"sector1"
  #,
  #"sector2"
)

crs$relevance <- "None"
crs[grepl(paste(minor.keywords, collapse = "|"), tolower(paste(crs$ProjectTitle, crs$ShortDescription, crs$LongDescription)))]$relevance <- "Minor"
crs[grepl(paste(major.keywords, collapse = "|"), tolower(crs$LongDescription))]$relevance <- "Minor"
crs[grepl(paste(major.keywords, collapse = "|"), tolower(paste(crs$ShortDescription, crs$ProjectTitle)))]$relevance <- "Major"

crs$check <- "No"
crs[relevance == "Minor"]$check <- "potential false positive"
crs[relevance != "None"][PurposeName %in% disqualifying.sectors]$check <- "potential false negative"
crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$check <- "potential false negative"

crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$relevance <- "None"
crs[relevance != "None"][PurposeName %in% disqualifying.sectors]$relevance <- "None"

crs$Gender <- as.character(crs$Gender)
crs[is.na(Gender)]$Gender <- "0"
crs[Gender != "1" & Gender != "2"]$Gender <- "No Gender component"
crs[Gender == "1"]$Gender <- "Partial Gender component"
crs[Gender == "2"]$Gender <- "Major Gender component"

crs_output <- crs
rm(crs)

source("project_code/split_and_save.R")
split_and_save(crs_output, "output", 0)

crs.years <- dcast.data.table(crs_output, Year ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
crs.donors <- dcast.data.table(crs_output, Year + DonorName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
crs.recipients <- dcast.data.table(crs_output, Year + RecipientName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
crs.sectors <- dcast.data.table(crs_output, Year + PurposeName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
crs.flows <- dcast.data.table(crs_output, Year + FlowName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))

fwrite(crs.years, "output/crs years.csv")
fwrite(crs.sectors, "output/crs sectors.csv")
fwrite(crs.flows, "output/crs flows.csv")
fwrite(crs.donors, "output/crs donors.csv")
fwrite(crs.recipients, "output/crs recipients.csv")

tocheck.positive <- crs_output[check == "potential false positive"]
tocheck.negative <- crs_output[check == "potential false negative"]
fwrite(tocheck.positive, "output/crs check positives.csv")
fwrite(tocheck.negative, "output/crs check negatives.csv")