suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
#setwd("C:/Users/jasminj/Documents/R/covid_gender_briefing-main")

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

fts <- fread("project_data/fts_flows.csv", encoding = "UTF-8")
codenames <- fread("project_data/fts_codenames.csv", encoding = "UTF-8")
deflators <- fread("project_data/fts_deflators.csv", encoding = "UTF-8")
source("https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_split_rows.R")

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
  "sourceObjects_UsageYear.name"
  ,
  "destinationObjects_GlobalCluster.name"
  ,
  "destinationObjects_Cluster.name"
  ,
  "destinationObjects_Organization.organizationTypes"
  ,
  "destinationObjects_Organization.organizationSubTypes"
)

fts <- fts[, ..keep]

fts <- fts[as.character(year) >= 2014]

major.keywords <- c(
  "reproductive.",
  "contraceptive.",
  "birth control.",
  "gender.",
  "female empowerment.",
  "empowerment* women.",
  "domestic violence.",
  "gender-based.* violence| violence.*gender.",
  "girl.*women. |women.* girl.",
  "GBV.",
  "women.* informal| informal.* women",
  "women",
  "woman",
  "girl",
  "female",
  "maternal",
  "SRH.",
  "ASRH.",
  "mother",
  "child marriage"
)

minor.keywords <- c(
  #"keyword3"
  #,
  #"keyword4"
)

disqualifying.keywords <- c(
  "\\bmen\\b",
  "\\bman\\b",
  "\\bboys\\b",
  "\\bmale\\b."
)


fts$relevance <- "None"
fts[grepl(paste(minor.keywords, collapse = "|"), tolower(paste(fts$description)))]$relevance <- "Minor"
fts[grepl(paste(major.keywords, collapse = "|"), tolower(paste(fts$description)))]$relevance <- "Major"

fts$check <- "No"
fts[relevance == "Minor"]$check <- "potential false positive"
fts[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(fts[relevance != "None"]$ProjectTitle, fts[relevance != "None"]$description, fts[relevance != "None"]$description)))]$check <- "potential false negative"

fts[, keywordcount := unlist(lapply(description, function(x) sum(gregexpr(paste0(major.keywords, collapse = "|"), x)[[1]] > 0, na.rm = T)))]
fts[, disqkeywordcount := unlist(lapply(description, function(x) sum(gregexpr(paste0(disqualifying.keywords, collapse = "|"), x)[[1]] > 0, na.rm = T)))]

fts_output <- fts
rm(fts)

#Global sector assigment
sector.decode <- (fts_output[!(sector %in% fts_output$destinationObjects_GlobalCluster.name), .(sector = unique(sector), new_sector = NA_character_)])
sector.decode[grepl("COVID-19", sector), new_sector := "COVID-19"]
sector.decode[grepl("Health", sector), new_sector := "Health"]
sector.decode[grepl("Emergency Livelihoods", sector), new_sector := "Early Recovery"]
sector.decode[grepl("Humanitarian Transportation", sector), new_sector := "Coordination and support"]
sector.decode[grepl("Multisector|All non-COVID|Multi-sector|Refugee|Réfugiés|Multipurpose|Multi Purpose|Multi-Purpose|RMMS", sector, ignore.case = T), new_sector := "Multi-sector"]
sector.decode[grepl("WASH", sector), new_sector := "Water Sanitation Hygiene"]
sector.decode[grepl("Shelter|NFI", sector), new_sector := "Emergency Shelter and NFI"]
sector.decode[grepl("Food Security|Sécurité Alimentaire|^Food$", sector, ignore.case = T), new_sector := "Food Security"]
sector.decode[grepl("Cluster not yet specified", sector, ignore.case = T), new_sector := "Unspecified"]
sector.decode[is.na(new_sector), new_sector := "Other"]

fts_output[sector %in% sector.decode$sector, sector := merge(fts_output[sector %in% sector.decode$sector, .(sector)], sector.decode, by = "sector")$new_sector]
fts_output[sector == "", sector := "Unspecified"]
fts_output[grepl("Protection", sector), sector := "Protection"]

#Split rows into individual recipient type where multiple are recorded
fts_output$recipient_type <- apply(matrix(paste(as.matrix(fts_output[, tstrsplit(destinationObjects_Organization.organizationTypes, "; ")]), as.matrix(fts_output[, tstrsplit(destinationObjects_Organization.organizationSubTypes, "; ")]), sep = ": "), nrow = nrow(fts_output)), 1, function(x) gsub("NA|: NA|; NA: NA|: NULL", "", paste(x, collapse = "; ")))
fts_output <- fts_split_rows(fts_output, value.cols = "amountUSD", split.col = "recipient_type", split.pattern = "; ", remove.unsplit = T)

#Merge deflators
fts_output <- merge(fts_output[, Donor := gsub(", Government of", "", sourceObjects_Organization.name)], codenames, by = "Donor", all.x = T)
fts_output[is.na(Donor), `Donor Country ID` := "Total DAC"]

deflators[, Deflators := Deflators[!is.na(Deflators)], by = deflatortype][is.na(Deflators), Deflators := 1]

fts_output <- merge(fts_output[, deflatortype := paste(`Donor Country ID`, sourceObjects_UsageYear.name)], deflators[, -"year"], by = "deflatortype", all.x = T)
fts_output[, amountUSD_defl := amountUSD/Deflators]
fts_output[, `:=` (deflatortype = NULL, Donor = NULL)]

#