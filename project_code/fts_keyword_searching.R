suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
#setwd("C:/Users/jasminj/Documents/R/covid_gender_briefing-main")

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

fts <- fread("project_data/fts_flows.csv", encoding = "UTF-8")
rec_codes <- fread("project_data/fts_recipientcodename.csv", encoding = "UTF-8")
channels <- fread("project_data/fts_deliverychannels.csv", encoding = "UTF-8")
ngo_types <- fread("project_data/fts_ngotype.csv", encoding = "UTF-8")[ngotype == "Southern International NGO", ngotype := "Southern international NGO"][ngotype == "Undefined NGO", ngotype := "Uncategorized NGO"]
source("https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_split_rows.R")

keep <- c(
  "id"
  ,
  "description"
  ,
  "year"
  ,
  "budgetYear"
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
  "sourceObjects_Organization.id"
  ,
  "sourceObjects_Organization.organizationTypes"
  ,
  "sourceObjects_Location.name"
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
  "destinationObjects_Organization.name"
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
  "child marriage",
  "salud reproductiva.",
  "obstetricia.",
  "Anticonceptivo.",
  "preservativos.",
  "domestic violence.",
  "sexo.",
  "mujer.",
  "empoderamiento femenino",
  "violencia sexual",
  "hija.",
  "chica.",
  "mujer.",
  "feminino",
  "feminine",
  "femininas",
  "salud sexual.",
  "contraceptif.",
  "Sexe",
  "Autonomisation des femmes",
  "Violence domestique",
  "violence familiale",
  "Violence sexiste",
  "fille.",
  "femme.",
  "femelle.",
  "Maternel.",
  "maternelle.",
  "mother",
  "madre",
  "child marriage",
  "mariage d'enfants",
  "mariage des enfants",
  "matrimonio infantil",
  "niña",
  "niñas"
)

disqualifying.keywords <- c(
  "\\bmen\\b",
  "\\bman\\b",
  "\\bboys\\b",
  "\\bmale\\b.",
  "\\bmasculino\\b",
  "\\bhombre\\b",
  "\\bhombres\\b",
  "\\bchico\\b.",
  "chicos",
  "masculin",
  "hommes",
  "homme",
  "niño",
  "garçon"
)

fts$relevance <- "None"

fts[sector == "Protection - Gender-Based Violence"]$relevance <- "Major: GBV"

fts[grepl(tolower(paste(major.keywords, collapse = "|")), tolower(paste(fts$description)))]$relevance <- "Major: Keyword"

fts$check <- "No"
fts[relevance != "None"][grepl(tolower(paste(disqualifying.keywords, collapse = "|")), tolower(paste(fts[relevance != "None"]$ProjectTitle, fts[relevance != "None"]$description, fts[relevance != "None"]$description)))]$check <- "potential false negative"

fts[, keywordcount := unlist(lapply(description, function(x) sum(gregexpr(tolower(paste0(major.keywords, collapse = "|")), x)[[1]] > 0, na.rm = T)))]
fts[, disqkeywordcount := unlist(lapply(description, function(x) sum(gregexpr(tolower(paste0(disqualifying.keywords, collapse = "|")), x)[[1]] > 0, na.rm = T)))]

fts_output <- fts

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
#fts_output[grepl("Protection", sector), sector := "Protection"]

#Merge recipient code lists
rec_orgs <- merge(ngo_types, channels, by = c("Recipient.Organization", "year"), all = T)
rec_orgs <- merge(rec_orgs, rec_codes, by = c("Recipient.Organization", "year"), all = T)
rec_orgs[is.na(rec_orgs)] <- ""

rec_orgs <- unique(rec_orgs[, -"year"])

fts_output <- merge(fts_output[, destinationObjects_Organization.name := gsub(", Government of", "", destinationObjects_Organization.name)], rec_orgs, by.x = c("destinationObjects_Organization.name"), by.y = c("Recipient.Organization"), all.x = T)

#NGO classification for those missing in codelist
fts_output[ngotype == "" & destinationObjects_Organization.organizationTypes == "NGO", ngotype := gsub("^ |NGO |", "", paste(destinationObjects_Organization.organizationSubTypes, destinationObjects_Organization.organizationTypes))]
fts_output[ngotype == "NGO", ngotype := "Uncategorized NGO"]
fts_output[grepl(";", ngotype), ngotype := "Other"]
fts_output[is.na(ngotype), ngotype := ""]

#Split rows into individual donors where multiple are recorded
fts_output <- fts_split_rows(fts_output, value.cols = "amountUSD", split.col = "sourceObjects_Organization.id", split.pattern = "; ", remove.unsplit = T)

fts_orgs <- data.table(fromJSON("https://api.hpc.tools/v1/public/organization")$data)
fts_orgs[, `:=` (type = ifelse(is.null(categories[[1]]$name), NA, categories[[1]]$name), location = ifelse(is.null(locations[[1]]$name), NA, locations[[1]]$name)), by = id]

fts_orgs_gov <- fts_orgs[type == "Government", .(sourceObjects_Organization.id = id, donor_country = location)]

#Manual development agency locations
fts_orgs_gov <- rbind(fts_orgs_gov,
                      data.table(sourceObjects_Organization.id = c("9946", "10399", "4058", "2987", "30", "6547"),
                                 donor_country = c("France", "Qatar", "United States", "Germany", "United Arab Emirates", "Taiwan, Province of China"))
)

#Merge orgs types for deflators
fts_output <- merge(fts_output, fts_orgs_gov, by = "sourceObjects_Organization.id", all.x = T)
fts_output[is.na(donor_country), donor_country := "Total DAC"]

#Add year where none recorded at source
fts_output[is.na(year), year := budgetYear]

deflators <- fread("project_data/usd_deflators_2021WEO.csv", encoding = "UTF-8", header = T)
deflators <- melt.data.table(deflators, id.vars = c("name", "ISO3"))
deflators <- deflators[, .(donor_country = name, year = as.numeric(as.character(variable)), deflator = value)]

fts_output <- merge(fts_output, deflators, by = c("donor_country", "year"), all.x = T)

fts_output[is.na(deflator)]$deflator <- merge(fts_output[is.na(deflator)][, -"deflator"], deflators[donor_country == "Total DAC", -"donor_country"], by = "year")$deflator

fts_output[, amountUSD_defl := amountUSD/deflator]

write.csv(fts_output, "fts_output.csv", fileEncoding = "UTF-8")
#