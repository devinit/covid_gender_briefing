suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

source("https://raw.githubusercontent.com/devinit/gha_automation/main/IHA/fts_curated_flows.R")
fts <- fts_curated_flows(years = 2018:2021, update = NA, dataset_path = "project_data")

#Assign a sector from available fields
fts[, sector := destinationObjects_GlobalCluster.name]
fts[is.na(sector) & !is.na(sourceObjects_GlobalCluster.name), sector := sourceObjects_GlobalCluster.name]
fts[is.na(sector) & !is.na(destinationObjects_Cluster.name), sector := destinationObjects_Cluster.name]

#Split rows into individual sectors where multiple are recorded
fts <- fts_split_rows(fts, value.cols = "amountUSD_defl", split.col = "sector", split.pattern = "; ", remove.unsplit = T)

fts <- fts[year %in% c(2018, 2019, 2020, 2021)]
fts[is.null(fts) | fts == "NULL"] <- NA
fts[, `:=` (reportDetails = NULL, childFlowIds = NULL)]

fwrite(fts, "project_data/fts_flows.csv")
