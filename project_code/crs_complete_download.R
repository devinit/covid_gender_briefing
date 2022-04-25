required.packages <- c("data.table", "rvest", "rstudioapi")
lapply(required.packages, require, character.only = T)

setwd(dirname(getActiveDocumentContext()$path))
lapply(c("functions/split_and_save.R"), source)

setwd("..")


base.url <- "https://stats.oecd.org/DownloadFiles.aspx?DatasetCode=CRS1"

downloads <- html_attr(html_nodes(read_html(base.url), "a"), "onclick")
years <- html_text(html_nodes(read_html(base.url), "a"))

downloads <- gsub("return OpenFile|[(][)];", "", downloads)
downloads <- gsub("_", "-", downloads)
downloads <- paste0("http://stats.oecd.org/FileView2.aspx?IDFile=", downloads)

years <- gsub("CRS | / SNPC.*", "", years)
start_years <- as.numeric(substr(years, 0, 4))
end_years <- as.numeric(paste0(substr(years, 0, 2), substr(years, nchar(years)-1, nchar(years))))

dt <- data.table(start_year = start_years, end_year = end_years, download = downloads)

##########
start_year <- 2019
end_year <- 2019
#########

chosen_years <- dt[end_year >= start & start_year <= end]

crs <- list()
for(i in 1:nrow(chosen_years)){
  download <- chosen_years$download[i]
  temp <- tempfile()
  download.file(download, temp, mode="wb", quiet=T)
  filename <- unzip(temp, list=T)$Name
  message(gsub(".txt", "", filename))
  unztemp <- unzip(temp, filename)
  crs[[i]] <- fread(unztemp, fill=T)
  unlink(temp)
  file.remove(unztemp)
}

crs <- rbindlist(crs)
gc()

split_and_save(crs, "project_data", 5)
