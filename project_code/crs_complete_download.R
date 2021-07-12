required.packages <- c("data.table", "rvest", "rstudioapi")
lapply(required.packages, require, character.only = T)

setwd(dirname(getActiveDocumentContext()$path))
lapply(c("functions/split_and_save.R"), source)

setwd("..")

base.url <- "https://stats.oecd.org/DownloadFiles.aspx?DatasetCode=CRS1"

downloads <- html_attr(html_nodes(read_html(base.url), "a"), "onclick")
downloads <- gsub("return OpenFile|[(][)];", "", downloads)
downloads <- gsub("_", "-", downloads)
downloads <- paste0("http://stats.oecd.org/FileView2.aspx?IDFile=", downloads)

crs <- list()
for(i in 1:length(downloads)){
  download <- downloads[i]
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
