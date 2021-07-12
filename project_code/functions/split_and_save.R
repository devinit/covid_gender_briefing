split_and_save <- function(crs, path="project_data", compression_level = 0){
  require("data.table")
  dataname <- deparse(substitute(crs))
  if(compression_level > 0){
    existingfiles <- paste0(path,"/",list.files(path, pattern = paste0(dataname, "_part.+[.]bz")))
    if(length(existingfiles)>0)invisible(lapply(existingfiles, file.remove))
    size <- object.size(crs)
    parts <- ceiling(size/500000000)
    crs.splits <- split(crs, factor(sort(rank(row.names(crs))%%parts)))
    invisible(sapply(1:length(crs.splits), function(x, i) write.csv(x[[i]], bzfile(paste0(path, "/",dataname,"_part", i, ".bz"), compression = compression_level)), x=crs.splits))
  } else {
    existingfiles <- paste0(path,"/",list.files(path, pattern = paste0(dataname, "_part.+[.]csv")))
    if(length(existingfiles)>0)invisible(lapply(existingfiles, file.remove))
    size <- object.size(crs)
    parts <- ceiling(size/75000000)
    crs.splits <- split(crs, factor(sort(rank(row.names(crs))%%parts)))
    invisible(sapply(1:length(crs.splits), function(x, i) fwrite(x[[i]], paste0(path, "/",dataname,"_part", i, ".csv")), x=crs.splits))
  }
}
