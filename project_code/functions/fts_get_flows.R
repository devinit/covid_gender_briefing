fts_get_flows <- function(year = NULL, planid = NULL, emergencyid = NULL, globalclusterid = NULL, destinationlocationid = NULL){
  lapply(c("data.table", "jsonlite"), require, character.only=T)
  if(!is.null(year)){
    year <- paste0("year=", paste0(year, collapse=","))
  }
  if(!is.null(planid)){
    planid <- paste0("planid=", paste0(planid, collapse=","))
  }
  if(!is.null(emergencyid)){
    emergencyid <- paste0("emergencyid=", paste0(emergencyid, collapse=","))
  }
  if(!is.null(globalclusterid)){
    globalclusterid <- paste0("globalclusterid=", paste0(globalclusterid, collapse=","))
  }
  if(!is.null(destinationlocationid)){
    destinationlocationid <- paste0("destinationlocationid:", paste0(destinationlocation, collapse=","))
  }
  
  call.filter <- NULL
  if(!is.null(destinationlocationid)){
    call.filter <- paste0("&filterby=", destinationlocationid)
  }
  
  hpc <- "https://api.hpc.tools/v1/public/fts/flow?"
  call.param <- paste(year, planid, emergencyid, globalclusterid, call.filter, "format=json&limit=1000", sep="&")
  call <- paste0(hpc, call.param)
  fts <- fromJSON(call, flatten=T)
  
  flowslist <- list()
  flowslist[[1]] <- fts$data$flows
  i <- 2
  while (!is.null(fts$meta$nextLink)){
    nextLink <- fts$meta$nextLink
    fts <- fromJSON(nextLink, flatten=T)
    flowslist[[i]] <- fts$data$flows
    i <- i + 1
  }
  
  flows <- rbindlist(flowslist, fill=T, use.names = T)
  return(flows)
}