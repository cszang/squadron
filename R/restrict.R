##' Extract data from netcdf file based on land id
##'
##' (Relatively) fast way to extract data from netcdf file structured
##'   by land id. The use case is to extract a subset of the full
##'   grid, based on a set of land ids. These can be non-consecutive.
##' @param f the path to the netcdf
##' @param var_id variable to extract
##' @param var_land_id variable used for land ids in the netcdf file
##' @param ids a vector of land ids
##' @param par run in parallel mode (not implemented)
##' @return a data.table
##' @import ncdf4
##' @import data.table
##' @import utils
##' @export
restrict <- function(f, var_id, var_land_id = "land_id", ids, par = FALSE) {
  if (par) {
    ## library(parallel)
    ## TODO: implement ;-)
  }
  nc <- nc_open(f)
  lid <- ncvar_get(nc, var_land_id)
  ids <- ids[ids %in% lid]
  subsetchunk <- lid %in% ids
  time <- seq(as.Date("1950-01-01"), length.out = 54787, by = "day")
  m <- length(time)
  chunks <- list()
  pb <- txtProgressBar(min = 1, max = m)
  for (i in 1:m) {
    thetime <- time[i]
    chunk <- ncvar_get(nc, var_id, start = c(1, i),
                      count = c(-1, 1))[subsetchunk]
    chunks[[i]] <- data.table(
      landid = ids,
      time = thetime,
      x    = chunk
    )
    setTxtProgressBar(pb, i)
  }
  cat("Just a another moment, please...\n")
  out <- rbindlist(chunks)
  close(pb)
  out
}
