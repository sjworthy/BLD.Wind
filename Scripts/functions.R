cfsr_dl_block <- function(variable = "wnd10m",
                        year = 1979, month = 1, day = 1, hmin = 0, hmax = 23,
                        xmin = 260, xmax = 270, ymin = 40, ymax = 50){

      if(! year %in% 1979:2010) stop("'year' must be between 1979 and 2010")
      if(! month %in% 1:12) stop("'month' must be between 1 and 12")
      if(min(xmin, xmax) < 0 | max(xmin, xmax) > 360) stop("longitude and latitude must be between 0 and 360")

      # open connection
      # example of original function url: "https://thredds.rda.ucar.edu/thredds/dodsC/files/g/ds093.1/1990/wnd10m.gdas.199001.grb2"
      # example with updated url: https://thredds.rda.ucar.edu/thredds/fileServer/files/g/d093001/1982/wnd10m.gdas.198211.grb2
  
      ym <- paste0(year, stringr::str_pad(month, 2, "left", "0"))
      
      #url <- paste0("https://www.ncei.noaa.gov/thredds/fileServer/model-cfs_reanl_ts/",
                    #ym, "/", variable, ".gdas.", ym, ".grb2")
  
      url <- paste0("https://thredds.rda.ucar.edu/thredds/fileServer/files/g/d093001/",
                    year, "/", variable, ".gdas.", ym, ".grb2")
      
      dir.create("cfsr_cache", showWarnings = FALSE)
      
      f <- file.path("cfsr_cache", basename(url))
      
      # increase timeout
      options(timeout = max(1800, getOption("timeout")))
      
      # download if needed (and not corrupted)
      if (!file.exists(f) || file.info(f)$size < 1e6) {
        message("... downloading ", basename(url), " ...")
        download.file(url, f, mode = "wb", method = "libcurl")
      } else {
        message("... using cached file ", basename(url), " ...")
      }
      
      # read with terra
      r <- terra::rast(f)
  
  message("... extracting wind components ...")
  # extract u and v components
  u <- r[[grep("u-component", names(r), ignore.case = TRUE)]]
  v <- r[[grep("v-component", names(r), ignore.case = TRUE)]]
  
  if(length(u) == 0 || length(v) == 0){
    stop("Could not find u/v wind components in GRIB file")
  }
  
  # spatial crop
  ext <- terra::ext(xmin, xmax, ymin, ymax)
  u <- terra::crop(u, ext)
  v <- terra::crop(v, ext)
  
  # time handling
  t_all <- terra::time(r)
  
  if(is.null(t_all)){
    stop("Time dimension not found in GRIB file")
  }
  
      date <- paste0(year, "-",
                 stringr::str_pad(month, 2, "left", 0), "-",
                 stringr::str_pad(day, 2, "left", 0))

      t_start <- as.POSIXct(
        paste0(date, " ", stringr::str_pad(hmin, 2, "left", "0"), ":00:00"),
        tz = "UTC")
      
      t_end <- as.POSIXct(
        paste0(date, " ", stringr::str_pad(hmax, 2, "left", "0"), ":00:00"),
        tz = "UTC")
      
      keep <- which(t_all >= t_start & t_all <= t_end)
      
      if(length(keep) == 0){
        stop("No time steps found in requested range")
      }
      
      u <- u[[keep]]
      v <- v[[keep]]
      
      # assign names
      names(u) <- make.unique(format(t_all[keep], "u %Y-%m-%d %H:%M:%S"))
      names(v) <- make.unique(format(t_all[keep], "v %Y-%m-%d %H:%M:%S"))
      
      message("... done ...")
      
      return(list(u = u, v = v))
}

cfsr_dl <- function(variable = "wnd10m",
                    years = 1979, months = 1, days = 1,
                    hlim = c(0, 23), xlim = c(260, 270), ylim = c(40, 50)){
  q <- expand.grid(variable = variable, day = days, month = months, year = years, hmin = min(hlim), hmax = max(hlim),
                   xmin = min(xlim), xmax = max(xlim), ymin = min(ylim), ymax = max(ylim))
  w <- purrr::pmap(q, cfsr_dl_block)
  c(terra::rast(purrr::map(w, "u")),
    terra::rast(purrr::map(w, "v")))
}
