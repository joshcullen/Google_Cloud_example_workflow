
# Function to shift longitude from [-180, 180] range, to [0,360] range

shift_longitude <- function(data, rev = FALSE) {
  ## data = data.frame containing column named lon (WGS84 proj)
  
  if (!rev) {
    # Check which coords need to be flipped
    data <- data |> 
      mutate(lon = ifelse(lon < 0, 360 + lon, lon))
    
  } else if (rev) {
    # Reverse lon to be negative values
    data <- data |> 
      mutate(lon = ifelse(lon > 180, lon - 360, lon))
  }
  
  
  return(data)
}

#----------------------------

# Simple function to extract covariate values from tracks
extract_covars <- function(tracks, covars, t_name, covar_names, buffer = NULL,
                           buffer_fun = NULL, buffer_crs = NULL) {
  ## tracks = data frame containing at least the coordinates (labeled 'lon' and 'lat'), and
  ##      some value of time depending on temporal resolution of rasters
  ## covars = a SpatRaster object (from {terra}) containing all environ covars across all time points
  ## t_name = character; The column name for the time index being used (whether daily or monthly)
  ## covar_names = vector; Names to be labeled for columns of each extracted covar
  ## buffer = numeric or vector; If single number, only 1 buffer distance in CRS provided by 'buffer_crs'; If vector of length 'tracks', each point has its own buffer radius applied
  ## buffer_fun = character; One of the function options provided by {exactextractr} for exact_extract()
  # buffer_crs = an EPSG number or a PROJ string for which to project the raster layer and buffer polygons since buffer should be provided in units other than arc degrees
  
  
  # Check whether covars is set to WGS84
  if (!is.lonlat(covars)) {
    stop("`covars` must be in WGS84 CRS")
  }
  
  # Check whether coords need to be flipped from (-180, 180) to (0,360)
  if (ext(covars)[1] > 0 && ext(covars)[2] > 0) {
    tracks <- tracks |> 
      mutate(lon = ifelse(lon < 0, 360 + lon, lon))
  }
  
  
  
  
  
  # Define covariates w/in SpatRaster object
  vars <- unique(varnames(covars))  #in case some rasters are concatenated w/ duplicate names
  
  # Check that length of `vars` and `covar_names` matches
  if (length(vars) != length(covar_names)) {
    stop("`covar_names` must match the length of `varnames(covars)`. It's possible that two sets of varnames are matching.")
  }
  
  # Define dates to extract
  ext.dates <- as.character(tracks[[t_name]])  #needs to be char string to match 'tmp' names
  
  
  
  ### Simple raster extraction ###
  
  if (is.null(buffer)) {
    
    # Extract values across covars
    ext_vals <- map(vars, ~{
      
      # Extract across all layers
      tmp <- extract(covars[.x], tracks[,c('lon','lat')], ID = FALSE)
      names(tmp) <- as.character(unique(time(covars)))
      
      #indicator var for which col to use
      ind <- sapply(ext.dates, \(x) which(names(tmp) == x)) |> 
        as.integer()
      
      # select time-matched values
      out <- tmp[cbind(seq_along(ind), ind)]
      
      return(out)
    }) |> 
      set_names(covar_names) |> 
      bind_cols()
    
  }
  
  
  
  ### Buffered raster extraction ###
  
  if (!is.null(buffer)) {
    
    # Extract values across covars
    ext_vals <- map(vars, ~{
      
      # Create buffered polygons around points
      buff_poly <- st_as_sf(tracks[,c('lon','lat')], coords = c('lon','lat'),
                            crs = 4326) |> 
        st_transform(buffer_crs) |> 
        st_buffer(buffer)
      
      # Change SpatRaster to projected CRS and rename layers
      covars2 <- covars[.x] |> 
        project(paste0('EPSG:', buffer_crs), threads = TRUE, by_util = FALSE)
      names(covars2) <- as.character(unique(time(covars2)))
      
      # Extract across all layers
      tmp <- exact_extract(covars2, buff_poly, fun = buffer_fun)
      names(tmp) <- as.character(unique(time(covars)))
      
      #indicator var for which col to use
      ind <- sapply(ext.dates, \(x) which(names(tmp) == x)) |> 
        as.integer()
      
      # select time-matched values
      out <- tmp[cbind(seq_along(ind), ind)]
      
      return(out)
    }) |> 
      set_names(covar_names) |> 
      bind_cols()
    
  }
  
  
  
  # Add extracted covars to tracks
  tracks_covars <- cbind(tracks, ext_vals)
  
  return(tracks_covars)
}

#----------------------------

# Function to add transformed coordinates

add_trans_coords <- function(data, coords, proj, new_proj) {
  ## data = a data.frame with coordinates to be re-projected
  ## coords = char vector; The names of the coordinates to be tranformed
  ## proj = either an EPSG code or proj string defining the projection of `coords`
  ## new_proj = an EPSG code or proj string defining the projection of the new coords to be added
  
  tmp <- sf::st_as_sf(data, coords = coords, crs = proj, remove = FALSE)
  
  if (sum(coords %in% c('x','X','y','Y')) > 0) {
    tmp |> 
      sf::st_transform(4326) |> 
      mutate(lon = unlist(map(geometry, 1)),
             lat = unlist(map(geometry, 2))) |>  #add cols from new CRS
      sf::st_drop_geometry()
  } else if (sum(coords %in% c('lon','long','longitude','Lon','Long','Longitude',
                               'lat','latitude','Lat','Latitude')) > 0) {
    tmp |> 
      sf::st_transform(new_proj) |> 
      mutate(x = unlist(map(geometry, 1)),
             y = unlist(map(geometry, 2))) |>  #add cols from new CRS
      sf::st_drop_geometry()
  } else {
    stop("Coordinate names must either be x/y or lon/lat.")
  }
  
}

#-----------------------------------

# Function to mask out land (or some polygon) from in-water UDs (or some other polygon)
# Per StackOverflow answer at https://stackoverflow.com/questions/71289669/intersection-keeping-non-intersecting-polygons-in-sf-r

st_mask <- function(layer, mask) {
  
  # First check if an intersection occurs
  tmp <- unlist(st_intersects(layer, mask))
  
  if (length(tmp > 0) & unique(st_geometry_type(layer)) %in% c("POLYGON","MULTIPOLYGON")) {
    intersection <- st_intersection(layer, mask)
    
    if (!is.null(names(intersection))) {  #when there's a df w/ column named 'geometry'
      diff1 <- st_difference(layer, st_union(intersection$geometry))
    } else {  #when object is simply an sf layer, not as df
      diff1 <- st_difference(layer, st_union(intersection))
    }
    
    
  } else if (length(tmp > 0) & unique(st_geometry_type(layer)) == "POINT") {
    diff1 <- layer[!st_intersects(layer, mask) |> lengths() > 0,]
    
  } else {  #for tracks/polygons where no intersection w/ land
    
    diff1 <- layer
    
  }
  
  return(diff1)
}

#-----------------------------------

# Simple function to extract covariate values from tracks for massive raster datasets
extract_covars_big <- function(points, covars, t_name, covar_names, buffer = NULL,
                               buffer_fun = NULL, buffer_crs = NULL, cores = 1) {
  ## points = data frame containing at least the coordinates (labeled 'lon' and 'lat'), and
  ##      some value of time depending on temporal resolution of rasters
  ## covars = a vector containing full file paths for all environ covars
  ## t_name = character; The column name for the time index being used (whether daily or monthly)
  ## covar_names = vector; Names to be labeled for columns of each extracted covar
  ## buffer = numeric or vector; If single number, only 1 buffer distance in CRS provided by 'buffer_crs'; If vector of length 'tracks', each point has its own buffer radius applied
  ## buffer_fun = character; One of the function options provided by {exactextractr} for exact_extract()
  # buffer_crs = an EPSG number or a PROJ string for which to project the raster layer and buffer polygons since buffer should be provided in units other than arc degrees
  ## cores = number of computer cores on which to distribute the processing load in parallel
  
  
  
  # Check that no dates/times are missing
  if (any(is.na(points[[t_name]]))) {
    stop("Missing values for `t_name` found. Remove all observations with missing dates/times before proceeding.")
  }
  
  # Check that no coordinates are missing
  if (any(is.na(points[["lon"]] | points[["lat"]]))) {
    stop("Missing values for longitude and/or latitude found. Remove all observations with missing coordinates before proceeding.")
  }
  
  
  
  # Define date bounds
  t.bounds <- range(points[[t_name]])
  
  # Define dates to extract
  ext.dates <- as.character(points[[t_name]])  #needs to be char string to match 'tmp' names
  
  
  
  
  
  
  ### Simple raster extraction ###
  
  if (is.null(buffer)) {
    
    plan("multisession", workers = cores)
    handlers(handler_progress(format = ":spin :current/:total [:bar] :percent in :elapsed",
                              width = 100,
                              clear = FALSE))
    
    # tic()
    progressr::with_progress({
      p <- progressr::progressor(along = covars)  #create progress bar
      
      # Extract values across covars
      ext_vals <- future_map(seq_along(covars), ~{
        
        # Load covar raster
        r <- rast(covars[[.x]])
        
        # Subset raster to min and max of points
        r <- r[[time(r) >= as_date(t.bounds[1]) & time(r) <= as_date(t.bounds[2])]]
        
        # Check whether covars is set to WGS84
        if (!is.lonlat(r)) {
          stop("`covars` must be in WGS84 CRS")
        }
        
        # Check whether coords need to be flipped from (-180, 180) to (0,360)
        if (ext(r)[1] > 0 && ext(r)[2] > 0) {
          points <- points |> 
            mutate(lon = ifelse(lon < 0, 360 + lon, lon))
        }
        
        # Split rasters into year chunks
        yr <- glue("{year(time(r))}")
        r_yr <- split(r, yr)
        
        ext_vals_var <- map(r_yr,
                            ~{
                              # Extract across all layers
                              tmp <- extract(.x, data.frame(points[,c('lon','lat')]), ID = FALSE)
                              names(tmp) <- as.character(time(.x))
                              
                              #indicator var for which col to use
                              ind <- sapply(ext.dates, \(z) which(names(tmp) == z)) |> 
                                as.integer()
                              
                              # select time-matched values
                              out <- tmp[cbind(seq_along(ind), ind)]
                              
                              return(out)
                            }) |> 
          set_names(unique(yr)) |> 
          bind_cols() |> 
          apply(1, first, na_rm = TRUE)
        
        p()  #update progress bar
        
        return(ext_vals_var)
        
      },
      .options = furrr_options(seed = NULL, packages = c('terra','lubridate','dplyr','glue'),
                               globals = c('t.bounds','ext.dates','tracks'))) |> 
        set_names(covar_names) |> 
        bind_cols()
      
    })
    # toc()
    plan("sequential")
  }
  
  
  
  # Add extracted covars to tracks
  points_covars <- cbind(points, ext_vals)
  
  return(points_covars)
}
