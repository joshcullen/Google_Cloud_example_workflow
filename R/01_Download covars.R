
### Download environmental covariates for CCE blue shark data ###

library(tidyverse)
library(sf)
library(terra)
library(glue)
library(tictoc)
# library(janitor)
library(future)
library(furrr)
library(progressr)

# source("R/utils.R")
path_copernicus_marine_toolbox <- 'copernicusmarine'


#################
### Load data ###
#################

dat <- read_csv("data/blsh_data.csv")
glimpse(dat)



# Define time range of data
time_range <- range(dat$date, dat$date, na.rm = TRUE)
x_range <- range(dat$lon, dat$lon, na.rm = TRUE)
y_range <- range(dat$lat, dat$lat, na.rm = TRUE)


###############################
### Download environ covars ###
###############################

# Create vector of covar names for GLORYS data download
vars <- c(sst = "thetao",
         east_vel = "uo",
         north_vel = "vo",
         ssh = "zos")


# Define output directory
outdir <- "data/rasters"


# Download rasters for GLORYS product
for (i in seq_along(vars)) {
  
  # Check to see if data need to be downloaded
  if (!file.exists(glue("{outdir}/{names(vars[i])}.nc"))) {
    
    print(glue("Downloading CMEMS data for {names(vars[i])}"))
    
    # Set depth value for covar download
    var_depth_range <- 0:1
    
    # Historical dynamic covariates
    productID <- "cmems_mod_glo_phy_my_0.083deg_P1D-m"
    out_name <- glue("{names(vars[i])}")
    
    command <- glue("{path_copernicus_marine_toolbox} subset -i {productID} \\
                -t {time_range[1]} -T {time_range[2]} \\
                -x {x_range[1]} -X {x_range[2]} -y {y_range[1]} -Y {y_range[2]} \\
                -z {var_depth_range[1]}. -Z {var_depth_range[2]}. \\
                {paste('--variable', vars[i], collapse = ' ')} \\
                -o {outdir} -f {out_name}")   
    system(command)
    
  }
  
}




# Download raster for Y wind
if (!file.exists(glue("{outdir}/ywind.nc"))) {
  
  productID <- "cmems_obs-wind_glo_phy_my_l3-metopa-ascat-des-0.125deg_P1D-i"
  variable <- "northward_wind"
  out_name <- "ywind"
  
  command <- glue("{path_copernicus_marine_toolbox} subset -i {productID} \\
                -t {time_range[1]} -T {time_range[2]} \\
                -x {x_range[1]} -X {x_range[2]} -y {y_range[1]} -Y {y_range[2]} \\
                  --variable {variable} -o {outdir} -f {out_name}")   
  system(command)
  
}




# Download raster for Chla-a
if (!file.exists(glue("{outdir}/CHL"))) {
  
  productID <- "cmems_obs-oc_glo_bgc-plankton_my_l4-gapfree-multi-4km_P1D"
  variable <- "CHL"
  out_name <- "CHL"
  
  command <- glue("{path_copernicus_marine_toolbox} subset -i {productID} \\
                -t {time_range[1]} -T {time_range[2]} \\
                -x {x_range[1]} -X {x_range[2]} -y {y_range[1]} -Y {y_range[2]} \\
                  --variable {variable} -o {outdir} -f {out_name}")   
  system(command)
  
}







###############################
### Generate derived covars ###
###############################

### Calculate raster for eddy kinetic energy (EKE) ###
command <- glue("cdo -L -expr,'eke=0.5*uo^2+0.5*vo^2' -merge {outdir}/north_vel.nc {outdir}/east_vel.nc {outdir}/eke.nc")
system(command)



### Calculate raster for rugosity ###
z <- rast(glue("{outdir}/gebco_2024.nc"))
z_sd <- focal(z, w = 5, fun = "sd", na.rm = TRUE)

writeRaster(z_sd, glue("{outdir}/z_sd.tiff"))




### Calculate raster for SD of SSH (ssh_sd) ###

# Create vector of layer names for reading 1 layer at a time
ssh_names <- rast(glue("{outdir}/ssh.nc")) |> 
  names()

# Split layer names into 10 chunks for processing
ssh_names_chunk <- split(ssh_names,
                         cut(seq_along(ssh_names), 10, labels = FALSE)
)

# Calculate SD of SSH in chunks
for (i in seq_along(ssh_names_chunk)) {
  
  plan("multisession", workers = availableCores() - 4)
  handlers(handler_progress(format = ":spin :current/:total [:bar] :percent in :elapsed",
                            width = 100,
                            clear = FALSE))
  
  # Process layers
  tic()
  progressr::with_progress({
    p <- progressr::progressor(along = ssh_names_chunk[[i]])  #create progress bar
    ssh_sd <- future_map(ssh_names_chunk[[i]],
                         ~{tmp <- focal(rast("data/rasters/ssh.nc", lyrs = .x),
                                        w = 5,
                                        fun = "sd",
                                        na.rm = TRUE)
                         p()
                         wrap(tmp)},  #need to wrap again for export
                         .options = furrr_options(seed = NULL))
  })
  toc()  #takes 1.5 min to run
  plan("sequential")  #return to single core
  
  tic()
  ssh_sd <- ssh_sd |> 
    map(unwrap, .progress = TRUE) |> 
    rast()
  toc()  #took 15 sec to run
  
  set.names(ssh_sd, gsub(x = ssh_names_chunk[[i]], pattern = "^zos", replacement = "ssh_sd"))
  
  # writeCDF(ssh_sd, glue("{outdir}/ssh_sd{i}.nc"), varname = "ssh_sd", overwrite = TRUE)
  writeRaster(ssh_sd,
              glue("{outdir}/ssh_sd{i}.tiff"),
              scale = 0.000305185094475746,  #value from ssh.nc
              datatype = "INT2U",  #Int16
              overwrite = TRUE)
  
  rm(ssh_sd)
}
# took 6 min

# Merge all ssh_sd files together
# files <- list.files(path = outdir, pattern = "ssh_sd.*\\.nc$", full.names = TRUE)[c(1,3:10,2)]
# command <- glue("cdo cat {paste(files, collapse = ' ')} {outdir}/ssh_sd.nc")
# system(command)

# Read in separate ssh_sd GeoTIFFs and combine into single file
ssh_sd <- glue("{outdir}/ssh_sd{1:10}.tiff") |> 
  map(rast) |> 
  rast()

writeRaster(ssh_sd,
            glue("{outdir}/ssh_sd.tiff"),
            scale = 0.000305185094475746,  #value from ssh.nc
            datatype = "INT2U",  #Int16
            overwrite = TRUE)

file.remove(glue("{outdir}/ssh_sd{1:10}.tiff"))  #delete chunk files






### Calculate raster for SD of SST (sst_sd) using 5 pixel window (~40 km) ###

# Create vector of layer names for reading 1 layer at a time
sst_names <- rast(glue("{outdir}/sst.nc")) |> 
  names()

# Split layer names into 10 chunks for processing
sst_names_chunk <- split(sst_names,
                         cut(seq_along(sst_names), 10, labels = FALSE)
)

# Calculate SD of sst in chunks
for (i in seq_along(sst_names_chunk)) {
  
  plan("multisession", workers = availableCores() - 4)
  handlers(handler_progress(format = ":spin :current/:total [:bar] :percent in :elapsed",
                            width = 100,
                            clear = FALSE))
  
  # Process layers
  tic()
  progressr::with_progress({
    p <- progressr::progressor(along = sst_names_chunk[[i]])  #create progress bar
    sst_sd <- future_map(sst_names_chunk[[i]],
                         ~{tmp <- focal(rast("data/rasters/sst.nc", lyrs = .x),
                                        w = 5,
                                        fun = "sd",
                                        na.rm = TRUE)
                         p()
                         wrap(tmp)},  #need to wrap again for export
                         .options = furrr_options(seed = NULL))
  })
  toc()  #takes 1.5 min to run
  plan("sequential")  #return to single core
  
  tic()
  sst_sd <- sst_sd |> 
    map(unwrap, .progress = TRUE) |> 
    rast()
  toc()  #took 15 sec to run
  
  set.names(sst_sd, gsub(x = sst_names_chunk[[i]], pattern = "^thetao", replacement = "sst_sd"))
  
  # writeCDF(sst_sd, glue("{outdir}/sst_sd{i}.nc"), varname = "sst_sd", overwrite = TRUE)
  writeRaster(sst_sd,
              glue("{outdir}/sst_sd{i}.tiff"),
              scale = 0.000732444226741791,  #value from sst.nc
              datatype = "INT2U",  #Int16
              overwrite = TRUE)
  
  rm(sst_sd)
}
# took 6 min


# Read in separate sst_sd GeoTIFFs and combine into single file
sst_sd <- glue("{outdir}/sst_sd{1:10}.tiff") |> 
  map(rast) |> 
  rast()

writeRaster(sst_sd,
            glue("{outdir}/sst_sd.tiff"),
            scale = 0.000732444226741791,  #value from sst.nc
            datatype = "INT2U",  #Int16
            overwrite = TRUE)

file.remove(glue("{outdir}/sst_sd{1:10}.tiff"))  #delete chunk files



