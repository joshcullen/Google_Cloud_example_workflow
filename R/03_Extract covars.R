
### Extract environmental covariates from all leatherback locations ###

library(tidyverse)
library(sf)
library(terra)
library(glue)
library(tictoc)
# library(bayesmove)
library(furrr)
library(progressr)
# library(janitor)
# library(giscoR)
# library(summarytools)
# library(exactextractr)

source("R/utils.R")


#################
### Load data ###
#################

dat <- read_csv("data/test/blsh_presabs_data.csv") |> 
  mutate(ptt = as.character(ptt))

glimpse(dat)
summary(dat)
# dfSummary(dat) |> 
#   view()





###############################
### Load environmental data ###
###############################

# Store file paths of dynamic covars
covar.files <- list.files(path = "data/test/rasters", full.names = TRUE) |> 
  str_subset(pattern = "z_sd|gebco|east|north", negate = TRUE)


# Load bathymetry
z <- rast("data/test/rasters/gebco_2024.nc")

# Load rugosity
z_sd <- rast("data/test/rasters/z_sd.tiff")





######################
### Extract covars ###
######################

# Dynamic layers (assuming all files have the same spatiotemporal resolution)
tic()
dat2 <- extract_covars_big(points = dat, covars = covar.files, t_name = "date",
                           covar_names = c("CHL","eke","ssh_sd","ssh","sst_sd","sst"),
                           cores = length(covar.files))
toc()  #takes 15 min


# Static layers
dat2$z <- extract(x = z,
                  y = dat2[,c("lon","lat")] |> 
                    shift_longitude(),
                  ID = FALSE) |> 
  unlist()

dat2$z_sd <- extract(x = z_sd,
                     y = dat2[,c("lon","lat")] |> 
                       shift_longitude(),
                     ID = FALSE) |> 
  unlist()






#################################
### Visualize tracks and data ###
#################################

# dat2 |> 
#   select(-c(x,y)) |> 
#   rename(x = lon,
#          y = lat,
#          id = ptt) |> 
#   filter(presabs == 1) |> 
#   shiny_tracks(epsg = 4326)




###################
### Export data ###
###################

write_csv(dat2, "data/test/blsh_covars_data.csv")




############################################
### Shut down Workstation after long run ###
############################################

source("R/gcp-workstation-shutdown.R")