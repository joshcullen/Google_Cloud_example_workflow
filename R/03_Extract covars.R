
### Extract environmental covariates from all leatherback locations ###

library(tidyverse)
library(sf)
library(terra)
library(glue)
library(tictoc)
library(bayesmove)
library(furrr)
library(progressr)
# library(janitor)
library(giscoR)
library(summarytools)
library(exactextractr)

source("R/utils.R")


#################
### Load data ###
#################

dat <- read_csv("data/blsh_presabs_data.csv") |> 
  mutate(ptt = as.character(ptt))

glimpse(dat)
summary(dat)
dfSummary(dat) |> 
  view()





########################
### Pre-process data ###
########################

### aerial survey
# dat_aerial2 <- dat_aerial |> 
#   mutate(date = as_date(mDateTime),
#          .after = mDateTime) |> 
#   rename(lon = mlon,
#          lat = mlat) |> 
#   shift_longitude()  #change from [-180,180] to [0,360]

### NOAA observer data
# dat_piro2 <- dat_piro |> 
#   mutate(across(where(is.character),  # Change all instances of "NULL" (chr string, not type NULL) to NA
#                 \(x) case_when(x == 'NULL' ~ NA,
#                                TRUE ~ x))) |> 
#   select(-species_common_name_57) |>  # Remove duplicate common name column
#   rename(species_common_name = species_common_name_38) |> 
#   mutate(across(c(soak_time:fltln_len, ldr_diam, set_begin_lat:haul_end_lon,
#                   set_begin_temp:num_hks_set, captured_lat:captured_lon,
#                   carapace_len_curved:tail_len),
#                 as.numeric)) |>  # Change column types for some from char to numeric
#   mutate(captured_datetime = as_datetime(captured_datetime), #convert to datetime
#          date = case_when(!is.na(captured_datetime) ~ as_date(captured_datetime),
#                           TRUE ~ as_date(haul_end_datetime)),
#          # month_year = str_replace(date, pattern = "..$", replacement = "01"),
#          lon = rowMeans(pick(set_begin_lon, set_end_lon, haul_begin_lon, haul_end_lon), na.rm = TRUE),
#          lat = rowMeans(pick(set_begin_lat, set_end_lat, haul_begin_lat, haul_end_lat), na.rm = TRUE),
#          obs = case_when(is.na(species_common_name) ~ 0,
#                          TRUE ~ 1)  #add column for pres/abs of leatherbacks
#   ) |> 
#   shift_longitude() |> 
#   drop_na(date, lon, lat)  #remove any rows with missing date or coords



###############################
### Load environmental data ###
###############################

# Store file paths of dynamic covars
covar.files <- list.files(path = "data/rasters", full.names = TRUE) |> 
  str_subset(pattern = "z_sd|gebco|east|north", negate = TRUE)


# Load bathymetry
z <- rast("data/rasters/gebco_2024.nc")

# Load rugosity
z_sd <- rast("data/rasters/z_sd.tiff")





######################
### Extract covars ###
######################

# Dynamic layers
tic()
dat2 <- extract_covars_big(points = dat, covars = covar.files, t_name = "date",
                           covar_names = c("CHL","eke","ssh_sd","ssh","sst_sd","sst"),
                           cores = length(covar.files))
toc()  #takes 3.25 min


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

dat2 |> 
  select(-c(x,y)) |> 
  rename(x = lon,
         y = lat,
         id = ptt) |> 
  filter(presabs == 1) |> 
  shiny_tracks(epsg = 4326)




###################
### Export data ###
###################

write_csv(dat2, "data/blsh_covars_data.csv")
