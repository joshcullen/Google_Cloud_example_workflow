
### Make predictions from fitted blue shark SDM ###

library(tidyverse)
library(mgcv)
library(tictoc)
library(terra)
library(rnaturalearth)
library(glue)

source("R/utils.R")


###########################
### Load model and data ###
###########################

# GAM model fit
fit <- readRDS("GAM_model_fit.rds")

# Blue shark data
dat <- read_csv("data/test/blsh_covars_data.csv") |> 
  mutate(ptt = as.character(ptt))

# Remove any obs w/ missing values and scale covars
dat2 <- dat |> 
  drop_na(CHL:z_sd) |> 
  mutate(across(.cols = c(CHL:z_sd),
                .fns = ~{scale(.x) |>
                    c()},
                .names = "{.col}_sc"))



#########################################
### Viz prediction for subset of days ###
#########################################

# Load rasters
covars <- list.files(path = "data/test/rasters", full.names = TRUE) |> 
  str_subset(pattern = "east|north|gebco|z_sd", negate = TRUE) |> 
  map(rast) |> 
  set_names(c("CHL","eke","ssh_sd","ssh","sst_sd","sst")) |> 
  map(~{.x[[time(.x) >= as_date("2004-12-01") &
              time(.x) <= as_date("2004-12-31")]]})

# Resample CHL to match spatial resolution of other covars (ideally have this as a saved file)
covars$CHL <- resample(covars$CHL, covars$sst, threads = TRUE)

z <- rast("data/test/rasters/gebco_2024.nc")
z_sd <- rast("data/test/rasters/z_sd.tiff")

# Center and scale covars
z_sc <- (z - mean(dat2$z)) / sd(dat2$z)
z_sc <- as.data.frame(z_sc, xy = TRUE) |>  
  rename(lon = x, lat = y) |> 
  shift_longitude(rev = TRUE) |> #adjust lon to be negative
  filter(lon < 0) |> 
  rast(crs = "EPSG:4326") |> 
  resample(covars$sst)  #match spatial extent of other covars
z_sd_sc <- (z_sd - mean(dat2$z_sd)) / sd(dat2$z_sd)
z_sd_sc <- as.data.frame(z_sd_sc, xy = TRUE) |>  
  rename(lon = x, lat = y) |> 
  shift_longitude(rev = TRUE) |> #adjust lon to be negative
  filter(lon < 0) |> 
  rast(crs = "EPSG:4326") |> 
  resample(covars$sst)  #match spatial extent of other covars
sst_sc <- (covars$sst - mean(dat2$sst)) / sd(dat2$sst)
sst_sd_sc <- (covars$sst_sd - mean(dat2$sst_sd)) / sd(dat2$sst_sd)
eke_sc <- (covars$eke - mean(dat2$eke)) / sd(dat2$eke)
ssh_sc <- (covars$ssh - mean(dat2$ssh)) / sd(dat2$ssh)
ssh_sd_sc <- (covars$ssh_sd - mean(dat2$ssh_sd)) / sd(dat2$ssh_sd)
CHL_sc <- (covars$CHL - mean(dat2$CHL)) / sd(dat2$CHL)

covars_rast <- c(CHL_sc, eke_sc, ssh_sd_sc, ssh_sc, sst_sd_sc, sst_sc)

# Plot examples per covar
idx <- seq(1, nlyr(covars_rast), by = 31)
plot(covars_rast[[idx]])
plot(c(z_sc, z_sd_sc))


# Change to list of DFs by date
dts <- as_date(time(covars_rast))                 # one entry per layer
groups <- split(seq_len(nlyr(covars_rast)), dts)    # indices grouped by date

covar_list <- imap(groups, ~{
  set_idx <- .x
  set_date <- .y
  
  # Add z_sc and z_sd_sc
  covar_sub <- c(covars_rast[[set_idx]],
                 z_sc,
                 z_sd_sc)
  df <- as.data.frame(covar_sub, xy = TRUE, na.rm = FALSE)
  names(df)[3:10] <- c("CHL_sc","eke_sc","ssh_sd_sc","ssh_sc","sst_sd_sc","sst_sc","z_sc","z_sd_sc")
  
  return(df)
})



## Make predictions (on subset of dates) ##
# covars_single <- covars |> 
#   map(~{.x[[time(.x) == as_date("2004-11-30")]]})
covar_pred <- covar_list |> 
  map(~{.x$pred <- predict(fit, newdata = .x[,3:10])
  
  return(dplyr::select(.x, x, y, pred))
  },
  .progress = TRUE)

covar_pred_df <- bind_rows(covar_pred, .id = "date")
 


# Plot predictions
world <- ne_countries(scale = 10, returnclass = 'sf')

ggplot() +
  geom_raster(data = covar_pred_df |> 
                filter(date >= "2004-12-01" & date <= "2004-12-09"),
              aes(x, y, fill = pred)) +
  scale_fill_viridis_c("log(Intensity)", option = "inferno") +
  geom_sf(data = world, fill = "grey70") +
  theme_bw() +
  coord_sf(xlim = ext(sst_sc)[1:2],
           ylim = ext(sst_sc)[3:4],
           expand = FALSE) +
  facet_wrap(~ date)



##################################################
### Export prediction files (raster and image) ###
##################################################

exp_idx <- seq_along(covar_pred)

# Export rasters
iwalk(exp_idx,
      ~writeRaster(x = rast(covar_pred[[.x]]),
                   filename = glue("Predictions/raster/blsh_pred_{names(covar_pred)[.x]}.tif")
                   ),
      .progress = TRUE
      )


# Export images
iwalk(exp_idx,
      ~{
        plt <- ggplot() +
          geom_raster(data = covar_pred[[.x]],
                      aes(x, y, fill = pred)) +
          scale_fill_viridis_c("log(Intensity)", option = "inferno") +
          geom_sf(data = world, fill = "grey70") +
          labs(title = glue("{names(covar_pred)[.x]} Prediction")) +
          theme_bw() +
          coord_sf(xlim = ext(sst_sc)[1:2],
                   ylim = ext(sst_sc)[3:4],
                   expand = FALSE)
        
        ggsave(filename = glue("Predictions/img/blsh_pred_{names(covar_pred)[.x]}.png"),
               plot = plt, width = 6, height = 8, units = "in", dpi = 400)
      },
      .progress = TRUE
)


