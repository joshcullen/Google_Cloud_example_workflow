
### Fit SDM for blue shark telemetry ###

library(tidyverse)
library(mgcv)
library(gratia)
library(tictoc)
library(terra)

source("R/utils.R")


#################
### Load data ###
#################

dat <- read_csv("data/blsh_covars_data.csv") |> 
  mutate(ptt = as.character(ptt))

summary(dat)

# Remove any obs w/ missing values and scale covars
dat2 <- dat |> 
  drop_na(CHL:z_sd) |> 
  mutate(across(.cols = c(CHL:z_sd),
                .fns = ~{scale(.x) |>
                    c()},
                .names = "{.col}_sc"))

table(dat2$presabs)


# Check correlation among covars
cor(dat2 |> 
      dplyr::select(CHL_sc:z_sd_sc))
#all corr < 0.7 (but 0.68 between SST and SSH)


#################
### Fit model ###
#################

tic()
fit <- gam(presabs ~ s(CHL_sc, k = 5, bs = "cr") + s(eke_sc, k = 5, bs = "cr") + s(ssh_sc, k = 5, bs = "cr") +
             s(ssh_sd_sc, k = 5, bs = "cr") + s(sst_sc, k = 5, bs = "cr") + s(sst_sd_sc, k = 5, bs = "cr") +
             s(z_sc, k = 5, bs = "cr") + s(z_sd_sc, k = 5, bs = "cr"),
           family = "binomial",
           data = dat2,
           weights = ifelse(dat2$presabs == 0, 1000, 1),
           method = "REML")
toc()  #took 48 sec


summary(fit)


## Viz partial effect plots
gratia::draw(fit)




#####################################
### Viz prediction for single day ###
#####################################

# Load rasters
covars <- list.files(path = "data/rasters", full.names = TRUE) |> 
  str_subset(pattern = "east|north|gebco|z_sd", negate = TRUE) |> 
  map(rast) |> 
  set_names(c("CHL","eke","ssh_sd","ssh","sst_sd","sst")) |> 
  map(~{.x[[time(.x) == as_date("2004-11-30")]]})

# Resample CHL to match spatial resolution of other covars
covars$CHL <- resample(covars$CHL, covars$sst, threads = TRUE)

z <- rast("data/rasters/gebco_2024.nc")
z_sd <- rast("data/rasters/z_sd.tiff")

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

covars_rast <- c(CHL_sc, eke_sc, ssh_sd_sc, ssh_sc, sst_sd_sc, sst_sc, z_sc, z_sd_sc)
plot(covars_rast)


# Change to DF
covars_df <- as.data.frame(covars_rast, xy = TRUE)
names(covars_df)[3:10] <- c("CHL_sc","eke_sc","ssh_sd_sc","ssh_sc","sst_sd_sc","sst_sc","z_sc","z_sd_sc")



## Predict
covars_df$pred <- predict(fit, newdata = covars_df[,3:10])

ggplot() +
  geom_raster(data = covars_df, aes(x, y, fill = pred)) +
  scale_fill_viridis_c("log(Intensity)", option = "inferno") +
  theme_bw() +
  coord_equal()
