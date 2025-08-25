
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

dat <- read_csv("data/test/blsh_covars_data.csv") |> 
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
toc()  #took 1.5 min


summary(fit)


## Viz partial effect plots
gratia::draw(fit)



########################
### Export model fit ###
########################

saveRDS(fit, file = "GAM_model_fit.rds")


