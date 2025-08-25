

### Generate available points over the spatial extents ###

library(tidyverse)
library(sf)
library(giscoR)
library(MetBrewer)
library(tictoc)
library(patchwork)
library(amt)
library(rnaturalearth)
library(ggspatial)
library(cmocean)

source("R/utils.R")


#################
### Load data ###
#################

dat <- read_csv("data/test/blsh_data.csv") |> 
  mutate(ptt = as.character(ptt))



## Load land spatial layers

# For data viz (coarser res)
world <- gisco_countries |> 
  filter(NAME_ENGL != "Antarctica") |> 
  st_shift_longitude() |> 
  st_crop(xmin = 100, xmax = 280, ymin = -40, ymax = 60) |> 
  st_shift_longitude()

# For data processing (i.e., polygon clipping; finer res)
world2 <- ne_countries(scale = 10, returnclass = 'sf') |> 
  st_transform(3832)



## Viz tracks

# Generate color palette for tracks
set.seed(123)
col.pal <- sapply(names(MetPalettes), function(x) met.brewer(name = x, n = 5, type = "continuous")) |>
  as.vector() |>
  sample(size = n_distinct(dat$ptt))

ggplot() +
  geom_sf(data = world, color = "grey70") +
  geom_point(data = dat |> 
               mutate(lon = ifelse(lon < 0, lon + 360, lon)),
             aes(lon, lat, group = ptt, color = ptt)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  scale_color_manual(values = col.pal) +
  labs(x="",y="") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_sf(xlim = c(195,257), ylim = c(0,55),
           expand = FALSE)





####################################################
### Calculate bounding box for available habitat ###
####################################################

# Project tracks
dat.sf <- dat |>
  st_as_sf(coords = c('lon','lat'), crs = 4326) |>
  st_transform(3832)

# Define bounding box of study extent for population
bbox <- dat.sf |> 
  st_bbox() |>
  st_as_sfc() |>
  st_buffer(10000)  #buffer each side by 10 km

ggplot() +
  geom_sf(data = world2, color = "grey70") +
  geom_sf(data = dat.sf, aes(color = ptt)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  scale_color_manual(values = col.pal) +
  geom_sf(data = bbox, color = "red", fill = "transparent", linewidth = 1) +
  labs(x="",y="") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        panel.grid = element_blank()) +
  coord_sf(xlim = st_bbox(dat.sf)[c(1,3)],
           ylim = st_bbox(dat.sf)[c(2,4)],
           expand = TRUE)



# Mask the bbox by the land layer to generate available pts in water
bbox_mask <- st_mask(bbox, world2)

# Check polygon of availability
ggplot() +
  geom_sf(data = world2) +
  geom_sf(data = bbox, color = "red", fill = "transparent", linewidth = 0.75) +
  geom_sf(data = bbox_mask, fill = "lightblue") +
  geom_sf(data = dat.sf, aes(color = ptt), show.legend = FALSE) +
  scale_color_manual(values = col.pal) +
  theme_bw() +
  coord_sf(xlim = st_bbox(dat.sf)[c(1,3)],
           ylim = st_bbox(dat.sf)[c(2,4)],
           expand = TRUE)




#################################
### Generate available points ###
#################################

# Generate available points and randomly assign per observation
# ratio of 1:30 (used:available)


### Availability from bounding box ###

set.seed(2025)
obs_id <- rep(1:nrow(dat), 30)

avail_bbox <- st_sample(bbox_mask, size = 30 * nrow(dat)) |>
  data.frame() |>
  mutate(obs_id = sample(obs_id),
         ptt = dat$ptt[obs_id],
         date = dat$date[obs_id],
         obs = 0,
         x = unlist(map(geometry, 1)),
         y = unlist(map(geometry, 2)),
         .before = geometry) |>
  st_sf() |> 
  add_trans_coords(coords = c('x','y'), proj = 3832, new_proj = 4326) #|> 
# shift_longitude()


# Viz example of available point for PTT 52130
ggplot() +
  geom_sf(data = world2) +
  geom_point(data = avail_bbox |> 
               filter(ptt == 52130), aes(x, y), size = 0.5) +
  geom_sf(data = dat.sf |> 
            filter(ptt == 52130), aes(color = month(date)), alpha = 0.75) +
  scale_color_cmocean("Month", name = "phase") +
  theme_bw() +
  coord_sf(xlim = st_bbox(dat.sf)[c(1,3)],
           ylim = st_bbox(dat.sf)[c(2,4)],
           expand = TRUE)


# Viz example of available point for PTT 100957
ggplot() +
  geom_sf(data = world2) +
  geom_point(data = avail_bbox |> 
               filter(ptt == 100957), aes(x, y), size = 0.5) +
  geom_sf(data = dat.sf |> 
            filter(ptt == 100957), aes(color = month(date)), alpha = 0.75) +
  scale_color_cmocean("Month", name = "phase") +
  theme_bw() +
  coord_sf(xlim = st_bbox(dat.sf)[c(1,3)],
           ylim = st_bbox(dat.sf)[c(2,4)],
           expand = TRUE)






###################################################
### Join all used and available points together ###
###################################################

used <- dat |>
  add_trans_coords(coords = c("lon","lat"), proj = 4326, new_proj = 3832) |> 
  dplyr::select(ptt, date, presabs, lon, lat, x, y)


avail_bbox2 <- avail_bbox |>
  rename(presabs = obs) |> 
  dplyr::select(ptt, date, presabs, lon, lat, x, y)



rsf_dat <- rbind(used, avail_bbox2)



###########################
### Export prepped data ###
###########################

write_csv(rsf_dat, "data/test/blsh_presabs_data.csv")
