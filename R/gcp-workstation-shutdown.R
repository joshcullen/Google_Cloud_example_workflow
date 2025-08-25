
# Run command to shut down workstation from the inside

### User-set vars
cluster_name = "workstation-cluster-1"
config_name = "posit-large"

### Code-driven vars
project_id <- system2("curl", "http://metadata.google.internal/computeMetadata/v1/project/project-id -H 'Metadata-Flavor: Google'", stdout = TRUE)

zone <- system2("curl", "http://metadata.google.internal/computeMetadata/v1/instance/zone -H 'Metadata-Flavor: Google'", stdout = TRUE)

# The output for zone will look like "projects/123456789/zones/us-east4-a"
# We need to extract the region from this string.
region <- gsub("projects/.*/zones/(.*)-.*", "\\1", zone)

workstation_name <- system2("curl", "http://metadata.google.internal/computeMetadata/v1/instance/hostname -H 'Metadata-Flavor: Google'", stdout = TRUE)
workstation_name = "w-joshcullen-me8xykmp"

full_path <- paste0(
  "projects/", project_id,
  "/locations/", region,
  "/workstationClusters/", cluster_name, 
  "/workstationConfigs/", config_name, 
  "/workstations/", workstation_name
)

print(full_path)

system(paste("gcloud workstations stop", full_path))
