
### Shell script to install necessary geospatial software and gcloud CLI for adding files from Google Cloud bucket ###

# Change to root directory if using R project
cd ..

# Install gcloud CLI
curl -O https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-cli-linux-x86_64.tar.gz

# Extracts contents of file
tar -xf google-cloud-cli-linux-x86_64.tar.gz

# Add gcloud CLI to PATH
./google-cloud-sdk/install.sh

# Initialize gcloud CLI (after restarting R session)
gcloud init --no-browser

# And then paste some version of this code into the Terminal (or PowerShell) for your own computer for auth
gcloud auth login --remote-bootstrap="https://accounts.google.com/o/oauth2/auth?{...}"

# Once you authenticate w/ NOAA account credentials via Google, paste the corresponding URL from your desktop/laptop Terminal into the RStudio terminal in the cloud
https://8085-w-joshcullen-me8xykmp.cluster-elnmuk7bnbbzqw4vcgtaay3c52.cloudworkstations.dev/?state={...}

# Of the different cloud projects, select "ggn-nmfs-usamlr-dev-7b99"

# Check that your NOAA account is successfully connected
gcloud auth list


# Create new folder (in R project) named 'data'
cd Google_Cloud_example_workflow
mkdir data

# Copy data files from Google Bucket (or from local computer)
gcloud storage cp -r gs://esd-climate-ecosystems-dev/test/ data

# Update package list
sudo apt-get update

# Install geospatial packages
sudo apt-get install libudunits2-dev gdal-bin libgdal-dev libgeos-dev libproj-dev libsqlite3-dev
