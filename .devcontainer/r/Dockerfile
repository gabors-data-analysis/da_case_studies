FROM ghcr.io/rocker-org/devcontainer/r-ver:4.5

ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
RUN apt-get update && apt-get install -y \
    cmake \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install renv
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Create project directory inside image
WORKDIR /usr/local/src/project

# Copy renv.lock (via symlink)
COPY .devcontainer/r/renv.lock renv.lock

# Restore packages
RUN R -e "renv::restore(lockfile = 'renv.lock', prompt = FALSE)"

# Set default working directory
WORKDIR /workspaces