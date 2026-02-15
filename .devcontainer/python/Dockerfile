# Base image
FROM mcr.microsoft.com/devcontainers/miniconda:3

ENV DEBIAN_FRONTEND=noninteractive

# Install devcontainer features dependencies
RUN apt-get update && apt-get install -y \
    bash \
    && apt-get clean

# Copy environment definition into image
COPY ch00-tech-prep/daenv_linux.yml /tmp/daenv_linux.yml

# Create conda environment
RUN conda env create -f /tmp/daenv_linux.yml && \
    conda clean -afy

# Set default shell to use the environment
ENV PATH=/opt/conda/envs/daenv/bin:$PATH

# Optional: activate environment automatically for bash
RUN echo "conda activate daenv" >> /etc/bash.bashrc
