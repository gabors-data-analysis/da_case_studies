FROM mcr.microsoft.com/devcontainers/miniconda:3

ENV DEBIAN_FRONTEND=noninteractive

# Install mamba
RUN conda install -n base -c conda-forge mamba -y && \
    conda clean -afy

# Copy env file (symlink resolves automatically)
COPY .devcontainer/python/daenv_linux.yml /tmp/daenv_linux.yml

# Create environment
RUN mamba env create -f /tmp/daenv_linux.yml && \
    conda clean -afy

# Make env default
ENV CONDA_DEFAULT_ENV=daenv
ENV PATH=/opt/conda/envs/daenv/bin:$PATH