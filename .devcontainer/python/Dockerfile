FROM mcr.microsoft.com/devcontainers/python:1-3.12-bookworm

ENV DEBIAN_FRONTEND=noninteractive \
    UV_PROJECT_ENVIRONMENT=/opt/venv \
    UV_PYTHON_INSTALL_DIR=/opt/uv/python \
    UV_LINK_MODE=copy

COPY --from=ghcr.io/astral-sh/uv:0.11.20 /uv /uvx /bin/

RUN apt-get update && apt-get install -y --no-install-recommends \
    graphviz \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /tmp/da-case-studies
COPY pyproject.toml uv.lock .python-version ./

RUN --mount=type=cache,target=/root/.cache/uv \
    mkdir -p /opt/uv/python \
    && uv sync --frozen \
    && chown -R vscode:vscode /opt/venv /opt/uv

ENV PATH="/opt/venv/bin:${PATH}"

WORKDIR /workspaces
