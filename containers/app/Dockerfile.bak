ARG IDE_BUILD_VERSION=dev
FROM node:24.3.0-bookworm-slim AS frontend-builder

WORKDIR /app

COPY app/frontend/package.json app/frontend/package-lock.json ./
RUN npm ci

COPY app/frontend ./
RUN npm run build

FROM python:3.12.10-slim AS base
FROM base AS backend-builder

WORKDIR /app
ENV PYTHONPATH='/app'

ENV POETRY_NO_INTERACTION=1 \
    POETRY_VIRTUALENVS_IN_PROJECT=1 \
    POETRY_VIRTUALENVS_CREATE=1 \
    POETRY_CACHE_DIR=/tmp/poetry_cache

RUN apt-get update -y \
    && apt-get install -y curl make git build-essential \
    && python3 -m pip install poetry --break-system-packages

# Copy all hanzo wheels
COPY wheels/hanzo_aci-0.3.1-py3-none-any.whl ./wheels/
COPY wheels/hanzo_mcp-0.6.13-py3-none-any.whl ./wheels/
COPY wheels/hanzo_mcp_client-0.1.0-py3-none-any.whl ./wheels/

COPY app/dev/pyproject.toml ./
RUN touch README.md
# Remove local path dependencies from pyproject.toml temporarily for poetry install
RUN sed -i '/hanzo-aci/d' pyproject.toml \
    && sed -i '/hanzo-mcp/d' pyproject.toml \
    && export POETRY_CACHE_DIR && poetry install --no-root && rm -rf $POETRY_CACHE_DIR

# Now install all hanzo packages directly into the virtual environment
RUN /app/.venv/bin/pip install wheels/hanzo_aci-0.3.1-py3-none-any.whl \
    wheels/hanzo_mcp-0.6.13-py3-none-any.whl \
    wheels/hanzo_mcp_client-0.1.0-py3-none-any.whl

FROM base AS ide-app

WORKDIR /app

# re-declare for this section
ARG IDE_BUILD_VERSION

ENV RUN_AS_IDE=true
# A random number--we need this to be different from the user's UID on the host machine
ENV IDE_USER_ID=42420
ENV SANDBOX_LOCAL_RUNTIME_URL=http://host.docker.internal
ENV USE_HOST_NETWORK=false
ENV WORKSPACE_BASE=/opt/workspace_base
ENV IDE_BUILD_VERSION=$IDE_BUILD_VERSION
ENV SANDBOX_USER_ID=0
ENV FILE_STORE=local
ENV FILE_STORE_PATH=/.ide
ENV INIT_GIT_IN_EMPTY_WORKSPACE=1
RUN mkdir -p $FILE_STORE_PATH
RUN mkdir -p $WORKSPACE_BASE

RUN apt-get update -y \
    && apt-get install -y curl ssh sudo \
    && rm -rf /var/lib/apt/lists/*

# Default is 1000, but OSX is often 501
RUN sed -i 's/^UID_MIN.*/UID_MIN 499/' /etc/login.defs
# Default is 60000, but we've seen up to 200000
RUN sed -i 's/^UID_MAX.*/UID_MAX 1000000/' /etc/login.defs

RUN groupadd app
RUN useradd -l -m -u $IDE_USER_ID -s /bin/bash ide && \
    usermod -aG app ide && \
    usermod -aG sudo ide && \
    echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
RUN chown -R ide:app /app && chmod -R 770 /app
RUN sudo chown -R ide:app $WORKSPACE_BASE && sudo chmod -R 770 $WORKSPACE_BASE
USER ide

ENV VIRTUAL_ENV=/app/.venv \
    PATH="/app/.venv/bin:$PATH" \
    PYTHONPATH='/app'

COPY --chown=ide:app --chmod=770 --from=backend-builder ${VIRTUAL_ENV} ${VIRTUAL_ENV}

COPY --chown=ide:app --chmod=770 ./app/dev/microagents ./microagents
COPY --chown=ide:app --chmod=770 ./app/dev/ide ./ide
COPY --chown=ide:app --chmod=777 ./app/dev/ide/runtime/plugins ./ide/runtime/plugins
COPY --chown=ide:app app/dev/pyproject.toml app/dev/poetry.lock README.md MANIFEST.in LICENSE ./

# This is run as "ide" user, and will create __pycache__ with ide:ide ownership
RUN python ide/core/download.py # No-op to download assets
# Add this line to set group ownership of all files/directories not already in "app" group
# ide:ide -> ide:app
RUN find /app \! -group app -exec chgrp app {} +

COPY --chown=ide:app --chmod=770 --from=frontend-builder /app/build ./frontend/build
COPY --chown=ide:app --chmod=770 ./app/dev/containers/app/entrypoint.sh /app/entrypoint.sh

USER root

WORKDIR /app

ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["uvicorn", "ide.server.listen:app", "--host", "0.0.0.0", "--port", "8888"]
