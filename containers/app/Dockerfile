ARG HANZO_BUILD_VERSION=dev
FROM node:21.7.2-bookworm-slim AS frontend-builder

WORKDIR /app

COPY ./frontend/package.json frontend/package-lock.json ./
RUN npm install -g npm@10.5.1
RUN npm ci

COPY ./frontend ./
RUN npm run build

FROM python:3.12.3-slim AS backend-builder

WORKDIR /app
ENV PYTHONPATH='/app'

ENV POETRY_NO_INTERACTION=1 \
    POETRY_VIRTUALENVS_IN_PROJECT=1 \
    POETRY_VIRTUALENVS_CREATE=1 \
    POETRY_CACHE_DIR=/tmp/poetry_cache

RUN apt-get update -y \
    && apt-get install -y curl make git build-essential \
    && python3 -m pip install poetry==1.8.2  --break-system-packages

COPY ./pyproject.toml ./poetry.lock ./
RUN touch README.md
RUN export POETRY_CACHE_DIR && poetry install --without evaluation --no-root && rm -rf $POETRY_CACHE_DIR

FROM python:3.12.3-slim AS hanzo-app

WORKDIR /app

ARG HANZO_BUILD_VERSION #re-declare for this section

ENV RUN_AS_HANZO=true
# A random number--we need this to be different from the user's UID on the host machine
ENV HANZO_USER_ID=42420
ENV SANDBOX_LOCAL_RUNTIME_URL=http://host.docker.internal
ENV USE_HOST_NETWORK=false
ENV WORKSPACE_BASE=/opt/workspace_base
ENV HANZO_BUILD_VERSION=$HANZO_BUILD_VERSION
ENV SANDBOX_USER_ID=0
ENV FILE_STORE=local
ENV FILE_STORE_PATH=/.hanzo-state
RUN mkdir -p $FILE_STORE_PATH
RUN mkdir -p $WORKSPACE_BASE

RUN apt-get update -y \
    && apt-get install -y curl ssh sudo

# Default is 1000, but OSX is often 501
RUN sed -i 's/^UID_MIN.*/UID_MIN 499/' /etc/login.defs
# Default is 60000, but we've seen up to 200000
RUN sed -i 's/^UID_MAX.*/UID_MAX 1000000/' /etc/login.defs

RUN groupadd app
RUN useradd -l -m -u $HANZO_USER_ID -s /bin/bash hanzo && \
    usermod -aG app hanzo && \
    usermod -aG sudo hanzo && \
    echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
RUN chown -R hanzo:app /app && chmod -R 770 /app
RUN sudo chown -R hanzo:app $WORKSPACE_BASE && sudo chmod -R 770 $WORKSPACE_BASE
USER hanzo

ENV VIRTUAL_ENV=/app/.venv \
    PATH="/app/.venv/bin:$PATH" \
    PYTHONPATH='/app'

COPY --chown=hanzo:app --chmod=770 --from=backend-builder ${VIRTUAL_ENV} ${VIRTUAL_ENV}
RUN playwright install --with-deps chromium

COPY --chown=hanzo:app --chmod=770 ./microagents ./microagents
COPY --chown=hanzo:app --chmod=770 ./hanzo ./hanzo
COPY --chown=hanzo:app --chmod=777 ./hanzo/runtime/plugins ./hanzo/runtime/plugins
COPY --chown=hanzo:app --chmod=770 ./hanzo/agenthub ./hanzo/agenthub
COPY --chown=hanzo:app ./pyproject.toml ./pyproject.toml
COPY --chown=hanzo:app ./poetry.lock ./poetry.lock
COPY --chown=hanzo:app ./README.md ./README.md
COPY --chown=hanzo:app ./MANIFEST.in ./MANIFEST.in
COPY --chown=hanzo:app ./LICENSE ./LICENSE

# This is run as "hanzo" user, and will create __pycache__ with hanzo:hanzo ownership
RUN python hanzo/core/download.py # No-op to download assets
# Add this line to set group ownership of all files/directories not already in "app" group
# hanzo:hanzo -> hanzo:app
RUN find /app \! -group app -exec chgrp app {} +

COPY --chown=hanzo:app --chmod=770 --from=frontend-builder /app/build ./frontend/build
COPY --chown=hanzo:app --chmod=770 ./containers/app/entrypoint.sh /app/entrypoint.sh

USER root

WORKDIR /app

ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["uvicorn", "hanzo.server.listen:app", "--host", "0.0.0.0", "--port", "3000"]
