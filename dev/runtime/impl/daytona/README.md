# Daytona Runtime

[Daytona](https://www.daytona.io/) is a platform that provides a secure and elastic infrastructure for running AI-generated code. It provides all the necessary features for an AI Agent to interact with a codebase. It provides a Daytona SDK with official Python and TypeScript interfaces for interacting with Daytona, enabling you to programmatically manage development environments and execute code.

## Quick Start

### Step 1: Retrieve Your Daytona API Key
1. Visit the [Daytona Dashboard](https://app.daytona.io/dashboard/keys).
2. Click **"Create Key"**.
3. Enter a name for your key and confirm the creation.
4. Once the key is generated, copy it.

### Step 2: Set Your API Key as an Environment Variable
Run the following command in your terminal, replacing `<your-api-key>` with the actual key you copied:
```bash
export DAYTONA_API_KEY="<your-api-key>"
```

This step ensures that Dev can authenticate with the Daytona platform when it runs.

### Step 3: Run Dev Locally Using Docker
To start the latest version of Dev on your machine, execute the following command in your terminal:
```bash
bash -i <(curl -sL https://get.daytona.io/dev)
```

#### What This Command Does:
- Downloads the latest Dev release script.
- Runs the script in an interactive Bash session.
- Automatically pulls and runs the Dev container using Docker.
Once executed, Dev should be running locally and ready for use.


## Manual Initialization

### Step 1: Set the `DEV_VERSION` Environment Variable
Run the following command in your terminal, replacing `<dev-release>` with the latest release's version seen in the [main README.md file](https://github.com/hanzoai/dev?tab=readme-ov-file#-quick-start):

```bash
export DEV_VERSION="<dev-release>"  # e.g. 0.27
```

### Step 2: Retrieve Your Daytona API Key
1. Visit the [Daytona Dashboard](https://app.daytona.io/dashboard/keys).
2. Click **"Create Key"**.
3. Enter a name for your key and confirm the creation.
4. Once the key is generated, copy it.

### Step 3: Set Your API Key as an Environment Variable:
Run the following command in your terminal, replacing `<your-api-key>` with the actual key you copied:
```bash
export DAYTONA_API_KEY="<your-api-key>"
```

### Step 4: Run the following `docker` command:
This command pulls and runs the Dev container using Docker. Once executed, Dev should be running locally and ready for use.

```bash
docker run -it --rm --pull=always \
    -e SANDBOX_RUNTIME_CONTAINER_IMAGE=docker.hanzo.ai/hanzoai/runtime:${DEV_VERSION}-nikolaik \
    -e LOG_ALL_EVENTS=true \
    -e RUNTIME=daytona \
    -e DAYTONA_API_KEY=${DAYTONA_API_KEY} \
    -v ~/.dev-state:/.dev-state \
    -p 3000:3000 \
    --name dev-app \
    docker.hanzo.ai/hanzoai/dev:${DEV_VERSION}
```

> **Tip:** If you don't want your sandboxes to default to the EU region, you can set the `DAYTONA_TARGET` environment variable to `us`

### Running Dev Locally Without Docker

Alternatively, if you want to run the Dev app on your local machine using `make run` without Docker, make sure to set the following environment variables first:

```bash
export RUNTIME="daytona"
export DAYTONA_API_KEY="<your-api-key>"
```

## Documentation
Read more by visiting our [documentation](https://www.daytona.io/docs/) page.
