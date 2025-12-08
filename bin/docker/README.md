# Docker Test Environment for hh200

This directory contains the necessary files to create a Docker-based test environment for the `hh200` binary. The setup uses `docker-compose` to orchestrate a multi-container application that includes the `hh200` service and an `httpbin` service for testing HTTP requests.

## Prerequisites

- Docker
- Docker Compose

## Files

- `Dockerfile`: This file defines the instructions to build a Docker image for the `hh200` binary. It uses a Haskell base image, copies the project files, and builds the binary using `stack`.

- `docker-compose.yml`: This file defines the multi-container Docker application. It consists of two services:
    - `hh200`: Builds the `hh200` Docker image using the `Dockerfile` and runs the binary. The web UI is exposed on port `8080`.
    - `httpbin`: Uses the `kennethreitz/httpbin` image to provide a simple HTTP request and response service. It is exposed on port `8000`.

## How to Run

1. **Build and start the services:**
   ```bash
   docker-compose up --build
   ```

2. **Access the services:**
   - **hh200 Web UI:** [http://localhost:8080](http://localhost:8080)
   - **httpbin:** [http://localhost:8000](http://localhost:8000)

3. **Stop the services:**
   ```bash
   docker-compose down
   ```

## Usage

With this setup, you can use the `hh200` web UI to send requests to the `httpbin` service (e.g., `http://httpbin:80/get`) to test the functionality of the `hh200` binary in a containerized environment. Note that within the Docker network, the `httpbin` service is accessible to the `hh200` service at the hostname `httpbin`.
