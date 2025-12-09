# Use an official Haskell image with stack
FROM haskell:9.6.5-slim

# Set the working directory
WORKDIR /app

# Copy dependency definition files
COPY hh200/package.yaml hh200/stack.yaml /app/hh200/

# Set the working directory for the build
WORKDIR /app/hh200

# Install dependencies first to leverage Docker layer caching
RUN stack setup
RUN stack build --only-dependencies

# Copy the rest of the hh200 project files
COPY hh200 /app/hh200

# Build and install the hh200 binary
RUN stack install

# The command to run the hh200 binary will be specified in docker-compose.yml
