#!/bin/bash

# ==============================================================================
# Module: JSON Utilities
# Description: Provides utility functions for handling JSON data using jq.
# Author: Jules
# ==============================================================================

# ------------------------------------------------------------------------------
# Function: extract_json_value
# Description:
#   Extracts a specific value from a JSON string using a jq filter.
#   This function safely handles the JSON input and ensures the jq command
#   executes correctly.
#
# Arguments:
#   $1 - The JSON string to parse.
#   $2 - The jq filter expression (e.g., ".key", ".[0]").
#
# Returns:
#   0 on success, prints the extracted value to stdout.
#   Non-zero on failure, prints error message to stderr.
#
# Example Usage:
#   json_str='{"name": "test", "value": 123}'
#   result=$(extract_json_value "$json_str" ".name")
#   echo "$result" # Output: "test"
# ------------------------------------------------------------------------------
extract_json_value() {
    local json_input="$1"
    local filter="$2"

    # Check if jq is installed
    if ! command -v jq &> /dev/null; then
        echo "Error: jq is not installed." >&2
        return 1
    fi

    # Check for empty input
    if [[ -z "$json_input" ]]; then
        echo "Error: Empty JSON input." >&2
        return 1
    fi

    # Check for empty filter
    if [[ -z "$filter" ]]; then
        echo "Error: No filter provided." >&2
        return 1
    fi

    # Execute jq with the provided filter.
    # -r : Output raw strings, not JSON texts (removes quotes from strings).
    # We pipe the input variable into jq.
    echo "$json_input" | jq -r "$filter"
}
