#!/usr/bin/env bats

# ==============================================================================
# Test Suite: JSON Parsing
# Description: Unit tests for the JSON utility functions in src/json_utils.sh
# ==============================================================================

# Load the library to be tested
setup() {
    # Determine the project root directory relative to this test file
    # BATS_TEST_DIRNAME is provided by bats
    PROJECT_ROOT="$BATS_TEST_DIRNAME/.."

    # Source the utility script
    source "$PROJECT_ROOT/src/json_utils.sh"
}

# ------------------------------------------------------------------------------
# Test Case: Parse a simple JSON object
# ------------------------------------------------------------------------------
@test "extract_json_value parses simple json object using jq" {
    # Define a sample JSON string
    local json='{"status": "success", "data": {"id": 101, "message": "hello world"}}'

    # Run the function to extract the 'status' field
    run extract_json_value "$json" ".status"

    # Assertions
    [ "$status" -eq 0 ]           # Check exit code is 0 (success)
    [ "$output" = "success" ]     # Check output matches expected value
}

# ------------------------------------------------------------------------------
# Test Case: Parse a nested JSON value
# ------------------------------------------------------------------------------
@test "extract_json_value parses nested json field" {
    local json='{"status": "success", "data": {"id": 101, "message": "hello world"}}'

    run extract_json_value "$json" ".data.message"

    [ "$status" -eq 0 ]
    [ "$output" = "hello world" ]
}

# ------------------------------------------------------------------------------
# Test Case: Handle invalid JSON gracefully (expecting jq error)
# ------------------------------------------------------------------------------
@test "extract_json_value handles invalid json" {
    local json='{invalid-json'

    run extract_json_value "$json" ".status"

    [ "$status" -ne 0 ]           # Should fail
    # Note: precise error message might vary by jq version, so just checking failure
}
