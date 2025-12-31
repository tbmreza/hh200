#!/bin/bash
set -e

# Resolve script directory to source utils correctly
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/json_utils.sh"

log() {
  echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" >&2
}

# Ensure jq is available
if ! command -v jq &> /dev/null; then
    log "Error: jq is required but not installed."
    exit 1
fi

# Main loop
while read -r line; do
  if [ -z "$line" ]; then
    continue
  fi

  # Extract request details
  # We extract id and method. id might be null for notifications.
  read -r id method < <(echo "$line" | jq -r '[.id // "null", .method] | @tsv')
  
  log "Received request: id=$id method=$method"

  case "$method" in
    initialize)
      # MCP Initialize Request
      response=$(jq -n --arg id "$id" '{
        jsonrpc: "2.0",
        id: ($id | tonumber? // $id),
        result: {
          protocolVersion: "2024-11-05",
          capabilities: {
            tools: {}
          },
          serverInfo: {
            name: "hh200-mcp-server",
            version: "0.1.0"
          }
        }
      }')
      echo "$response"
      ;;
      
    notifications/initialized)
      log "Client initialized."
      ;;
      
    tools/list)
      # List available tools
      response=$(jq -n --arg id "$id" '{
        jsonrpc: "2.0",
        id: ($id | tonumber? // $id),
        result: {
          tools: [
            {
              name: "hh200",
              description: "Execute the hh200 tool",
              inputSchema: {
                type: "object",
                properties: {
                  args: {
                    type: "array",
                    items: { type: "string" },
                    description: "Arguments to pass to hh200"
                  }
                },
                required: ["args"]
              }
            }
          ]
        }
      }')
      echo "$response"
      ;;

    tools/call)
      # Execute a tool
      # TODO: Parse params and call actual binary
      # For now, we return a placeholder response
      
      tool_name=$(echo "$line" | jq -r '.params.name')
      
      if [[ "$tool_name" == "hh200" ]]; then
          # In a real implementation, we would execute: hh200 "${args[@]}"
          # and capture output.
          
          response=$(jq -n --arg id "$id" '{
            jsonrpc: "2.0",
            id: ($id | tonumber? // $id),
            result: {
              content: [
                {
                  type: "text",
                  text: "Tool execution placeholder. hh200 called."
                }
              ]
            }
          }')
      else
          response=$(jq -n --arg id "$id" '{
            jsonrpc: "2.0",
            id: ($id | tonumber? // $id),
            error: {
              code: -32601,
              message: "Tool not found"
            }
          }')
      fi
      echo "$response"
      ;;

    ping)
        response=$(jq -n --arg id "$id" '{
            jsonrpc: "2.0",
            id: ($id | tonumber? // $id),
            result: {}
        }')
        echo "$response"
        ;;
        
    *)
      # Method not found
      log "Unknown method: $method"
      if [ "$id" != "null" ]; then
         response=$(jq -n --arg id "$id" '{
          jsonrpc: "2.0",
          id: ($id | tonumber? // $id),
          error: {
            code: -32601,
            message: "Method not found"
          }
        }')
        echo "$response"
      fi
      ;;
  esac
done
