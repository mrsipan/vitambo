#!/bin/bash
# Run the vim clone
cd "$(dirname "$0")"
exec clojure -M -m vitambo.core "$@"
