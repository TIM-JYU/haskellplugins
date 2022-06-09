#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

# This is used by the container to start relevant services

/hbin/ChoicesPlugin -p5001 &
/hbin/MultipleChoicesPlugin -p5002 &
/hbin/ShortNotePlugin -p5003 &
/hbin/GraphVizPlugin -p5004 
