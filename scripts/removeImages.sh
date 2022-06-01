#!/bin/bash

# Remove images with no container associated
docker image prune -a
# Get stopped containers
STOPPED_CONTAINERS=$(docker ps -a -q -f status=exited)
# Are there any stopped containers
if ! test -z "$STOPPED_CONTAINERS" 
then
  # If so remove them
  docker rm $STOPPED_CONTAINERS
fi
