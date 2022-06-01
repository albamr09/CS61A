#!/bin/bash

# Where the source files are "$HOME/CS61A" or whatever
SOURCE_DIR=$(dirname $(cd $(dirname "${BASH_SOURCE[0]}") && pwd))

# Load config attributes
source $SOURCE_DIR/scripts/config.sh

DOCKER_CONTAINER=$(docker ps -q -f ancestor=$TAG)

# Are there any containers
if test -z "$DOCKER_CONTAINER" 
then
  # if not 
  echo "There are no containers under $TAG"
else
  #if so execute bash interactive mode in already running container
  docker exec -it $DOCKER_CONTAINER bash
fi
