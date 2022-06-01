#!/bin/bash

# Where docker file is (could be replaced with . or .., but this is sensitive to where you run the file from)
SOURCE_DIR=$(dirname $(cd $(dirname "${BASH_SOURCE[0]}") && pwd))

# Load config attributes
source $SOURCE_DIR/scripts/config.sh

# -it: iterative mode -> opens bash
# --mount: will map the contents of this directory to the folder CS61A in our container
# --rm: remove container after exiting
docker run --rm -it --mount "type=bind,source=$SOURCE_DIR,target=/CS61A" $(docker build -t $TAG -q $SOURCE_DIR)
