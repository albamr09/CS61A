TAG="ubuntu/stk"

# -it: iterative mode -> opens bash
# --mount: will map the contents of this directory to the folder CS61A in our container
# --rm: remove container after exiting
docker run --rm -it --mount "type=bind,source=$(pwd),target=/CS61A" $(docker build -t $TAG -q .)
