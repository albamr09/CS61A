TAG="ubuntu/stk"
docker run --rm -it $(docker build -t $TAG -q .)
