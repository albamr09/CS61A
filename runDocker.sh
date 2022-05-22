TAG="ubuntu/stk"
sudo docker run --rm -it $(sudo docker build -t $TAG -q .)
