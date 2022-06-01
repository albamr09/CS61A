FROM ubuntu:latest
MAINTAINER albamr09

# Updat and install dependencies
RUN apt update && apt -y upgrade
RUN dpkg --add-architecture i386
RUN apt update
RUN apt-get dist-upgrade
RUN apt-get -y install libsm6:i386
RUN apt-get -y install libx11-6:i386

# Create directory
RUN mkdir CS61A
# Copy stk installer
COPY ./resources/STk/stk_4.0.1-2_all.deb ./

# Install stk (ignore errors)
RUN dpkg -i stk_4.0.1-2_all.deb || true
RUN apt -y --fix-broken install
WORKDIR /CS61A/
