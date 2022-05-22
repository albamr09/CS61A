FROM ubuntu:latest
MAINTAINER albamr09

RUN apt update && apt -y upgrade
RUN dpkg --add-architecture i386
RUN apt update
RUN apt-get dist-upgrade
RUN apt-get -y install libsm6:i386
RUN apt-get -y install libx11-6:i386

# Create directory
RUN mkdir CS61A
# Copy everything to directory
COPY ./ ./CS61A/

# Install stk (ignore errors)
RUN dpkg -i /CS61A/resources/STk/stk_4.0.1-2_all.deb || true
RUN apt -y --fix-broken install
