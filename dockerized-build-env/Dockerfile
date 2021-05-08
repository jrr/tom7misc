FROM ubuntu:20.04

RUN apt-get update -y && DEBIAN_FRONTEND=noninteractive apt-get install -y bash-completion make g++ libsdl1.2-dev

RUN echo "source /etc/profile.d/bash_completion.sh" >> ~/.bashrc