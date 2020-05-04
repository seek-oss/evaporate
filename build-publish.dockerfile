FROM ubuntu:18.04
MAINTAINER SEEK Ltd.

RUN apt-get update &&
	apt-get install -y libgmp-dev curl &&
	apt-get clean

# Needed to avoid exceptions when printing unicode to stdout/stderr
ENV LC_ALL C.UTF-8

ENV APP_DIR /app

# Install application
RUN mkdir $APP_DIR
WORKDIR $APP_DIR
ADD .publish $APP_DIR

ENTRYPOINT ["./evaporate"]
