
# JDK
FROM eclipse-temurin:17 AS buildstage

# Update the package list and install unzip
RUN apt-get update && apt-get install -y unzip && rm -rf /var/lib/apt/lists/*

# SBT 
ENV SBT_VERSION=1.10.7
RUN curl -L -o sbt-$SBT_VERSION.zip https://github.com/sbt/sbt/releases/download/v$SBT_VERSION/sbt-$SBT_VERSION.zip
RUN unzip sbt-$SBT_VERSION.zip -d sbt

# Copy the tool
WORKDIR /tool
ADD . /tool

# Compile to JavaScript
RUN /sbt/sbt/bin/sbt 'fastOptJS'

# Set the server to serve the generated website
FROM nginx:alpine
COPY --from=buildstage /tool/lib/caos/tool/  /usr/share/nginx/html
