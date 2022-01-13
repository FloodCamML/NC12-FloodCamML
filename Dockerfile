FROM rocker/shiny-verse:4.1.0

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    file \
    libedit2 \
    libssl-dev \
    lsb-release \
    psmisc \
    procps \
    wget \
    zlib1g-dev \
    libxml2-dev \
    libpq-dev \
    libssh2-1-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libmagick++-dev && \
    rm -rf /var/lib/apt/lists/

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean
    
# install packages
RUN install2.r shinyalert waiter magick tippy httr shinyWidgets shinydisconnect shinyjs googledrive googlesheets4 keras readr bs4Dash

# create new user so it doesn't run as root
RUN groupadd -r shinyapp && useradd --no-log-init -r -g shinyapp shinyapp

# copy necessary files
ADD app.R /home/shinyapp/app.R
ADD models /home/shinyapp/models
# ADD keys /home/shinyapp/keys
ADD keys/google_key.json /home/shinyapp/google_key.json
ADD keys/google_keys.R /home/shinyapp/google_keys.R

ADD text /home/shinyapp/text
ADD ui /home/shinyapp/ui
ADD flood-camml.css /home/shinyapp/flood-camml.css

# change working directory
WORKDIR /home/shinyapp

# change to new 'shinyapp' user
USER shinyapp

EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('app.R', host = '0.0.0.0', port = 3838)"]