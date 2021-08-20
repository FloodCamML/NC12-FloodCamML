FROM rocker/shiny-verse:4.0.5

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

# install miniconda, and set the appropriate path variables.
# install Python 3.7 (Miniconda) and Tensorflow Python packages then set path variables.
RUN wget --quiet https://repo.anaconda.com/miniconda/Miniconda3-py39_4.9.2-Linux-x86_64.sh -O ~/miniconda.sh && \
    /bin/bash ~/miniconda.sh -b -p /opt/conda && \
    rm ~/miniconda.sh && \
    /opt/conda/bin/conda clean -tipsy && \
    ln -s /opt/conda/etc/profile.d/conda.sh /etc/profile.d/conda.sh && \
    echo ". /opt/conda/etc/profile.d/conda.sh" >> ~/.bashrc && \
    echo "conda activate base" >> ~/.bashrc
ENV PATH /opt/conda/bin:$PATH

# install tensorflow and h5py using the pip that links to miniconda (the default pip is for python 2.7)
RUN /opt/conda/bin/conda install tensorflow keras pillow && \
    /opt/conda/bin/conda clean -tipsy

# let R know the right version of python to use
ENV RETICULATE_PYTHON /opt/conda/bin/python
    
# install packages
RUN install2.r shinydashboard shinyalert waiter magick tippy httr shinyWidgets shinydisconnect shinyjs googledrive googlesheets4 keras readr

# create new user so it doesn't run as root
RUN groupadd -r shinyapp && useradd --no-log-init -r -g shinyapp shinyapp

# copy necessary files
ADD app.R /home/shinyapp/app.R
ADD models /home/shinyapp/models
ADD keys /home/shinyapp/keys
ADD text /home/shinyapp/text
ADD ui /home/shinyapp/ui

# change working directory
WORKDIR /home/shinyapp

# change to new 'shinyapp' user
USER shinyapp

EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('app.R', host = '0.0.0.0', port = 3838)"]