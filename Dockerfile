FROM openanalytics/r-base

MAINTAINER Jason P. Pickering "jason@dhis2.org"

# system libraries of general use
RUN apt-get update && apt-get install -y \
sudo \
libcurl4-gnutls-dev \
libcairo2-dev \
libxt-dev \
libssl-dev \
libssh2-1-dev \
libssl1.0.0

# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
libmpfr-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny','magrittr','openxlsx','devtools'), repos='https://cloud.r-project.org/')"
RUN R -e "devtools::install_github('jason-p-pickering/datim-validation')"

# copy the app to the image
RUN mkdir /root/validation
COPY validation /root/validation

#COPY Rprofile.site /usr/lib/R/etc/
  
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/validation')"]