FROM openanalytics/r-base

MAINTAINER Jason P. Pickering "jason@dhis2.org"

# system libraries of general use
RUN apt-get update && apt-get install -y \
sudo \
libcurl4-gnutls-dev \
libssl-dev \
libxml2-dev

# install shinyProxy
RUN mkdir -p /opt/shinyproxy/
RUN wget https://www.shinyproxy.io/downloads/shinyproxy-2.2.1.jar -O /opt/shinyproxy/shinyproxy.jar


# basic shiny functionality
RUN R -e "install.packages(c('shiny','shinyjs','magrittr','openxlsx','devtools','roxygen2','doMC'), repos='https://cloud.r-project.org/')"
RUN R -e "devtools::install_github('jason-p-pickering/datim-validation')"

# copy the app to the image
RUN mkdir /root/validation
COPY validation /root/validation

COPY Rprofile.site /usr/lib/R/etc/
  
EXPOSE 3939

CMD ["R", "-e", "shiny::runApp('/root/validation')"]
