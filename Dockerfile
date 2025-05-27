FROM dockerregistryspa.azurecr.io/dockerregistryspa/spa-rshiny-base:4.3.0-1.0.0
COPY . /app
RUN apt-get update && apt-get install -y \
    libxt6 \
    libcairo2-dev \
    libjpeg-dev \
    libpng-dev
RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_local('/app')"
# Set entrypoint and pass runtime arguments to the CMD
RUN echo 'library(megaplots); megaplots::launch_megaplots()' > /srv/shiny-server/app.R
USER root
