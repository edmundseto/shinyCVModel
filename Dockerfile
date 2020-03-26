FROM openanalytics/r-base

MAINTAINER Edmund Seto "eseto@uw.edu"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
    libopenblas-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install dependencies of the SYVisualization app
RUN R -e "install.packages(c('shinythemes'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('deSolve'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('reshape'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('glue'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('ggplot2'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('DT'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/shinyCVModel
COPY shinyCVModel /root/shinyCVModel

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

ENV OPENBLAS_NUM_THREADS=2
ENV GOTO_NUM_THREADS=2
ENV OMP_NUM_THREADS=2

CMD ["R", "-e shiny::runApp('/root/shinyCVModel')"]
