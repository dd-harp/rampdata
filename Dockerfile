FROM rocker/geospatial:4.0.2-ubuntugis
LABEL Name=rampdata Version=0.0.1

RUN apt-get install -y \
    fortunes \
    libssh-dev \
    littler

RUN R -e "devtools::install_github('REditorSupport/languageserver', force = TRUE)" && \
    install2.r \
      renv

CMD ["sh", "-c", "/usr/games/fortune -a | cowsay"]
