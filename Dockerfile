FROM rocker/geospatial:4.0.2-ubuntugis
LABEL Name=rampdata Version=0.0.1

RUN apt-get install -y \
    fortunes \
    libssh-dev

RUN R -e 'install.packages("renv")'

CMD ["sh", "-c", "/usr/games/fortune -a | cowsay"]
