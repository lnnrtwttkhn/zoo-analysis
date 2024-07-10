FROM r-base:4.3.1
WORKDIR /project

ENV RENV_CONFIG_AUTOLOADER_ENABLED FALSE
ENV RENV_AUTOLOAD_ENABLED FALSE

RUN apt-get update

RUN echo 'APT::Get::Install-Recommends "false";' >> /etc/apt/apt.conf

# external dependencies
RUN apt-get install -y pandoc cmake && apt-get clean

# install renv
# details: https://rstudio.github.io/renv/articles/docker.html#creating-docker-images-with-renv
ENV RENV_VERSION v1.0.7
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

# copy renv.lock file
COPY renv.lock renv.lock

# specify library paths for package installation
RUN mkdir -p renv
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# restore packages specified in the lockfile:
RUN R -e "renv::restore()"
