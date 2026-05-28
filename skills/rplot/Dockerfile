FROM r-base:4.4.1

ENV R_REPOS=https://cloud.r-project.org

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates \
    curl \
    gfortran \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libssl-dev \
    libtiff-dev \
    libxml2-dev \
    libxt6 \
    zlib1g-dev \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /skills/rplot

COPY scripts/install_deps.R /tmp/install_deps.R
RUN Rscript /tmp/install_deps.R --skip-local-package

COPY package/codex.rplot /tmp/codex.rplot
RUN R CMD INSTALL /tmp/codex.rplot && rm -rf /tmp/codex.rplot

COPY . /skills/rplot
