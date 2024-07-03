# Project <insert name>

[![Codespell](https://github.com/lnnrtwttkhn/zoo-analysis/actions/workflows/codespell.yml/badge.svg)](https://github.com/lnnrtwttkhn/zoo-analysis/actions/workflows/codespell.yml)
[![Docker](https://img.shields.io/badge/docker-%230db7ed.svg?style=for-the-badge&logo=docker&logoColor=white)](https://hub.docker.com/repository/docker/lnnrtwttkhn/zoo-analysis/)

## Dataset structure

- All inputs (i.e. building blocks from other sources) are located in
  `inputs/`.
- All custom code is located in `code/`.

## Requirements

### Docker

The recipe for the Docker container is specified in the [Dockerfile](Dockerfile).
The container can be build by running `make docker-build` (see [Makefile](Makefile)).
The container can be pushed to the [container registry](https://git.mpib-berlin.mpg.de/wittkuhn/zoo-modeling/container_registry) by running `make docker-push` (see [Makefile](Makefile)).

To get the required R packages out of the `renv` environment, the following command is helpful:

```R
unique(renv::dependencies()$Package)
```

### Apptainer

On high-performance computing (HPC) clusters we can not use Docker, so we need to create an [Apptainer](https://apptainer.org/) (formerly known as "Singularity").
On an HPC cluster, an Apptainer container can be build from the Dockerfile by running `make zoo-analysis.sif` (for details, see the [Makefile](Makefile)).
This will generate an Apptainer container called `zoo-analysis.sif` that can then be used on the HPC cluster.
The command basically pulls the Docker container from the container registry and turns it into an Apptainer container called `zoo-analysis.sif`.
