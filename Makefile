# version of the docker container:
DOCKER_VERSION = latest
# platform of the docker container:
DOCKER_PLATFORM = linux/amd64

bids:
	datalad get -J 8 input/bids/*/*/func/*events*.tsv
	
.PHONY: rest
rest:
	datalad get -J 8 input/decoding/sub-*/decoding/*scheme-7*_time_shift-4*decoding*
	datalad get -J 8 input/decoding/sub-*/decoding/*scheme-9*_time_shift-4*decoding*

# build docker container:
.PHONY: docker-build
docker-build:
	docker build --platform $(DOCKER_PLATFORM) -t lnnrtwttkhn/zoo-analysis:$(DOCKER_VERSION) .

# push the docker container to the registry:
.PHONY: docker-push
docker-push:
	docker push lnnrtwttkhn/zoo-analysis:$(DOCKER_VERSION)

# create an apptainer container from the docker image:
zoo-analysis_latest.sif:
	apptainer pull --force "zoo-analysis_latest.sif" docker://lnnrtwttkhn/zoo-analysis:latest

.PHONY: apptainer-shell
apptainer-shell:
	apptainer shell --contain --bind $(pwd):/mnt:rw zoo-analyis_$(DOCKER_VERSION).sif

.PHONY: slopes-hpc
slopes-hpc: code/decoding/zoo-analysis-decoding-slopes-hpc.R
	datalad unlock output/slopes && \
	Rscript --vanilla '$<'
