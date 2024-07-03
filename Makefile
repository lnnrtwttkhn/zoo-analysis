# version of the docker container:
DOCKER_VERSION = 0.1
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
zoo-analyis.sif:
	apptainer pull --force "zoo-analyis.sif" docker://lnnrtwttkhn/zoo-analysis:$(DOCKER_VERSION)

.PHONY: apptainer-shell
apptainer-shell:
	apptainer shell --contain --bind $(pwd):/mnt:rw zoo-analyis.sif
