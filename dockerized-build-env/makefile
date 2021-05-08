# Want to build for Linux but you're on something else? No problem!
# Fire up Docker, then run `make docker-build` to produce a
# docker image of a build environment suitable for escapex
# (e.g. with g++ and SDL).
#
# Then fire it up interactively with `make linux-env`. You'll
# find the same filesystem available, but now your'e in Linux!
#

docker-build:
	docker build -f Dockerfile -t linux-build-env .

# Shell into it interactively
linux-env:
	docker-compose run -w /app dockerized-build
