DOCKER_IMAGE=	checker_mqchecker
REGISTRY=	repo.treescale.com/bluectf/messagequeue-checker
TAG=		latest

.PHONY: publish
publish:
	docker login repo.treescale.com -u bluectf -p bluectf123
	@# You may want to comment out this command if the DOCKER_IMAGE value
	@# changed.
	docker rmi ${REGISTRY}:${TAG}
	docker rmi ${DOCKER_IMAGE}:${TAG}
	docker-compose build
	docker tag ${DOCKER_IMAGE}:${TAG} ${REGISTRY}:${TAG}
	docker push ${REGISTRY}:${TAG}
