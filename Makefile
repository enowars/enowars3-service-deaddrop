SERVICE_DIR=	./service
CHECKER_CMD=	python3 ./checker/checker.py

FLAG=	ENO6QMAAAeETi6mGPeJgd83rWfM2U3bcg8KZLsICovytDw=
NOISE=	=This=Is=Neither=A=Flag=Nor=A=Love=Song===31337

# Service up.
.PHONY: su
su:
	${MAKE} -C ${SERVICE_DIR} up

# Service down.
.PHONY: sd
sd:
	${MAKE} -C "${SERVICE_DIR}" down

.PHONY: test
test:
	${MAKE} sd
	${MAKE} su

	sleep 1

	${CHECKER_CMD} run havoc
	${CHECKER_CMD} run putflag --flag "${FLAG}"
	${CHECKER_CMD} run getflag --flag "${FLAG}"
	${CHECKER_CMD} run putnoise --flag "${NOISE}"
	${CHECKER_CMD} run getnoise --flag "${NOISE}"

	${MAKE} sd

RELEASE_BRANCH=	release
REPOSITORY_URL=	ssh://git@git.eno.host:7999/blue/service-messagequeue.git
RELEASE_DIR=	./new-release
.PHONY: update-release-branch
update-release-branch:
	git clone ${REPOSITORY_URL} ${RELEASE_DIR}
	cd -- ${RELEASE_DIR}
	git checkout -b ${RELEASE_BRANCH}
	git rm -r .

	# Docker files
	git checkout master ${SERVICE_DIR}/Dockerfile
	git checkout master ${SERVICE_DIR}/docker-compose.yml
	# Build infrastructure.
	git checkout master ${SERVICE_DIR}/Makefile
	git checkout master ${SERVICE_DIR}/config
	git checkout master ${SERVICE_DIR}/erlang.mk
	git checkout master ${SERVICE_DIR}/relx.config
	# Source of the service
	git checkout master ${SERVICE_DIR}/src

	# QA
	${MAKE} -C ${SERVICE_DIR} up
	${MAKE} -C ${SERVICE_DIR} down

	# Release
	git commit --message="Release the service"
	git push --set-upstream origin ${RELEASE_BRANCH}

	# Clean up
	cd ..
	rm -rf -- ${RELEASE_DIR}

.PHONY: ultra-clean
ultra-clean:
	find . -name '*.log' -delete
	find . -name '*.core' -delete
	${MAKE} -C service distclean
	rm -rf -- .data
	rm -rf -- checker/.data
	rm -rf -- ${RELEASE_DIR}
