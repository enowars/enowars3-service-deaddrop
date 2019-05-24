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

.PHONY: release-clone
release-clone:
	git clone ${REPOSITORY_URL} ${RELEASE_DIR}

.PHONY: release-update
release-update:
	${MAKE} -C ${RELEASE_DIR} -f ${PWD}/Makefile do-release-update

.PHONY: do-release-update
do-release-update:
	git checkout ${RELEASE_BRANCH}
	git rm -r .

	# Service ####################################################################
	# Docker files
	git checkout --force master service/Dockerfile
	git checkout --force master service/docker-compose.yml
	mv service/docker-compose.yml service/docker-compose.yml.template
	git rm service/docker-compose.yml
	git add service/docker-compose.yml.template
	# Build infrastructure
	git checkout --force master ${SERVICE_DIR}/Makefile
	git checkout --force master ${SERVICE_DIR}/config
	git checkout --force master ${SERVICE_DIR}/erlang.mk
	git checkout --force master ${SERVICE_DIR}/relx.config
	# Source of the service
	git checkout --force master ${SERVICE_DIR}/src

	# Checker ####################################################################
	git checkout --force master checker/Dockerfile
	git checkout --force master checker/checker.py
	git checkout --force master checker/docker-compose.yml
	git checkout --force master checker/nginx.conf
	git checkout --force master checker/requirements.txt
	git checkout --force master checker/uwsgi.ini

	env PAGER=cat git diff --staged
	git status

.PHONY: release-qa
release-qa:
	git checkout ${RELEASE_BRANCH}
	sed -i 's/TEAMID/eeee/g' ${SERVICE_DIR}/docker-compose.yml
	${MAKE} -C ${SERVICE_DIR} up
	${MAKE} -C ${SERVICE_DIR} down
	git checkout -- ${SERVICE_DIR}/docker-compose.yml

.PHONY: release-push
release-push:
	${MAKE} MASTER_HASH="$$(git log -n 1 --pretty=format:"%H")" -C ${RELEASE_DIR} -f ${PWD}/Makefile do-release-push

.PHONY: do-release-push
do-release-push:
	git commit --message="Release the service (based on ${MASTER_HASH})"
	git push --set-upstream origin ${RELEASE_BRANCH}

.PHONY: release-clean
release-clean:
	rm -rf -- ${RELEASE_DIR}

.PHONY: sync-release-to-master
sync-release-to-master:
	git checkout master
	git ls-tree -r origin/release --name-only | xargs -n 1 git checkout --force origin/release
	env PAGER=cat git diff --staged

.PHONY: ultra-clean
ultra-clean:
	find . -name '*.log' -delete
	find . -name '*.core' -delete
	${MAKE} -C service distclean
	rm -rf -- .data
	rm -rf -- checker/.data
	rm -rf -- ${RELEASE_DIR}
