SERVICE_DIR=	./service
TEST_TEAMID=	eeee
TEST_SERVICE_IP_ADDRESS=	[fd00:1337:${TEST_TEAMID}:ba17::1]
CHECKER_CMD=	python3 ./checker/checker.py run -a "${TEST_SERVICE_IP_ADDRESS}"

FLAG=	ENO6QMAAAeETi6mGPeJgd83rWfM2U3bcg8KZLsICovytDw=
NOISE=	=This=Is=Neither=A=Flag=Nor=A=Love=Song===31337

${SERVICE_DIR}/docker-compose.yml: ${SERVICE_DIR}/docker-compose.yml.template
	sed 's/TEAMID/${TEST_TEAMID}/g' ${SERVICE_DIR}/docker-compose.yml.template > ${SERVICE_DIR}/docker-compose.yml

.PHONY: se
se:
	docker exec -it service_queue_1 sh

.PHONY: sa
sa:
	docker attach service_queue_1

# Service up.
.PHONY: su
su: ${SERVICE_DIR}/docker-compose.yml
	${MAKE} -C ${SERVICE_DIR} up

# Service down.
.PHONY: sd
sd: ${SERVICE_DIR}/docker-compose.yml
	${MAKE} -C "${SERVICE_DIR}" down

TEST_DELAY=	2

.PHONY: test-setup
test-setup:
	sed 's/TEAMID/${TEST_TEAMID}/g' ${SERVICE_DIR}/docker-compose.yml.template > ${SERVICE_DIR}/docker-compose.yml
	${MAKE} sd
	${MAKE} su

.PHONY: test-teardown
test-teardown:
	${MAKE} sd

.PHONY: do-test-smoke
do-test-smoke:
	${CHECKER_CMD} havoc
	${CHECKER_CMD} putflag --flag "${FLAG}"
	${CHECKER_CMD} getflag --flag "${FLAG}"
	${CHECKER_CMD} putnoise --flag "${NOISE}"
	${CHECKER_CMD} getnoise --flag "${NOISE}"
	${CHECKER_CMD} havoc

.PHONY: test-smoke
test-smoke:
	${MAKE} test-setup
	sleep ${TEST_DELAY}
	${MAKE} do-test-smoke
	${MAKE} test-teardown

.PHONY: do-test-exploit
do-test-exploit:
	${CHECKER_CMD} putflag --flag "${FLAG}"
	${CHECKER_CMD} putnoise --flag "${NOISE}"
	${CHECKER_CMD} exploit --flag "${FLAG}"

.PHONY: test-exploit
test-exploit:
	${MAKE} test-setup
	sleep ${TEST_DELAY}
	${MAKE} do-test-exploit
	${MAKE} test-teardown

.PHONY: test
test:
	${MAKE} test-smoke
	${MAKE} test-exploit

RELEASE_BRANCH=	release
REPOSITORY_URL=	ssh://git@git.eno.host:7999/blue/service-deaddrop.git
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
	git checkout --force master service/docker-compose.yml.template
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

VERSION_FORMAT=	[0-9][0-9]*.[0-9][0-9]*.[0-9][0-9]*
.PHONY: release-tag
release-tag:
	${MAKE} ${RELEASE_DIR} -f ${PWD}/Makefile do-release-tag

.PHONY: do-release-tag
do-release-tag:
	@echo "Checking if new version matches \"${VERSION_FORMAT}\"..."
	@expr "${VERSION}" : '${VERSION_FORMAT}' >/dev/null || \
		{ echo "ERROR: Version \"${VERSION}\" does not match \"${VERSION_FORMAT}\"..." >&2 && false; }
	@echo "Checking if the new version is unique..."
	@git tag | grep -o -v "${VERSION}" >/dev/null || \
		{ echo "ERROR: Version \"${VERSION}\" is not unique..." >&2 && false; }
	@echo "Checking if the new version is higher than previous versions..."
	@{ git tag; echo "${VERSION}"; } | grep -o '${VERSION_FORMAT}' | sort | tail -n 1 | grep '${VERSION}' >/dev/null || \
		{ echo "ERROR: Version \"${VERSION}\" is not higher than previous versions..." >&2 && false; }
	git tag ${VERSION}
	git push --tags

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

.PHONY: release
release: release-clean
	${MAKE} release-clone
	${MAKE} release-update
	${MAKE} release-push
	${MAKE} release-tag
	${MAKE} release-clean

CLIENT=	./scripts/client
.PHONY: debug-restart-and-watch
debug-restart-and-watch:
	${MAKE} sd
	${MAKE} su
	sleep 2

	${CLIENT} a '- priv'
	${CLIENT} a pub
	${CLIENT} p '- priv' 'private message 1'
	${CLIENT} p 'pub' 'public message 1'

	${MAKE} sa

.PHONY: sync-release-to-master
sync-release-to-master:
	git checkout master
	git ls-tree -r origin/release --name-only | xargs -n 1 git checkout --force origin/release
	env PAGER=cat git diff --staged

.PHONY: clean
clean:
	find . -name '*.log' -delete
	find . -name '*.core' -delete

.PHONY: ultra-clean
ultra-clean:
	${MAKE} clean
	${MAKE} -C service distclean
	rm -rf -- .data
	rm -rf -- checker/.data
	rm -rf -- ${RELEASE_DIR}
