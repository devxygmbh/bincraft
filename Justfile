image OS ARCH:
    if echo "{{OS}}" | grep -qi "redhat"; then \
        docker buildx build --progress plain --build-arg ARCH={{ARCH}} --build-arg GITHUB_PAT="$GITHUB_PAT" --build-arg RED_HAT_DEV_PW="$RED_HAT_DEV_PW" --platform linux/{{ARCH}} -f docker/Containerfile-{{OS}} -t devxygmbh/{{ARCH}}-binaries-r-{{OS}}:latest --push .; \
    else \
        docker buildx build --progress plain --build-arg GITHUB_PAT="$GITHUB_PAT" --build-arg ARCH={{ARCH}} -f docker/Containerfile-{{OS}} --platform linux/{{ARCH}} -t devxygmbh/{{ARCH}}-binaries-r-{{OS}}:latest --push .; \
    fi

# just build-all ubuntu-2404 noble arm64 later 1
# just build-all ubuntu-2204 jammy arm64 webshot 1
# just build-all alpine-320 alpine320 arm64 A3 1
# just build-all redhat-8 rhel8 arm64 httpuv 1
# just build-all redhat-9 rhel9 arm64 httpuv 1
build-all OS OS_ID ARCH PACKAGE NCPUS:
  docker run --rm -it --platform linux/{{ARCH}} -v ./:/package -e AWS_ACCESS_KEY_ID="$AWS_ACCESS_KEY_ID" -e AWS_SECRET_ACCESS_KEY="$AWS_SECRET_ACCESS_KEY" -e PGPASS="$PGPASS" -e NCPUS={{NCPUS}} --pull=always devxygmbh/{{ARCH}}-binaries-r-{{OS}}:latest bash -c 'R -q -e "install.packages(\"pak\", repos = sprintf(\"https://r-lib.github.io/p/pak/devel/%s/%s/%s\", .Platform\$pkgType, R.Version()\$os, R.Version()\$arch))" && cd package && R CMD INSTALL . && R -q -e "options(progressr.enable = TRUE, repos = structure(c(CRAN = \"https://cloud.r-project.org\"))); progressr::handlers(global = TRUE); bincraftR::build_binary_package(\"{{PACKAGE}}\", platform = \"{{OS}}\", debug=TRUE, force=TRUE, deps_verbose = TRUE)"'

# just build-single ubuntu-2204 jammy arm64 MPCR 1.1.3 1
# just build-single ubuntu-2404 noble arm64 webshot2 latest 1
# just build-single alpine-320 alpine320 arm64 odbc 1.5.0 1
build-single OS OS_ID ARCH PACKAGE TAG NCPUS:
  docker run --rm -it --platform linux/{{ARCH}} -v ./:/package -e AWS_ACCESS_KEY_ID="$AWS_ACCESS_KEY_ID" -e AWS_SECRET_ACCESS_KEY="$AWS_SECRET_ACCESS_KEY" -e PGPASS="$PGPASS" -e NCPUS={{NCPUS}} --pull=always devxygmbh/{{ARCH}}-binaries-r-{{OS}}:latest bash -c 'cd package && R CMD INSTALL . && R -q -e "install.packages(\"pak\", repos = sprintf(\"https://r-lib.github.io/p/pak/devel/%s/%s/%s\", .Platform\$pkgType, R.Version()\$os, R.Version()\$arch))" && R -q -e "options(progressr.enable = TRUE, repos = structure(c(CRAN = \"https://cloud.r-project.org\")));progressr::handlers(global = TRUE); bincraftR::build_binary_package(\"{{PACKAGE}}\", tag = \"{{TAG}}\", platform = \"{{OS}}\", debug=TRUE, force=TRUE, deps_verbose = TRUE)"'

# just process-updates jammy amd64 lubridate::interval(lubridate::today() - 2, lubridate::today() - 20)
# just process-updates redhat-9 arm64 'lubridate::interval(lubridate::today() - 4, lubridate::today() - 4)'
process-updates OS ARCH interval:
  docker run --rm -it --platform linux/{{ARCH}} -e AWS_ACCESS_KEY_ID="$AWS_ACCESS_KEY_ID" -e AWS_SECRET_ACCESS_KEY="$AWS_SECRET_ACCESS_KEY" -e PGPASS="$PGPASS" --pull=always devxygmbh/{{ARCH}}-binaries-r-{{OS}}:latest R -q -e "options(progressr.enable = TRUE);progressr::handlers(global = TRUE); bincraftR::process_cran_updates(interval = {{interval}}, platform = \"{{OS}}\")"