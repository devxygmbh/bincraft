test_that("build_single_tag clones the ref and builds the version it names", {
  skip_if_not_installed("mockery")

  binary_output_path <- withr::local_tempdir()
  local_clone_dir <- withr::local_tempdir()
  cloned_ref <- NULL
  output_tag <- NULL

  mockery::stub(
    build_single_tag,
    "check_build_skip_conditions",
    function(...) list(should_skip = FALSE)
  )
  mockery::stub(
    build_single_tag,
    "clone_repository",
    function(package_name, tag, source_org_url, local_clone_dir_single) {
      cloned_ref <<- tag
      dir.create(local_clone_dir_single, recursive = TRUE)
      writeLines(
        c("Package: bincraft", "Version: 4.5.2.9999"),
        file.path(local_clone_dir_single, "DESCRIPTION")
      )
    }
  )
  mockery::stub(
    build_single_tag,
    "execute_package_build",
    function(...) list(success = TRUE, build_time = 0L)
  )
  mockery::stub(
    build_single_tag,
    "handle_build_output_files",
    function(package_name, tag, ...) {
      output_tag <<- tag
      list(file_exists = TRUE, file_size = 1L)
    }
  )

  build_single_tag(
    package_name = "bincraft",
    platform = "alpine-323",
    arch = "amd64",
    binary_output_path = binary_output_path,
    local_clone_dir = local_clone_dir,
    source_org_url = "https://codefloe.com/rpkgs",
    tag = "v5.0.0",
    install_system_dependencies = FALSE
  )

  # The clone needs the ref verbatim; everything downstream needs the version.
  expect_identical(cloned_ref, "v5.0.0")
  expect_identical(output_tag, "5.0.0")
  expect_identical(
    readLines(file.path(local_clone_dir, "bincraft_5.0.0", "DESCRIPTION")),
    c("Package: bincraft", "Version: 5.0.0")
  )
})

test_that("build_single_tag leaves DESCRIPTION alone for a ref naming no version", {
  skip_if_not_installed("mockery")

  binary_output_path <- withr::local_tempdir()
  local_clone_dir <- withr::local_tempdir()
  desc <- c("Package: bincraft", "Version: 4.5.2.9999")

  mockery::stub(
    build_single_tag,
    "check_build_skip_conditions",
    function(...) list(should_skip = FALSE)
  )
  mockery::stub(
    build_single_tag,
    "clone_repository",
    function(package_name, tag, source_org_url, local_clone_dir_single) {
      dir.create(local_clone_dir_single, recursive = TRUE)
      writeLines(desc, file.path(local_clone_dir_single, "DESCRIPTION"))
    }
  )
  mockery::stub(
    build_single_tag,
    "execute_package_build",
    function(...) list(success = TRUE, build_time = 0L)
  )
  mockery::stub(
    build_single_tag,
    "handle_build_output_files",
    function(...) list(file_exists = TRUE, file_size = 1L)
  )

  build_single_tag(
    package_name = "bincraft",
    platform = "alpine-323",
    arch = "amd64",
    binary_output_path = binary_output_path,
    local_clone_dir = local_clone_dir,
    source_org_url = "https://codefloe.com/rpkgs",
    tag = "main",
    install_system_dependencies = FALSE
  )

  expect_identical(
    readLines(file.path(local_clone_dir, "bincraft_main", "DESCRIPTION")),
    desc
  )
})
