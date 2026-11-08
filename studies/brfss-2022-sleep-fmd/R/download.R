# Fetch the official LLCP2022 transport file and refuse to continue if the
# bytes are not the object recorded for this study.

sha256_file <- function(path) {
  out <- system2("sha256sum", shQuote(path), stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    stop("sha256sum failed for ", path)
  }
  sub("[[:space:]].*$", "", out[[1]])
}

download_llcp <- function(dest_zip) {
  dir.create(dirname(dest_zip), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(dest_zip) && sha256_file(dest_zip) == BRFSS_SOURCE$zip_sha256) {
    return(list(
      path = dest_zip,
      sha256 = BRFSS_SOURCE$zip_sha256,
      bytes = file.info(dest_zip)$size,
      retrieval = "cached file matched the pinned SHA-256",
      verified_at_utc = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
    ))
  }
  message("Downloading LLCP2022 XPT zip from CDC.")
  headers <- c("User-Agent" = BRFSS_SOURCE$user_agent)
  ok <- try(
    download.file(
      BRFSS_SOURCE$data_url,
      dest_zip,
      method = "libcurl",
      mode = "wb",
      quiet = TRUE,
      headers = headers
    ),
    silent = TRUE
  )
  if (inherits(ok, "try-error") || !file.exists(dest_zip)) {
    stop(
      "Could not download the BRFSS file from the official URL. ",
      "No estimates will be produced. ",
      if (inherits(ok, "try-error")) conditionMessage(attr(ok, "condition")) else ""
    )
  }
  got <- sha256_file(dest_zip)
  if (!identical(got, BRFSS_SOURCE$zip_sha256)) {
    stop(
      "Downloaded SHA-256 does not match the pinned hash.\n  got: ", got,
      "\n  expected: ", BRFSS_SOURCE$zip_sha256,
      "\nThe file was left in place and will not be analyzed."
    )
  }
  list(
    path = dest_zip,
    sha256 = got,
    bytes = file.info(dest_zip)$size,
    retrieval = "downloaded from the official URL in this run",
    verified_at_utc = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  )
}

unzip_llcp <- function(zip_path, dest_dir) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  listing <- unzip(zip_path, list = TRUE)
  if (nrow(listing) != 1) {
    stop("Expected one member in the XPT zip, found ", nrow(listing))
  }
  member <- listing$Name[[1]]
  unzip(zip_path, exdir = dest_dir)
  extracted <- file.path(dest_dir, member)
  if (!file.exists(extracted)) {
    # Some unzip builds trim the trailing space CDC put on the member name.
    alt <- list.files(dest_dir, pattern = "LLCP2022", full.names = TRUE)
    if (length(alt) != 1) stop("Could not find the extracted transport file.")
    extracted <- alt[[1]]
  }
  clean <- file.path(dest_dir, "LLCP2022.XPT")
  if (!identical(normalizePath(extracted, mustWork = FALSE), normalizePath(clean, mustWork = FALSE))) {
    if (file.exists(clean)) file.remove(clean)
    if (!file.rename(extracted, clean)) stop("Could not rename the transport file.")
  }
  list(
    member_name = member,
    member_name_repr = encodeString(member),
    path = clean,
    bytes = file.info(clean)$size,
    sha256 = sha256_file(clean)
  )
}
