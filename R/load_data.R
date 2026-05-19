load_bcr_data <- function(
) {
  etl_dir <- Sys.getenv(
    "FAMCARE_ETL_GOVERNED"
  )
  
  if (
    etl_dir == ""
  ) {
    stop(
      "Environment variable FAMCARE_ETL_GOVERNED is not set.",
      "Please define it in your .Renviron."
    )
  }
  
  # Create a dedicated environment for setup.R
  setup_env <- new.env(
    parent = globalenv()
  )
  setup_env$etl_dir <- etl_dir
  
  # Source setup.R into that environment
  sys.source(
    file.path(
      etl_dir,
      "etl",
      "setup.R"
    ),
    envir = setup_env
  )
  
  # Source bcr.R into the same environment
  sys.source(
    file.path(
      etl_dir,
      "etl",
      "bcr.R"
    ),
    envir = setup_env
  )
  
  # Now run the ETL from that environment
  setup_env$run_bcr_etl(
    setup_env$bcr_paths
  )
}