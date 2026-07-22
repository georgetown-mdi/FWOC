############################################################
# OpenCommons R2 Downloader
#
# Downloads:
#   - CSV ZIP files
#   - CSV Agency files
#   - Parquet ZIP files
#   - Parquet Hive partitions
#   - Stata ZIP files
#   - Stata Agency files
#
# Uses OpenCommons as proxy layer.
#
############################################################


library(httr)
library(jsonlite)


############################################################
# CONFIGURATION
############################################################

OPENCOMMONS_URL <- "https://opencommons.mdi.georgetown.edu"

OUTPUT_DIR <- "opencommons_downloads"


############################################################
# DOWNLOAD OPTIONS
############################################################

# Download agency ZIP files
# FALSE = skip agency files (recommended default)
# TRUE  = discover and download all agency files
DOWNLOAD_AGENCY <- FALSE

# Hive partitions
# FALSE = skip Hive partitions (recommended default)
# TRUE  = download Hive partitions
DOWNLOAD_HIVE <- FALSE

# Hive partition scope
# TRUE  = download all Hive years/months
# FALSE = download only selected subset
DOWNLOAD_ALL_HIVE <- FALSE


if (DOWNLOAD_ALL_HIVE) {

  years_to_download <- 2012:2024
  months_to_download <- 1:12

} else {

  # Example subset (modify as needed)
  years_to_download <- c(2024)
  months_to_download <- c(1, 2)

}


############################################################
# CREATE OUTPUT DIRECTORY
############################################################

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


############################################################
# LIST OBJECTS FROM OPENCOMMONS
############################################################

list_objects <- function(prefix) {

  url <- paste0(
    OPENCOMMONS_URL,
    "/r2/list?prefix=",
    URLencode(prefix, reserved = TRUE)
  )

  response <- GET(url)

  stop_for_status(response)

  result <- fromJSON(
    content(
      response,
      as = "text",
      encoding = "UTF-8"
    ),
    simplifyDataFrame = TRUE
  )

  if (
    is.null(result$Contents) ||
    NROW(result$Contents) == 0
  ) {
    return(data.frame())
  }

  as.data.frame(
    result$Contents,
    stringsAsFactors = FALSE
  )
}


#################################################
# CONVERT OBJECT DATA FRAME TO LIST OF OBJECTS
#########################################

objects_to_list <- function(objects) {

  if (NROW(objects) == 0) {
    return(list())
  }

  lapply(
    seq_len(NROW(objects)),
    function(i) {
      as.list(objects[i, , drop = FALSE])
    }
  )
}


############################################################
# GET SINGLE OBJECT METADATA
############################################################

get_object <- function(key) {

  objects <- list_objects(key)

  if (
    NROW(objects) == 0 ||
    !"Key" %in% names(objects)
  ) {
    stop(
      paste(
        "Object not found:",
        key
      )
    )
  }

  match <- objects[
    objects$Key == key,
    ,
    drop = FALSE
  ]

  if (NROW(match) == 0) {
    stop(
      paste(
        "Exact object not found:",
        key
      )
    )
  }

  if (NROW(match) > 1) {
    warning(
      "Multiple objects matched; using the first match: ",
      key
    )
  }

  as.list(
    match[1, , drop = FALSE]
  )
}


############################################################
# DOWNLOAD OBJECT USING SIGNED URL
############################################################

download_object <- function(object) {

  key <- object$Key

  destination <- file.path(
    OUTPUT_DIR,
    key
  )

  dir.create(
    dirname(destination),
    recursive = TRUE,
    showWarnings = FALSE
  )

  if (file.exists(destination)) {

    message(
      "Skipping existing file: ",
      key
    )

    return(
      data.frame(
        file = key,
        status = "SKIPPED",
        stringsAsFactors = FALSE
      )
    )
  }

  message(
    "Downloading: ",
    key
  )

  result <- tryCatch({

    download.file(
      object$signedUrl,
      destination,
      method = "libcurl",
      mode = "wb"
    )

    data.frame(
      file = key,
      status = "SUCCESS",
      stringsAsFactors = FALSE
    )

  }, error = function(e) {

    data.frame(
      file = key,
      status = paste(
        "FAILED:",
        e$message
      ),
      stringsAsFactors = FALSE
    )

  })

  return(result)
}


############################################################
# BUILD DOWNLOAD LIST
############################################################

files <- list()


############################################################
# FIXED ZIP FILES
############################################################

fixed_files <- c(
  "csv/acc_csv.zip",
  "csv/sep_csv.zip",

  "parquet/zip/acc_hive.zip",
  "parquet/zip/emp_hive.zip",
  "parquet/zip/sep_hive.zip",

  "stata/acc_dta.zip",
  "stata/sep_dta.zip"
)


for (file in fixed_files) {

  files <- append(
    files,
    list(
      get_object(file)
    )
  )

}


############################################################
# AGENCY FILE DISCOVERY
############################################################

if (DOWNLOAD_AGENCY) {

  message(
    "Discovering CSV agency files..."
  )

  csv_agency <- list_objects(
    "csv/agency/"
  )

  message(
    "Discovering Stata agency files..."
  )

  stata_agency <- list_objects(
    "stata/agency/"
  )

  files <- c(
    files,
    objects_to_list(csv_agency),
    objects_to_list(stata_agency)
  )

}


############################################################
# HIVE PARTITION GENERATOR
############################################################

add_hive_partitions <- function(
    dataset,
    years,
    months
) {

  objects <- list()

  for (year in years) {

    for (month in months) {

      key <- sprintf(
        "parquet/hive/%s/year=%d/month=%d/part-0.parquet",
        dataset,
        year,
        month
      )

      objects <- append(
        objects,
        list(
          get_object(key)
        )
      )

    }

  }

  return(objects)
}


############################################################
# HIVE DOWNLOADS
############################################################

if (DOWNLOAD_HIVE) {

  files <- c(
    files,

    add_hive_partitions(
      "acc_hive",
      years_to_download,
      months_to_download
    ),

    add_hive_partitions(
      "emp_hive",
      years_to_download,
      months_to_download
    ),

    add_hive_partitions(
      "sep_hive",
      years_to_download,
      months_to_download
    )
  )

}


############################################################
# OPTIONAL SUBSET FILTERING
############################################################

# Example:
# Download only AF agency files:
#
# files <- files[
#   grepl(
#     "AF_",
#     vapply(
#       files,
#       function(x) x$Key,
#       character(1)
#     )
#   )
# ]


############################################################
# DOWNLOAD EVERYTHING
############################################################

results <- do.call(
  rbind,
  lapply(
    files,
    download_object
  )
)


############################################################
# DOWNLOAD LOG
############################################################

write.csv(
  results,
  "opencommons_download_log.csv",
  row.names = FALSE
)

message(
  "OpenCommons download completed."
)
