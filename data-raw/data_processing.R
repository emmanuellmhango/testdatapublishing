# Description ------------------------------------------------------------------
# R script to process uploaded raw data into a tidy, analysis-ready data frame
# Load packages ----------------------------------------------------------------
## Run the following code in console if you don't have the packages
## install.packages(c("usethis", "fs", "here", "readr", "readxl", "openxlsx"))
library(usethis)
library(fs)
library(here)
library(readr)
library(readxl)
library(openxlsx)

# Read data --------------------------------------------------------------------
data_in <- readr::read_csv("data-raw/testdatapublishing.csv")
# codebook <- readxl::read_excel("data-raw/codebook.xlsx") |>
# clean_names()

# Tidy data --------------------------------------------------------------------
## Clean the raw data into a tidy format here
testdatapublishing[] <- lapply(data_in,function(x) {
  if (is.character(x)) {
    stringi::stri_enc_toutf8(x)
  } else {
    x
  }
})

check_utf8 <- function(df) {
  invalid_cols <- sapply(df, function(column) {
    if (!is.character(column)) return(FALSE) # Only check character columns
    any(sapply(column, function(x) {
      if (is.na(x)) return(FALSE) # Ignore NA values
      tryCatch({
        iconv(x, from = "UTF-8", to = "UTF-8", sub = "byte") != x
      }, error = function(e) TRUE) # Treat errors as non-UTF-8
    }))
  })

  # Get column names with issues
  bad_cols <- names(df)[invalid_cols]

  if (length(bad_cols) > 0) {
    cat("Non-UTF characters detected in columns:", paste(bad_cols, collapse = ", "), "\n")
  } else {
    cat("No non-UTF characters found.\n")
  }
}
testdatapublishing$country <- iconv(testdatapublishing$country, from = "UTF-8", to = "UTF-8", sub = "")

# Run check
check_utf8(testdatapublishing)

# Export Data ------------------------------------------------------------------
usethis::use_data(testdatapublishing, overwrite = TRUE)
fs::dir_create(here::here("inst", "extdata"))
readr::write_csv(testdatapublishing,
                 here::here("inst", "extdata", paste0("testdatapublishing", ".csv")))
openxlsx::write.xlsx(testdatapublishing,
                     here::here("inst", "extdata", paste0("testdatapublishing", ".xlsx")))
