#shared i/o

PH_COLS <- c(
  "policy.number","first.name","surname","inception.year","age.at.inception",
  "premium","sum.assured","term","sex","smoker.status","exit","year.of.exit"
)

resolve_path <- function(path) {
  if (file.exists(path)) return(path)

  alt <- file.path("data", path)
  if (file.exists(alt)) return(alt)

  stop(sprintf("File not found: '%s' (also tried '%s')", path, alt), call. = FALSE)
}

read_pholders <- function(path) {
  path <- resolve_path(path)

  df <- read.csv(
    path,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA"),
    check.names = FALSE
  )

  #make sure no confusion with files
  names(df) <- trimws(names(df))

  #trim strings field
  str_cols <- intersect(c("policy.number","first.name","surname","sex","smoker.status","exit"), names(df))
  for (cc in str_cols) {
    df[[cc]] <- trimws(as.character(df[[cc]]))
    df[[cc]][df[[cc]] == ""] <- NA
  }

  #numeric columns
  num_cols <- intersect(c("inception.year","age.at.inception","premium","sum.assured","term","year.of.exit"), names(df))
  for (cc in num_cols) {
    df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
  }

  df
}

write_pholders <- function(df, path) {
  write.csv(df, path, row.names = FALSE, na = "")
  invisible(TRUE)
}

write_csv_nice <- function(df, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write.csv(df, path, row.names = FALSE, na = "")
  invisible(TRUE)
}
