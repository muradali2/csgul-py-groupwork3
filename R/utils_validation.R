#shared defs

ALLOWED_SEX    <- c("Male","Female")
ALLOWED_SMOKER <- c("Non-smoker","Smoker","Ex-smoker")
ALLOWED_EXIT   <- c("Death","Withdrawl","End")

stopf <- function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)

assert_has_cols <- function(df, cols) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) stopf("Missing columns: %s", paste(missing, collapse = ", "))
  invisible(TRUE)
}

end_year_vec <- function(df) {
  df[["inception.year"]] + df[["term"]] - 1
}