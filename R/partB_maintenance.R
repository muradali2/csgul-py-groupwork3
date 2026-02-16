#part b

#misc
.stopf <- function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)

.assert_scalar_num <- function(x, name) {
  if (length(x) != 1 || is.na(x) || !is.numeric(x)) .stopf("%s must be a single numeric value", name)
  invisible(TRUE)
}

.assert_scalar_chr <- function(x, name) {
  if (length(x) != 1 || is.na(x) || !nzchar(trimws(x))) .stopf("%s must be a non-empty string", name)
  invisible(TRUE)
}

.assert_one_of <- function(x, allowed, name) {
  if (!(x %in% allowed)) .stopf("%s must be one of: %s", name, paste(allowed, collapse = ", "))
  invisible(TRUE)
}


#1. add new policyholder
add_policyholder <- function(policy.number,
                             first.name,
                             surname,
                             inception.year,
                             age.at.inception,
                             premium,
                             sum.assured,
                             term,
                             sex,
                             smoker.status,
                             ph_path = "pholders.csv") {

  # require utils
  if (!exists("read_pholders")) .stopf("Source R/utils_io.R before calling add_policyholder().")
  if (!exists("assert_has_cols")) .stopf("Source R/utils_validation.R before calling add_policyholder().")

  ph_path <- resolve_path(ph_path)
  df <- read_pholders(ph_path)
  assert_has_cols(df, PH_COLS)

  #basic checks
  .assert_scalar_chr(policy.number, "policy.number")
  .assert_scalar_chr(first.name, "first.name")
  .assert_scalar_chr(surname, "surname")
  .assert_scalar_num(inception.year, "inception.year")
  .assert_scalar_num(age.at.inception, "age.at.inception")
  .assert_scalar_num(premium, "premium")
  .assert_scalar_num(sum.assured, "sum.assured")
  .assert_scalar_num(term, "term")
  .assert_scalar_chr(sex, "sex")
  .assert_scalar_chr(smoker.status, "smoker.status")

  policy.number <- trimws(policy.number)
  first.name <- trimws(first.name)
  surname <- trimws(surname)
  sex <- trimws(sex)
  smoker.status <- trimws(smoker.status)

  #uniqueness
  if (policy.number %in% df[["policy.number"]]) .stopf("policy.number '%s' already exists", policy.number)

  # validity rules cwk
  if (age.at.inception < 65 || age.at.inception > 75) .stopf("age.at.inception must be between 65 and 75")
  if (term <= 0) .stopf("term must be positive")
  if (term > (90 - age.at.inception)) .stopf("term too long: must satisfy term <= 90 - age.at.inception")
  if (premium <= 0) .stopf("premium must be positive (you do not need to calculate it)")
  if (sum.assured <= 0) .stopf("sum.assured must be positive")

  .assert_one_of(sex, ALLOWED_SEX, "sex")
  .assert_one_of(smoker.status, ALLOWED_SMOKER, "smoker.status")

  new_row <- data.frame(
    policy.number    = policy.number,
    first.name       = first.name,
    surname          = surname,
    inception.year   = as.numeric(inception.year),
    age.at.inception = as.numeric(age.at.inception),
    premium          = as.numeric(premium),
    sum.assured      = as.numeric(sum.assured),
    term             = as.numeric(term),
    sex              = sex,
    smoker.status    = smoker.status,
    exit             = NA,
    year.of.exit     = NA,
    stringsAsFactors = FALSE
  )

  df2 <- rbind(df, new_row)
  write_pholders(df2, ph_path)
  invisible(df2)
}

#2. record deaths
record_deaths <- function(policy_numbers, year, ph_path = "pholders.csv") {
  if (!exists("read_pholders")) .stopf("Source R/utils_io.R before calling record_deaths().")
  if (!exists("is_active_start")) .stopf("Source R/utils_validation.R before calling record_deaths().")

  ph_path <- resolve_path(ph_path)
  df <- read_pholders(ph_path)
  assert_has_cols(df, PH_COLS)

  .assert_scalar_num(year, "year")
  policy_numbers <- trimws(as.character(policy_numbers))
  policy_numbers <- policy_numbers[nzchar(policy_numbers)]

  #check existence
  missing <- setdiff(policy_numbers, df[["policy.number"]])
  if (length(missing) > 0) .stopf("These policy numbers do not exist: %s", paste(missing, collapse = ", "))


  idx <- match(policy_numbers, df[["policy.number"]])
  active_ok <- is_active_start(df, year)[idx] & is.na(df[["exit"]][idx])

  if (any(!active_ok)) {
    bad <- policy_numbers[!active_ok]
    .stopf("These are not active policies at the start of %s (or already exited): %s", year, paste(bad, collapse = ", "))
  }

  df[["exit"]][idx] <- "Death"
  df[["year.of.exit"]][idx] <- as.numeric(year)

  write_pholders(df, ph_path)
  invisible(df)
}

#3. recording withdrawals
record_withdrawals <- function(policy_numbers, year, ph_path = "pholders.csv") {
  if (!exists("read_pholders")) .stopf("Source R/utils_io.R before calling record_withdrawals().")
  if (!exists("is_active_start")) .stopf("Source R/utils_validation.R before calling record_withdrawals().")

  ph_path <- resolve_path(ph_path)
  df <- read_pholders(ph_path)
  assert_has_cols(df, PH_COLS)

  .assert_scalar_num(year, "year")
  policy_numbers <- trimws(as.character(policy_numbers))
  policy_numbers <- policy_numbers[nzchar(policy_numbers)]

  missing <- setdiff(policy_numbers, df[["policy.number"]])
  if (length(missing) > 0) .stopf("These policy numbers do not exist: %s", paste(missing, collapse = ", "))

  idx <- match(policy_numbers, df[["policy.number"]])

  #stop withdraw
  active_ok <- is_active_start(df, year)[idx] & is.na(df[["exit"]][idx])

  if (any(!active_ok)) {
    bad <- policy_numbers[!active_ok]
    .stopf("These are not eligible to withdraw in %s (not active at start or already exited): %s", year, paste(bad, collapse = ", "))
  }

  df[["exit"]][idx] <- "Withdrawl"   # spelling matches data file
  df[["year.of.exit"]][idx] <- as.numeric(year)

  write_pholders(df, ph_path)
  invisible(df)
}

#4. close mature plicy
close_matured_policies <- function(year, ph_path = "pholders.csv") {
  if (!exists("read_pholders")) .stopf("Source R/utils_io.R before calling close_matured_policies().")
  if (!exists("end_year_vec")) .stopf("Source R/utils_validation.R before calling close_matured_policies().")

  ph_path <- resolve_path(ph_path)
  df <- read_pholders(ph_path)
  assert_has_cols(df, PH_COLS)

  .assert_scalar_num(year, "year")

  end_year <- end_year_vec(df)

  #only active and this year end
  idx <- which(is.na(df[["exit"]]) & !is.na(end_year) & end_year == year)

  if (length(idx) > 0) {
    df[["exit"]][idx] <- "End"
    df[["year.of.exit"]][idx] <- as.numeric(year)
    write_pholders(df, ph_path)
  }

  invisible(df)
}