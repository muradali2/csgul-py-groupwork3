#part a

detect_errors_partA <- function(raw_path,
                                asof_year = 2026,
                                out_path = "output/errors.csv") {

  if (!exists("read_pholders")) stop("Source R/utils_io.R before calling detect_errors_partA().", call. = FALSE)
  if (!exists("assert_has_cols")) stop("Source R/utils_validation.R before calling detect_errors_partA().", call. = FALSE)

  df <- read_pholders(raw_path)
  assert_has_cols(df, PH_COLS)

  end_year <- end_year_vec(df)

  err_list <- list()
  push_err <- function(idx, field, type, msg) {
    if (length(idx) == 0) return(invisible(NULL))
    if (length(msg) == 1) msg <- rep(msg, length(idx))
    err_list[[length(err_list) + 1]] <<- data.frame(
      policy.number = df[["policy.number"]][idx],
      field         = field,
      error.type    = type,
      message       = msg,
      stringsAsFactors = FALSE
    )
  }

  #misc errors
  #missing required fields
  required <- c("policy.number","first.name","surname","inception.year","age.at.inception","term","sex","smoker.status")
  for (cc in required) {
    push_err(which(is.na(df[[cc]])), cc, "missing", paste0(cc, " is blank"))
  }

  #duplicate nums
  dup <- which(!is.na(df[["policy.number"]]) & duplicated(df[["policy.number"]]))
  push_err(dup, "policy.number", "duplicate", "Duplicate policy.number appears in file")

  #cwk rules
  #when inception year after as of year
  push_err(
    which(!is.na(df[["inception.year"]]) & df[["inception.year"]] > asof_year),
    "inception.year",
    "future_inception",
    paste0("As data is from ", asof_year, ", cannot have a policy incepting after ", asof_year)
  )

  #age must be 65–75
  push_err(
    which(!is.na(df[["age.at.inception"]]) &
            (df[["age.at.inception"]] < 65 | df[["age.at.inception"]] > 75)),
    "age.at.inception",
    "age_out_of_range",
    "Too old/young at inception: age.at.inception must be between 65 and 75"
  )

  #term must not take beyond 90th year
  push_err(
    which(!is.na(df[["term"]]) & !is.na(df[["age.at.inception"]]) &
            df[["term"]] > (90 - df[["age.at.inception"]])),
    "term",
    "term_too_long",
    "Term too long as it takes them beyond 90"
  )

  #smoking status valid
  push_err(
    which(!is.na(df[["smoker.status"]]) & !(df[["smoker.status"]] %in% ALLOWED_SMOKER)),
    "smoker.status",
    "invalid_value",
    paste0("The smoking status is not valid (allowed: ", paste(ALLOWED_SMOKER, collapse = ", "), ")")
  )

  #sex valid
  push_err(
    which(!is.na(df[["sex"]]) & !(df[["sex"]] %in% ALLOWED_SEX)),
    "sex",
    "invalid_value",
    paste0("Invalid sex (allowed: ", paste(ALLOWED_SEX, collapse = ", "), ")")
  )

  #exit/year consistency
  exit_blank  <- is.na(df[["exit"]])
  yexit_blank <- is.na(df[["year.of.exit"]])

  push_err(which(exit_blank & !yexit_blank), "year.of.exit", "should_be_blank", "year.of.exit present but exit is blank")
  push_err(which(!exit_blank & yexit_blank), "year.of.exit", "missing", "exit is present but year.of.exit is blank")

  #exit value valid
  push_err(
    which(!exit_blank & !(df[["exit"]] %in% ALLOWED_EXIT)),
    "exit",
    "invalid_value",
    paste0("Exit must be one of: ", paste(ALLOWED_EXIT, collapse = ", "))
  )

  #no exits in as of year or later
  push_err(
    which(!yexit_blank & df[["year.of.exit"]] >= asof_year),
    "year.of.exit",
    "exit_in_asof_year_or_later",
    paste0("Can't have an exit in ", asof_year, " or later (file created on 01/01/26)")
  )

  #exit year before inception
  push_err(
    which(!yexit_blank & !is.na(df[["inception.year"]]) & df[["year.of.exit"]] < df[["inception.year"]]),
    "year.of.exit",
    "before_inception",
    "Policy ending before it is incepted"
  )

  #should have 'End' if exit is blank but term already elapsed
  push_err(
    which(exit_blank & !is.na(end_year) & end_year <= (asof_year - 1)),
    "exit",
    "missing_exit",
    "Exit is blank but policy term has ended before the as-of date"
  )

  push_err(
    which(df[["exit"]] == "End" &
            !is.na(df[["year.of.exit"]]) & !is.na(end_year) &
            df[["year.of.exit"]] != end_year),
    "year.of.exit",
    "end_year_mismatch",
    "Can't have come to the end of the policy (exit='End') because year.of.exit doesn't match inception.year + term - 1"
  )

  #death/Withdrawl shouldnt happen after contract ended
  push_err(
    which(df[["exit"]] %in% c("Death","Withdrawl") &
            !is.na(df[["year.of.exit"]]) & !is.na(end_year) &
            df[["year.of.exit"]] > end_year),
    "year.of.exit",
    "after_term_end",
    "Exit occurs after the policy term ended"
  )

  errors <- if (length(err_list) == 0) {
    data.frame(policy.number=character(), field=character(), error.type=character(), message=character(),
               stringsAsFactors = FALSE)
  } else {
    errors <- do.call(rbind, err_list)
    errors <- errors[order(errors$policy.number, errors$field), ]
    errors
  }

  write_csv_nice(errors, out_path)
  invisible(errors)
}
