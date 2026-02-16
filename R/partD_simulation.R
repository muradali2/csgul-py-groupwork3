
# Part D
#Simulation of deaths
simulate_deaths <- function(year,
                            ph_path = "pholders.csv",
                            mort_path = "mortrates.csv",
                            srates_path = "srates.csv",
                            seed = NULL) {

  if (!exists("read_pholders")) stop("Source utils_io.R first.", call. = FALSE)
  if (!exists("resolve_path")) stop("resolve_path() not found. Add it to utils_io.R.", call. = FALSE)
  if (!exists("is_active_start")) stop("Source utils_validation.R first.", call. = FALSE)

  if (length(year) != 1 || is.na(year)) stopf("year must be a single value")
  year <- as.numeric(year)

  if (!is.null(seed)) {
    if (length(seed) != 1 || is.na(seed)) stopf("seed must be a single value or NULL")
    set.seed(as.integer(seed))
  }

  # ---- Load policyholders (maintained file) ----
  ph <- read_pholders(ph_path)
  assert_has_cols(ph, PH_COLS)

  # active at start of year AND not already exited
  active <- is_active_start(ph, year) & is.na(ph[["exit"]])
  phA <- ph[active, , drop = FALSE]

  # If no one is active, return a empty deaths table
  out_cols <- c("policy.number","first.name","surname","year","age","sex","smoker.status","qx","benefit")
  if (nrow(phA) == 0) {
    return(data.frame(matrix(ncol = length(out_cols), nrow = 0,
                             dimnames = list(NULL, out_cols)),
                      stringsAsFactors = FALSE))
  }

  # Age in the simulated year
  age <- phA[["age.at.inception"]] + (year - phA[["inception.year"]])
  if (any(is.na(age))) stopf("Missing age/inception info for some active policies")
  age_int <- as.integer(round(age))
  if (any(abs(age - age_int) > 1e-8)) stopf("Age in year is not an integer for some policies")
  age <- age_int

  # Load mortality rates
  mort <- read.csv(resolve_path(mort_path), stringsAsFactors = FALSE, check.names = FALSE)
  names(mort) <- trimws(names(mort))
  if (!all(c("Age","Male","Female") %in% names(mort))) {
    stopf("mortrates.csv must have columns: Age, Male, Female")
  }

  # lookup base qx
  idx_age <- match(age, mort[["Age"]])
  if (any(is.na(idx_age))) {
    bad_ages <- unique(age[is.na(idx_age)])
    stopf("Mortality rates missing for age(s): %s", paste(bad_ages, collapse = ", "))
  }

  sex <- trimws(as.character(phA[["sex"]]))
  if (any(!(sex %in% c("Male","Female")))) stopf("Invalid sex values in pholders file")

  q_base <- ifelse(sex == "Male", mort[["Male"]][idx_age], mort[["Female"]][idx_age])
  q_base <- as.numeric(q_base)

  # Load smoker adjustment rates
  sr <- read.csv(resolve_path(srates_path), stringsAsFactors = FALSE, check.names = FALSE)
  names(sr) <- trimws(names(sr))
  if (!all(c("Gender","Non-smoker","Smoker","Ex-Smoker") %in% names(sr))) {
    stopf("srates.csv must have columns: Gender, Non-smoker, Smoker, Ex-Smoker")
  }

  # Map smoker.status to srates column name
  status <- trimws(as.character(phA[["smoker.status"]]))
  status_key <- tolower(gsub("[^a-z]", "", status)) # "Ex-smoker" -> "exsmoker"

  col_for_status <- ifelse(status_key == "nonsmoker", "Non-smoker",
                           ifelse(status_key == "smoker", "Smoker",
                                  ifelse(status_key == "exsmoker", "Ex-Smoker", NA)))

  if (any(is.na(col_for_status))) {
    bad <- unique(status[is.na(col_for_status)])
    stopf("Invalid smoker.status value(s): %s", paste(bad, collapse = ", "))
  }

  # Factor per row (by sex + smoker status)
  sex_idx <- match(sex, sr[["Gender"]])
  if (any(is.na(sex_idx))) stopf("srates.csv missing Gender row for Male/Female")

  factor <- mapply(function(r, colnm) sr[[colnm]][r], sex_idx, col_for_status)
  factor <- as.numeric(factor)

  # final qx
  qx <- q_base * factor
  qx <- pmin(pmax(qx, 0), 1)  # clamp to [0,1]

  # simulate deaths
  u <- runif(nrow(phA))
  died <- (u < qx)

  deaths <- phA[died, , drop = FALSE]
  if (nrow(deaths) == 0) {
    return(data.frame(matrix(ncol = length(out_cols), nrow = 0,
                             dimnames = list(NULL, out_cols)),
                      stringsAsFactors = FALSE))
  }

  # attach qx and benefit
  qx_d <- qx[died]
  data.frame(
    policy.number  = deaths[["policy.number"]],
    first.name     = deaths[["first.name"]],
    surname        = deaths[["surname"]],
    year           = year,
    age            = age[died],
    sex            = deaths[["sex"]],
    smoker.status  = deaths[["smoker.status"]],
    qx             = qx_d,
    benefit        = as.numeric(deaths[["sum.assured"]]),
    stringsAsFactors = FALSE
  )
}
