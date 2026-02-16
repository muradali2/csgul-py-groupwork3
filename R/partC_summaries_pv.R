# Part C
# Summaries and present value style calculations


.make_12_summary <- function(df_sub, value_col) {
  # df_sub must have: sex, smoker.status, and value_col present
  v <- df_sub[[value_col]]
  sx <- df_sub[["sex"]]
  sm <- df_sub[["smoker.status"]]

  # ensure numeric
  v <- suppressWarnings(as.numeric(v))
  v[is.na(v)] <- 0

  out_names <- c(
    "All policies",
    "Male", "Female",
    "Non-smoker", "Smoker", "Ex-smoker",
    "Male | Non-smoker", "Male | Smoker", "Male | Ex-smoker",
    "Female | Non-smoker", "Female | Smoker", "Female | Ex-smoker"
  )

  # Start all zeros and then fill what exists
  out <- setNames(rep(0, length(out_names)), out_names)

  # All
  out["All policies"] <- sum(v)

  #  By sex
  by_sex <- tapply(v, sx, sum)
  for (nm in intersect(names(by_sex), c("Male", "Female"))) out[nm] <- by_sex[[nm]]

  # By smoker status
  by_smoke <- tapply(v, sm, sum)
  for (nm in intersect(names(by_smoke), c("Non-smoker", "Smoker", "Ex-smoker"))) out[nm] <- by_smoke[[nm]]

  # By sex x smoker
  comb <- interaction(sx, sm, sep = " | ", drop = TRUE)
  by_both <- tapply(v, comb, sum)
  for (nm in intersect(names(by_both), names(out))) out[nm] <- by_both[[nm]]

  data.frame(
    category = names(out),
    total = as.numeric(out),
    stringsAsFactors = FALSE
  )
}

# 1) Premium sumary for a given yr
summarise_premiums <- function(year, ph_path = "pholders.csv") {
  if (!exists("read_pholders")) stop("Source utils_io.R first.", call. = FALSE)
  if (!exists("is_active_start")) stop("Source utils_validation.R first.", call. = FALSE)

  if (length(year) != 1 || is.na(year)) stopf("year must be a single value")
  year <- as.numeric(year)

  df <- read_pholders(ph_path)
  assert_has_cols(df, PH_COLS)

  active <- is_active_start(df, year)
  df_sub <- df[active, , drop = FALSE]

  # Premium from all active policies at start of yr
  .make_12_summary(df_sub, "premium")
}

# ---- 2) Death benefit summary for a given year ----
summarise_death_benefits <- function(year, ph_path = "pholders.csv") {
  if (!exists("read_pholders")) stop("Source utils_io.R first.", call. = FALSE)

  if (length(year) != 1 || is.na(year)) stopf("year must be a single value")
  year <- as.numeric(year)

  df <- read_pholders(ph_path)
  assert_has_cols(df, PH_COLS)

  died <- (!is.na(df$exit) & df$exit == "Death") &
          (!is.na(df$year.of.exit) & df$year.of.exit == year)

  df_sub <- df[died, , drop = FALSE]

  # Death benefit is sum.assured for those who died in that year
  .make_12_summary(df_sub, "sum.assured")
}

# ---- 3) Policy-level nominal totals + accumulated-to-end-of-year values ----
policy_cashflows <- function(policy.number, year, i, ph_path = "pholders.csv") {
  if (!exists("read_pholders")) stop("Source utils_io.R first.", call. = FALSE)
  if (!exists("end_year_vec")) stop("Source utils_validation.R first.", call. = FALSE)

  if (length(policy.number) != 1 || is.na(policy.number) || !nzchar(trimws(policy.number))) {
    stopf("policy.number must be a non-empty string")
  }
  if (length(year) != 1 || is.na(year)) stopf("year must be a single value")
  if (length(i) != 1 || is.na(i)) stopf("i must be a single value")

  policy.number <- trimws(as.character(policy.number))
  year <- as.numeric(year)
  i <- as.numeric(i)

  df <- read_pholders(ph_path)
  assert_has_cols(df, PH_COLS)

  idx <- match(policy.number, df$policy.number)
  if (is.na(idx)) stopf("Policy number '%s' not found", policy.number)

  row <- df[idx, , drop = FALSE]
  inc <- row$inception.year
  term <- row$term
  prem <- row$premium
  sa <- row$sum.assured
  ex <- row$exit
  yex <- row$year.of.exit

  if (is.na(inc) || is.na(term) || is.na(prem) || is.na(sa)) {
    stopf("Policy '%s' has missing core fields (inception/year/term/premium/sum.assured)", policy.number)
  }

  contractual_end <- inc + term - 1

  # last yr premium could be paid is min(year, contractual_end, year.of.exit if it exists)
  last_year <- min(year, contractual_end, ifelse(is.na(yex), Inf, yex))

  prem_years <- if (year < inc) integer(0) else seq(from = inc, to = last_year, by = 1)
  n_prem <- length(prem_years)

  # nominal totals up to end of 'year'
  nominal_prem <- prem * n_prem

  nominal_benefit <- 0
  benefit_year <- NA_real_
  if (!is.na(ex) && ex == "Death" && !is.na(yex) && yex <= year) {
    nominal_benefit <- sa
    benefit_year <- yex
  }

  # Accumulated to end of yr
  # Premium at start of yr y accumulates for (year - y + 1) years
  acc_prem <- if (n_prem == 0) 0 else sum((1 + i)^(year - prem_years + 1)) * prem

  # Death benefit paid mid-year d accumulates for (year - d + 0.5) yrs
  acc_benefit <- 0
  if (!is.na(benefit_year)) {
    acc_benefit <- nominal_benefit * (1 + i)^(year - benefit_year + 0.5)
  }

  data.frame(
    policy.number = policy.number,
    year = year,
    i = i,
    nominal_premium_total = nominal_prem,
    nominal_benefit_total = nominal_benefit,
    accumulated_premium_to_end_year = acc_prem,
    accumulated_benefit_to_end_year = acc_benefit,
    stringsAsFactors = FALSE
  )
}
