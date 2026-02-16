We organised the project so data/ holds the input CSVs, R/ holds the functions, and output/ for generated files. pholdersraw.csv is read-only and pholders.csv is the maintained file that Parts B C and D will work on.

From the root, you can load everything with:

source("R/00_source_all.R")

After that, the functions should exist in your environment.

Guide: Add policies first, then run the simulation after the book is updated (in Part D), then do end-of-year maintenance in the order deaths, withdrawals, and maturities.

Part C summarise premiums/benefits by the required groupings, and it can value a single policy’s cashflows using the timing assumptions: premiums at the start of year, death benefits mid-year.


Part A: error detection

detect_errors_partA() reads pholdersraw.csv and produces an “errors report” suitable to send back to a data provider. It returns a data frame and also writes a CSV (default output/errors.csv). It does not mutate any files.

detect_errors_partA("data/pholdersraw.csv", asof_year = 2026, out_path = "output/errors.csv")


Part B: maintaining the live file

Part B updates pholders.csv. Each function loads the file, validates inputs (so we don’t write nonsense), then it writes the updated file back.

add_policyholder() appends a new policy after checking it’s valid (unique policy number, valid age/term constraints, valid categories, etc.).

record_deaths() stamps exit="Death" + year.of.exit=year for policies that were active at the start of that year.

record_withdrawals() stamps exit="Withdrawl" + year.of.exit=year, and refuses to act on policies that have already exited.

close_matured_policies() marks still-active policies whose contractual end year is year as exit="End".

e.g.:

file.copy("data/pholders.csv", "output/pholders_test.csv", overwrite = TRUE)

record_deaths(c("P1003001","P1003002"), 2026, ph_path = "output/pholders_test.csv")
record_withdrawals(c("P1003111"), 2026, ph_path = "output/pholders_test.csv")
close_matured_policies(2026, ph_path = "output/pholders_test.csv")


Part C: summaries and policy cashflows

summarise_premiums(year, ...) returns the total premium paid in that year (policies active at the start of the year), split into the required 12 categories.

summarise_death_benefits(year, ...) returns total death benefits paid in that year (sum assured for exit="Death" in that year), split the same way.

policy_cashflows(policy.number, year, i, ...) returns nominal totals up to end of year plus the same cashflows accumulated to end-of-year using the timing assumptions.

summarise_premiums(2026)
summarise_death_benefits(2024)
policy_cashflows("P1003001", year = 2025, i = 0.04)


Part D: simulation

simulate_deaths(year, ...) takes policies active at the start of the year, computes qx from mortrates.csv (age/sex) and multiplies by a smoker factor from srates.csv, then simulates deaths. It returns a table of simulated deaths (policy number, year, age, qx, benefit, etc.).