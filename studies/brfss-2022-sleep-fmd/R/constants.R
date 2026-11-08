# Locked source identity and codebook checks.
# Frequencies are the unweighted counts printed in the 2022 LLCP codebook
# (USCODE22_LLCP_102523.HTML) for all reporting areas combined.

BRFSS_SOURCE <- list(
  landing_url = "https://www.cdc.gov/brfss/annual_data/annual_2022.html",
  data_url = "https://www.cdc.gov/brfss/annual_data/2022/files/LLCP2022XPT.zip",
  codebook_url = "https://www.cdc.gov/brfss/annual_data/2022/zip/codebook22_llcp-v2-508.zip",
  design_url = "https://www.cdc.gov/brfss/annual_data/2022/pdf/Complex-Sampling-Weights-and-Preparing-Module-Data-for-Analysis-2022-508.pdf",
  overview_url = "https://www.cdc.gov/brfss/annual_data/2022/pdf/Overview_2022-508.pdf",
  compare_url = "https://www.cdc.gov/brfss/annual_data/2022/pdf/Compare_2022-508.pdf",
  calculated_url = "https://www.cdc.gov/brfss/annual_data/2022/pdf/2022-calculated-variables-version4-508.pdf",
  weighting_url = "https://www.cdc.gov/brfss/annual_data/2022/pdf/2022-Weighting-Description-508.pdf",
  zip_sha256 = "b6319c949b7289f8020fc903c3c2d38fe7e0b28ad703dcd81547f5cd0c0dcfeb",
  zip_bytes = 84614105L,
  expected_records = 445132L,
  user_agent = "Mozilla/5.0 (compatible; research-repro/1.0; mailto:research@example.com)"
)

# 50 states and the District of Columbia. FIPS 11 is DC.
# Any other code, including 66 Guam, 72 Puerto Rico, and 78 US Virgin Islands,
# is out of the target population.
STATE_ALLOWLIST <- c(
  1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
  25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45,
  46, 47, 48, 49, 50, 51, 53, 54, 55, 56
)

TERRITORY_FIPS <- c(GU = 66L, PR = 72L, VI = 78L)

# Codebook unweighted frequencies, all reporting areas.
CODEBOOK_SLEPTIM1 <- c(valid_1_24 = 439679L, dk_77 = 4792L, refused_99 = 658L, blank = 3L)
CODEBOOK_MENTHLTH <- c(days_1_30 = 170836L, none_88 = 265229L, dk_77 = 6589L, refused_99 = 2475L, blank = 3L)
CODEBOOK_MENT14D <- c(zero = 265229L, days_1_13 = 110616L, days_14_plus = 60220L, missing = 9067L)
CODEBOOK_SEXVAR <- c(male = 209239L, female = 235893L)

# Usual-sleep sensitivity. Primary analysis keeps every instrument-valid hour, 1 to 24.
# The sensitivity drops reported usual averages below 3 hours or above 12 hours.
SLEEP_PLAUSIBLE_MIN <- 3L
SLEEP_PLAUSIBLE_MAX <- 12L

AGE_LABELS <- c(
  "1" = "18 to 24",
  "2" = "25 to 29",
  "3" = "30 to 34",
  "4" = "35 to 39",
  "5" = "40 to 44",
  "6" = "45 to 49",
  "7" = "50 to 54",
  "8" = "55 to 59",
  "9" = "60 to 64",
  "10" = "65 to 69",
  "11" = "70 to 74",
  "12" = "75 to 79",
  "13" = "80 or older"
)

ANALYSIS_COLUMNS <- c(
  "SLEPTIM1", "MENTHLTH", "_MENT14D", "_AGEG5YR",
  "SEXVAR", "_STATE", "_LLCPWT", "_STSTR", "_PSU"
)

# Read for the audit, then dropped before estimation so they cannot be used as substitutes.
AUDIT_ONLY_COLUMNS <- c("_AGE80", "_SEX", "BIRTHSEX")
