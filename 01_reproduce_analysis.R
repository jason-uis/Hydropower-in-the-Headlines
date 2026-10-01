# ==============================================================================
# Hydropower in the Headlines
# Reproducibility script
# ==============================================================================

# This script reproduces the final MEI calculations and robustness analyses.
# It does not rerun translation or article retrieval.
#
# Required input:
#   data/article_level_emotions.csv
#
# The input file should contain:
#   row_id, source, date,
#   anger_score, anticipation_score, disgust_score, fear_score,
#   joy_score, sadness_score, trust_score,
#   native_polarity
#
# The seven emotion scores are proportional shares of emotion-bearing tokens.
#
# MEI measures the emotional orientation of media discourse surrounding
# hydropower infrastructure over time. It is not a direct measure of
# public opinion or social acceptance.

# ------------------------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "readr",
  "lubridate"
)

missing <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing) > 0) {
  install.packages(missing)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
})

# ------------------------------------------------------------------------------
# 2. Paths
# ------------------------------------------------------------------------------

repo_dir <- "."
data_dir <- file.path(repo_dir, "data")
results_dir <- file.path(repo_dir, "results")

dir.create(results_dir, showWarnings = FALSE)

input_file <- file.path(
  data_dir,
  "article_level_emotions.csv"
)

# ------------------------------------------------------------------------------
# 3. Theory-informed MEI weights
# ------------------------------------------------------------------------------

mei_weights <- c(
  joy_score          =  0.35,
  trust_score        =  0.40,
  anticipation_score =  0.25,
  anger_score        = -0.30,
  fear_score         = -0.40,
  disgust_score      = -0.20,
  sadness_score      = -0.10
)

emotion_cols <- names(mei_weights)

# ------------------------------------------------------------------------------
# 4. Load data
# ------------------------------------------------------------------------------

if (!file.exists(input_file)) {
  stop(
    "Required input file not found: ",
    input_file
  )
}

master <- read_csv(
  input_file,
  show_col_types = FALSE
) %>%
  mutate(
    date = as.Date(date)
  )

required_columns <- c(
  "row_id",
  "source",
  "date",
  emotion_cols,
  "native_polarity"
)

missing_columns <- setdiff(
  required_columns,
  names(master)
)

if (length(missing_columns) > 0) {
  stop(
    "The input file is missing: ",
    paste(missing_columns, collapse = ", ")
  )
)

# ------------------------------------------------------------------------------
# 5. Article-level MEI
# ------------------------------------------------------------------------------

master <- master %>%
  mutate(
    MEI = rowSums(
      across(
        all_of(emotion_cols),
        ~ .x * mei_weights[cur_column()]
      ),
      na.rm = TRUE
    )
  )

# ------------------------------------------------------------------------------
# 6. Equal-weight specification
# ------------------------------------------------------------------------------

supportive_emotions <- c(
  "trust_score",
  "joy_score",
  "anticipation_score"
)

equal_weights <- setNames(
  ifelse(
    names(mei_weights) %in% supportive_emotions,
    1 / length(mei_weights),
    -1 / length(mei_weights)
  ),
  names(mei_weights)
)

master <- master %>%
  mutate(
    MEI_equal = rowSums(
      across(
        all_of(emotion_cols),
        ~ .x * equal_weights[cur_column()]
      ),
      na.rm = TRUE
    )
  )

# ------------------------------------------------------------------------------
# 7. Monthly aggregation
# ------------------------------------------------------------------------------

monthly_mei <- master %>%
  filter(
    !is.na(MEI),
    !is.na(date)
  ) %>%
  mutate(
    month = floor_date(date, "month")
  ) %>%
  group_by(
    source,
    month
  ) %>%
  summarise(
    MEI = mean(MEI, na.rm = TRUE),
    MEI_equal = mean(MEI_equal, na.rm = TRUE),
    n_articles = n(),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 8. Equal-weight robustness
# ------------------------------------------------------------------------------

equal_weight_correlations <- monthly_mei %>%
  group_by(source) %>%
  summarise(
    correlation = cor(
      MEI,
      MEI_equal,
      use = "complete.obs"
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 9. Weight perturbation robustness
# ------------------------------------------------------------------------------

set.seed(1234)

perturbation_results <- vector("list", 1000)

for (i in seq_len(1000)) {

  perturbed_weights <- mei_weights *
    runif(
      length(mei_weights),
      min = 0.50,
      max = 1.50
    )

  names(perturbed_weights) <- names(mei_weights)

  perturbed_monthly <- master %>%
    filter(!is.na(date)) %>%
    mutate(
      month = floor_date(date, "month"),
      MEI_perturbed = rowSums(
        across(
          all_of(emotion_cols),
          ~ .x * perturbed_weights[cur_column()]
        ),
        na.rm = TRUE
      )
    ) %>%
    group_by(source, month) %>%
    summarise(
      MEI_perturbed = mean(
        MEI_perturbed,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  base_monthly <- monthly_mei %>%
    select(source, month, MEI)

  perturbation_results[[i]] <- perturbed_monthly %>%
    left_join(
      base_monthly,
      by = c("source", "month")
    ) %>%
    group_by(source) %>%
    summarise(
      correlation = cor(
        MEI_perturbed,
        MEI,
        use = "complete.obs"
      ),
      .groups = "drop"
    ) %>%
    mutate(iteration = i)
}

perturbation_results <- bind_rows(
  perturbation_results
)

perturbation_summary <- perturbation_results %>%
  group_by(source) %>%
  summarise(
    mean_correlation = mean(
      correlation,
      na.rm = TRUE
    ),
    sd_correlation = sd(
      correlation,
      na.rm = TRUE
    ),
    minimum_correlation = min(
      correlation,
      na.rm = TRUE
    ),
    maximum_correlation = max(
      correlation,
      na.rm = TRUE
    ),
    below_085 = sum(
      correlation < 0.85,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 10. Native-language polarity comparison
# ------------------------------------------------------------------------------

monthly_native <- master %>%
  filter(
    !is.na(native_polarity),
    !is.na(date)
  ) %>%
  mutate(
    month = floor_date(date, "month")
  ) %>%
  group_by(
    source,
    month
  ) %>%
  summarise(
    native_polarity = mean(
      native_polarity,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 11. Save results
# ------------------------------------------------------------------------------

write_csv(
  master,
  file.path(
    results_dir,
    "article_level_results.csv"
  )
)

write_csv(
  monthly_mei,
  file.path(
    results_dir,
    "monthly_mei.csv"
  )
)

write_csv(
  equal_weight_correlations,
  file.path(
    results_dir,
    "equal_weight_correlations.csv"
  )
)

write_csv(
  perturbation_results,
  file.path(
    results_dir,
    "weight_perturbation_results.csv"
  )
)

write_csv(
  perturbation_summary,
  file.path(
    results_dir,
    "weight_perturbation_summary.csv"
  )
)

write_csv(
  monthly_native,
  file.path(
    results_dir,
    "monthly_native_polarity.csv"
  )
)

message("Reproduction complete. Results saved in: ", results_dir)
