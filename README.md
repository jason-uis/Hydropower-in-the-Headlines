# Hydropower in the Headlines

Reproducibility materials for:

**Hydropower in the Headlines: A Media Discourse Model for Tracking Emotional Orientations towards Hydropower in Europe**

## Contents

- `01_reproduce_analysis.R` — main analysis script for the Media Emotion Index (MEI), equal-weight robustness check, weight perturbation analysis, and monthly aggregation.
- `data/` — legally shareable input/derived data required by the analysis.
- `results/` — generated output files.
- `figures/` — figures generated from the analysis.

## Important data note

The underlying news articles and translated article text are not redistributed where source or copyright restrictions prevent this. The repository therefore contains only data that can legally be shared.

The English translations used for the NRC emotion analysis were generated before publication using the Google Cloud Translation API. The translation step is not rerun by the reproducibility script and no API credentials are required.

## Reproduction

1. Download or clone this repository.
2. Open `01_reproduce_analysis.R` in R/RStudio.
3. Set the working directory to the repository root, or run the script from the root directory.
4. Install the packages listed at the top of the script if necessary.
5. Run the script.

The script writes the main article-level and monthly results to `results/`.

## Analytical scope

The MEI measures the emotional orientation of media discourse surrounding hydropower infrastructure over time. It is not a direct measure of public opinion or social acceptance.
