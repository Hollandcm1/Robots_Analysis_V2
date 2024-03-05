# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("tibble", "R.matlab", "here", "openxlsx", "dplyr", "stringr", 
               "ggplot2", "tidyr", "purrr") # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source(files = "R_targets_scripts")

# Target List
list(
  tar_target(
    name = codes,
    command = import_codebook()
  ),
  # Separate targets to extract each element
  tar_target(
    name = codes_conditions,
    command = codes$conditions
  ),
  tar_target(
    name = codes_participant_conditions,
    command = codes$participant_conditions
  ),
  tar_target(
    name = maps,
    command = pull_maps(codes_participant_conditions, codes_conditions, environments)
  )
  # tar_target(
  #   name = c(codes.conditions, codes.participant_conditions),
  #   command = import_codebook()
  #   # format = "qs" # Efficient storage for general data objects.
  # )
  # tar_target(
  #   name = maps,
  #   command = import_maps()
  # )
  # tar_target(
  #   name = conditions_and_environments,
  #   command = source(here('scripts', 'define_environments_and_conditions.R'))
  # )
)

# tar_manifest()
#tar_visnetwork()
# tar_make()
