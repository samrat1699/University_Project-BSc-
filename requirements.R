# requirements.R

packages <- c("dplyr", "tidyr", "DescTools", "nnet", "car")

installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)

if (length(to_install)) {
  install.packages(to_install)
}

# Load the packages
lapply(packages, library, character.only = TRUE)
