required_packages <- c(
  "shiny",
  "bslib",
  "tidyverse",
  "scales",
  "nflplotR",
  "nflreadr",
  "slider",
  "quarto"
)

installed <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!pkg %in% installed) {
    install.packages(pkg)
  }
}

invisible(lapply(required_packages, library, character.only = TRUE))

message("All required packages are installed and loaded.")