# ==== Initialization ====
library(devtools)

# Meta information ====
pkg_name <- "BCM"
pkg_path <- file.path("release", pkg_name)
pkg_version <- "17.1.0"

pkg_authors <- c(
  person("Stephy", "Thomas", email = "stephy@bcmcollege.ac.in", role = c("aut")),
  person("Rakesh", "Poduval", email = "poduval.rakesh@icloud.com", role = c("cre")))

RScripts <- c("01-info.R")
RFiles <- file.path("R", RScripts)

# Package setup ====

# clear the directory (to restart package building) ====
unlink(pkg_path, force = TRUE, recursive = TRUE)

# _Create the package directory ====
create_package(
  pkg_path,
  fields = list(
    Title = "BCM College Kotayam",
    Description = "Result analysis",
    Version = pkg_version,
    Date = Sys.Date(),
    `Authors@R` = pkg_authors,
    License = "GPL-2",
    Encoding = "UTF-8",
    Suggests = "",
    Imports =  "dplyr, tidyr"),
  rstudio = FALSE,
  roxygen = TRUE,
  check_name = TRUE,
  open = FALSE)

# _Store internal data in new package ====
load("03-analysis/student-performance/01-prep.RData", verbose = T)
sysdata <- "df"
tmp <- tempfile(fileext = ".RData")
save(list = sysdata, file = tmp)
file.copy(tmp, file.path(pkg_path, "R", "sysdata.rda"), 
          overwrite = TRUE, copy.date = TRUE)

# _Copy the R scripts into sub directory R ====
file.copy(RFiles, file.path(pkg_path, "R"))

# _Copy release notes
file.copy("NEWS.md", pkg_path, overwrite = T)

# _Create Rd files (help files) from roxygen comments ====
document(pkg_path)

# _Build the package (generates rp.tar.gz) ====
build(pkg_path)

# _Check package ====
check(pkg_path, check_dir = pkg_path, document = FALSE, manual = TRUE)

# _Install ====
install(pkg_path)

