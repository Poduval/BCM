# Build package ====
library(devtools)

# _ignoring package un related files ====
use_build_ignore(c('01-contents.pages', '01-contents.pdf',
                   '02-documents', '03-analysis', '04-packaging.R', 
                   '05-release', "renv", 'renv.lock'))

# _integrating data used ====
load("03-analysis/student-performance/01-prep.RData", verbose = T)
bcmdata <- df
use_data(bcmdata, overwrite = TRUE) # store data in new package

# _further steps ====
document()
build(path = "05-release/", quiet = FALSE,
      binary = FALSE, vignettes = TRUE) 
check(quiet = TRUE)
install()
# cd 05-release
# R CMD INSTALL --build BCM_17.1.0.tar.gz

# Test package ====
packageDescription("BCM")
news(package = "BCM")
vignette(package = "BCM")

library(BCM)
getNamespaceExports("BCM")

?bcm_programs
?bcm_papers
?bcm_results
?bcmdata

bcm_programs()
bcm_papers()
bcm_results()
