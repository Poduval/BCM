
# Build package ====
library(devtools)

use_build_ignore(c('01-contents.pages', '01-contents.pdf',
                   '02-documents', '03-analysis', '04-packaging.R', 
                   "renv", 'renv.lock'))

load("03-analysis/student-performance/01-prep.RData", verbose = T)
use_data(df, overwrite = TRUE) # store data in new package

document() # create help files from roxygen

build(quiet = FALSE) # build the package (generates bcm.tar.gz)

check(quiet = TRUE) # check package

install() # install package


# Test package ====
packageDescription("BCM")
news(package = "BCM")

library(BCM)
getNamespaceExports("BCM")

bcm_programs()
bcm_papers()
bcm_results()
