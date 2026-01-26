###############################################################################
# NAME:         M - UPDATE GENERAL TOOLBOX
# AUTHOR:       Pieter Overdevest
###############################################################################

# If you modify functions, run this block to update the package.

# 1. Unload current version to prevent "corrupt lazy-load database" errors
try(detach("package:generaltoolbox", unload = TRUE), silent = TRUE)

# 2. Regenerate documentation (.Rd files) and NAMESPACE
devtools::document()

# 3. Install fresh version (skips external dependency updates for speed)
devtools::install(upgrade = "never")