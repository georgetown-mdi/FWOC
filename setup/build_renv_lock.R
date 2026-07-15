# Author: Mark Richardson and Claude Code
# Purpose: Generate renv.lock — a manifest of the R packages (and the exact
#          versions) the FWOC pipeline depends on for reproducibility


# Option for a parsimonious lockfile with hashes
options(renv.lockfile.version = 1)

# prompt = FALSE keeps it non-interactive (safe to source).
renv::snapshot(project = ".", prompt = FALSE)
