#!/usr/bin/Rscript --vanilla

# run_all.R

# This steering script sources all of the non stand-lone code below to read in
# data, conduct PCAs, fit models, build tables and figures.

# read, clean and merged all the morphological and flammability traits data.
# Produces "alldata"
source("./scripts/read_data.R")

# script that reads the thermocouple data logger data during burning. Produces
# "hobos_wider"

DATA_CACHE_DIR <- "./results"
#source("./scripts/read_hobos.R")  ## Do this once
## After running the above once, just read saved data:
hobos_wider <- readRDS(file.path(DATA_CACHE_DIR, "hobos_wider"))

# run PCAs requires burn_trials and hobos_wider to exist:

source("./scripts/flam_pca.R")
source("./scripts/analysis.R") # analysis.R
source("./scripts/figures.R") # figures.R


