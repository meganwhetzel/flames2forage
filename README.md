# Code for, "From flames to forage: elk foraging patterns vary predictively with fire history"

## Authors  
*  Megan M. Whetzel  
*  Brian J. Smith  
*  Alex A. Howe  
*  Courtney E. Check  
*  Tal Avgar  
*  Larissa L. Yocom  

## Manuscript Status  
This draft manuscript has been prepared for submission.

## About Repository
This repository contains the code necessary to recreate analyses and figures for this manuscript.  

### Version  
Archived versions of this repository are available through Zenodo. The latest version is always available at this DOI:  
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15271497.svg)](https://doi.org/10.5281/zenodo.15271497)

**Repository version 0.1**  
This release was created prior to peer-review.  
*Repository version 0.1.1* was created to assign Zenodo DOI.
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15271498.svg)](https://doi.org/10.5281/zenodo.15271498)

### Scripts  
*  `01_fit_HMM.R` fits the hidden Markov model to estimate behavioral states of each step,
*  `02_fit_GAMs.R` fits generalized additive models to relate the behavioral state to environmental and temporal covariates,
*  `03_figures.R` creates the final manuscript figures from the fitted models,
*  `99_fun.R` contains custom helper functions used in multiple previous scripts.