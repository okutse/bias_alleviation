# Machine learning for precision optimization and bias correction using covariate adjustment in randomized trials

This repository holds additional materials related to the implementation of machine learning (ML) methods for bias correction and precision optimization using covariate adjustment in randomized trials. The repository is organized into folders for each implemented ML algorithm in the associated paper.

The repository is organized into three folders:

- The `Data` folder contains 2 sub-folders
  
  (i) `dgp1` contains the data-generating mechanism under MAR. Information about the saved data sets is explained in the script `data_generation_function.R`
    
  (ii) `dgp2` contains the data generating mechanism under a more complex missing data mechanism (non-ignorable).

  The missing data mechanism is summarized in the file `informative_missing_data.Rmd`. Under this data generation mechanism, adjustment analyses were performed using all observed adjustment covariates (full).

  These results were compared to analyses omitting an adjustment covariate informative of the missing data mechanism (misspecified adjustment model). Results are saved for each analysis under each folder. 
