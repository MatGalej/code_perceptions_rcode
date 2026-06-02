# Introduction

This repository serves as the main location of the analysis code and figures used within the *AI Aversion in Students’ Perceptions of Code Quality* paper. It includes both the code that was used to clean the dataset, as well as the analysis code that was used within the study itself.

## Important Information

* Please note that the ```DataClean.R``` **must** be run first, before the ```analysis.R``` file can be run.
* The following dependencies are required to run this code:
    * ggplot2
    * dplyr
    * tidyr
    * ordinal
    * lubridate
    * scales
    * purrr

## Instructions to run code

1. Clone repository via ```git clone```
2. Ensure that all required dependencies are installed
3. Execute the ```DataClean.R``` file, followed by the ``analysis.R``` file.
    * ```DataClean.R``` requires that a ```RawData.csv``` file exists within the top most folder of the repository.