# Stress Testing: Parameter Recovery and Model Recovery

This directory contains the scripts used for *Part 1* of the thesis. More specifically, it contains the following subfolders:

- **fitting/**: This contains all scripts to simulate data and subsequently fit the models for parameter and model recovery. The .R files contain the code to run the models, while the .sh files contain the commands to run the .R files from the command line in parallel and save output in .log files. The preparing_fitting.Rmd prepares the models for fitting and defines the parameter combinations used for simulation of data. The output is not provided due to size limitations.
- **parameter_recovery/**: This contains the scripts and outputs for parameter recovery.  
- **model_recovery/**: This contains the scripts and outputs for model recovery. 
- **quality_checks/**: This contains the scripts and outputs for quality checks performed on fitted models. Note that these were only performed on a random sample of the fitted models.  
- **robustness/**: This contains the scripts used to perform the robustness check for the T-cafRL and LN-cafRL models, as described in the Appendix of the thesis. 

Note that there are slight differences in the variable names in the code and those mentioned in the paper. For a mapping of values see the README file in the main directory. 