# Simulation Models and Scripts

This directory contains the models which can be used for simulation, accompanied by scripts to run the simulation and generate relevant plots. More specifically it contains the following subfolders: 

- **yearsley/**: This contains the Y-RL model which can be used for simulation.  
- **talwar/**: This  contains the T-fRL and T-cafRL model developed based on Talwar et al. (2021) for simulations.
- **ln/**: This contains the LN-fRL and LN-cafRL models developed as part of the thesis for simulations.

While the simulation functions are defined in .R files within these subfolders, the .Rmd files allow simulation of data and generation of plots, and the simulation_figure directories contain generated figures based on simulated data. Note that there are slight differences in the variable names in the code and those mentioned in the paper. For a mapping of values see the README file in the main directory. 

All simulations can also be explored in a ShinyApp: https://nicoledwenger.shinyapps.io/CognitiveModelling_IED/. 