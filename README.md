# Cogntive Modelling of the IED
### Systematic Stress Testing and Insights into Schizophrenia

This repository contains the code of the Master's Thesis by Louise Nyholm Jensen & Nicole Dwenger, developed in the spring semester of 2023. Note that the code the developed ShinyApp can be found in a separate [repository](https://github.com/LNJ-ND/MasterShiny). More specifically, this repository contains the code for 5 cognitive models, for simulation and inference, and scripts used for analyses in the thesis. Note that the models were developed based on [Yearsley et al., (2021)](https://onlinelibrary.wiley.com/doi/full/10.1002/aur.2484?casa_token=6uC3SDV_6TsAAAAA%3A4Qjli8PLXYnFKMUodY2gSuccndm17_boUv74jwz5IKJZCpsruEgHOwjcVTHMwTulfb1-C0PbRQK0z38) and [Talwar et al. (2021)](https://www.biorxiv.org/content/10.1101/2021.10.05.463165v1.abstract). 

This repository contains the following folders: 
- **1-simulation/**: This contains the implemented models for simulation, and corresponding scripts to generate simulation and simulation figures.
- **2-models/**: This contains the the implemented models for inference, and corresponding scripts to fit the models. 
- **3-recovery/**: This contains all scripts used for *Part 1* of the thesis, specifically fitting the models to simulated data, parameter recovery, model recovery, robustness checks, and quality checks. Note that visualizations of quality checks are not provided in this folder, due to size limits. However, they can be provided upon request. 
- **4-empirical/**: This contains all scripts used for *Part 2* of the thesis, specifically fitting the models to empirical data, model comparison, analysis of parameters, and quality checks. Note that visualizations of quality checks are only provided for the model by Yearsley et al. (2021; Y-RL). Quality checks for the remaining models can be provided upon request.
- **utils/**: This contains utility scripts to install packages and to define scripts used throughout the repository. 

&nbsp;
---
&nbsp;

#### Mapping of Variable Names
Note that variables names used in the code may not exactly match parameter names described in the thesis, as they were renamed for consistency. Thus, in the following a mapping of names is provided to enhance understanding of the code in relation to the thesis. This mapping is provided for each of the model groups (based on original author/developer). 

- choicep / correctp: probability of making the correct choice *p(S_1)*
- stimulus: stimulus set *SS1, SS2*
- group: used in simulation, defines a group name
- n_correct: used in simulation, defines the criterion to complete a stage (6 trials)

##### Model based on Yearsley et al. (2021): Y-RL
- att1-att4: attention weights / feature weights *W(f)*
- weight1-weight4: feedback function / update signal *U(f)*
- stim_att: expected value *V(S)*
- f: dimension shift parameter / attention switching parameter *φ*
- r: reward learning rate *α_r*
- p: punishment learning rate *α_p* 
- d / d_param: decision consistency parameter *d* 
- lambda: disengagement/focus parameter *λ* 
- switch1: continuation parameter *C* 
- switch2: continuation parameter *C*

##### Model based on Talwar et al. (2021): T-fRL and T-cafRL
- f1-f4: feature weights *W(f)*
- V: expected value *V(S)*
- alpha: generic learning rate *α*
- beta: inverse temperature *β*
- theta_init / theta0: initial dimension weight *θ_0*
- theta: dimension weight *θ*

##### Model based on LN: LN-fRL and LN-cafRL
- f1-f4: feature weights *W(f)*
- V: expected value *V(S)*
- alpha_pos: reward learning rate *α_r*
- alpha_neg: punishment learning rate *α_p*
- beta: inverse temperature *β*
- theta_init / theta0: initial dimension weight *θ_0*
- theta: dimension weight *θ*
