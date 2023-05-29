# Stan Models 

This directory contains the Stan models, accompanied by scripts to run the models. More specifically it contains the following subfolders: 

- **yearsley/**: This contains the Y-RL model based on Yearsley et al. (2021)
- **talwar/**: This  contains the T-fRL and T-cafRL model developed based on Talwar et al. (2021), (T-cafRL with all free parameters: cafRL_loglik.stan, and with inverse temperature beta as fixed parameter: cafRL_alpha_pos_alpha_neg_theta0_loglik.stan)
- **ln/**: This contains the LN-fRL and LN-cafRL models developed as part of the thesis (LN-cafRL with all free parameters: ln_cafRL_loglik.stan, and with inverse temperature beta as fixed parameter: ln_cafRL_alpha_pos_alpha_neg_theta0_loglik.stan)

All subfolders contain scripts called **run_models.Rmd**, which can be used to run to simulate data, fit the models, and run common diagnostic checks. Note that there are slight differences in the variable names in the code and those mentioned in the paper. For a mapping of values see the README file in the main directory. 