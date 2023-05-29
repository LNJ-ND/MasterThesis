# Install packages
install.packages("brms")
install.packages("pacman")
pacman::p_load("tidyverse", "tictoc", "brms", "random", "cowplot", "RColorBrewer", "R.utils")

# Install these (newer rstan) to be able to use our syntax (newer) in the conversion
# ref: https://discourse.mc-stan.org/t/parser-doesnt-recognise-array-n-x/26191
remove.packages(c("StanHeaders", "rstan"))
install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
