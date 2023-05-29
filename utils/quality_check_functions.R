
# Parameter recovery overall plot (currently used in the code for parmeter recovery)
recovery_overall = function(df, var1_true, var1_est, name, y_labs) {
  
  df[, var1_true] <- as.factor(df[, var1_true])
  
  plot = ggplot(df, aes_string(x = var1_true, y = var1_est)) + 
    geom_point(size = 0.5) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_smooth(formula = y ~ x, method = 'lm', size = 0.5, alpha = 0.05, aes(group = 1)) + 
    ggtitle(paste("Parameter Recovery Check for", name)) + 
    xlab(paste("True", name)) + 
    ylab(paste("Estimated", name)) +
    scale_color_brewer(palette = "Set2") +
    scale_y_continuous(breaks = y_labs) +
    theme_bw()
  
  return(plot)
  
}

# Parameter recovery across values of another variable (interaction) (wrap over n_agents) (currently used in the code for parmeter recovery)
recovery_over_var_3 = function(df, var1_true, var1_pred, var2_true, var2_true_pretty, name, y_labs) {
  
  df[, var1_true] <- as.factor(df[, var1_true])
  df[, var2_true] <- as.factor(df[, var2_true])
  
  plot = ggplot(df, aes_string(x = var1_true, y = var1_pred)) + 
    geom_point(aes_string(color = var2_true), size = 0.5) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_smooth(aes_string(color = var2_true, group = var2_true), formula = y ~ x, method = 'lm', size = 0.5, alpha = 0.05) + 
    ggtitle(paste("Parameter Recovery Check for", name)) + 
    labs(colour = paste("True", var2_true_pretty)) +
    xlab(paste("True", name)) + 
    ylab(paste("Estimated", name)) +
    scale_color_brewer(palette = "Set2") +
    scale_y_continuous(breaks = y_labs) +
    theme_bw() +
    theme(legend.position = "right",
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8))
  
  return(plot)
  
}

# Prior posterior update check
plot_prior_posterior = function(draws_df, prior, posterior, parameter){
  
  plot = ggplot(draws_df) +
    theme_bw() + 
          geom_density(aes_string(prior, fill = shQuote("Prior")), alpha = 0.3) + 
          geom_density(aes_string(posterior, fill = shQuote("Posterior")), alpha = 0.3) + 
          scale_fill_manual(name = 'Distribution', guide = 'legend',
                           values = c('Prior' = 'lightblue', 
                                    'Posterior' = 'darkblue')) +
          xlab(paste(parameter)) +
          ylab("Prior/Posterior Density")
          ggtitle(paste(parameter, "Prior/Posterior")) +
          theme(legend.position = 'right')
  
  return(plot)
}

# MCMC chains
plot_chains = function(draws_df, posterior){
  plot = ggplot(draws_df, aes_string(".iteration", posterior, group = ".chain", color = ".chain")) +
          geom_line() +
          theme_bw()
  
  return(plot)
  
}

# Get prior predictive checks (hist)
pp_predictive_hist = function(par, fit, d) {
  
  # Extract prior predictions
  pred <- as.data.frame(fit, pars = par)
  # Get n correct for each iteration
  n_correct = data.frame(rowSums(pred))
  names(n_correct) = c("pred")
  # Get rate instead of total correct, i.e. divide by total trials
  n_correct$pred_rate = n_correct$pred/ncol(pred)
  
  # Plot n_correct
  plot = ggplot(n_correct) + 
    xlim(0, 1) +
    geom_histogram(aes(pred_rate), color = "darkblue", fill = "blue", alpha = 0.3, binwidth = 0.01) + 
    geom_point(x = sum(d$choice)/nrow(d), y = 0, color = "red", shape = 17, size = 3) +
    xlab("Predicted rate of correct choice (n_correct/n_trials)") +
    ylab("Posterior Density") +
    theme_classic()
  
  return(plot)
  
}

# Get prior predictive checks density
pp_predictive_dens = function(par, fit, d, n_sample) {
  
  # Extract predictions
  pred <- as.matrix(fit, pars = par)
  subset_pred = pred[sample(1:dim(pred)[1], n_sample, replace = FALSE), ]
  # Density plot
  plot = ppc_dens_overlay(d$choice, subset_pred)
  plot + labs(x = "Choice", y = "Density") + theme(text = element_text(family = "sans"))
  
  return(plot)
  
}  


##### Plots not currently used #####

# Plot for parameter recovery (old)
plot_parameter_recovery = function(draws_df, true, predicted, name){
  
  plot = ggplot(draws_df, aes_string(true, predicted)) + 
    geom_point(alpha = 0.1) + 
    geom_smooth(method = 'lm', aes(colour = "red")) + 
    ggtitle(paste("Parameter Recovery Check for", name)) + 
    xlab(name) + 
    ylab(paste("Predicted", name)) +
    theme_bw()
  
  return(plot)
  
}

# Plot for parameter recovery which can be used to facet wrap over number of agents (old)
plot_parameter_recovery_facet = function(draws_df, true, predicted, name){
  
  plot = ggplot(draws_df, aes_string(true, predicted)) + 
    geom_point(alpha = 0.1) + 
    geom_smooth(formula = y ~ x, method = 'lm', aes(colour = "red")) + 
    ggtitle(paste("Parameter Recovery Check for", name)) + 
    xlab(name) + 
    ylab(paste("Predicted", name)) +
    facet_wrap(~n_agent) +
    theme_bw()
  
  return(plot)
  
}


# Parameter recovery across values of another variable (interaction) (wrap over n_agents)
recovery_over_var = function(df, var1_true, var1_pred, var2_true, name) {
  
  df[, var1_true] <- as.factor(df[, var1_true])
  df[, var2_true] <- as.factor(df[, var2_true])
  
  plot = ggplot(df, aes_string(x = var1_true, y = var1_pred)) + 
    geom_point(aes_string(color = var2_true), size = 0.5) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_smooth(aes_string(color = var2_true, group = var2_true), formula = y ~ x, method = 'lm', size = 0.5) + 
    ggtitle(paste("Parameter Recovery Check for", name)) + 
    xlab(name) + 
    ylab(paste("Predicted", name)) +
    facet_wrap(~n_agent) +
    scale_color_brewer(palette = "Dark2") +
    theme_bw()
  
  return(plot)
  
}

# Parameter recovery across values of another variable (interaction) (flexible facet wrap)
recovery_over_var_2 = function(df, var1_true, var1_pred, var2_true, wrap, name) {
  
  df[, var1_true] <- as.factor(df[, var1_true])
  df[, var2_true] <- as.factor(df[, var2_true])
  
  plot = ggplot(df, aes_string(x = var1_true, y = var1_pred)) + 
    geom_point(aes_string(color = var2_true), size = 0.5) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_smooth(aes_string(color = var2_true, group = var2_true), formula = y ~ x, method = 'lm', size = 0.5, alpha = 0.05) + 
    ggtitle(paste("Parameter Recovery Check for", name)) + 
    xlab(paste("True", name)) + 
    ylab(paste("Predicted", name)) +
    facet_wrap(wrap) +
    scale_color_brewer(palette = "Set2") +
    theme_bw()
  
  return(plot)
  
}

# Correlation values over n_agents (old)
plot_corr = function(df, title) {
  plot = ggplot(df) + 
    geom_point(aes(y = cor_p, x = n_agent), color = "blue", size = 0.7) +
    geom_smooth(aes(y = cor_p, x = n_agent), color = "blue", linewidth = 0.6, method = "lm", formula = "y ~ x") + 
    geom_point(aes(y = cor_log, x = n_agent), color = "red", size = 0.7) + 
    geom_smooth(aes(y = cor_log, x = n_agent), color = "red", linewidth = 0.6, method = "lm", formula = "y ~ x") + 
    ylim(-1,1) +
    labs(title = title) +
    theme_bw()
  
  return(plot)
}