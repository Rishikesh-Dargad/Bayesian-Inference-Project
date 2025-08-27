library(MASS)
library(tidyverse)

# Simulation settings
n_rep <- 100
n <- 500
censoring_rate <- 0.15
beta_list <- list(c(0,4), c(0,-3), c(-2,1))
tau_list <- list(c(-2,4), c(-2,-4), c(0,0)) # placeholder for intercepts
sigma2 <- c(0.2, 0.1, 0.2)

nu_scenarios <- list(
  Equal = c(5,5,5),
  Unequal = c(3,6,10)
)

models <- c("T", "VG")
methods <- c("Hier", "Non-Hier")
priors <- c("Asis", "TG", "Pareto", "Jeffreys")

evaluate_performance <- function(true_nu, est_nu_mat) {
  std <- apply(est_nu_mat, 2, sd)
  arb <- colMeans(abs((est_nu_mat - matrix(true_nu, nrow=nrow(est_nu_mat), ncol=3, byrow=TRUE)) / true_nu))
  rrmse <- sqrt(colMeans(((est_nu_mat - matrix(true_nu, nrow=nrow(est_nu_mat), ncol=3, byrow=TRUE)) / true_nu)^2))
  return(list(STD=std, ARB=arb, RRMSE=rrmse))
}

generate_data <- function(n, beta_list, tau_list, sigma2, nu, censoring_rate) {
  X <- cbind(1, runif(n, -2, 2))
  group_probs <- rep(1/3, 3)
  Z <- sample(1:3, n, replace=TRUE, prob=group_probs)
  
  y <- numeric(n)
  for (i in 1:3) {
    idx <- which(Z == i)
    mu <- X[idx, ] %*% beta_list[[i]]
    y[idx] <- mu + sqrt(sigma2[i]) * rt(length(idx), df=nu[i])
  }
  
  # Apply left censoring
  cutoff <- quantile(y, probs=censoring_rate)
  y_cens <- ifelse(y < cutoff, cutoff, y)
  is_censored <- y < cutoff
  
  return(list(y=y_cens, X=X, Z=Z, censoring=is_censored))
}

# Placeholder: Replace this with your actual MCMC model fitting code
fit_moe_model <- function(model_type, method, prior, y, X, is_censored, K=3) {
  # Return dummy values for now
  nu_est <- if (model_type == "T") rep(5, K) else rep(7, K)
  return(nu_est + rnorm(K, sd=0.2))  # Slight noise for variation
}

# Main simulation loop
results_list <- list()

for (scenario_name in names(nu_scenarios)) {
  nu_true <- nu_scenarios[[scenario_name]]
  
  for (model in models) {
    for (method in methods) {
      for (prior in priors) {
        nu_estimates <- matrix(NA, nrow=n_rep, ncol=3)
        for (r in 1:n_rep) {
          data <- generate_data(n, beta_list, tau_list, sigma2, nu_true, censoring_rate)
          nu_hat <- fit_moe_model(model, method, prior, data$y, data$X, data$censoring)
          nu_estimates[r, ] <- nu_hat
        }
        
        metrics <- evaluate_performance(nu_true, nu_estimates)
        
        for (j in 1:3) {
          results_list[[length(results_list)+1]] <- tibble(
            Scenario = scenario_name,
            Model = model,
            Method = method,
            Prior = prior,
            Index = paste0("v", j),
            STD = metrics$STD[j],
            ARB = metrics$ARB[j],
            RRMSE = metrics$RRMSE[j]
          )
        }
      }
    }
  }
}

# Combine and print table
results_df <- bind_rows(results_list)
results_wide <- results_df %>%
  pivot_wider(names_from = Index, values_from = c(STD, ARB, RRMSE)) %>%
  arrange(Scenario, Model, Prior, Method)

print(results_wide)