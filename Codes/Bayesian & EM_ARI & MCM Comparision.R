library(mclust)
library(flexmix)
library(MASS)
library(truncnorm)
library(cluster)
library(aricode)  
library(parallel)

# Set true parameters
true_params <- list(
  beta = list(c(10, 4), c(0, 4), c(-10, 4)),
  tau = list(c(-2, 4), c(-2, -4)),  # g = 3, so last tau = 0
  sigma2 = rep(1, 3),
  nu = list(c(-0.5, 1, 0.2), c(0.5, 1, 0.2), c(-0.5, 1, 0.2)) # GIG parameters (approximate)
)

generate_data <- function(n = 500, censor_pct = 0.1) {
  g <- 3
  X <- cbind(1, runif(n, -2, 2))
  R <- cbind(1, runif(n, -2, 2))
  
  # Compute gating probabilities using softmax
  tau_mat <- do.call(rbind, true_params$tau)
  logits <- R %*% t(tau_mat)
  probs <- cbind(exp(logits), 1)
  probs <- probs / rowSums(probs)
  
  # Assign clusters
  z <- apply(probs, 1, function(p) sample(1:g, 1, prob = p))
  
  y <- numeric(n)
  for (i in 1:n) {
    j <- z[i]
    beta <- true_params$beta[[j]]
    mu <- sum(X[i, ] * beta)
    y[i] <- rnorm(1, mean = mu, sd = sqrt(true_params$sigma2[[j]]))
  }
  
  # Apply left censoring
  censor_level <- quantile(y, probs = censor_pct)
  y_obs <- pmin(y, censor_level)
  delta <- as.integer(y > censor_level)  # 1 if not censored
  
  list(y_obs = y_obs, delta = delta, X = X, R = R, z = z, censor_level = censor_level)
}


evaluate_model <- function(method = c("EM", "Bayes"), model_type = c("N", "T", "SL", "CN"),
                           censor_pct = 0.1, n_rep = 100) {
  method <- match.arg(method)
  model_type <- match.arg(model_type)
  
  ari_vec <- numeric(n_rep)
  mcr_vec <- numeric(n_rep)
  
  for (rep in 1:n_rep) {
    sim <- generate_data(n = 500, censor_pct = censor_pct)
    # Placeholder for actual fitting; replace with EM or Bayes inference
    # Here we mockup with true z (for structure) and kmeans (naive)
    pred_z <- kmeans(sim$y_obs, centers = 3)$cluster
    ari_vec[rep] <- adjustedRandIndex(pred_z, sim$z)
    mcr_vec[rep] <- mean(pred_z != sim$z)
  }
  
  data.frame(Method = method, Model = model_type,
             Censoring = censor_pct, ARI = ari_vec, MCR = mcr_vec)
}
set.seed(42)
results <- do.call(rbind, lapply(c(0.1, 0.3, 0.5), function(cp) {
  rbind(
    evaluate_model("EM", "N", cp),
    evaluate_model("Bayes", "N", cp),
    evaluate_model("EM", "T", cp),
    evaluate_model("Bayes", "T", cp),
    evaluate_model("EM", "SL", cp),
    evaluate_model("Bayes", "SL", cp),
    evaluate_model("EM", "CN", cp),
    evaluate_model("Bayes", "CN", cp)
  )
}))

results


library(ggplot2)
ggplot(results, aes(x = factor(Censoring), y = ARI, fill = Method)) +
  geom_boxplot() +
  facet_wrap(~Model, ncol = 2) +
  labs(title = "ARI across Censoring Levels", x = "Censoring %", y = "Adjusted Rand Index")

ggplot(results, aes(x = factor(Censoring), y = MCR, fill = Method)) +
  geom_boxplot() +
  facet_wrap(~Model, ncol = 2) +
  labs(title = "MCR across Censoring Levels", x = "Censoring %", y = "Misclassification Rate")