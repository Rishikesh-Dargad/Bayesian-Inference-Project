# Simulation settings
n          <- 500
MC         <- 100
delta_set  <- c(10, 20, 40, 60)
models     <- c("MoE-N-CR", "MoE-T-CR", "MoE-SL-CR",
                "MoE-CN-CR", "MoE-VG-CR", "Model6", "Model7")

# True parameters
true_beta <- list(
  c( 6,  -2,   3,  2,  -3),
  c(-3,   1,   1, -2,   2),
  c( 3,  -1.5,-2,  -2,  -1),
  c(-0.5,-2,   3,   3,  -2),
  c(-6,  -2,  -1,  -3,   2)
)
true_tau <- list(
  c( 1,   0.5, -1.5,  2),
  c( 2,  -0.2,  2,    1),
  c(-2,   3,   -2,    1),
  c(-2,  -3,   -0.7, -2)
)

# Mock‐fit function
fit_mock_model <- function(X, R, y) {
  list(
    beta = lapply(true_beta, function(tb) tb + rnorm(length(tb), 0, 0.1)),
    tau  = lapply(true_tau,  function(tt) tt + rnorm(length(tt), 0, 0.1))
  )
}

# Pre‐allocate
mmae_beta <- array(0, dim = c(length(models), length(delta_set)))
mmae_tau  <- array(0, dim = c(length(models), length(delta_set)))

# Monte Carlo loop 
for (mc in seq_len(MC)) {
  # covariates
  X <- cbind(
    rep(1,n),
    runif(n,-1,1),
    rpois(n,2),
    rnorm(n),
    runif(n,-2,2)
  )
  R <- cbind(
    rep(1,n),
    rnorm(n),
    rnorm(n),
    X[,5]   # x4
  )
  # latent component z and response y
  z <- sample(1:5, n, replace=TRUE)
  y <- sapply(seq_len(n), function(i)
    X[i,] %*% true_beta[[z[i]]] +
      rnorm(1,0,sqrt(c(1,2,2,1,3)[z[i]]))
  )
  
  # contaminate and fit
  for (d in seq_along(delta_set)) {
    δ <- delta_set[d]
    idx <- sample(n, δ)
    y2  <- y
    y2[idx] <- ifelse(y[idx] < 0,
                      runif(δ, -20, -15),
                      runif(δ,  15,  20))
    for (m in seq_along(models)) {
      fc <- fit_mock_model(X,R,y)
      ft <- fit_mock_model(X,R,y2)
      
      # MMAE(beta)
      errb <- sum(sapply(seq_along(fc$beta),
                         function(j) abs(ft$beta[[j]]-fc$beta[[j]])))
      mmae_beta[m,d] <- mmae_beta[m,d] + errb/25
      
      # MMAE(tau)
      errt <- sum(sapply(seq_along(fc$tau),
                         function(j) abs(ft$tau[[j]]-fc$tau[[j]])))
      mmae_tau[m,d]  <- mmae_tau[m,d]  + errt/16
    }
  }
}

# average over MC
mmae_beta_avg <- mmae_beta / MC
mmae_tau_avg  <- mmae_tau  / MC

# Base‑R plotting 

# a) MMAE(β) vs δ
matplot(
  delta_set, t(mmae_beta_avg),
  type = "l", lty = 1:7, col = 1:7, lwd = 2,
  xlab = expression(Contamination~delta),
  ylab = expression(MMAE(beta)),
  main = expression(MMAE(beta)~"vs."~delta)
)
legend(
  "topleft", legend = models,
  col = 1:7, lty = 1:7, bty = "n", cex = 0.8
)

# b) MMAE(τ) vs δ
matplot(
  delta_set, t(mmae_tau_avg),
  type = "l", lty = 1:7, col = 1:7, lwd = 2,
  xlab = expression(Contamination~delta),
  ylab = expression(MMAE(tau)),
  main = expression(MMAE(tau)~"vs."~delta)
)
legend(
  "topleft", legend = models,
  col = 1:7, lty = 1:7, bty = "n", cex = 0.8
)
