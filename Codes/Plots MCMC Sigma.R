library(plotly)

# -----------------------------------------------------
# MCMC Trace Plots for Variance Parameters (σ²₁ and σ²₂)
# -----------------------------------------------------

# Helper function to generate trace plots
plot_trace <- function(data, iter_col, value_col, model_name, var_label, dist_name) {
  plot_ly(data, x = as.formula(paste0("~", iter_col)), 
          y = as.formula(paste0("~", value_col)), 
          type = 'scatter', mode = 'lines', name = paste0(var_label, " (", dist_name, ")")) %>%
    layout(
      title = paste0("Trace of ", var_label, " for ", model_name, " Model"),
      xaxis = list(title = 'Iteration'),
      yaxis = list(title = var_label)
    )
}

# Define iteration indices
sigma_iter <- (burnin + 1):nsim

# -----------------------
# Trace plots for σ²₁
# -----------------------

# Normal model
sigma_normal <- as.data.frame(fits$N$SIGMA)
sigma_normal$Iter <- sigma_iter
plot_trace(sigma_normal, "Iter", "V1", "Normal", "σ²₁", "Normal")

# Laplace model
sigma_laplace <- as.data.frame(fits$La$SIGMA)
sigma_laplace$Iter <- sigma_iter
plot_trace(sigma_laplace, "Iter", "V1", "Laplace", "σ²₁", "Laplace")

# t-distribution model
sigma_t <- as.data.frame(fits$T$SIGMA)
sigma_t$Iter <- sigma_iter
plot_trace(sigma_t, "Iter", "V1", "t-distribution", "σ²₁", "t-distribution")

# Slash model
sigma_slash <- as.data.frame(fits$Slash$SIGMA)
sigma_slash$Iter <- sigma_iter
plot_trace(sigma_slash, "Iter", "V1", "Slash", "σ²₁", "Slash")

# Variance-Gamma model
sigma_vg <- as.data.frame(fits$VG$SIGMA)
sigma_vg$Iter <- sigma_iter
plot_trace(sigma_vg, "Iter", "V1", "Variance-Gamma", "σ²₁", "VG")

# Two-piece Normal (TIN) model
sigma_tin <- as.data.frame(fits$TIN$SIGMA)
sigma_tin$Iter <- sigma_iter
plot_trace(sigma_tin, "Iter", "V1", "Two-piece Normal (TIN)", "σ²₁", "TIN")

# Contaminated-Normal (CN) model
sigma_cn <- as.data.frame(fits$CN$SIGMA)
sigma_cn$Iter <- sigma_iter
plot_trace(sigma_cn, "Iter", "V1", "Contaminated-Normal (CN)", "σ²₁", "CN")


# -----------------------
# Trace plots for σ²₂
# -----------------------

# Use the same iteration range as σ²₁
sigma_iter <- (burnin + 1):nsim

# Normal model
sigma_normal <- as.data.frame(fits$N$SIGMA)
sigma_normal$Iter <- sigma_iter
plot_trace(sigma_normal, "Iter", "V2", "Normal", "σ²₂", "Normal")

# Laplace model
sigma_laplace <- as.data.frame(fits$La$SIGMA)
sigma_laplace$Iter <- sigma_iter
plot_trace(sigma_laplace, "Iter", "V2", "Laplace", "σ²₂", "Laplace")

# t-distribution model
sigma_t <- as.data.frame(fits$T$SIGMA)
sigma_t$Iter <- sigma_iter
plot_trace(sigma_t, "Iter", "V2", "t-distribution", "σ²₂", "t-distribution")

# Slash model
sigma_slash <- as.data.frame(fits$Slash$SIGMA)
sigma_slash$Iter <- sigma_iter
plot_trace(sigma_slash, "Iter", "V2", "Slash", "σ²₂", "Slash")

# Variance-Gamma model
sigma_vg <- as.data.frame(fits$VG$SIGMA)
sigma_vg$Iter <- sigma_iter
plot_trace(sigma_vg, "Iter", "V2", "Variance-Gamma", "σ²₂", "VG")

# Two-piece Normal (TIN) model
sigma_tin <- as.data.frame(fits$TIN$SIGMA)
sigma_tin$Iter <- sigma_iter
plot_trace(sigma_tin, "Iter", "V2", "Two-piece Normal (TIN)", "σ²₂", "TIN")

# Contaminated-Normal (CN) model
sigma_cn <- as.data.frame(fits$CN$SIGMA)
sigma_cn$Iter <- sigma_iter
plot_trace(sigma_cn, "Iter", "V2", "Contaminated-Normal (CN)", "σ²₂", "CN")
