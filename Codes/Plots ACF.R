library(plotly)

# --- ACF of Intercept β₀₁ for Component 1 ---
df_acf_b01 <- do.call(rbind, lapply(names(fits), function(m) {
  beta <- fits[[m]]$BETA[[1]][, 1]  # Extract β₀₁ (intercept) from Component 1
  acf_res <- acf(beta, plot = FALSE)  # Compute ACF without plotting
  data.frame(model = m,
             lag   = as.numeric(acf_res$lag),
             acf   = as.numeric(acf_res$acf))  # Store results in dataframe
}))

# Plot ACF of β₀₁
plot_ly(df_acf_b01, x = ~lag, y = ~acf, color = ~model,
        type = 'scatter', mode = 'lines+markers') %>%
  layout(
    title = 'ACF of Intercept (β₀₁) – Component 1 Across Models',
    xaxis = list(title = 'Lag'),
    yaxis = list(title = 'ACF')
  )

# --- Repeat the above for β₁₁ to β₄₁ (Component 1 Coefficients) ---
df_acf_b11 <- do.call(rbind, lapply(names(fits), function(m) {
  beta <- fits[[m]]$BETA[[1]][, 2]  # Extract β₁₁
  acf_res <- acf(beta, plot = FALSE)
  data.frame(model = m, lag = as.numeric(acf_res$lag), acf = as.numeric(acf_res$acf))
}))
plot_ly(df_acf_b11, x = ~lag, y = ~acf, color = ~model,
        type = 'scatter', mode = 'lines+markers') %>%
  layout(title = 'ACF of β₁₁ – Component 1 Across Models',
         xaxis = list(title = 'Lag'),
         yaxis = list(title = 'ACF'))

df_acf_b21 <- do.call(rbind, lapply(names(fits), function(m) {
  beta <- fits[[m]]$BETA[[1]][, 3]  # Extract β₂₁
  acf_res <- acf(beta, plot = FALSE)
  data.frame(model = m, lag = as.numeric(acf_res$lag), acf = as.numeric(acf_res$acf))
}))
plot_ly(df_acf_b21, x = ~lag, y = ~acf, color = ~model,
        type = 'scatter', mode = 'lines+markers') %>%
  layout(title = 'ACF of β₂₁ – Component 1 Across Models',
         xaxis = list(title = 'Lag'),
         yaxis = list(title = 'ACF'))

df_acf_b31 <- do.call(rbind, lapply(names(fits), function(m) {
  beta <- fits[[m]]$BETA[[1]][, 4]  # Extract β₃₁
  acf_res <- acf(beta, plot = FALSE)
  data.frame(model = m, lag = as.numeric(acf_res$lag), acf = as.numeric(acf_res$acf))
}))
plot_ly(df_acf_b31, x = ~lag, y = ~acf, color = ~model,
        type = 'scatter', mode = 'lines+markers') %>%
  layout(title = 'ACF of β₃₁ – Component 1 Across Models',
         xaxis = list(title = 'Lag'),
         yaxis = list(title = 'ACF'))

df_acf_b41 <- do.call(rbind, lapply(names(fits), function(m) {
  beta <- fits[[m]]$BETA[[1]][, 5]  # Extract β₄₁
  acf_res <- acf(beta, plot = FALSE)
  data.frame(model = m, lag = as.numeric(acf_res$lag), acf = as.numeric(acf_res$acf))
}))
plot_ly(df_acf_b41, x = ~lag, y = ~acf, color = ~model,
        type = 'scatter', mode = 'lines+markers') %>%
  layout(title = 'ACF of β₄₁ – Component 1 Across Models',
         xaxis = list(title = 'Lag'),
         yaxis = list(title = 'ACF'))

# --- ACF of Coefficients for Component 2: β₀₂ to β₄₂ ---
df_acf_b02 <- do.call(rbind, lapply(names(fits), function(m) {
  beta <- fits[[m]]$BETA[[2]][, 1]  # Extract β₀₂
  acf_res <- acf(beta, plot = FALSE)
  data.frame(model = m, lag = as.numeric(acf_res$lag), acf = as.numeric(acf_res$acf))
}))
plot_ly(df_acf_b02, x = ~lag, y = ~acf, color = ~model,
        type = 'scatter', mode = 'lines+markers') %>%
  layout(title = 'ACF of Intercept (β₀₂) – Component 2 Across Models',
         xaxis = list(title = 'Lag'),
         yaxis = list(title = 'ACF'))

df_acf_b12 <- do.call(rbind, lapply(names(fits), function(m) {
  beta <- fits[[m]]$BETA[[2]][, 2]  # Extract β₁₂
  acf_res <- acf(beta, plot = FALSE)
  data.frame(model = m, lag = as.numeric(acf_res$lag), acf = as.numeric(acf_res$acf))
}))
plot_ly(df_acf_b12, x = ~lag, y = ~acf, color = ~model,
        type = 'scatter', mode = 'lines+markers') %>%
  layout(title = 'ACF of β₁₂ – Component 2 Across Models',
         xaxis = list(title = 'Lag'),
         yaxis = list(title = 'ACF'))

df_acf_b22 <- do.call(rbind, lapply(names(fits), function(m) {
  beta <- fits[[m]]$BETA[[2]][, 3]  # Extract β₂₂
  acf_res <- acf(beta, plot = FALSE)
  data.frame(model = m, lag = as.numeric(acf_res$lag), acf = as.numeric(acf_res$acf))
}))
plot_ly(df_acf_b22, x = ~lag, y = ~acf, color = ~model,
        type = 'scatter', mode = 'lines+markers') %>%
  layout(title = 'ACF of β₂₂ – Component 2 Across Models',
         xaxis = list(title = 'Lag'),
         yaxis = list(title = 'ACF'))

df_acf_b32 <- do.call(rbind, lapply(names(fits), function(m) {
  beta <- fits[[m]]$BETA[[2]][, 4]  # Extract β₃₂
  acf_res <- acf(beta, plot = FALSE)
  data.frame(model = m, lag = as.numeric(acf_res$lag), acf = as.numeric(acf_res$acf))
}))
plot_ly(df_acf_b32, x = ~lag, y = ~acf, color = ~model,
        type = 'scatter', mode = 'lines+markers') %>%
  layout(title = 'ACF of β₃₂ – Component 2 Across Models',
         xaxis = list(title = 'Lag'),
         yaxis = list(title = 'ACF'))

df_acf_b42 <- do.call(rbind, lapply(names(fits), function(m) {
  beta <- fits[[m]]$BETA[[2]][, 5]  # Extract β₄₂
  acf_res <- acf(beta, plot = FALSE)
  data.frame(model = m, lag = as.numeric(acf_res$lag), acf = as.numeric(acf_res$acf))
}))
plot_ly(df_acf_b42, x = ~lag, y = ~acf, color = ~model,
        type = 'scatter', mode = 'lines+markers') %>%
  layout(title = 'ACF of β₄₂ – Component 2 Across Models',
         xaxis = list(title = 'Lag'),
         yaxis = list(title = 'ACF'))

# --- ACF of Variance Parameters (SIGMA) ---
# Component 1 Variance (σ²₁)
df_acf_sigma1 <- do.call(rbind, lapply(names(fits), function(m) {
  sigma <- fits[[m]]$SIGMA[, 1]  # Extract σ²₁
  acf_res <- acf(sigma, plot = FALSE)
  data.frame(model = m, lag = as.numeric(acf_res$lag), acf = as.numeric(acf_res$acf))
}))
plot_ly(df_acf_sigma1, x = ~lag, y = ~acf, color = ~model,
        type = 'scatter', mode = 'lines+markers') %>%
  layout(title = 'ACF of σ²₁ (Component 1 Variance) Across Models',
         xaxis = list(title = 'Lag'),
         yaxis = list(title = 'ACF'))

# Component 2 Variance (σ²₂)
df_acf_sigma2 <- do.call(rbind, lapply(names(fits), function(m) {
  sigma <- fits[[m]]$SIGMA[, 2]  # Extract σ²₂
  acf_res <- acf(sigma, plot = FALSE)
  data.frame(model = m, lag = as.numeric(acf_res$lag), acf = as.numeric(acf_res$acf))
}))
plot_ly(df_acf_sigma2, x = ~lag, y = ~acf, color = ~model,
        type = 'scatter', mode = 'lines+markers') %>%
  layout(title = 'ACF of σ²₂ (Component 2 Variance) Across Models',
         xaxis = list(title = 'Lag'),
         yaxis = list(title = 'ACF'))
