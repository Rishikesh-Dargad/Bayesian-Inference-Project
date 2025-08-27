library(plotly)

# β₀₁
df_b01 <- do.call(rbind, lapply(names(fits), function(m) {
  b0 <- fits[[m]]$BETA[[1]][,1]
  d  <- density(b0)
  data.frame(model = m, x = d$x, y = d$y)
}))
p_b01 <- plot_ly(df_b01,
                 x=~x, y=~y,
                 color=~model,
                 type='scatter', mode='lines') %>%
  layout(
    title = 'Posterior Densities of β₀₁ Across Models',
    xaxis = list(title='β₀₁'),
    yaxis = list(title='Density')
  )
p_b01


# β₁₁
df_b11 <- do.call(rbind, lapply(names(fits), function(m) {
  b1 <- fits[[m]]$BETA[[1]][,2]
  d  <- density(b1)
  data.frame(model = m, x = d$x, y = d$y)
}))
p_b11 <- plot_ly(df_b11,
                 x=~x, y=~y,
                 color=~model,
                 type='scatter', mode='lines') %>%
  layout(
    title = 'Posterior Densities of β₁₁ Across Models',
    xaxis = list(title='β₁₁'),
    yaxis = list(title='Density')
  )
p_b11


# # β₂₁
df_b21 <- do.call(rbind, lapply(names(fits), function(m) {
  b2 <- fits[[m]]$BETA[[1]][,3]
  d  <- density(b2)
  data.frame(model = m, x = d$x, y = d$y)
}))
p_b21 <- plot_ly(df_b21,
                 x=~x, y=~y,
                 color=~model,
                 type='scatter', mode='lines') %>%
  layout(
    title = 'Posterior Densities of β₂₁ Across Models',
    xaxis = list(title='β₂₁'),
    yaxis = list(title='Density')
  )
p_b21


# β₃₁
df_b31 <- do.call(rbind, lapply(names(fits), function(m) {
  b3 <- fits[[m]]$BETA[[1]][,4]
  d  <- density(b3)
  data.frame(model = m, x = d$x, y = d$y)
}))
p_b31 <- plot_ly(df_b31,
                 x=~x, y=~y,
                 color=~model,
                 type='scatter', mode='lines') %>%
  layout(
    title = 'Posterior Densities of β₃₁ Across Models',
    xaxis = list(title='β₃₁'),
    yaxis = list(title='Density')
  )
p_b31


# β₄₁
df_b41 <- do.call(rbind, lapply(names(fits), function(m) {
  b4 <- fits[[m]]$BETA[[1]][,5]
  d  <- density(b4)
  data.frame(model = m, x = d$x, y = d$y)
}))
p_b41 <- plot_ly(df_b41,
                 x=~x, y=~y,
                 color=~model,
                 type='scatter', mode='lines') %>%
  layout(
    title = 'Posterior Densities of β₄₁ Across Models',
    xaxis = list(title='β₄₁'),
    yaxis = list(title='Density')
  )
p_b41



# β₀₂
df_b02 <- do.call(rbind, lapply(names(fits), function(m) {
  b0 <- fits[[m]]$BETA[[2]][,1]
  d  <- density(b0)
  data.frame(model = m, x = d$x, y = d$y)
}))
p_b02 <- plot_ly(df_b02, x=~x, y=~y, color=~model, type='scatter', mode='lines') %>%
  layout(title = 'Posterior Densities of β₀₂ Across Models', xaxis = list(title='β₀₂'), yaxis = list(title='Density'))
p_b02


# β₁₂
df_b12 <- do.call(rbind, lapply(names(fits), function(m) {
  b1 <- fits[[m]]$BETA[[2]][,2]
  d  <- density(b1)
  data.frame(model = m, x = d$x, y = d$y)
}))
p_b12 <- plot_ly(df_b12, x=~x, y=~y, color=~model, type='scatter', mode='lines') %>%
  layout(title = 'Posterior Densities of β₁₂ Across Models', xaxis = list(title='β₁₂'), yaxis = list(title='Density'))
p_b12


# β₂₂
df_b22 <- do.call(rbind, lapply(names(fits), function(m) {
  b2 <- fits[[m]]$BETA[[2]][,3]
  d  <- density(b2)
  data.frame(model = m, x = d$x, y = d$y)
}))
p_b22 <- plot_ly(df_b22, x=~x, y=~y, color=~model, type='scatter', mode='lines') %>%
  layout(title = 'Posterior Densities of β₂₂ Across Models', xaxis = list(title='β₂₂'), yaxis = list(title='Density'))
p_b22


# β₃₂
df_b32 <- do.call(rbind, lapply(names(fits), function(m) {
  b3 <- fits[[m]]$BETA[[2]][,4]
  d  <- density(b3)
  data.frame(model = m, x = d$x, y = d$y)
}))
p_b32 <- plot_ly(df_b32, x=~x, y=~y, color=~model, type='scatter', mode='lines') %>%
  layout(title = 'Posterior Densities of β₃₂ Across Models', xaxis = list(title='β₃₂'), yaxis = list(title='Density'))
p_b32


# β₄₂
df_b42 <- do.call(rbind, lapply(names(fits), function(m) {
  b4 <- fits[[m]]$BETA[[2]][,5]
  d  <- density(b4)
  data.frame(model = m, x = d$x, y = d$y)
}))
p_b42 <- plot_ly(df_b42, x=~x, y=~y, color=~model, type='scatter', mode='lines') %>%
  layout(title = 'Posterior Densities of β₄₂ Across Models', xaxis = list(title='β₄₂'), yaxis = list(title='Density'))
p_b42



# σ²₁
df_sig1 <- do.call(rbind, lapply(names(fits), function(m) {
  s1 <- fits[[m]]$SIGMA[,1]
  d  <- density(s1)
  data.frame(model = m, x = d$x, y = d$y)
}))
p_sig1 <- plot_ly(df_sig1, x=~x, y=~y, color=~model, type='scatter', mode='lines') %>%
  layout(title = 'Posterior Densities of σ²₁ Across Models', xaxis = list(title='σ²₁'), yaxis = list(title='Density'))
p_sig1


# σ²₂
df_sig2 <- do.call(rbind, lapply(names(fits), function(m) {
  s2 <- fits[[m]]$SIGMA[,2]
  d  <- density(s2)
  data.frame(model = m, x = d$x, y = d$y)
}))
p_sig2 <- plot_ly(df_sig2, x=~x, y=~y, color=~model, type='scatter', mode='lines') %>%
  layout(title = 'Posterior Densities of σ²₂ Across Models', xaxis = list(title='σ²₂'), yaxis = list(title='Density'))
p_sig2

