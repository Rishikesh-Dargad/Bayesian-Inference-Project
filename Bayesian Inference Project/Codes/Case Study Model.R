## Load "Used Function.R" and "Main Function.R" before running this code

library(truncnorm)   
library(GIGrvg)     
library(mvtnorm)    
library(UPG)        
library(coda)        
library(loo)        

wage <- read.csv("wage data.csv", stringsAsFactors = FALSE)

# response & censoring indicator
Y  <- wage$hours / 1000
cc <- as.integer(wage$hours == 0)

# expert covariates
x1 <- wage$age
x2 <- log(wage$fincome)
x3 <- wage$unemp
x4 <- (wage$fincome - wage$wage * wage$hours) / 1000
X  <- cbind(1, x1, x2, x3, x4)

# gating covariates
R  <- cbind(1, x2, x4)

n <- length(Y)
G <- 2

# PRIORS & MCMC SETTINGS
nsim   <- 30000
burnin <- 10000
p      <- ncol(X)
q1     <- ncol(R)

mu0     <- rep(0, p)
Sigma0  <- diag(1000, p)
alfa1   <- 0.001
alfa2   <- 0.001

# hyper‑priors for ν (for T, Slash, VG, TIN, CN)
nu01     <- 2
nu02     <- 0.1
nuprior  <- "Gama"
numethod <- "Hierar"
hypernu0 <- c(nu01, nu02)

z_init <- kmeans(Y, centers = G)$cluster

# FIT MoE–SMN–CR FOR EACH DISTRIBUTION 
models <- list(
  N     = "N",
  La    = "La",
  T     = "T",
  Slash = "Slash",
  VG    = "VG",
  TIN   = "TIN",
  CN    = "CN"
)

fits <- list()
for(m in names(models)){
  cat("Fitting MoE-", m, "-CR ...\n", sep = "")
  fits[[m]] <- Gibbs.MoE.Cr(
    cc        = cc,
    y         = Y,
    r         = R,
    x         = X,
    Cens.type = "Left",
    G         = G,
    type      = models[[m]],
    mu0       = mu0,
    Sigma0    = Sigma0,
    alfa1     = alfa1,
    alfa2     = alfa2,
    nu01      = if(m %in% c("T","Slash","VG","TIN","CN")) nu01     else NULL,
    nu02      = if(m %in% c("T","Slash","VG","TIN","CN")) nu02     else NULL,
    numethod  = if(m %in% c("T","VG","TIN","CN"))         numethod else NULL,
    nuprior   = if(m %in% c("T","VG","TIN","CN"))         nuprior  else NULL,
    hypernu0  = if(m %in% c("T","Slash","VG","TIN","CN")) hypernu0 else NULL,
    nsim      = nsim,
    burnin    = burnin,
    pivotal.z = z_init
  )
}

# EXTRACT POSTERIOR SUMMARIES 
all_summaries <- list()
for(m in names(fits)){
  fit <- fits[[m]]
  # collect in one big matrix: BETA1, BETA2, TAU1, TAU2, SIGMA, NU (if any), GAMMA (if CN)
  mats <- list(
    # expert regressions: β₀₁…β₄₁, then β₀₂…β₄₂
    BETA1 = fit$BETA[[1]],
    BETA2 = fit$BETA[[2]],
    # gating for component 1 only: τ₀₁, τ₁₁, τ₂₁
    TAU1  = fit$TAU[[1]],
    # component variances σ²₁, σ²₂
    SIGMA = fit$SIGMA
  )
  # degrees‑of‑freedom if present
  if(!is.null(fit$NU) && ncol(fit$NU)==2) {
    mats$NU <- fit$NU
  }
  # contamination weights only for CN
  if(m=="CN" && !is.null(fit$GAMMA)) {
    mats$GAMMA <- fit$GAMMA
  }
  
  # bind into one matrix
  Mbig <- do.call(cbind, mats)
  
  param_names <- c(
    # β₀₁…β₄₁
    paste0("beta", 0:4, "1"),
    # β₀₂…β₄₂
    paste0("beta", 0:4, "2"),
    # σ²₁, σ²₂
    "sigma2_1", "sigma2_2",
    # τ₀₁, τ₁₁, τ₂₁
    paste0("tau", 0:2, "1"),
    # ν₁, ν₂ if they exist
    if(!is.null(fit$NU) && ncol(fit$NU)==2) c("nu1","nu2") else NULL,
    # γ₁, γ₂ for contaminated‐normal
    if(m=="CN") c("gamma1","gamma2") else NULL
  )
  colnames(Mbig) <- param_names
  
  # now turn into an mcmc and summarize
  mc   <- as.mcmc(Mbig)
  smry <- summary(mc)$statistics[,c("Mean","SD")]
  hpd  <- HPDinterval(mc, prob=0.95)
  colnames(hpd) <- c("HPDlo","HPDhi")
  summ <- cbind(smry, hpd)
  
  
  cat("\n--- Posterior summary for MoE-", m, "-CR ---\n", sep = "")
  print(round(summ,4))
  
  all_summaries[[m]] <- summ
  write.csv(summ,
            file = paste0("PostSummary_MoE-", m, "-CR.csv"),
            row.names = TRUE)
}

# COMPUTE MODEL SELECTION CRITERIA 
# LPML, DIC, EAIC, EBIC, WAIC1, WAIC2.

model_criteria <- data.frame(
  Model = names(fits),
  LPML  = NA_real_,
  DIC   = NA_real_,
  EAIC  = NA_real_,
  EBIC  = NA_real_,
  WAIC1 = NA_real_,
  WAIC2 = NA_real_,
  stringsAsFactors = FALSE
)

for(m in names(fits)){
  fit <- fits[[m]]
  S   <- nsim - burnin
  log_lik <- matrix(NA, nrow=S, ncol=n)
  
  # for each posterior draw s, recompute log p(y_i | θ^(s))
  for(s in 1:S){
    # extract parameter vectors at iteration s
    # BETA:
    Beta1 <- fit$BETA[[1]][s, ]
    Beta2 <- fit$BETA[[2]][s, ]
    # TAU:
    Tau1  <- fit$TAU[[1]][s, ]
    Tau2  <- fit$TAU[[2]][s, ]
    # SIGMA:
    sig2  <- fit$SIGMA[s, ]
    # nu (if present):
    nu    <- if(!is.null(fit$NU)) fit$NU[s,] else NULL
    # gamma (if CN):
    gamma <- if(m=="CN") fit$GAMMA[s,] else NULL
    
    # compute gating probabilities π_i1, π_i2:
    η1 <- as.numeric(R %*% Tau1)
    η2 <- as.numeric(R %*% Tau2)
    expη <- cbind(exp(η1), exp(η2))
    π   <- expη / rowSums(expη)
    
    f1 <- switch(m,
                 N     = dnorm(Y, mean = as.numeric(X %*% Beta1), sd = sqrt(sig2[1])),
                 La    = dLaplace(Y, mu = as.numeric(X %*% Beta1), sigma = sqrt(sig2[1]), log=FALSE),
                 T     = exp(dT.c(cc=0, y=Y, mu = as.numeric(X %*% Beta1), sigma2=sig2[1], nu=nu[1], log=TRUE, Cens.type="Non")),
                 Slash = dSlash(Y, mu = as.numeric(X %*% Beta1), sigma2=sig2[1], nu=nu[1], log=FALSE),
                 VG    = dVG(Y, mu = as.numeric(X %*% Beta1), sigma2=sig2[1], nu=nu[1], log=FALSE),
                 TIN   = dTIN(Y, mu = as.numeric(X %*% Beta1), sigma2=sig2[1], nu=nu[1], log=FALSE),
                 CN    = dCN(Y, mu = as.numeric(X %*% Beta1), sigma2=sig2[1], nu=nu[1], gama=gamma[1], log=FALSE)
    )
    f2 <- switch(m,
                 N     = dnorm(Y, mean = as.numeric(X %*% Beta2), sd = sqrt(sig2[2])),
                 La    = dLaplace(Y, mu = as.numeric(X %*% Beta2), sigma = sqrt(sig2[2]), log=FALSE),
                 T     = exp(dT.c(cc=0, y=Y, mu = as.numeric(X %*% Beta2), sigma2=sig2[2], nu=nu[2], log=TRUE, Cens.type="Non")),
                 Slash = dSlash(Y, mu = as.numeric(X %*% Beta2), sigma2=sig2[2], nu=nu[2], log=FALSE),
                 VG    = dVG(Y, mu = as.numeric(X %*% Beta2), sigma2=sig2[2], nu=nu[2], log=FALSE),
                 TIN   = dTIN(Y, mu = as.numeric(X %*% Beta2), sigma2=sig2[2], nu=nu[2], log=FALSE),
                 CN    = dCN(Y, mu = as.numeric(X %*% Beta2), sigma2=sig2[2], nu=nu[2], gama=gamma[2], log=FALSE)
    )
    
    # mixture density
    mix_f <- π[,1] * f1 + π[,2] * f2
    log_lik[s, ] <- log(mix_f + .Machine$double.eps)
  }
  
  # WAIC 
  wa <- waic(log_lik)
  WAIC1 <- wa$estimates["waic","Estimate"]
  
  # WAIC2
  p_waic2 <- sum(apply(log_lik, 2, var))
  lppd    <- sum(log(colMeans(exp(log_lik))))
  WAIC2   <- -2*(lppd - p_waic2)
  
  # LPML
  cpo <- 1 / colMeans(exp(-log_lik))
  LPML <- sum(log(cpo))
  
  # DIC
  deviance  <- -2 * rowSums(log_lik)
  Dbar      <- mean(deviance)
  pD        <- var(deviance)/2
  DIC       <- Dbar + pD
  
  # EAIC
  EAIC <- Dbar + 2*pD
  
  # EBIC
  EBIC <- EAIC + log(n)*pD
  
  model_criteria[ model_criteria$Model==m , 2:7 ] <-
    round(c(LPML, DIC, EAIC, EBIC, WAIC1, WAIC2), 3)
}

cat("\n--- Model selection criteria ---\n")
print(model_criteria)

write.csv(model_criteria,
          file = "Model_Selection_Criteria.csv",
          row.names = FALSE)
