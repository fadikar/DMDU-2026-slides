# ============================================================================
# Gaussian Process Tutorial 
# ============================================================================
# This tutorial demonstrates Gaussian Processes without using any libraries
# We'll cover:
# 1. Covariance kernel functions
# 2. Sampling from unconditioned GPs
# 3. Conditioning on observations
# 4. Sampling from conditioned GPs
# ============================================================================

# ============================================================================
# 1. COVARIANCE KERNEL FUNCTIONS
# ============================================================================

# Squared Exponential (RBF) Kernel
# K(x, x') = sigma^2 * exp(-||x - x'||^2 / (2 * l^2))
squared_exponential_kernel <- function(x1, x2, sigma = 1.0, length_scale = 1.0) {
  # x1, x2: vectors or matrices of inputs
  # sigma: output scale (amplitude)
  # length_scale: controls how quickly correlation decays with distance
  
  # Compute pairwise distances
  if (is.vector(x1)) x1 <- matrix(x1, ncol = 1)
  if (is.vector(x2)) x2 <- matrix(x2, ncol = 1)
  
  # Compute squared Euclidean distances
  n1 <- nrow(x1)
  n2 <- nrow(x2)
  
  K <- matrix(0, nrow = n1, ncol = n2)
  for (i in 1:n1) {
    for (j in 1:n2) {
      dist_sq <- sum((x1[i, ] - x2[j, ])^2)
      K[i, j] <- sigma^2 * exp(-dist_sq / (2 * length_scale^2))
    }
  }
  return(K)
}

# Matern Kernel (nu = 5/2)
# Smoother than RBF, more realistic for many applications
matern_kernel <- function(x1, x2, sigma = 1.0, length_scale = 1.0, nu = 5/2) {
  # nu: smoothness parameter (higher = smoother)
  
  if (is.vector(x1)) x1 <- matrix(x1, ncol = 1)
  if (is.vector(x2)) x2 <- matrix(x2, ncol = 1)
  
  n1 <- nrow(x1)
  n2 <- nrow(x2)
  
  K <- matrix(0, nrow = n1, ncol = n2)
  
  for (i in 1:n1) {
    for (j in 1:n2) {
      dist <- sqrt(sum((x1[i, ] - x2[j, ])^2))
      
      if (dist == 0) {
        K[i, j] <- sigma^2
      } else {
        # Matern formula
        sqrt_term <- sqrt(2 * nu) * dist / length_scale
        K[i, j] <- sigma^2 * (2^(1 - nu) / gamma(nu)) * 
                   (sqrt_term^nu) * besselK(sqrt_term, nu)
      }
    }
  }
  return(K)
}

# Periodic Kernel
# K(x, x') = sigma^2 * exp(-2 * sin^2(pi * |x - x'| / period) / l^2)
periodic_kernel <- function(x1, x2, sigma = 1.0, length_scale = 1.0, period = 1.0) {
  # Useful for periodic functions
  
  if (is.vector(x1)) x1 <- matrix(x1, ncol = 1)
  if (is.vector(x2)) x2 <- matrix(x2, ncol = 1)
  
  n1 <- nrow(x1)
  n2 <- nrow(x2)
  
  K <- matrix(0, nrow = n1, ncol = n2)
  
  for (i in 1:n1) {
    for (j in 1:n2) {
      dist <- abs(x1[i, 1] - x2[j, 1])
      sin_term <- sin(pi * dist / period)
      K[i, j] <- sigma^2 * exp(-2 * sin_term^2 / length_scale^2)
    }
  }
  return(K)
}

# Linear Kernel
# K(x, x') = sigma^2 * (x^T * x' + c)
linear_kernel <- function(x1, x2, sigma = 1.0, c = 0.0) {
  
  if (is.vector(x1)) x1 <- matrix(x1, ncol = 1)
  if (is.vector(x2)) x2 <- matrix(x2, ncol = 1)
  
  K <- sigma^2 * (x1 %*% t(x2) + c)
  return(K)
}

# ============================================================================
# 2. HELPER FUNCTIONS FOR GP SAMPLING
# ============================================================================

# Cholesky decomposition with numerical stability
safe_cholesky <- function(K, jitter = 1e-6) {
  # Add small jitter to diagonal for numerical stability
  K_jittered <- K + diag(jitter, nrow(K))
  
  tryCatch({
    L <- chol(K_jittered)
    return(t(L))  # Return lower triangular
  }, error = function(e) {
    # If still fails, increase jitter
    K_jittered <- K + diag(1e-4, nrow(K))
    L <- chol(K_jittered)
    return(t(L))
  })
}

# Sample from multivariate normal: N(mu, Sigma)
mvrnorm_manual <- function(n, mu, Sigma) {
  # n: number of samples
  # mu: mean vector
  # Sigma: covariance matrix
  
  L <- safe_cholesky(Sigma)
  z <- matrix(rnorm(length(mu) * n), nrow = length(mu), ncol = n)
  samples <- mu + L %*% z
  return(t(samples))  # Return as n x d matrix
}

# ============================================================================
# 3. UNCONDITIONED GP SAMPLING
# ============================================================================

sample_unconditioned_gp <- function(x_grid, kernel_func, kernel_params, n_samples = 5) {
  # x_grid: points where we want to sample
  # kernel_func: covariance kernel function
  # kernel_params: list of parameters for the kernel
  # n_samples: number of function samples to draw
  
  # Compute covariance matrix
  K <- do.call(kernel_func, c(list(x1 = x_grid, x2 = x_grid), kernel_params))
  
  # Mean is zero for unconditioned GP
  mu <- rep(0, nrow(K))
  
  # Sample from GP
  samples <- mvrnorm_manual(n_samples, mu, K)
  
  return(samples)
}

# ============================================================================
# 4. CONDITIONED GP SAMPLING
# ============================================================================

sample_conditioned_gp <- function(x_grid, x_obs, y_obs, kernel_func, kernel_params, 
                                   noise_var = 0.01, n_samples = 5) {
  # x_grid: points where we want to sample
  # x_obs: observed input locations
  # y_obs: observed output values
  # kernel_func: covariance kernel function
  # kernel_params: list of parameters for the kernel
  # noise_var: observation noise variance
  # n_samples: number of function samples to draw
  
  # Ensure proper dimensions
  if (is.vector(x_grid)) x_grid <- matrix(x_grid, ncol = 1)
  if (is.vector(x_obs)) x_obs <- matrix(x_obs, ncol = 1)
  if (is.vector(y_obs)) y_obs <- matrix(y_obs, ncol = 1)
  
  n_grid <- nrow(x_grid)
  n_obs <- nrow(x_obs)
  
  # Compute covariance matrices
  K_ff <- do.call(kernel_func, c(list(x1 = x_grid, x2 = x_grid), kernel_params))
  K_oo <- do.call(kernel_func, c(list(x1 = x_obs, x2 = x_obs), kernel_params))
  K_fo <- do.call(kernel_func, c(list(x1 = x_grid, x2 = x_obs), kernel_params))
  
  # Add noise to observations
  K_oo_noisy <- K_oo + diag(noise_var, n_obs)
  
  # Compute conditional mean and covariance
  # mu_f|o = K_fo * (K_oo + noise)^{-1} * y_o
  # Sigma_f|o = K_ff - K_fo * (K_oo + noise)^{-1} * K_of
  
  L_oo <- safe_cholesky(K_oo_noisy)
  
  # Solve K_oo_noisy * alpha = y_obs
  alpha <- backsolve(t(L_oo), forwardsolve(L_oo, y_obs))
  
  mu_conditional <- K_fo %*% alpha
  
  # Compute conditional covariance
  # K_fo * (K_oo + noise)^{-1} * K_of
  V <- forwardsolve(L_oo, t(K_fo))
  K_conditional <- K_ff - t(V) %*% V
  
  # Sample from conditional GP
  samples <- mvrnorm_manual(n_samples, as.vector(mu_conditional), K_conditional)
  
  return(list(
    samples = samples,
    mean = as.vector(mu_conditional),
    cov = K_conditional
  ))
}

# ============================================================================
# 5. VISUALIZATION FUNCTIONS
# ============================================================================

plot_gp_samples <- function(x_grid, samples, x_obs = NULL, y_obs = NULL, 
                            title = "", ylim = NULL) {
  # Plot GP samples with optional observations
  
  n_samples <- nrow(samples)
  
  # Determine y limits
  if (is.null(ylim)) {
    ylim <- range(samples) * 1.2
  }
  
  # Plot first sample to initialize
  plot(x_grid, samples[1, ], type = 'l', col = rgb(0.2, 0.2, 0.8, 0.5),
       ylim = ylim, xlab = 'x', ylab = 'z(x)', main = title, lwd = 1.5)
  
  # Plot remaining samples
  if (n_samples > 1) {
    for (i in 2:n_samples) {
      lines(x_grid, samples[i, ], col = rgb(0.2, 0.2, 0.8, 0.5), lwd = 1.5)
    }
  }
  
  # Plot observations if provided
  if (!is.null(x_obs) && !is.null(y_obs)) {
    points(x_obs, y_obs, col = 'red', pch = 19, cex = 2)
  }
  
  grid()
}

plot_gp_posterior <- function(x_grid, mean, cov, x_obs = NULL, y_obs = NULL,
                              title = "", ylim = NULL) {
  # Plot GP posterior with uncertainty bands
  
  # Compute standard deviations
  std <- sqrt(diag(cov))
  
  # Determine y limits
  if (is.null(ylim)) {
    ylim <- range(c(mean - 2*std, mean + 2*std)) * 1.2
  }
  
  # Plot mean
  plot(x_grid, mean, type = 'l', col = 'blue', lwd = 2,
       ylim = ylim, xlab = 'x', ylab = 'z(x)', main = title)
  
  # Plot uncertainty bands (±2 std)
  polygon(c(x_grid, rev(x_grid)), 
          c(mean + 2*std, rev(mean - 2*std)),
          col = rgb(0.2, 0.2, 0.8, 0.2), border = NA)
  
  # Replot mean
  lines(x_grid, mean, col = 'blue', lwd = 2)
  
  # Plot observations if provided
  if (!is.null(x_obs) && !is.null(y_obs)) {
    points(x_obs, y_obs, col = 'red', pch = 19, cex = 2)
  }
  
  grid()
}

# ============================================================================
# 6. MAIN TUTORIAL
# ============================================================================

if(0){
# Set random seed for reproducibility
set.seed(42)

# Create x grid
x_grid <- seq(-5, 5, length.out = 100)

# Define observation points
x_obs <- c(-4, -1, 2, 4)
y_obs <- sin(x_obs) + rnorm(length(x_obs), 0, 0.1)

# ============================================================================
# PART 1: UNCONDITIONED GP SAMPLES
# ============================================================================

cat("\n=== PART 1: UNCONDITIONED GP SAMPLES ===\n")
cat("Drawing samples from GP prior with different kernels\n\n")

# Create a 2x2 plot layout
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# 1. Squared Exponential Kernel
cat("1. Squared Exponential (RBF) Kernel\n")
samples_se <- sample_unconditioned_gp(
  x_grid, 
  squared_exponential_kernel,
  list(sigma = 1.0, length_scale = 1.0),
  n_samples = 5
)
plot_gp_samples(x_grid, samples_se, 
                title = "Squared Exponential Kernel\n(smooth, infinitely differentiable)")

# 2. Matern Kernel
cat("2. Matern Kernel (nu = 5/2)\n")
samples_matern <- sample_unconditioned_gp(
  x_grid,
  matern_kernel,
  list(sigma = 1.0, length_scale = 1.0, nu = 5/2),
  n_samples = 5
)
plot_gp_samples(x_grid, samples_matern,
                title = "Matern Kernel (nu=5/2)\n(less smooth than RBF)")

# 3. Periodic Kernel
cat("3. Periodic Kernel\n")
samples_periodic <- sample_unconditioned_gp(
  x_grid,
  periodic_kernel,
  list(sigma = 1.0, length_scale = 1.0, period = 2.0),
  n_samples = 5
)
plot_gp_samples(x_grid, samples_periodic,
                title = "Periodic Kernel\n(repeating patterns)")

# 4. Linear Kernel
cat("4. Linear Kernel\n")
samples_linear <- sample_unconditioned_gp(
  x_grid,
  linear_kernel,
  list(sigma = 1.0, c = 0.0),
  n_samples = 5
)
plot_gp_samples(x_grid, samples_linear,
                title = "Linear Kernel\n(polynomial functions)")

# ============================================================================
# PART 2: CONDITIONED GP SAMPLES
# ============================================================================

cat("\n=== PART 2: CONDITIONED GP SAMPLES ===\n")
cat("Drawing samples from GP posterior given observations\n\n")

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# 1. Squared Exponential - Conditioned
cat("1. Squared Exponential Kernel (Conditioned)\n")
result_se <- sample_conditioned_gp(
  x_grid, x_obs, y_obs,
  squared_exponential_kernel,
  list(sigma = 1.0, length_scale = 1.0),
  noise_var = 0.01,
  n_samples = 5
)
plot_gp_samples(x_grid, result_se$samples, x_obs, y_obs,
                title = "Squared Exponential (Conditioned)\n(passes through observations)")

# 2. Matern - Conditioned
cat("2. Matern Kernel (Conditioned)\n")
result_matern <- sample_conditioned_gp(
  x_grid, x_obs, y_obs,
  matern_kernel,
  list(sigma = 1.0, length_scale = 1.0, nu = 5/2),
  noise_var = 0.01,
  n_samples = 5
)
plot_gp_samples(x_grid, result_matern$samples, x_obs, y_obs,
                title = "Matern Kernel (Conditioned)")

# 3. Periodic - Conditioned
cat("3. Periodic Kernel (Conditioned)\n")
result_periodic <- sample_conditioned_gp(
  x_grid, x_obs, y_obs,
  periodic_kernel,
  list(sigma = 1.0, length_scale = 1.0, period = 2.0),
  noise_var = 0.01,
  n_samples = 5
)
plot_gp_samples(x_grid, result_periodic$samples, x_obs, y_obs,
                title = "Periodic Kernel (Conditioned)")

# 4. Linear - Conditioned
cat("4. Linear Kernel (Conditioned)\n")
result_linear <- sample_conditioned_gp(
  x_grid, x_obs, y_obs,
  linear_kernel,
  list(sigma = 1.0, c = 0.0),
  noise_var = 0.01,
  n_samples = 5
)
plot_gp_samples(x_grid, result_linear$samples, x_obs, y_obs,
                title = "Linear Kernel (Conditioned)")

# ============================================================================
# PART 3: POSTERIOR MEAN AND UNCERTAINTY
# ============================================================================

cat("\n=== PART 3: POSTERIOR MEAN AND UNCERTAINTY ===\n")
cat("Showing posterior mean with 95% confidence bands\n\n")

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# 1. Squared Exponential - Posterior
cat("1. Squared Exponential Posterior\n")
plot_gp_posterior(x_grid, result_se$mean, result_se$cov, x_obs, y_obs,
                  title = "Squared Exponential\n(Mean ± 2 Std Dev)")

# 2. Matern - Posterior
cat("2. Matern Posterior\n")
plot_gp_posterior(x_grid, result_matern$mean, result_matern$cov, x_obs, y_obs,
                  title = "Matern Kernel\n(Mean ± 2 Std Dev)")

# 3. Periodic - Posterior
cat("3. Periodic Posterior\n")
plot_gp_posterior(x_grid, result_periodic$mean, result_periodic$cov, x_obs, y_obs,
                  title = "Periodic Kernel\n(Mean ± 2 Std Dev)")

# 4. Linear - Posterior
cat("4. Linear Posterior\n")
plot_gp_posterior(x_grid, result_linear$mean, result_linear$cov, x_obs, y_obs,
                  title = "Linear Kernel\n(Mean ± 2 Std Dev)")
}