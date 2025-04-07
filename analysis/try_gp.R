# Load libraries
library(ggplot2)
library(dplyr)

# Define RBF kernel function
rbf_kernel <- function(x1, x2, lengthscale) {
  exp(- (outer(x1, x2, "-")^2) / (2 * lengthscale^2))
}

# Generate input values
X <- seq(-3, 3, length.out = 100)

# Compute kernel values for different lengthscales
K_small <- rbf_kernel(X, 0, lengthscale = 0.5)
K_medium <- rbf_kernel(X, 0, lengthscale = 1.0)
K_large <- rbf_kernel(X, 0, lengthscale = 2.0)

# Create a dataframe for plotting
df <- data.frame(
  X = rep(X, 3),
  K_value = c(K_small, K_medium, K_large),
  Lengthscale = rep(c("l = 0.5", "l = 1.0", "l = 2.0"), each = length(X))
)

# Plot the RBF kernel for different lengthscales
ggplot(df, aes(x = X, y = K_value, color = Lengthscale)) +
  geom_line(size = 1) +
  labs(title = "Effect of Lengthscale (l) in the RBF Kernel",
       x = "Distance from Reference Point",
       y = "Kernel Value (Correlation)") +
  theme_minimal() +
  theme(legend.position = "top")


set.seed(123)

# Generate training data
X_train <- seq(-5, 5, length.out = 1000)
Y_train <- sin(X_train) + rnorm(length(X_train), sd = 0.2)  # Noisy sine function

# Plot training data
plot(X_train, Y_train, pch = 16, col = "black", main = "Training Data",
     xlab = "X", ylab = "Y")

library(kernlab)
# Fit GP with RBF kernel
gp_rbf <- gausspr(X_train, Y_train, kernel = "rbfdot")

# Fit GP with Matérn kernel (approximation: inverse multiquadric kernel)
gp_matern <- gausspr(X_train, Y_train, kernel = "laplacedot")

# Fit GP with Linear kernel
gp_linear <- gausspr(X_train, Y_train, kernel = "vanilladot")

# Fit GP with Polynomial kernel (degree 3)
gp_poly <- gausspr(X_train, Y_train, kernel = "polydot", kpar = list(degree = 3))


# Make predictions at test points
Y_pred_rbf <- predict(gp_rbf, X_test)
Y_pred_matern <- predict(gp_matern, X_test)
Y_pred_linear <- predict(gp_linear, X_test)
Y_pred_poly <- predict(gp_poly, X_test)

compute_goodness_of_fit <- function(X, Y, kernel_function) {
  n <- length(Y)
  
  # Compute covariance matrix K using the chosen kernel
  K <- kernel_function(X, X) + diag(1e-6, n)  # Add jitter for numerical stability
  
  # Compute inverse of K
  K_inv <- solve(K)
  
  # Compute quadratic form Y^T K^{-1} Y
  fit_quality <- as.numeric(t(Y) %*% K_inv %*% Y)
  
  # Compute determinant log|K|
  K_det <- determinant(K, logarithm = TRUE)$modulus
  
  # Compute Log Marginal Likelihood (LML)
  lml <- -0.5 * fit_quality - 0.5 * K_det - (n / 2) * log(2 * pi)
  
  return(list(fit_quality=fit_quality, lml=lml))
}

# Define RBF Kernel
rbf_kernel <- function(x1, x2, lengthscale = 1, variance = 1) {
  variance * exp(- (outer(x1, x2, "-")^2) / (2 * lengthscale^2))
}

# Define Matérn Kernel
matern_kernel <- function(x1, x2, lengthscale = 1, variance = 1) {
  variance * (1 + abs(outer(x1, x2, "-")) / lengthscale) * 
    exp(-abs(outer(x1, x2, "-")) / lengthscale)
}

# Define Linear Kernel
linear_kernel <- function(x1, x2) {
  outer(x1, x2, "*")  # Dot product (linear)
}

# Define Polynomial Kernel
poly_kernel <- function(x1, x2, degree = 3, c = 1) {
  (outer(x1, x2, "*") + c)^degree
}
# Generate example data
set.seed(123)
X_train <- seq(-5, 5, length.out = 10)
Y_train <- sin(X_train) + rnorm(length(X_train), sd = 0.2)

# Generate test data (true function without noise)
X_test <- seq(-6, 6, length.out = 100)
Y_test_true <- sin(X_test)+ rnorm(length(X_train), sd = 0.2)  # True function values

# Compute for each kernel
results_rbf <- compute_goodness_of_fit(X_train, Y_train, rbf_kernel)
results_matern <- compute_goodness_of_fit(X_train, Y_train, matern_kernel)
results_linear <- compute_goodness_of_fit(X_train, Y_train, linear_kernel)
results_poly <- compute_goodness_of_fit(X_train, Y_train, poly_kernel)

# Store in a DataFrame
goodness_of_fit_results <- data.frame(
  Kernel = c("RBF", "Matérn", "Linear", "Polynomial"),
  Fit_Quality = c(results_rbf$fit_quality, results_matern$fit_quality, 
                  results_linear$fit_quality, results_poly$fit_quality),
  Log_Marginal_Likelihood = c(results_rbf$lml, results_matern$lml, 
                              results_linear$lml, results_poly$lml)
)

# Print results
print(goodness_of_fit_results)

# Convert to data frame for ggplot
plot_data <- data.frame(
  X_test = rep(X_test, 4),
  Y_pred = c(Y_pred_rbf, Y_pred_matern, Y_pred_linear, Y_pred_poly),
  Kernel = rep(c("RBF", "Matérn", "Linear", "Polynomial"), each=length(X_test))
)

# True test function values (for reference)
test_data <- data.frame(X_test = X_test, Y_test = Y_test_true)

# Convert to data frame for ggplot
plot_data <- data.frame(
  X_test = rep(X_test, 4),
  Y_pred = c(Y_pred_rbf, Y_pred_matern, Y_pred_linear, Y_pred_poly),
  Kernel = rep(c("RBF", "Matérn", "Linear", "Polynomial"), each=length(X_test))
)

train_data <- data.frame(X_train = X_train, Y_train = Y_train)
# Plot results using ggplot
ggplot(plot_data, aes(x=X_test, y=Y_pred, color=Kernel)) +
  geom_line(size=1) +  # Plot GP predictions
  geom_point(data=test_data, aes(x=X_test, y=Y_test), color="black", size=1.5) +  # True test function values
  labs(title="GP Fit with Different Kernels", x="X", y="Y") +
  theme_minimal() +
  theme(legend.position="top")
