
# R script for Figure 2 (on Comparative Statics)

library(ggplot2)  
if (!requireNamespace("patchwork", quietly = TRUE)) {  
  install.packages("patchwork", repos="https://cran.rstudio.com/")  
}  
library(patchwork)  

## Define parameter ranges and representative values  
mu_seq <- seq(0.1, 0.99, length.out = 200)  
gamma_seq <- seq(0.01, 0.99, length.out = 200)  
P_seq <- seq(0.1, 10, length.out = 200)  
gamma_vals <- c(0.2, 0.5, 0.8)  
mu_vals <- c(0.2, 0.5, 0.8)  
P_vals <- c(0.5, 1, 2)  

get_ylim <- function(y) {  
  q <- quantile(y, probs = c(0.01, 0.99), na.rm = TRUE)  
  c(q[1], q[2])  
}  

## 1. ∂f/∂P vs μ for several γ  
plot1_data <- do.call(rbind, lapply(gamma_vals, function(gamma) {  
  data.frame(  
    mu = mu_seq,  
    dfdP = (gamma - 1) / ((mu_seq - 1) * (1 + gamma)),  
    gamma = as.factor(gamma)  
  )  
}))  
p1 <- ggplot(plot1_data, aes(x = mu, y = dfdP, color = gamma)) +  
  geom_line(linewidth = 1) +  
  labs(title = "∂f/∂P vs μ ", x = expression(mu), y = expression(partialdiff*f/P), color = expression(gamma)) +  
  theme_minimal() +  
  coord_cartesian(ylim = get_ylim(plot1_data$dfdP))  

## 2. ∂f/∂γ vs μ for several P (γ = 0.5)  
plot2_data <- do.call(rbind, lapply(P_vals, function(P) {  
  gamma <- 0.5  
  data.frame(  
    mu = mu_seq,  
    dfdgamma = P * ((mu_seq - 1) - (gamma - 1)) / ((mu_seq - 1) * (1 + gamma)^2),  
    P = as.factor(P)  
  )  
}))  
p2 <- ggplot(plot2_data, aes(x = mu, y = dfdgamma, color = P)) +  
  geom_line(linewidth = 1) +  
  labs(title = "∂f/∂γ vs μ (γ=0.5)", x = expression(mu), y = expression(partialdiff*f/gamma), color = "P") +  
  theme_minimal() +  
  coord_cartesian(ylim = get_ylim(plot2_data$dfdgamma))  

## 3. ∂f/∂μ vs γ for several P (μ = 0.5)  
plot3_data <- do.call(rbind, lapply(P_vals, function(P) {  
  mu <- 0.5  
  data.frame(  
    gamma = gamma_seq,  
    dfdmu = -P * (gamma_seq - 1) / ((1 + gamma_seq) * (mu - 1)^2),  
    P = as.factor(P)  
  )  
}))  
p3 <- ggplot(plot3_data, aes(x = gamma, y = dfdmu, color = P)) +  
  geom_line(linewidth = 1) +  
  labs(title = "∂f/∂μ vs γ (μ=0.5)", x = expression(gamma), y = expression(partialdiff*f/mu), color = "P") +  
  theme_minimal() +  
  coord_cartesian(ylim = get_ylim(plot3_data$dfdmu))  

## 4. ∂f/∂P vs γ for several μ  
plot4_data <- do.call(rbind, lapply(mu_vals, function(mu) {  
  data.frame(  
    gamma = gamma_seq,  
    dfdP = (gamma_seq - 1) / ((mu - 1) * (1 + gamma_seq)),  
    mu = as.factor(mu)  
  )  
}))  
p4 <- ggplot(plot4_data, aes(x = gamma, y = dfdP, color = mu)) +  
  geom_line(linewidth = 1) +  
  labs(title = "∂f/∂P vs γ", x = expression(gamma), y = expression(partialdiff*f/P), color = expression(mu)) +  
  theme_minimal() +  
  coord_cartesian(ylim = get_ylim(plot4_data$dfdP))  

## 5. ∂f/∂μ vs P for several γ (μ = 0.5)  
plot5_data <- do.call(rbind, lapply(gamma_vals, function(gamma) {  
  mu <- 0.5  
  data.frame(  
    P = P_seq,  
    dfdmu = -P_seq * (gamma - 1) / ((1 + gamma) * (mu - 1)^2),  
    gamma = as.factor(gamma)  
  )  
}))  
p5 <- ggplot(plot5_data, aes(x = P, y = dfdmu, color = gamma)) +  
  geom_line(linewidth = 1) +  
  labs(title = "∂f/∂μ vs P (μ=0.5)", x = "P", y = expression(partialdiff*f/mu), color = expression(gamma)) +  
  theme_minimal() +  
  coord_cartesian(ylim = get_ylim(plot5_data$dfdmu))  

## 6. ∂f/∂γ vs P for several μ (γ = 0.5)  
plot6_data <- do.call(rbind, lapply(mu_vals, function(mu) {  
  gamma <- 0.5  
  data.frame(  
    P = P_seq,  
    dfdgamma = P_seq * ((mu - 1) - (gamma - 1)) / ((mu - 1) * (1 + gamma)^2),  
    mu = as.factor(mu)  
  )  
}))  
p6 <- ggplot(plot6_data, aes(x = P, y = dfdgamma, color = mu)) +  
  geom_line(linewidth = 1) +  
  labs(title = "∂f/∂γ vs P (γ=0.5)", x = "P", y = expression(partialdiff*f/gamma), color = expression(mu)) +  
  theme_minimal() +  
  coord_cartesian(ylim = get_ylim(plot6_data$dfdgamma))  

## Arrange all six plots in a 2x3 grid
(p4 | p2 | p1) /  
  (p6 | p3 | p5)  




# R script for Figure 3 (3D visual with slider)

install.packages("plotly")

gamma_vals <- seq(0.01, 0.99, length.out = 40)
P_vals <- seq(0.1, 1.0, length.out = 40)
mu_vals <- seq(0.1, 0.99, by = 0.05)

surface_list <- lapply(mu_vals, function(mu_val) {
  grid <- expand.grid(gamma = gamma_vals, P = P_vals)
  grid$f <- with(grid, P * (gamma - 1) / ((mu_val - 1) * (1 + gamma)))
  f_matrix <- matrix(grid$f, nrow = length(P_vals), ncol = length(gamma_vals))
  list(z = f_matrix, mu = mu_val)
})

surface_list[[1]]$z[1:6, 1:6]

library(plotly)

frames <- lapply(seq_along(mu_vals), function(i) {
  list(
    data = list(
      list(
        x = gamma_vals,
        y = P_vals,
        z = surface_list[[i]]$z,
        type = 'surface',
        colorscale = 'Jet',
        showscale = TRUE
      )
    ),
    name = as.character(mu_vals[i])
  )
})

fig <- plot_ly(
  x = ~gamma_vals, y = ~P_vals, z = ~surface_list[[1]]$z,
  type = 'surface',
  colorscale = 'Jet',
  showscale = TRUE
)

fig <- fig %>% layout(
  scene = list(
    xaxis = list(title = 'gamma'),
    yaxis = list(title = 'P'),
    zaxis = list(title = 'f(gamma, mu, P)')
  ),
  title = 'Interactive 3D Surface Plot of f(gamma, mu, P) with mu Slider',
  sliders = list(
    list(
      active = 0,
      currentvalue = list(prefix = 'mu = '),
      pad = list(t = 50),
      steps = lapply(seq_along(mu_vals), function(i) {
        list(
          method = 'animate',
          args = list(list(as.character(mu_vals[i])), list(mode = 'immediate', frame = list(duration = 500, redraw = TRUE), transition = list(duration = 0))),
          label = as.character(mu_vals[i])
        )
      })
    )
  )
)

fig$x$frames <- frames
fig
