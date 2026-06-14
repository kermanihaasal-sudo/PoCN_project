#first article
library(igraph)



calc_payoff <- function(g, state, neigh_list) {
  n <- vcount(g)
  payoff <- numeric(n)
  
  for (v in 1:n) {
    nb <- as.integer(neigh_list[[v]])
    if (length(nb) > 0) {
      payoff[v] <- sum(state[nb] == state[v])
    }
  }
  return(payoff)
}

active_density <- function(state, ed) {
  active_edges <- sum(state[ed[, 1]] != state[ed[, 2]])
  return(active_edges / nrow(ed))
}

voter_step <- function(state, nb, v) {
  if (length(nb) == 0) {
    return(state)
  }
  chosen_nb <- sample(nb, 1)
  state[v] <- state[chosen_nb]
  return(state)
}

imitation_step <- function(state, payoff, nb, v) {
  if (length(nb) == 0) {
    return(state)
  }
  best_nb <- nb[which.max(payoff[nb])]
  if (payoff[best_nb] > payoff[v]) {
    state[v] <- state[best_nb]
  }
  return(state)
}

run_model_1 <- function(g, state, q, t_max = 200) {
  n <- vcount(g)
  neigh_list <- as_adj_list(g)
  ed <- as_edgelist(g, names = FALSE)
  
  nA <- numeric(t_max)
  
  payoff <- calc_payoff(
    g = g,
    state = state,
    neigh_list = neigh_list
  )
  
  for (t in 1:t_max) {
    order_nodes <- sample(
      1:n,
      n,
      replace = FALSE
    )
    
    for (v in order_nodes) {
      nb <- as.integer(neigh_list[[v]])
      if (length(nb) == 0) {
        next
      }
      
      old_state <- state[v]
      
      if (runif(1) < q) {
        state <- voter_step(
          state = state,
          nb = nb,
          v = v
        )
      } else {
        state <- imitation_step(
          state = state,
          payoff = payoff,
          nb = nb,
          v = v
        )
      }
      
      if (state[v] != old_state) {
        payoff[v] <- sum(state[nb] == state[v])
        for (u in nb) {
          nb_u <- as.integer(neigh_list[[u]])
          payoff[u] <- sum(state[nb_u] == state[u])
        }
      }
    }
    
    nA[t] <- active_density(
      state = state,
      ed = ed
    )
    
    if (nA[t] == 0) {
      if (t < t_max) {
        nA[(t + 1):t_max] <- 0
      }
      break
    }
  }
  
  tau <- which(nA == 0)[1]
  if (is.na(tau)) {
    tau <- t_max
  }
  
  return(list(
    state = state,
    nA = nA,
    tau = tau
  ))
}

# ==============================================================================
#Tau plots for ER
# ==============================================================================
set.seed(123)
N_sizes  <- c(200, 500, 1000) 
avg_k    <- 9
Q        <- c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
n_runs   <- 10
max_mcs  <- 2000

mean_results <- matrix(NA, nrow = length(N_sizes), ncol = length(Q))

for (i in 1:length(N_sizes)) {
  current_N <- N_sizes[i]
  cat("\n--- Simulating for ER Network Size N =", current_N, "---\n")
  
  p_ER <- avg_k / (current_N - 1)
  g_ER <- erdos.renyi.game(n = current_N, p = p_ER, type = "gnp", directed = FALSE)
  
  for (j in 1:length(Q)) {
    cat("Computing q =", Q[j], "...\n")
    tau_vector <- numeric(n_runs)
    
    for (r in 1:n_runs) {
      strategies0 <- sample(c(0, 1), current_N, replace = TRUE)
      result <- run_model_1(g_ER, strategies0, Q[j], t_max = max_mcs)
      tau_vector[r] <- result$tau
    }
    mean_results[i, j] <- mean(tau_vector)
  }
}

output_path_er <- output_path_er <- "C:/Users/zohreh/Desktop/complex/task_35graphs/u_shape_multi_N_ER.png"
png(filename = output_path_er, width = 850, height = 800, res = 120)

plot_colors <- c("red", "blue", "darkgreen")
plot_pchs   <- c(15, 17, 19)

plot(Q, mean_results[1, ], type = "b", pch = plot_pchs[1], col = plot_colors[1], 
     lwd = 2, log = "y", ylim = c(min(mean_results), max(mean_results) * 1.5),
     xlab = "q", ylab = "Characteristic Time tau (MCS)",
     main = "Replication of Figure 3a: Size Dependence Study")

for (i in 2:length(N_sizes)) {
  lines(Q, mean_results[i, ], type = "b", pch = plot_pchs[i], col = plot_colors[i], lwd = 2)
}

mtext(paste0("Network Type: Erdös-Rényi (ER) | Average Degree <k> = ", avg_k), 
      side = 3, line = 0.5, cex = 0.9, col = "darkgray", font = 3)

legend("topright", legend = paste("N =", N_sizes), 
       col = plot_colors, pch = plot_pchs, lty = 1, lwd = 2, bty = "n", cex = 0.95)

dev.off()
cat("\nER plot saved to:\n", output_path_er, "\n")


# ==============================================================================
#Tau plots for BA
# ==============================================================================
set.seed(123)
N_sizes_ba  <- c(1000, 1500, 2000) 
m_edges     <- 2       
max_mcs_ba  <- 5000    

mean_results_ba <- matrix(NA, nrow = length(N_sizes_ba), ncol = length(Q))

for (i in 1:length(N_sizes_ba)) {
  current_N <- N_sizes_ba[i]
  cat("\n--- Simulating for Scale-Free (BA) Network Size N =", current_N, "---\n")
  
  g_BA <- barabasi.game(n = current_N, m = m_edges, directed = FALSE)
  
  for (j in 1:length(Q)) {
    cat("Computing q =", Q[j], "...\n")
    tau_vector <- numeric(n_runs)
    
    for (r in 1:n_runs) {
      strategies0 <- sample(c(0, 1), current_N, replace = TRUE)
      result <- run_model_1(g_BA, strategies0, Q[j], t_max = max_mcs_ba)
      tau_vector[r] <- result$tau
    }
    mean_results_ba[i, j] <- mean(tau_vector)
  }
}

output_path_ba <- output_path_er <- "C:/Users/zohreh/Desktop/complex/task_35graphs/u_shape_multi_N.png"
png(filename = output_path_ba, width = 850, height = 800, res = 120)

plot_colors_ba <- c("orange", "purple", "darkblue")
plot_pchs_ba   <- c(15, 17, 19) 

mean_results_ba[mean_results_ba <= 0] <- 1
mean_results_ba[is.infinite(mean_results_ba)] <- max_mcs_ba

explicit_ymin <- max(1, min(mean_results_ba))
explicit_ymax <- max(mean_results_ba) * 1.5

plot(Q, mean_results_ba[1, ], type = "b", pch = plot_pchs_ba[1], col = plot_colors_ba[1], 
     lwd = 2, log = "y", ylim = c(explicit_ymin, explicit_ymax),
     xlab = "q", ylab = "Characteristic Time tau (MCS)",
     main = "Replication of Figure 3c/3d: Scale-Free Network Study")

for (i in 2:length(N_sizes_ba)) {
  lines(Q, mean_results_ba[i, ], type = "b", pch = plot_pchs_ba[i], col = plot_colors_ba[i], lwd = 2)
}

mtext(paste0("Network Type: Barabási-Albert (BA) Scale-Free  |  m = ", m_edges), 
      side = 3, line = 0.5, cex = 0.9, col = "darkgray", font = 3)

legend("topright", legend = paste("N =", N_sizes_ba), 
       col = plot_colors_ba, pch = plot_pchs_ba, lty = 1, lwd = 2, bty = "n", cex = 0.95)

dev.off()
cat("\nBA plot saved to:\n", output_path_ba, "\n")


# ==============================================================================
#NA plot for ER
# ==============================================================================
set.seed(42)
N_nodes  <- 1000  
avg_k    <- 9     
max_time <- 200   
n_runs   <- 30    

p_ER_decay <- avg_k / (N_nodes - 1)
g_ER_decay <- erdos.renyi.game(n = N_nodes, p = p_ER_decay, type = "gnp", directed = FALSE)

q_values  <- c(0.05, 0.15, 0.35, 0.65, 0.80, 0.90)
plot_cols <- c("black", "red", "green", "blue", "orange", "magenta")

decay_results <- matrix(0, nrow = length(q_values), ncol = max_time)

for (j in 1:length(q_values)) {
  cat("Simulating decay curves for q =", q_values[j], "...\n")
  run_matrix <- matrix(0, nrow = n_runs, ncol = max_time)
  
  for (r in 1:n_runs) {
    strategies0 <- sample(c(0, 1), N_nodes, replace = TRUE)
    result <- run_model_1(g_ER_decay, strategies0, q_values[j], t_max = max_time)
    run_matrix[r, ] <- result$nA
  }
  decay_results[j, ] <- colMeans(run_matrix)
}

output_path_decay <- "C:/Users/zohreh/Desktop/complex/task_35graphs/active_link_decay_fig4a.png"
png(filename = output_path_decay, width = 850, height = 800, res = 120)

decay_results[decay_results <= 0] <- 0.001

plot(1:max_time, decay_results[1, ], type = "l", col = plot_cols[1], lwd = 2.5,
     log = "xy", xlim = c(1, max_time), ylim = c(0.005, 0.6),
     xlab = "t (Monte Carlo Steps)", ylab = "n_A(t)",
     main = "Replication of Figure 4a: Active Link Decays")

for (j in 2:length(q_values)) {
  lines(1:max_time, decay_results[j, ], col = plot_cols[j], lwd = 2.5)
}

mtext(paste0("Erdös-Rényi (ER) Network  |  N = ", N_nodes, "  |  <k> = ", avg_k), 
      side = 3, line = 0.5, cex = 0.9, col = "darkgray", font = 3)

legend("bottomleft", legend = paste("q =", q_values), col = plot_cols, lty = 1, lwd = 2.5, bty = "n", cex = 0.9)

dev.off()
cat("\nActive link decay plot saved to:\n", output_path_decay, "\n")


#################################################################################
#################################################################################

#second article

library(igraph)
library(ggplot2)
library(dplyr)
library(tidyr)

set.seed(42) 


# Basic Parameters

N <- 300
mean_k <- 6
max_steps <- 300  
n_runs <- 5       
epsilon <- 0.01
beta <- 1
alpha_fixed <- 0.01 

lambda_values <- 10^seq(-2, 3, length.out = 35)

# ==============================================================================
# constructing networks
g_ER <- erdos.renyi.game(N, p = mean_k / (N - 1), directed = FALSE)
E(g_ER)$weight <- 1

radius <- sqrt(mean_k / (N * pi))
g_RGG <- sample_grg(N, radius)
E(g_RGG)$weight <- 1

g_BA <- sample_pa(n = N, m = 3, directed = FALSE)
E(g_BA)$weight <- 1

# ==============================================================================

#initialization
initialize_network_states <- function(g) {
  n_nodes <- vcount(g)
  theta <- runif(n_nodes, min = -pi, max = pi)
  omega <- runif(n_nodes, min = -0.5, max = 0.5) 
  strategy <- sample(c(0, 1), n_nodes, replace = TRUE)
  return(list(theta = theta, omega = omega, strategy = strategy))
}

#computing Theta Dot
phase_velocity <- function(N, adj_matrix, theta, omega, strategy, lambda) {
  theta_dot <- numeric(N)
  for (i in 1:N) {
    neighbors <- which(adj_matrix[i, ] == 1)
    if (length(neighbors) > 0) {
      coupling_force <- sum(sin(theta[neighbors] - theta[i]))
      theta_dot[i] <- omega[i] + strategy[i] * lambda * coupling_force
    } else {
      theta_dot[i] <- omega[i]
    }
  }
  return(theta_dot)
}

params <- function(N, adj_matrix, theta) {
  local_r <- numeric(N)
  for (i in 1:N) {
    neighbors <- which(adj_matrix[i, ] == 1)
    if (length(neighbors) > 0) {
      sum_phase <- sum(exp(1i * theta[neighbors]))
      local_r[i] <- Mod(sum_phase) / length(neighbors)
    } else {
      local_r[i] <- 0
    }
  }
  return(local_r)
}

# Fermi Strategy Update
update_strategies <- function(N, adj_matrix, strategy, benefit, penalty, alpha, beta) {
  payoff <- benefit - alpha * (penalty / (2 * pi))
  next_strategy <- strategy
  
  for (i in 1:N) {
    neighbors <- which(adj_matrix[i, ] == 1)
    if (length(neighbors) > 0) {
      target_neighbor <- sample(neighbors, size = 1)
      p_imitation <- 1 / (1 + exp(-beta * (payoff[target_neighbor] - payoff[i])))
      if (runif(1) < p_imitation) {
        next_strategy[i] <- strategy[target_neighbor]
      }
    }
  }
  return(next_strategy)
}

# fermi Simulation
# ==============================================================================

kuramoto_fermi_sim <- function(g, lambda, alpha, max_steps, epsilon, beta) {
  N <- vcount(g)
  adj_matrix <- as.matrix(as_adj(g, sparse = TRUE))
  deg <- degree(g)
  
  degrees <- quantile(deg[deg > 0], probs = seq(0, 1, length.out = 5))
  k_groups <- cut(deg, breaks = unique(degrees), include.lowest = TRUE, labels = paste0("k", 1:(length(unique(degrees))-1)))
  
  states <- initialize_network_states(g)
  theta  <- states$theta
  omega  <- states$omega
  strategy <- states$strategy
  theta_dot_pv <- omega
  
  steady_state_steps <- (max_steps - 50):max_steps
  r_L_accumulator <- numeric(N)
  
  for (step in 1:max_steps) {
    theta_dot_current <- phase_velocity(N, adj_matrix, theta, omega, strategy, lambda)
    
    theta <- theta + epsilon * theta_dot_current
    theta <- ((theta + pi) %% (2 * pi)) - pi  
    benefit <- params(N, adj_matrix, theta)
    
    penalty <- abs(theta_dot_current - theta_dot_pv)
    strategy <- update_strategies(N, adj_matrix, strategy, benefit, penalty, alpha, beta)
    
    theta_dot_pv <- theta_dot_current
    
    if (step %in% steady_state_steps) {
      r_L_accumulator <- r_L_accumulator + benefit
    }
  }
  
  r_L_time_averaged <- r_L_accumulator / length(steady_state_steps)
  
  simulation_results <- data.frame(k_group = k_groups, r_L = r_L_time_averaged) %>%
    filter(!is.na(k_group)) %>% 
    group_by(k_group) %>%
    summarise(mean_rL = mean(r_L), .groups = 'drop')
  
  return(simulation_results)
}

# ==============================================================================
run <- function(g, network_name, lambda_vals, alpha_val, total_runs) {
  data <- data.frame()
  
  for (lam in lambda_vals) {
    cat(sprintf("Simulating: Network=%s | Lambda=%.3f\n", network_name, lam))
    
    run_accumulator <- list()
    for (r in 1:total_runs) {
      run_accumulator[[r]] <- kuramoto_fermi_sim(g, lam, alpha_val, max_steps, epsilon, beta)
    }
    
    lambda_summary <- bind_rows(run_accumulator) %>%
      group_by(k_group) %>%
      summarise(mean_rL = mean(mean_rL), .groups = 'drop') %>%
      mutate(lambda = lam, network = network_name)
    
    data <- bind_rows(data, lambda_summary)
  }
  return(data)
}

results_ER  <- run(g_ER, "ER", lambda_values, alpha_fixed, n_runs)
results_RGG <- run(g_RGG, "RGG", lambda_values, alpha_fixed, n_runs)
results_BA  <- run(g_BA, "BA", lambda_values, alpha_fixed, n_runs)

# ==============================================================================

plot_dataframe <- bind_rows(results_ER, results_RGG, results_BA)
plot_dataframe$network <- factor(plot_dataframe$network, levels = c("ER", "RGG", "BA"))

p_sync <- ggplot(plot_dataframe, aes(x = lambda, y = mean_rL, color = k_group, group = k_group)) +
  geom_line(size = 1.1, alpha = 0.9) +
  facet_wrap(~network, scales = "fixed", ncol = 3) +
  
  geom_hline(yintercept = 0.63, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0.6, linetype = "dashed", color = "firebrick") + 
  geom_vline(xintercept = 300, linetype = "dashed", color = "lightseagreen") + 
  
  scale_x_log10(breaks = c(10^-2, 10^-1, 1, 10, 10^2, 10^3),
                labels = expression(10^-2, 10^-1, 10^0, 10^1, 10^2, 10^3)) +
  scale_y_continuous(limits = c(0.4, 1.02), breaks = seq(0.4, 1.0, by = 0.1)) +
  
  scale_color_manual(values = c("#1F77B4", "#9467BD", "#E377C2", "#FF7F0E"),
                     name = expression(k),
                     labels = c(expression(k[1]), expression(k[2]), expression(k[3]), expression(k[4]))) +
  
  labs(x = bquote(lambda), y = bquote(langle*r[L]*rangle[k])) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14)
  )

output_directory <- "C:/Users/zohreh/Desktop/complex/task_35graphs/PRL_replication_plot.png"
ggsave(
  filename = output_directory,
  plot = p_sync,
  width = 11.5,     
  height = 5.5,     
  dpi = 300
)

cat("The modular widescreen plot has been successfully saved to:", output_directory, "\n")


