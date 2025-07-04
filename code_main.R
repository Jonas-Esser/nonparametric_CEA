data <- read.csv("c:\\Users\\jes238\\OneDrive - Vrije Universiteit Amsterdam\\Documents\\suBART_tutorial\\data_example_complete.csv")

# data formatting 
data$education <- as.factor(data$education)
data[data == -99] <- NA

# data preparation ####
X <- data[,c("t", "age", "sex", "education")]
Y <- data[,c("c", "q")]

# estimation of propensity scores ####
library(subart)

ps_fit <- subart(x_train = X[,c("age", "sex", "education")],
                 y_mat = as.matrix(X$t),
                 x_test = X[,c("age", "sex", "education")],
                 n_tree = 100,
                 n_mcmc = 3000,
                 n_burn = 1000,
                 varimportance = TRUE)

X_ps <- X
X_ps$ps <- apply(pnorm(ps_fit$y_hat_test), 1, mean)

# further data preparation ####

X_test <- rbind(X_ps, X_ps)
X_test$t <- c(rep(0, nrow(X)), rep(1, nrow(X)))

# fitting the suBART model ####

suBART_ps_fit <- subart(x_train = X_ps,
                        y_mat = as.matrix(Y),
                        x_test = X_test,
                        n_tree = 100,
                        n_mcmc = 3000,
                        n_burn = 1000,
                        varimportance = FALSE)

# obtaining the results ####

suBART_ps_fit$ATE <- matrix(NA, nrow = 2000, ncol = 2)
for (i in 1:2000){
  suBART_ps_fit$ATE[i,] <- c(
    mean(suBART_ps_fit$y_hat_test[X_test$t == 1,1,i]) - mean(suBART_ps_fit$y_hat_test[X_test$t == 0,1,i]),
    mean(suBART_ps_fit$y_hat_test[X_test$t == 1,2,i]) - mean(suBART_ps_fit$y_hat_test[X_test$t == 0,2,i])
    
  )
}

MCMC_sample <- data.frame(
  Delta_c = suBART_ps_fit$ATE[,1],
  Delta_q = suBART_ps_fit$ATE[,2]
)
MCMC_sample$INB20 <- 20000 * MCMC_sample$Delta_q - MCMC_sample$Delta_c
MCMC_sample$INB50 <- 50000 * MCMC_sample$Delta_q - MCMC_sample$Delta_c

lambda <- seq(0, 50000, length.out = 1000)
p <- rep(NA, length(lambda))
for (i in 1:length(lambda)){
  p[i] <- mean(lambda[i]*MCMC_sample$Delta_q - MCMC_sample$Delta_c > 0)
}
CEAC <- data.frame(lambda, p)

# Plotting results ####
library(ggplot2)
library(gridExtra)

CEplane <- ggplot(MCMC_sample) + 
  geom_point(mapping = aes(Delta_q, Delta_c), size = 1, alpha = 0.3, show.legend = FALSE) +
  geom_point(aes(mean(Delta_q), mean(Delta_c)), color = "red", size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = expression(Delta[q]), y = expression(Delta[c])) +
  scale_x_continuous(limits = c(-0.05, 0.15)) +
  scale_y_continuous(limits = c(-100, 1000)) +
  theme_classic() +
  theme(text = element_text(size = 14)) 


CEAC <- ggplot(data = CEAC) +
  geom_line(aes(x = lambda, y = p), linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ylim(0,1) +
  labs(x = expression(lambda)) +
  labs(y = "Probability of cost-effectiveness") +
  theme_classic() +
  theme(text = element_text(size = 14), axis.line.y = element_blank(), legend.position = "right") +
  scale_x_continuous(label=paste0((0:5) * 10, "k")) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)

grid.arrange(CEplane, CEAC, nrow = 1)

