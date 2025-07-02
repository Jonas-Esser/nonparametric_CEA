library(ggplot2)
library(BART)

N_sim <- 100
N <- 200
X <- rnorm(N)
ATE_linear_sim1 <- rep(NA, N_sim)
ATE_linear_sim2 <- rep(NA, N_sim)
ATE_BART_sim1 <- rep(NA, N_sim)
ATE_BART_sim2 <- rep(NA, N_sim)
EY_0 <- 3 * sin(X) * X^2
EY_1 <- EY_0 + 1
ATE_true <- mean(EY_1 - EY_0)

#### single replication ####
A <- rbinom(N, 1, 1/(1 + exp(-3*X)))
Y <- (1 - A)*EY_0 + A*EY_1 + rnorm(N)
test <- data.frame(X = c(seq(-3, 3, length.out = 1000),seq(-3, 3, length.out = 1000)),
                   A = c(rep(0, 100), rep(1, 100))
)
train <- data.frame(X,A)
BART_fit <- wbart(train, Y, test)
test$Y_pred <- BART_fit$yhat.test.mean
train$Y <- Y
ggplot() +
  geom_point(data = train, mapping = aes(X,Y)) +
  geom_line(data = test, mapping = aes(X, Y_pred), color = "red") +
  facet_wrap(. ~ A) + 
  theme_classic()

#### repeated sampling ####

test <- matrix(c(rep(X, 2), rep(0, N), rep(1, N)), ncol = 2)

# sim 1: random assignment
for (i in 1:N_sim){
  A <- rbinom(N, 1, 0.5)
  Y <- (1 - A)*EY_0 + A*EY_1 + rnorm(N)
  ATE_linear_sim1[i] <- lm(Y ~ A + X)$coefficients[2]
  train <- data.frame(X,A)
  BART_mod <- wbart(train, Y, test)
  ATE_BART_sim1[i] <- mean(BART_mod$yhat.test.mean[(N+1):(2*N)] - BART_mod$yhat.test.mean[1:N])
}

# sim 2: nonrandom assigment
for (i in 1:N_sim){
  #A <- rbinom(N, 1, pnorm((2)*X))
  A <- rbinom(N, 1, 1/(1 + exp(-2*X)))
  Y <- (1 - A)*EY_0 + A*EY_1 + rnorm(N)
  ATE_linear_sim2[i] <- lm(Y ~ A + X)$coefficients[2]
  train <- data.frame(X,A)
  BART_mod <- wbart(train, Y, test)
  ATE_BART_sim2[i] <- mean(BART_mod$yhat.test.mean[(N+1):(2*N)] - BART_mod$yhat.test.mean[1:N])
  
}

data_sim <- data.frame(sim = c(rep("randomized",N_sim),rep("non-randomized",N_sim)),
                       ATE_linear = c(ATE_linear_sim1,ATE_linear_sim2),
                       ATE_BART = c(ATE_BART_sim1,ATE_BART_sim2)
)
data_sim$sim <- factor(data_sim$sim, levels = c("randomized", "non-randomized"))

ggplot(data_sim) + 
  geom_histogram(aes(x = ATE_linear), bins = 50, color = "black", fill = "grey") +
  scale_x_continuous(limits = c(-1,2)) + 
  geom_vline(xintercept = 1, color = "red") +
  facet_wrap(. ~ sim) +
  theme_classic() + 
  theme(axis.title.x = element_blank())

ggplot(data_sim) + 
  geom_histogram(aes(x = ATE_BART), bins = 50, color = "black", fill = "grey") +
  scale_x_continuous(limits = c(-1,2)) + 
  geom_vline(xintercept = 1, color = "red") +
  facet_wrap(. ~ sim) +
  theme_classic() + 
  theme(axis.title.x =  element_blank())

