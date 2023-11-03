sample(c(0,1), size = 20, replace = TRUE, prob = c(0.3, 0.7))

rbinom(n = 20, size = 1, prob = 0.7)


x <- seq(-4, 4, length.out = 100)
p <- 1/(1 + exp(-x))
plot(x, p, type = "l")


set.seed(1)
gender <- sample(c(0,1), size = 100, replace = TRUE)
age <- round(runif(100, 18, 80))

lp <- -9 + 3.5*gender + 0.2*age

p <- 1/(1 + exp(-lp))
summary(p)

y <- rbinom(n = 100, size = 1, prob = p)
y


mod <- glm(y ~ gender + age, family = "binomial")
summary(mod)



set.seed(1)
gender <- sample(c(0,1), size = 1000, replace = TRUE)
age <- round(runif(1000, 18, 80))
xb <- -9 + 3.5*gender + 0.2*age
p <- 1/(1 + exp(-xb))
y <- rbinom(n = 1000, size = 1, prob = p)
mod <- glm(y ~ gender + age, family = "binomial")
summary(mod)

predict(mod, type="response", newdata = data.frame(cbind(age,gender)))[1:10]
p[1:10]

#generate data then build model
simFit <- function(n){
  gender <- sample(c(0,1), size = n, replace = TRUE)
  age <- round(runif(n, 18, 80))
  xb <- -9 + 3.5*gender + 0.2*age
  p <- 1/(1 + exp(-xb))
  y <- rbinom(n = n, size = 1, prob = p)
  mod <- glm(y ~ gender + age, family = binomial)
  s.out <- summary(mod)
  return(s.out$coeff[1])
}

powerEst <- function(N, n){
  r.out <- replicate(n = N, simFit(n = n))
  r.out
}
#10 repeatition, sample size of 100
powerEst(10,100)

#Three different sample sizes, 10 repeatiton  
ss <- seq(300,1000,100) # various sample sizes
p.out <- sapply(ss, function(x)powerEst(N = 10, n = x))
