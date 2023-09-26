packages <- c("dplyr", "netmeta", "parallel", "foreach", "doParallel", "doRNG","DEoptimR")
lapply(packages, library, character.only = TRUE)

# it seems like this didn't use at this stage
# log(k1/k2)=lor
lor2prob <- function(p1, lor){
  p2 <- p1/(p1 + exp(lor)*(1-p1))
  return(p2)
}

# rearrange the data to the long type: one arm one study per row
wide2long <- function(MTCdata){
  N <- max(MTCdata$Number.of.arms)
  study <- rep(MTCdata$Study.number,N)
  t <- NULL
  n <- NULL
  r <- NULL
  for(i in c(2,1,3)){
    r <- c(r, eval(parse(text = paste0("MTCdata$Number.of.Event.in.arm.",i, sep = ""))))
    n <- c(n, eval(parse(text = paste0("MTCdata$Total.number.in.arm.",i, sep = ""))))
    t <- c(t, eval(parse(text = paste0("MTCdata$Arm.",i, sep = ""))))
  }
  res <- data.frame(id = study, t = t, r = r, n = n)
  res <- res %>% dplyr::filter(!is.na(n)) %>% arrange(id)
  res
}



# the type of the new_s is the same as what wide2long() generate above.

new_study <- function(p_baseline, p_trt2, sigma, pig_alloc = c(50,50)){
  r_vec <- rbinom(2, size = pig_alloc, prob = c(p_baseline, p_trt2))
  new_s <- data.frame(id = rep(100,2), t = c("Ceftiofur pin", "Tildipirosin"),
                      r = r_vec, n = pig_alloc)
  return(new_s)
}

############## simlation ###############

### with previous network
bio_equal <- function(p_baseline, p_trt2, sigma, re_sigma = 0, pig_alloc, data_prev){
  #res_TE <- numeric(1)
  #res_seTE <- numeric(1)
  #power <- numeric(length(p_trt2_vector))
  new_s <- new_study(p_baseline, p_trt2, sigma = sigma, pig_alloc = pig_alloc)
  data_final <- rbind(data_prev, new_s)
  BRD_new <- netmeta::pairwise(treat = t, event = r, n = n, studlab = id, data = data_final, allstudies = T, sm = "OR")
  nma_res <- netmeta::netmeta(TE,seTE,treat1,treat2,studlab,data=BRD_new,sm="OR",
                              comb.fixed =F,comb.random = T, tol.multiarm.se = 0.01)
  #z <- true_lor/se
  #power <- pnorm(z-qnorm(0.975))+pnorm(-z-qnorm(0.975))
  lor_hat <- nma_res$TE.fixed[10,2]
  se <- nma_res$seTE.fixed[10,2]
  power <- ifelse(abs(lor_hat)/se > qnorm(0.975),1,0)
  
  return(matrix(c(power,lor_hat,se),ncol = 3))
}

### non-inferiority and superiority without previous network
bioeq_single_s <- function(p_baseline, p_trt2, pig_alloc){
  #power <- numeric(1)
  
  # random part in the simulation, rbinom
  BRD_s <- new_study(p_baseline, p_trt2, sigma = 0, pig_alloc = pig_alloc)
  BRD_new_s <- netmeta::pairwise(treat = t, event = r, n = n, studlab = id, data = BRD_s, allstudies = T, sm = "OR")
  # TE: Estimate of treatment effect (log odds ratio, mean difference)
  # seTE: S.E. of TE
  # sm: summary measure,
  # comb.fixed: whether a fixed effects (common effects) network meta-analysis should be conducted.
  # comb.random: whether a random effects network meta-analysis should be conducted.
  nma_s <- netmeta::netmeta(TE,seTE,treat1,treat2,studlab,data=BRD_new_s,sm="OR",
                            comb.fixed =F,comb.random = T, tol.multiarm.se = 0.01)
  #z <- true_lor/se
  #power <- pnorm(z-qnorm(0.975))+pnorm(-z-qnorm(0.975))
  # lor_TILD_2_CEFTP
  lor_hat <- nma_s$TE.fixed[2,1]
  se <- nma_s$seTE.fixed[2,1]
  power <- ifelse(abs(lor_hat)/se > qnorm(0.975),1,0)
  return(matrix(c(power,lor_hat,se),ncol = 3))
}

con_fn <- function(n, beta1, beta2,s){
  n1 <- n[1]
  n2 <- n[2]
  n1 + n2 - s
}

var_fn <- function(n, beta1, beta2, s){
  mu_1 <- exp(beta1)/(1+exp(beta1))^2
  mu_2 <- exp(beta1 + beta2)/(1+exp(beta1 + beta2))^2
  1/(mu_1 * n[1]) + 1/(mu_2 * n[2])
}


Withprev_formula <- function(lor_old_hat, lor_new_hat, sigma_old_hat, sigma_new_hat){
  X <- matrix(c(1,1),nrow = 2)
  sigma <- diag(c(sigma_old_hat^2, sigma_new_hat^2))
  y <- matrix(c(lor_old_hat, lor_new_hat),nrow = 2)
  lor_hat <- solve(t(X) %*% solve(sigma) %*% X) %*% t(X) %*% solve(sigma) %*% y
  se <- sqrt(solve(t(X) %*% solve(sigma) %*% X))
  power <- ifelse(abs(lor_hat)/se > qnorm(0.975),1,0)
  return(matrix(c(power,lor_hat,se),ncol = 3))
}

Single_study <- function(p_baseline, p_trt2, pig_alloc){
  #power <- numeric(1)
  
  # random part in the simulation, rbinom
  BRD_s <- new_study(p_baseline, p_trt2, sigma = 0, pig_alloc = pig_alloc)
  BRD_new_s <- netmeta::pairwise(treat = t, event = r, n = n, studlab = id, data = BRD_s, allstudies = T, sm = "OR")
  # TE: Estimate of treatment effect (log odds ratio, mean difference)
  # seTE: S.E. of TE
  # sm: summary measure,
  # comb.fixed: whether a fixed effects (common effects) network meta-analysis should be conducted.
  # comb.random: whether a random effects network meta-analysis should be conducted.
  nma_s <- netmeta::netmeta(TE,seTE,treat1,treat2,studlab,data=BRD_new_s,sm="OR",
                            comb.fixed =F,comb.random = T, tol.multiarm.se = 0.01)

  lor_hat <- nma_s$TE.fixed[2,1]
  se <- nma_s$seTE.fixed[2,1]

  return(c(lor_hat,se))
}

