source(file = "./functions.R")
library(OssaNMA)
option_list <- list(
  optparse::make_option("--r", default = 1,
                        help = "different row [default %default].")
)
opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)
seeds <- c(68, 68, 68, 234)

BRD <- read.csv("./data/updated_dataset.csv", stringsAsFactors = F)
BRD_r <- BRD 
BRD_r$Number.of.Event.in.arm.2[BRD_r$Study.number == 2] <- BRD_r$Number.of.Event.in.arm.2[BRD_r$Study.number == 2] - 0.5
BRD_long <- wide2long(BRD_r)
BRD_pair <- netmeta::pairwise(treat = t, event = r, n = n, studlab = id, data = BRD_long, allstudies = T, sm = "OR")
nma_old <- netmeta(TE,seTE,treat1,treat2,studlab,data=BRD_pair,sm="OR",comb.fixed = T,comb.random = F)


# get p_CEFTP
p_nac <- BRD_long %>% filter(t == "No active control") %>% summarise(p = sum(r)/sum(n)) %>% pull
lor_nac_2_CEFTP <- nma_old$TE.fixed[8,2]
p_CEFTP <- lor2prob(p_nac,lor_nac_2_CEFTP)

# get the risk table
lor_nac_2_all <- nma_old$TE.fixed[8, ]
risk_all <- lor2prob(p_nac,lor_nac_2_all)
risk_all <- as.data.frame(risk_all)
risk_all$trt <- rownames(risk_all)
rownames(risk_all) <- rep(1:nrow(risk_all))
colnames(risk_all)[1] <- "p"

# need to change every time; from 1 to 4
r=opt$r

dat_para <- expand.grid(lor=c(0.299,0.4,0.5,0.6),p_CEFTP=p_CEFTP)
dat_para$p_TILD <- lor2prob(p_CEFTP,-dat_para$lor)
dat_para <- dat_para[r,]
risk_all[risk_all$trt=="Tildipirosin","p"] <- dat_para$p_TILD[1]

lor_old_hat_r <- dat_para$lor[1]
p_TILD_c <- dat_para$p_TILD[1]
# get optimal sample size
# sigma_prev <- nma_old$seTE.fixed[10,2]
# pig_alloc_c <- SolveSampleSize_Withprev(p_CEFTP,p_TILD_c,sigma_prev,0.8)
BRD_extend <- BRD
risk_1 <- risk_all
colnames(risk_1) <- c("p1","Arm.1")
risk_2 <- risk_all
colnames(risk_2) <- c("p2","Arm.2")
risk_3<- risk_all
colnames(risk_3) <- c("p3","Arm.3")
BRD_extend <- merge(BRD_extend,risk_1,by="Arm.1",all.x  = T)
BRD_extend <- merge(BRD_extend,risk_2,by="Arm.2",all.x = T)
BRD_extend <- merge(BRD_extend,risk_3,by="Arm.3",all.x = T)


nrep <- 10000
seed_r <- seeds[r]
set.seed(seed_r, kind = "L'Ecuyer-CMRG")
registerDoParallel(cores = 16)


table_with_prev <- foreach (rep=1:nrep, .combine = rbind)%dopar%{
  
  ### simulation the whole network###
  BRD_extend$Number.of.Event.in.arm.1=rbinom(nrow(BRD_extend), 
                                             size =BRD_extend$Total.number.in.arm.1, 
                                             prob = BRD_extend$p1)
  BRD_extend$Number.of.Event.in.arm.2=rbinom(nrow(BRD_extend), 
                                             size =BRD_extend$Total.number.in.arm.2, 
                                             prob = BRD_extend$p2)
  BRD_extend$Number.of.Event.in.arm.3=rbinom(nrow(BRD_extend), 
                                             size =BRD_extend$Total.number.in.arm.3, 
                                             prob = BRD_extend$p3)
  
  BRD_new_long <- wide2long(BRD_extend) 
  
  BRD_new_pair <- netmeta::pairwise(treat = t, event = r, n = n, studlab = id, data = BRD_new_long, allstudies = T, sm = "OR")
  nma_sim_prev <- netmeta(TE,seTE,treat1,treat2,studlab,data=BRD_new_pair,sm="OR",comb.fixed = T,comb.random = F)
  
  sigma_prev <- nma_sim_prev$seTE.fixed[10,2]
  pig_alloc_c <- ssnma(p1 = p_CEFTP,p2 = p_TILD_c, enma_sigma = sigma_prev,
                       power = 0.8, allocation = "even")$sample_size
  
  ### simulation the new trial###
  new_s <- new_study(p_CEFTP,p_TILD_c, sigma = 0, pig_alloc = pig_alloc_c)
  BRD_new_s <- netmeta::pairwise(treat = t, event = r, n = n, studlab = id, data = new_s, allstudies = T, sm = "OR")
  nma_s <- netmeta::netmeta(TE,seTE,treat1,treat2,studlab,data=BRD_new_s,sm="OR",
                            comb.fixed =F,comb.random = T, tol.multiarm.se = 0.01)
  ### result of the new trial when analyze it without the eNMA###
  lor_single_hat <- nma_s$TE.fixed[2,1]
  sigma_single_hat <- nma_s$seTE.fixed[2,1]
  power_single <- ifelse(abs(lor_single_hat)/sigma_single_hat > qnorm(0.975),1,0)
  result_without <- c(power_single,lor_single_hat,sigma_single_hat)
  ### result of the new trial when analyze it with the eNMA###
  result_with <- Withprev_formula(lor_old_hat = nma_sim_prev$TE.fixed[10,2], # different in each setting
                                   lor_new_hat = lor_single_hat,
                                   sigma_old_hat = sigma_prev,
                                   sigma_new_hat =  sigma_single_hat)
  # return(c(pig_alloc_c, result_with, result_without, nma_sim_prev$TE.fixed[10,2], sigma_prev))
  return(c(pig_alloc_c, result_with, result_without))
  
}

BE <- table_with_prev
dat_para[1,4:11] <- apply(BE, 2, mean)
colnames(dat_para) <- c('lor', 'p_CEFTP', 'p_TILD',
                        'n1', 'n2', 
                        'uNMA_power', 'uNMA_lor', 'uNMA_SE', 
                        'RCT_power', 'RCT_lor', 'RCT_SE')
dat_para <- dat_para[,-c(7,10)]
dat_para[,c('n1','n2')] <- round(dat_para[,c('n1','n2')], 0)
dat_para[,2:3] <- round(dat_para[,2:3],4)
dat_para[,6:9] <- round(dat_para[,6:9],3)
write.csv(dat_para, file = paste0("./table/table2_even_lor_",r,".csv"), row.names = F)


