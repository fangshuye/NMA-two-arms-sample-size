source(file = "./functions.R")
library(OssaNMA)
BRD <- read.csv("./data/updated_dataset.csv", stringsAsFactors = F)
BRD_r <- BRD 
BRD_r$Number.of.Event.in.arm.2[BRD_r$Study.number == 2] <- BRD_r$Number.of.Event.in.arm.2[BRD_r$Study.number == 2] - 0.5
BRD_long <- wide2long(BRD_r)
BRD_pair <- netmeta::pairwise(treat = t, event = r, n = n, studlab = id, data = BRD_long, allstudies = T, sm = "OR")
nma_old <- netmeta(TE,seTE,treat1,treat2,studlab,data=BRD_pair,sm="OR",comb.fixed = T,comb.random = F)

# get p
p_nac <- BRD_long %>% filter(t == "No active control") %>% summarise(p = sum(r)/sum(n)) %>% pull
lor_nac_2_CEFTP <- nma_old$TE.fixed[8, 2]
p_CEFTP <- lor2prob(p_nac,lor_nac_2_CEFTP)


dat_para <- expand.grid(n=c(100,200,500,1000,2000),lor=c(0.299,0.5),even_allc=c("Yes","No"))
dat_para <- dat_para[order(dat_para$n,dat_para$lor),]

for (c in 1:nrow(dat_para)) {
  p_TILD_c <- lor2prob(p_CEFTP,-dat_para$lor[c])
  s <- dat_para$n[c]
  if(dat_para$even_allc[c]=="Yes"){
    pig_alloc_c <- rep(s/2,2)
  }else{
    pig_alloc_c <- ssanma(p1 = p_CEFTP, p2 = p_TILD_c, 
                          enma_sigma = 0, N = s)$sample_alloc
  }
  dat_para[c,4:5] <- pig_alloc_c
}


write.csv(dat_para, file = "./data/data_para.csv", row.names = F)

