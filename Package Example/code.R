## Install R packages (if not already installed)
#   install.packages("netmeta")
#   install.packages("OssaNMA")

library(OssaNMA)

## ssnma

ssnma(p1 = 0.2, p2 = 0.3, enma_sigma = 0.3, power.level = 0.8, 
      sig.level = 0.05, method = "with", allocation = "uneven")

ssnma(p1 = 0.2, p2 = 0.3, enma_sigma = 0.3, power.level = 0.8, 
      sig.level = 0.05, method = "with", allocation = "even")

ssnma(p1 = 0.2, p2 = 0.3, enma_sigma = 0.3, power.level = 0.8, 
      sig.level = 0.05, method = "without", allocation = "uneven")

ssnma(p1 = 0.2, p2 = 0.3, enma_sigma = 0.3, power.level = 0.8, 
      sig.level = 0.05, method = "without", allocation = "even")

## ssanma

ssanma(p1 = 0.2, p2 = 0.3, enma_sigma = 0.3, N = 200, sig.level = 0.05,
       method = "with")

ssanma(p1 = 0.2, p2 = 0.3, enma_sigma = 0.3, N = 200, sig.level = 0.05,
       method = "without")


# Empirical illustrations

data(BRDdat)
head(BRDdat)

library(netmeta)
nma_res <- netmeta(TE,seTE,treat1,treat2,studlab,
                   data=BRDdat,
                   sm="OR",comb.fixed = T,comb.random = F)

enma_sigma <- nma_res$seTE.fixed['Ceftiofur pin','Tildipirosin']
enma_sigma


# The risk of NMA is calculate by pooling the arm-level data from the existing network. 
# The arm-level data is not provided in the package so the value is given directly here.
p_nac <- 0.68
# extract the log odds ratio between NAC and two treatments from nma_res
lor_nac_enro <- nma_res$TE.fixed['No active control','Ceftiofur pin']
lor_nac_flor <- nma_res$TE.fixed['No active control','Tildipirosin']
# calculate risk of Ceftiofur pin, name it as p1
p1 <- p_nac/(p_nac + exp(lor_nac_enro)*(1-p_nac))
# calculate risk of Tildipirosin, name it as p2
p2 <- p_nac/(p_nac + exp(lor_nac_flor)*(1-p_nac))

p1

p2


ssnma(p1 = p1, p2 = p2, enma_sigma = enma_sigma, power.level = 0.8, 
      sig.level = 0.05, method = "with", allocation = "uneven")

ssanma(p1 = p1, p2 = p2, enma_sigma = enma_sigma, N = 800, sig.level = 0.05,
       method = "with")



