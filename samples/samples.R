library(BSDA)

results = c()

cints1B = c()
cints1A = c()

cints3B = c()
cints3A = c()

for(i in 1:1000) {
  s1 <- rbinom(n = 2*1067, p=.5, size = 1)
  
  s2 <- rbinom(n = 2*1067, p=.55, size = 1)
  
  ztest = z.test(s1, s2, sigma.x = sd(s1), sigma.y=sd(s2))
  result = ztest$p.value < .05
  results = append(results, result)

  # calculate 
  
  s3 <- rbinom(n = 2*1067, p=.05, size = 1)
  mean(s2)
  
  prop1 <- prop.test(length(s1[s1==1]), length(s1))
  cints1B <- append(cints1B, prop1$conf.int[2])
  cints1A <- append(cints1A, prop1$conf.int[1])
  
  prop3 <- prop.test(length(s3[s3==1]), length(s3))
  cints3B <- append(cints3B, prop3$conf.int[2])
  cints3A <- append(cints3A, prop3$conf.int[1])
}

mean(s1)
mean(s2)
mean(s3)

mean(results) # this is the power estimate
mean(cints1B) - mean(s1) # this is the confidence interval for the sample with p=.5
mean(cints3B) - mean(s3)  # this is the confidence interval for the sample with p=0.05

library(binom)
binom.confint(length(s3[s3==1]), length(s3))
prop.test(length(s3[s3==1]), length(s3))

s1 <- rbinom(n = 1067, p=.05, size = 1) 
pbar_s1 = mean(s1)
pbar_s1
SE = sqrt(pbar_s1∗(1−pbar_s1)/n)
E = qnorm(.975)∗SE
E # margin of error
SE

# power calculation

pA=0.65
pB=0.85
kappa=1
alpha=0.05
beta=0.20
(nB=(pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha/2)+qnorm(1-beta))/(pA-pB))^2)
ceiling(nB) # 70
z=(pA-pB)/sqrt(pA*(1-pA)/nB/kappa+pB*(1-pB)/nB)
(Power=pnorm(z-qnorm(1-alpha/2))+pnorm(-z-qnorm(1-alpha/2)))

(n=3*4)