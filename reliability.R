library(haven)
library(tidyverse)
library(psych)
library(mice)
library(Amelia)
library(car)
library(Hmisc)
library(janitor)

##libraries and looking at data
uncleandat <- read_sav("C:/Users/bc1256/Downloads/belong.sav")

summary(uncleandat)
psych::describe(uncleandat)
missmap(uncleandat)
mean(is.na(uncleandat))

## creating difference response scales
sd = select(uncleandat, sds1, sds2, sds3, sds4, sds5, sds6, sds7, sds8, sds9, sds10, sds11, sds12, sds13)
b = select(uncleandat, b1, b2, b3, b4, b5, b6, b7, b8 ,b9, b10, b11 ,b12, b13, b14 ,b15, b16, b17,b18, b19, b20 ,b21, b22, b23, b24, b25)
bcr = select(uncleandat, bcrit1, bcrit2, bcrit3, bcrit4, bcrit5, bcrit6, bcrit7, bcrit8 ,bcrit9, bcrit10, bcrit11 ,bcrit12, bcrit13, bcrit14 ,bcrit15, bcrit16, bcrit17,bcrit18)
bco = select(uncleandat, bconv1, bconv2, bconv3, bconv4, bconv5, bconv6, bconv7, bconv8, bconv9, bconv10, bconv11, bconv12)
reb = select(uncleandat, rb1, rb2, rb3, rb4, rb5, rb6, rb7, rb8 ,rb9, rb10, rb11 ,rb12, rb13, rb14 ,rb15, rb16, rb17,rb18, rb19, rb20,rb21, rb22, rb23, rb24, rb25)

## looking at belonging scale
summary(b)
psych::describe(b)
summary(sd)
psych::describe(sd)
summary(bcr)
psych::describe(bcr)
summary(bco)
psych::describe(bco)

## reverse coding and compiling
keysb   = c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,-1,-1,1,-1,-1,1,-1,1)
keysd   = c(1,1,1,1,-1,1,-1,1,-1,-1,1,1,-1)
keysbcr = c(1,1,-1,1,1,-1,1,1,-1,1,1,-1,1,1,1,-1,1,1)

rb1 <- reverse.code(keysb,b)
rsd1 <- reverse.code(keysd, sd)
rbcr1 <- reverse.code(keysbcr, bcr)
rbco1 <- bco
reb1 <- reverse.code(keysb,reb)

rb1 <- as.data.frame(rb1)
rsd1 <- as.data.frame(rsd1)
rbcr1 <- as.data.frame(rbcr1)
rbco1 <- as.data.frame(rbco1)
reb1 <- as.data.frame(reb1)

## missing data
missmap(rb1)
mean(is.na(rb1))
btest <-rb1
btest[is.na(btest)] <- 2

missmap(rsd1)
mean(is.na(rsd1))
sdtest <-rsd1
sdtest[is.na(sdtest)] <- 2

missmap(rbcr1)
mean(is.na(rbcr1))
bcrtest <-rbcr1
bcrtest[is.na(bcrtest)] <- 3

missmap(rbco1)
mean(is.na(rbco1))
bcotest <-rbco1
bcotest[is.na(bcotest)] <- 4

tretest <- cbind(btest,reb1)
missmap(tretest)
mean(is.na(tretest))
tebtest <- na.omit(tretest)
missmap(tebtest)

dat <- data.frame(btest,sdtest,bcrtest, bcotest)

## total scores
cleandat <- dat %>%
  mutate (
    sdt  = (sds1 + sds2 + sds3 + sds4 + sds5. + sds6 + sds7. + sds8 + sds9. + sds10. + sds11 + sds12 + sds13.)/13,
    bt   = (b1. + b2. + b3. + b4. + b5. + b6. + b7. + b8. + b9. + b10. + b11. + b12. + b13. + b14. + b15 + b16 + b17 + b18. + b19. + b20 + b21. + b22. + b23 + b24. + b25)/25,
    bcrt = (bcrit1 + bcrit2 + bcrit3. + bcrit4 + bcrit5 + bcrit6. + bcrit7 + bcrit8 + bcrit9. + bcrit10 + bcrit11 + bcrit12. + bcrit13 + bcrit14 + bcrit15 + bcrit16. + bcrit17 +bcrit18)/18, 
    bcot = (bconv1 + bconv2 + bconv3 + bconv4 + bconv5 + bconv6 + bconv7 + bconv8 + bconv9))

alpha(btest)
alpha(sdtest)
alpha(bcrtest)
alpha(bcotest)

cor.dat <- cleandat %>% select (sdt,bt,bcrt,bcot)
res2 <- rcorr(as.matrix(cor.dat))
res2
plot(cor.dat$bt, cor.dat$bcrt, na.action = na.exclude)

#test-retest - correlation between time one and time two. 
alphtretest <- tebtest %>%
  mutate (t1 = (b1. + b2. + b3. + b4. + b5. + b6. + b7. + b8. + b9. + b10. + b11. + b12. + b13. + b14. + b15 + b16 + b17 + b18. + b19. + b20 + b21. + b22. + b23 + b24. + b25)/25,
          t2 = (rb1. + rb2. + rb3. + rb4. + rb5. + rb6. + rb7. + rb8. + rb9. + rb10. + rb11. + rb12. + rb13. + rb14. + rb15 + rb16 + rb17 + rb18. + rb19. + rb20 + rb21. + rb22. + rb23 + rb24. + rb25)/25)

alphte <- alphtretest %>%
  select (t1,t2)

t.test(alphte$t1, alphte$t2, paired = TRUE)
cor.test(alphte$t1, alphte$t2)
plot(alphte$t1, alphte$t2)
mean(alphte$t1)
mean(alphte$t2)