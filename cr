library(zoo)
library(AER)
library(dplyr)
library(MASS)
library(mvtnorm)
library(rddtools)
library(scales)
library(stargazer)
library(tidyr)


# conjunto de datos STAR
data(STAR)

head(STAR)
dim(STAR)
#obtener nombre de las variables
names(STAR)
head(STAR, 2)
STAR[1,]
#para obtener entradas que no sean NA
STAR[1, !is.na(STAR[1, ])]
head(STAR$star1,10)

fmk <- lm(I(readk + mathk) ~ stark, data = STAR)
fm1 <- lm(I(read1 + math1) ~ star1, data = STAR)
fm2 <- lm(I(read2 + math2) ~ star2, data = STAR)
fm3 <- lm(I(read3 + math3) ~ star3, data = STAR)



coeftest(fmk, vcov = vcovHC, type= "HC1")
coeftest(fm1, vcov = vcovHC, type= "HC1")
coeftest(fm2, vcov = vcovHC, type= "HC1")
coeftest(fm3, vcov = vcovHC, type= "HC1")



EER <- list(sqrt(diag(vcovHC(fmk, type = "HC1"))),
            sqrt(diag(vcovHC(fm1, type = "HC1"))),
            sqrt(diag(vcovHC(fm2, type = "HC1"))),
            sqrt(diag(vcovHC(fm2, type = "HC1"))))



stargazer(fmk,fm1,fm2,fm3,type="text", model.numbers = F,
          column.labels = c("Grado K", "Grado 1", "Grado 2", "Grado 3"),
          dep.var.labels.include = FALSE,se = EER )


STARK <- STAR %>%
  transmute(gender,
            ethnicity,
            stark,
            readk,
            mathk,
            lunchk,
            experiencek,
            schoolidk, degreek) %>%
  mutate(black = ifelse(ethnicity == "afam", 1, 0),
         race = ifelse(ethnicity == "afam" | ethnicity == "cauc", 1, 0),
         boy = ifelse(gender == "male", 1, 0), degreeks = ifelse(degreek == "specialist" , 1, 0))





gradeK1 <- lm(I(mathk + readk) ~ stark + experiencek + degreeks,
              data = STARK)
gradeK2 <- lm(I(mathk + readk) ~ stark + experiencek + degreeks + schoolidk,
              data = STARK)
gradeK3 <- lm(I(mathk + readk) ~ stark + experiencek + degreeks + boy + lunchk
              + black + race + schoolidk,
              data = STARK)


coeftest(gradeK1, vcov. = vcovHC, type = "HC1")
coeftest(gradeK2, vcov. = vcovHC, type = "HC1")
coeftest(gradeK3, vcov. = vcovHC, type = "HC1")


EER1 <- list(sqrt(diag(vcovHC(fmk, type = "HC1"))),
             sqrt(diag(vcovHC(gradeK1, type = "HC1"))),
             sqrt(diag(vcovHC(gradeK2, type = "HC1"))),
             sqrt(diag(vcovHC(gradeK3, type = "HC1"))))


stargazer(fmk,gradeK1,gradeK2,gradeK3,type="text", model.numbers = F, omit.table.layout = "n", digits = 3,
          column.labels = c("k1", "k2", "k3", "k4"),dep.var.caption  = "Dependent Variable: Test Score en Kinder",
          omit="schoolid",dep.var.labels.include = FALSE, 
          add.lines=list(c("School indicators?", "no", "no", "yes", "yes")),se = EER1)

#######################################################################################
STAR1 <- STAR %>%
  transmute(gender,
            ethnicity,
            star1,
            read1,
            math1,
            lunch1,
            experience1,
            schoolid1, degree1) %>%
  mutate(black = ifelse(ethnicity == "afam", 1, 0),
         race = ifelse(ethnicity == "afam" | ethnicity == "cauc", 1, 0),
         boy = ifelse(gender == "male", 1, 0), degree1s = ifelse(degree1 == "specialist" , 1, 0))


grade11 <- lm(I(math1 + read1) ~ star1 + experience1 + degree1s,
              data = STAR1)
grade12 <- lm(I(math1 + read1) ~ star1 + experience1 + degree1s + schoolid1,
              data = STAR1)
grade13 <- lm(I(math1 + read1) ~ star1 + experience1 + degree1s + boy + lunch1
              + black + race + schoolid1,
              data = STAR1)


coeftest(grade11, vcov. = vcovHC, type = "HC1")
coeftest(grade12, vcov. = vcovHC, type = "HC1")
coeftest(grade13, vcov. = vcovHC, type = "HC1")



EER2 <- list(sqrt(diag(vcovHC(fm1, type = "HC1"))),
             sqrt(diag(vcovHC(grade11, type = "HC1"))),
             sqrt(diag(vcovHC(grade12, type = "HC1"))),
             sqrt(diag(vcovHC(grade13, type = "HC1"))))


stargazer(fm1,grade11,grade12,grade13,type="text", model.numbers = F, omit.table.layout = "n", 
          digits = 3,column.labels = c("1", "1.1", "1.2", "1.3"), 
          dep.var.caption  = "Dependent Variable: Test Score en Primer grado",
          omit="schoolid",dep.var.labels.include = FALSE, add.lines=list(c("School indicators?", "no", "no", "yes", "yes")),
          se = EER2)

####################################################################################


STAR2 <- STAR %>%
  transmute(gender,
            ethnicity,
            star2,
            read2,
            math2,
            lunch2,
            experience2,
            schoolid2, degree2 ) %>%
  mutate(black = ifelse(ethnicity == "afam", 1, 0),
         race = ifelse(ethnicity == "afam" | ethnicity == "cauc", 1, 0),
         boy = ifelse(gender == "male", 1, 0), degree2s = ifelse(degree2 == "specialist" , 1, 0))

grade21 <- lm(I(math2 + read2) ~ star2 + experience2 + degree2s,
              data = STAR2)
grade22 <- lm(I(math2 + read2) ~ star2 + experience2 + degree2s + schoolid2,
              data = STAR2)
grade23 <- lm(I(math2 + read2) ~ star2 + experience2 + degree2s + boy + lunch2
              + black + race + schoolid2,
              data = STAR2)
