library(ggplot2)
library(psych)
library(MASS)
library(stargazer)
library(reshape2)

# GRAPHS ------------------
panel <- read.csv("/Users/cooperlawrenz/Documents/School_Fall_2023/Thesis/Data/Extract Aug 1 2023/panel.csv")

panel.A <- subset(panel, !is.na(panel$PHQDEP))
plot.1 <- ggplot(panel.A, aes(x = as.factor(PHQDEP), y = INCTOT, fill = as.factor(PHQDEP))) +
  geom_boxplot(size = .5,outlier.shape = NA) +
  coord_cartesian(ylim=c(-50, 150)) +
  theme_classic() +
  labs( x = "Depression Score", y = " Personal Total Yearly Income In Thousands USD (Base Year - 2009)", title = "Figure 1",subtitle = "Hypothesized Relationship Between Income and Depression",caption  = "Note: Outliers Omitted",fontface="italic") +
  theme(
       title = element_text(family = "sans",size = 12),
       axis.title.x = element_text(family = "sans",size = 9,face = "italic"),
       axis.title.y = element_text(family = "sans",size = 9,face = "italic"),
       plot.subtitle = element_text(family = "sans",size = 9,face = "italic"),
       plot.caption = element_text(family = "sans",size = 9,hjust = 0))+
  scale_fill_brewer(palette="PuBu",breaks = "PHQDEP") 

plot.1


plot.2 <- ggplot(panel.A, aes(x = as.factor(PHQDEP), y = FTOTINCMEPS, fill = as.factor(PHQDEP))) +
  geom_boxplot(size = .5,outlier.shape = NA) +
  coord_cartesian(ylim=c(-50, 150)) +
  theme_classic() +
  labs( x = "Depression Score", y = " Personal Total Yearly Income In Thousands USD (Base Year - 2009)", title = "Figure 2",subtitle = "Hypothesized Relationship Between Family Income and Depression",caption  = "Note: Outliers Omitted",fontface="italic") +
  theme(
    title = element_text(family = "sans",size = 12),
    axis.title.x = element_text(family = "sans",size = 9,face = "italic"),
    axis.title.y = element_text(family = "sans",size = 9,face = "italic"),
    plot.subtitle = element_text(family = "sans",size = 9,face = "italic"),
    plot.caption = element_text(family = "sans",size = 9,hjust = 0))+
  scale_fill_brewer(palette="PuBu",breaks = "PHQDEP")

plot.2

panel$YEAR <- as.factor(panel$YEAR)
panel$MEPSID <- as.factor(panel$MEPSID)
panel$DUID <- as.factor(panel$DUID)
panel$REGIONMEPS <- as.factor(panel$REGIONMEPS)
panel$SEX <- as.factor(panel$SEX)
panel$MARSTAT <- as.factor(panel$MARSTAT)
panel$HEALTH <- as.factor(panel$HEALTH)
panel$EDUC <- as.factor(panel$EDUC)
panel$AEFFORT <- factor(panel$AEFFORT,levels = c(0,1,2,3,4))
panel$AHOPELESS <- factor(panel$AHOPELESS,levels = c(0,1,2,3,4))
panel$ANERVOUS <- factor(panel$ANERVOUS,levels = c(0,1,2,3,4))
panel$ARESTLESS <- factor(panel$ARESTLESS,levels = c(0,1,2,3,4))
panel$ASAD <- factor(panel$ASAD,levels = c(0,1,2,3,4))
panel$AWORTHLESS <- factor(panel$AWORTHLESS,levels = c(0,1,2,3,4))
panel$PHQINTR <- factor(panel$PHQINTR,levels = c(0,1,2,3))
panel$PHQDEP <- factor(panel$PHQDEP,levels = c(0,1,2,3))
panel$SMOKENOW <- as.factor(panel$SMOKENOW)
panel$RACEA <- as.factor(panel$RACEA)
panel$OCCCATRD <- as.factor(panel$OCCCATRD) #Maybe dont use b/c of DF loss
panel$CANCEREV <- as.factor(panel$CANCEREV)
panel$INSULIN <- as.factor(panel$INSULIN)
panel$INTERVLANG <- as.factor(panel$INTERVLANG)

# Initial ord reg w/o control 

panel$INCTOT <- panel$INCTOT/10
panel$INCTOT2 <- panel$INCTOT^2

panel$FTOTINCMEPS <- panel$FTOTINCMEPS/10
panel$FTOTINCMEPS2 <- panel$FTOTINCMEPS^2

QDEP.2 <- polr(PHQDEP ~ INCTOT + INCTOT2, data = panel, Hess = TRUE)
summary(QDEP.2)

AEFT.2 <- polr(AEFFORT ~ INCTOT + INCTOT2, data = panel, Hess = TRUE)

AHOPS.2 <- polr(AHOPELESS ~ INCTOT + INCTOT2, data = panel, Hess = TRUE)

ARES.2 <- polr(ARESTLESS ~ INCTOT + INCTOT2, data = panel, Hess = TRUE)

ASAD.2 <- polr(ASAD ~ INCTOT + INCTOT2, data = panel, Hess = TRUE)

AWORS.2 <- polr(AWORTHLESS ~ INCTOT + INCTOT2, data = panel, Hess = TRUE)

QINTR.2 <- polr(PHQINTR ~ INCTOT + INCTOT2, data = panel, Hess = TRUE)

stargazer(QDEP.2,AEFT.2,AHOPS.2,ARES.2,ASAD.2,AWORS.2,QINTR.2,apply.coef = exp,style = "qje")
#ORD reg -----
#.3 is No instrument  ------.4 is insturment(percent of fam inc) -------.5 is instrument(cap gains)

#The quadratic func form puts the apex at about 130,000 USD

model.1 <- polr(PHQDEP ~ INCTOT + INCTOT2, data = panel, Hess = TRUE)
summary(model.1)

model.2 <- polr(PHQDEP ~ INCTOT + INCTOT2+ FTOTINCMEPS + YEAR + REGIONMEPS +SEX + MARSTAT, data = panel, Hess = TRUE)
summary(model.2)


QDEP.3 <- polr(PHQDEP ~ INCTOT + INCTOT2 + FTOTINCMEPS + FTOTINCMEPS2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                  EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE +ANERVOUS,
                  data = panel, Hess = TRUE)
AEFT.3 <-polr(AEFFORT ~ INCTOT + INCTOT2 + FTOTINCMEPS + FTOTINCMEPS2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                           EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
                          data = panel, Hess = TRUE)
AHOPS.3 <-polr(AHOPELESS ~ INCTOT + INCTOT2 + FTOTINCMEPS + FTOTINCMEPS2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                         EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
                       data = panel, Hess = TRUE)
ARES.3 <-polr(ARESTLESS ~ INCTOT + INCTOT2 + FTOTINCMEPS + FTOTINCMEPS2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                         EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
                       data = panel, Hess = TRUE)
ASAD.3 <-polr(ASAD ~ INCTOT + INCTOT2 + FTOTINCMEPS + FTOTINCMEPS2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                         EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
                       data = panel, Hess = TRUE)
AWORS.3 <-polr(AWORTHLESS ~ INCTOT + INCTOT2 + FTOTINCMEPS + FTOTINCMEPS2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                         EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
                       data = panel, Hess = TRUE)
QINTR.3 <-polr(PHQINTR ~ INCTOT + INCTOT2 + FTOTINCMEPS + FTOTINCMEPS2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                         EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
                       data = panel, Hess = TRUE)

stargazer(QDEP.3 ,QINTR.3,AEFT.3 ,AHOPS.3,ARES.3,ASAD.3,AWORS.3, apply.coef = exp, type = "html",style = "qje",
          out = "/Users/cooperlawrenz/Documents/School Fall 2023/Thesis/Data/Extract Aug 1 2023/results/noInstPHQDEPFULLquad.html")

keep.1 <- c("INCTOT","FTOTINCMEPS","SEX2","MARSTAT30","HEALTH5","EDUC400","SMOKENOW2","RACE200","CANCEREV2","INVERVLANG2","ANERVOUS1","INCTOT2","FTOTINCMEPS2")
stargazer(QDEP.3 ,QINTR.3,AEFT.3 ,AHOPS.3,ARES.3,ASAD.3,AWORS.3, keep = keep.1,apply.coef = exp, type = "html",style = "qje")


# fracfamInc Instrument ----------------------------------------------


#Prepare data
panel.2 <- panel[, names(panel) %in% c("INCTOT","FTOTINCMEPS","YEAR","REGIONMEPS","SEX","MARSTAT","HEALTH",
                                       "EDUC","SMOKENOW","RACEA","CANCEREV","INTERVLANG","FAMSIZE","ANERVOUS",
                                       "fracfamInc","capGains","fraccapgains","PHQDEP","AHOPELESS","ASAD","AWORTHLESS","AEFFORT","ARESTLESS","PHQINTR")]
panel.2 <- na.omit(panel.2)

fracfamincIV.1 <- lm(INCTOT ~ FTOTINCMEPS + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                         EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS + fracfamInc,
                       data = panel.2)
summary(fracfamincIV.1)
fracfaminc.hat <- fitted.values(fracfamincIV.1)

fracfaminc.hat2 <- fracfaminc.hat^2

QDEP.4 <- polr(PHQDEP ~  fracfaminc.hat + fracfaminc.hat2 + FTOTINCMEPS + FTOTINCMEPS2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                         EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
                       data = panel.2, Hess = TRUE)
AEFT.4 <-polr(AEFFORT ~  fracfaminc.hat + fracfaminc.hat2 + FTOTINCMEPS + FTOTINCMEPS2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
              data = panel.2, Hess = TRUE)
AHOPS.4 <-polr(AHOPELESS ~  fracfaminc.hat + fracfaminc.hat2 + FTOTINCMEPS + FTOTINCMEPS2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                 EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
               data = panel.2, Hess = TRUE)
ARES.4 <-polr(ARESTLESS ~ fracfaminc.hat + fracfaminc.hat2 + FTOTINCMEPS + FTOTINCMEPS2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                                EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
                                data = panel.2, Hess = TRUE)
ASAD.4 <- polr(ASAD ~  fracfaminc.hat + fracfaminc.hat2 + FTOTINCMEPS + FTOTINCMEPS2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                                EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
                                 data = panel.2, Hess = TRUE)
AWORS.4 <- polr(AWORTHLESS ~ fracfaminc.hat + fracfaminc.hat2 + FTOTINCMEPS + FTOTINCMEPS2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                              EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
                            data = panel.2, Hess = TRUE)
QINTR.4 <-polr(PHQINTR ~  fracfaminc.hat + fracfaminc.hat2 + FTOTINCMEPS + FTOTINCMEPS2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                 EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
               data = panel.2, Hess = TRUE)


stargazer(QDEP.4,QINTR.4,AEFT.4,AHOPS.4,ARES.4,ASAD.4,AWORS.4, apply.coef = exp, type = "html",style = "qje",
          out = "/Users/cooperlawrenz/Documents/School Fall 2023/Thesis/Data/Extract Aug 1 2023/results/INSTfamincFULLquad.html")


keep.2 <- c("INCTOT","FTOTINCMEPS","SEX2","MARSTAT30","HEALTH5","EDUC400","SMOKENOW2","RACE200","CANCEREV2","INVERVLANG2","ANERVOUS1","fracfaminc.hat","fracfaminc.hat2","FTOTINCMEPS2")
stargazer(QDEP.4,QINTR.4,AEFT.4,AHOPS.4,ARES.4,ASAD.4,AWORS.4, keep = keep.2,apply.coef = exp,style = "qje")


# capGains Instruemnt ------------------------------------------

capGainsIV.1 <- lm(FTOTINCMEPS ~ INCTOT + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                       EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS + capGains,
                     data = panel.2)
summary(capGainsIV.1)
capGains.hat <- fitted.values(capGainsIV.1)

capGains.hat2 <- capGains.hat2^2 
#DO I PUT INCTOT AND FAMINCTOT INTO INCTOT squared??????

QDEP.5 <- polr(PHQDEP ~ INCTOT + INCTOT2 + capGains.hat + capGains.hat2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                 EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
               data = panel.2, Hess = TRUE)
AEFT.5 <-polr(AEFFORT ~ INCTOT + INCTOT2 +  capGains.hat + capGains.hat2+ YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
              data = panel.2, Hess = TRUE)
AHOPS.5 <-polr(AHOPELESS ~ INCTOT + INCTOT2 +  capGains.hat + capGains.hat2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                 EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
               data = panel.2, Hess = TRUE)
ARES.5 <-polr(ARESTLESS ~ INCTOT + INCTOT2 +  capGains.hat + capGains.hat2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
              data = panel.2, Hess = TRUE)
ASAD.5 <- polr(ASAD ~ INCTOT + INCTOT2 +  capGains.hat + capGains.hat2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                 EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
               data = panel.2, Hess = TRUE)
AWORS.5 <- polr(AWORTHLESS ~ INCTOT + INCTOT2 +  capGains.hat + capGains.hat2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                  EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
                data = panel.2, Hess = TRUE)
QINTR.5 <-polr(PHQINTR ~ INCTOT + INCTOT2 +  capGains.hat + capGains.hat2 + YEAR + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                 EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,
               data = panel.2, Hess = TRUE)


stargazer(QDEP.5,QINTR.5,AEFT.5,AHOPS.5,ARES.5,ASAD.5,AWORS.5, apply.coef = exp, type = "html",style = "qje",
          out = "/Users/cooperlawrenz/Documents/School Fall 2023/Thesis/Data/Extract Aug 1 2023/results/INSTcapGainsFULLquad.html")

keep.3 <- c("INCTOT","FTOTINCMEPS","SEX2","MARSTAT30","HEALTH5","EDUC400","SMOKENOW2","RACE200","CANCEREV2","INVERVLANG2","ANERVOUS1","capGains.hat","fraccapgains","INCTOT2","capGains.hat2")
stargazer(QDEP.5,QINTR.5,AEFT.5,AHOPS.5,ARES.5,ASAD.5,AWORS.5, keep = keep.3,apply.coef = exp,style = "qje")



# Proportional Odds ----------------------


sf <- function(y) {
  c('Y>=0' = qlogis(mean(y >= 0)),
    'Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

(s <- with(panel.2, summary(as.numeric(PHQDEP) ~ INCTOT + FTOTINCMEPS,
                            fun=sf)))

s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]

 plot(s, which=1:3, pch=1:3, xlab='logit', main='', xlim=range(-5,0))

 
#Predictions ----------------------------
 
model.predict <- polr(PHQDEP ~ INCTOT + FTOTINCMEPS + REGIONMEPS +SEX + MARSTAT + HEALTH + 
                        EDUC + SMOKENOW + RACEA + CANCEREV + INTERVLANG + FAMSIZE + ANERVOUS,data = panel.2,Hess=TRUE)
newdat <- data.frame(
   INCTOT = rep(seq(from = -10, to = 100, length.out = 100),4),
   FTOTINCMEPS = rep(c(46.68)),
   #YEAR = rep(unique(panel$YEAR), length.out = 200),
   REGIONMEPS = rep("3"),
   SEX = rep(c("1")),
   MARSTAT= rep("20"),
   HEALTH = rep("2"),
   EDUC = rep("201"),
   SMOKENOW = rep("1"),
   RACEA = rep("100"),
   CANCEREV = rep("1"),
   INTERVLANG = rep("1"),
   FAMSIZE = rep(2),
   ANERVOUS = rep("0")
)
newdat <- cbind(newdat, predict(model.predict,newdat,type = "probs"))
lnewdat <- melt(newdat, id.vars = c("INCTOT","FTOTINCMEPS","REGIONMEPS","SEX","MARSTAT","HEALTH","EDUC",
                                    "SMOKENOW","RACEA","CANCEREV","INTERVLANG","FAMSIZE","ANERVOUS"), 
                variable.name = "PHQDEP",value.name = "Probability")
ggplot(lnewdat, aes(x= INCTOT,y = Probability,color = PHQDEP))+
  labs(title = "Figure 5",subtitle = "Change in probability all else constant ")+
  geom_line()+
  theme_linedraw()+
  scale_color_manual(values = brewer.pal(11, "RdYlBu")[c(8,9,10,11)])
dggplot(lnewdat, aes(x= FTOTINCMEPS,y = Probability,color = Level))+
  geom_line()


model.predict1 <- polr(PHQDEP ~ INCTOT, data = panel.2, Hess = TRUE)
newdata.1 <- data.frame(
  INCTOT = rep(seq(from = -10, to = 100, length.out = 100),4)
)
newdata.1 <- cbind(newdata.1, predict(model.predict1, newdata.1,type ="probs"))
lnewdat <- melt(newdata.1, id.vars = "INCTOT",variable.name = "PHQDEP",value.name = "Probability")
ggplot(lnewdat, aes(x= INCTOT,y = Probability, color = PHQDEP))+
  labs(title = "Figure 4",subtitle = "Change in probability") +
  geom_line()+
  theme_linedraw()+
  scale_color_manual(values = brewer.pal(11, "RdYlBu")[c(8,9,10,11)])



#Table Making ---------------------------------
Variable_Desciptions_ <- read.csv("/Users/cooperlawrenz/Documents/School Fall 2023/Thesis/Data/Extract Aug 1 2023/Variable Desciptions .csv")



styled_table <- kable(Variable_Desciptions_, "html", align = "c") %>%
  kable_classic(full_width = FALSE) %>%
  row_spec(0, bold = TRUE) 


#pandoc can print exponentiated polr model no trouble

pandoc.table(QDEP.3)

summary(QDEP.3, digits = 3)

sapply(QDEP.3, is.language)
QDEP.3 <- Filter(Negate(is.language), QDEP.3)
sapply(QDEP.3, is.infinite)
sapply(QDEP.3, is.nan)

#ord.intercepts = FALSE
