library("styler")
library("tidyverse")
library("lme4")
library("ggplot2")
library("tidyr")
library("ggeffects")
library("Amelia")
library("table1")
library("patchwork")
library("parameters")
library("prettycode")
library("table1")
library("gghighlight")
library("sjPlot")
library("see")
#library("viridis")
#library("ggpubr")
#library("wesanderson")
library("ggsci")
library("papaja")
#library("here")
library("citr")
library("texreg")


ldf.5<-readRDS("~/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2020/ldf.5") 

# filter only those who are measured 
dfben  <-ldf.5 %>%
  dplyr:: filter(Wave !=2009)%>% # exclude wave 1 (no spirit measures that wave)
  dplyr::filter(YearMeasured==1)%>%
  dplyr::group_by(Id) %>% filter(n() > 2)%>% # select those who have responded to at least 3 waves # Also works for 5 and 7
  dplyr::filter(n() !=0)%>%
  dplyr::ungroup(Id)%>%
  dplyr:: mutate(Years = as.numeric(years))%>%
  dplyr::mutate(Age.10yrs = (Age/10))%>%
  dplyr::mutate(Religion.Church.Log = log(Religion.Church+ 1))%>%
  dplyr::mutate(Pol.Orient.S = scale(Pol.Orient, center = TRUE, scale = TRUE),
         Age.10yrs.C = scale(Age.10yrs, center = TRUE, scale = FALSE),
         Male = factor(Gender),
         Employed = factor(Employed),
         Partner = factor(Partner),
         Edu.S = scale(as.numeric(Edu) ,scale=TRUE,center=TRUE),
         Education = as.numeric(Edu),
         Urban = factor(Urban),
         Believe.Spirit = as.factor(Believe.Spirit),
         Believe.God = as.factor(Believe.God),
         EthnicCategories = factor(EthnicCats),
         LIFESAT.S = scale(LIFESAT, center = TRUE, scale = TRUE),
         PWI.S = scale(PWI, center = TRUE, scale = TRUE),
         Deprivation.S = scale(NZdep, scale=TRUE, center=TRUE),
         Relid.C = scale(Relid, scale = TRUE,center=TRUE),
         Religious = as.factor(Religious),
         Religion.Church.Log.C = scale(Religion.Church.Log, center = TRUE, scale = FALSE))

# nice labels
table1::label(dfben$Age.10yrs.C) <- "Age in Decades (C)"
table1::label(dfben$PWI) <- "Personal Wellbeing"
table1::label(dfben$LIFESAT) <- "Life Satisfaction"

table1::label(dfben$Deprivation.S) <- "Deprivation (S)"
table1::label(dfben$Pol.Orient) <- "Political Conservative"
table1::label(dfben$Pol.Orient.S) <- "Political Conservative (S)"
table1::label(dfben$NZdep) <- "NZ Deprivation Index"
dfben$Partner <- factor(dfben$Partner, labels = c("No Partner","Has Partner"))
dfben$Employed <- factor(dfben$Employed, labels = c("Not Employed","Employed"))
table1::label(dfben$Edu.S) <- "Education (S)"

# beliefs indicator
dfben$Beliefs<- factor(ifelse(dfben$Believe.God == "Not Believe God" & dfben$Believe.Spirit == "Not Believe Spirit", "_Skeptic_", 
                                        ifelse(dfben$Believe.God == "Not Believe God" & dfben$Believe.Spirit == "Believe Spirit", "_SpiritExcludesGod_",
                                               ifelse(dfben$Believe.God == "Believe God" & dfben$Believe.Spirit == "Believe Spirit","GodAndSpirit","GodExcludesSpirit"))))


mod.1 <-lmer (PWI ~  1 +  Years * Beliefs +  Age.10yrs.C + Deprivation.S + Edu.S  + Employed + EthnicCats + Male  + Partner  + Pol.Orient.S + Urban + (1|Id), data= dfben, na.action = na.omit)
mod.2 <-lmer (LIFESAT ~ 1 + Years * Beliefs+ Age.10yrs.C + Deprivation.S + Edu.S  + Employed + EthnicCats + Male  + Partner  + Pol.Orient.S + Urban + (1|Id), data= dfben, na.action = na.omit)
summary(mod.1)
summary(mod.2)

