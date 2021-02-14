
library("tidyverse")
library("dagitty")
library("ggdag")


tidy_ggdag <- dagify(
  WB ~ belief + years + partner + nzdep + urban + male + pols + empl,
  WB ~~ partner + years,
  belief ~ years + age + male + ethn,
  partner ~ nzdep + age + belief, 
  nzdep ~ empl,
  pols ~ age  + empl + ethn + years,
  empl ~ nzdep + edu + ethn,
  exposure =  "belief",
  outcome =   "WB")%>%
  tidy_dagitty()

ggdag(tidy_ggdag) +
  theme_dag()

ggdag_adjustment_set(tidy_ggdag, node_size = 14) + 
  theme(legend.position = "bottom") 
# Male
# Ethn
# AgeC