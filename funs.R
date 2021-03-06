# funs.R
# functions used in this analysis
data_read_mymachine <- function(){
  out <-readRDS("/Users/jbul176/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/ldf.5")
}
#  #out <-readRDS("~/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2020/ldf.5") # previous submission did not have 2019/20 waves
# this code is for citing packages

cite_packages <- function() {
  citation(package = "base", lib.loc = "/Users/josephbulbulia/Dropbox/BIBS")
  toLatex(sessionInfo(), locale = FALSE)
  sapply(names(sessionInfo()$otherPkgs), function(x) print(citation(x), style = "Bibtex"))
  out <- sapply(names(sessionInfo()$otherPkgs),
                function(x) print(citation(x), style = "Bibtex"))
  print(out)
}


# data clean
data_clean_spirit_wellbeing <- function(df,y) {
  #function for cleaning this dataset for any arbitrary number of waves
  n_waves =  as.numeric(y)
  out <- df %>%
    dplyr::mutate(Beliefs = factor(ifelse(df$Believe.God == "Not Believe God" & df$Believe.Spirit == "Not Believe Spirit", "Disbeliever",
                                          ifelse(df$Believe.God == "Not Believe God" & df$Believe.Spirit == "Believe Spirit", "SpiritExcludesGod",
                                                 ifelse(df$Believe.God == "Believe God" & df$Believe.Spirit == "Believe Spirit","GodAndSpirit","GodExcludesSpirit")))))%>%
    dplyr::filter(Wave !=2009)%>% # exclude wave 1 (no spirit measures that wave)
    droplevels() %>% #dropp unused waves
    dplyr::filter(YearMeasured==1)%>%
    dplyr::group_by(Id) %>%
    dplyr::filter(n() > n_waves-1)%>% # select those who have responded to at least n waves #
    dplyr::filter(n() !=0)%>%
    dplyr::ungroup(Id)%>%
    dplyr:: mutate(Years = as.numeric(years))%>%
    dplyr:: mutate(yearsC = scale(years, center=TRUE,scale=FALSE))%>%
    dplyr::mutate(Age.10yrs = (Age/10))%>%
    dplyr::mutate(Religion.Church.Log = log(Religion.Church+ 1))%>%
    dplyr::mutate(Political_Conservativism_S = scale(Pol.Orient, center = TRUE, scale = TRUE),
                  Age_in_Decades_C = scale(Age.10yrs, center = TRUE, scale = FALSE),
                  Male = factor(Gender),
                  Employed = factor(Employed),
                  Has_Partner = factor(Partner),
                  Education_S = scale(as.numeric(Edu) ,scale=TRUE,center=TRUE),
                  Education = as.numeric(Edu),
                  Urban = factor(Urban),
                  Believe_Spirit = as.factor(Believe.Spirit),
                  Believe_God = as.factor(Believe.God),
                  Ethnic_Categories = factor(EthnicCats),
                  LIFESAT_S = scale(LIFESAT, center = TRUE, scale = TRUE),
                  PWI_S = scale(PWI, center = TRUE, scale = TRUE),
                  Deprivation_S = scale(NZdep, scale=TRUE, center=TRUE),
                  Relid_C = scale(Relid, scale = TRUE,center=TRUE),
                  Religious = as.factor(Religious),
                  Religion_Church_Log_C = scale(Religion.Church.Log, center = TRUE, scale = FALSE))%>%
    dplyr::rename(Political_Conservativism = Pol.Orient)

  return(out)
}


# function for showing unique ids
show_unique_id <- function(x){
  numb <- length(unique(x)) # count # of ids
  print(numb)
}

# Function to tally number of responses for the ids
count_waves_participants <- function(x){
  out<-dplyr::count(tally(group_by(x, Id), sort = TRUE, name="number_waves"), number_waves)
  print(out)
}


# demographic table
demographic_table <- function(x){
  table1::table1(~ Age +
                   NZdep +
                   Education +
                   Employed +
                   Ethnic_Categories +
                   Male  +
                   Has_Partner  +
                   Political_Conservativism +
                   Urban +
                   Beliefs +
                   LIFESAT + PWI|Wave, data = x,
                 overall = F)
}

beliefs_table <- function(x){
  table1::table1(~ Beliefs|Wave, data = x,
                 overall = F)
}


# latex demographic table
#library("furniture")
demographic_table_latex <- function(x){
  furniture::table1(d_3,
                    Age,
                    NZdep,
                    Education,
                    Employed,
                    Ethnic_Categories,
                    Male,Has_Partner,
                    Political_Conservativism,
                    Urban,
                    Beliefs,
                    LIFESAT,
                    PWI,
                    splitby = ~Wave,
                    overall=F,
                    output = "latex2",
                    booktabs=TRUE)
}


sample_report <-function(x){
  x %>%
  dplyr::select(Age,
                NZdep,
                Education,
                Employed,
                Ethnic_Categories,
                Male,
                Has_Partner,
                Political_Conservativism,
                Urban,
                Beliefs,
                LIFESAT,
                PWI,
                Wave)%>%
  dplyr::group_by(Wave)%>%
  report::report()%>%
  summary()
}

# function for predicting PWI  Not used

# graph_predictions_pwi <- function(x){
#   pl1 <- ggeffects::ggpredict(model = x, terms = c("years [0:9]","Beliefs"),
#                               ci.lvl = 0.95,
#                               type = "fe",
#                               typical = "mean",
#                               back.transform = TRUE,
#                               ppd = FALSE,
#                               interval = "confidence")
#   plot(pl1, facets = T) +  gghighlight::gghighlight()  +  theme_blank()+
#     ggtitle("Predicted Values of Personal Wellbeing")
# }


# graph_predictions_ls <- function(x){
#   pl1 <- ggeffects::ggpredict(model = x, terms = c("years [0:9]","Beliefs"),
#                               ci.lvl = 0.95,
#                               type = "fe",
#                               typical = "mean",
#                               back.transform = TRUE,
#                               ppd = FALSE,
#                               interval = "confidence")
#   plot(pl1, facets = T) +  gghighlight::gghighlight()  +  theme_blank()+
#     ggtitle("Predicted Values of Life Satisfaction")
# }



graph_predictions <- function(x, y) {
  out <- ggeffects::ggpredict(
      model = x,
      terms = c("Age_within [minmax]", "Beliefs"),
      ci.lvl = 0.95,
      type = "fe",
      typical = "mean",
      back.transform = TRUE,
      ppd = FALSE,
      plot(out, facets = T) +  gghighlight::gghighlight()  +  theme_blank() + ggtitle(y))
}

get_predictions <- function(x) {
  out <- ggeffects::ggpredict(
      model = x,
      terms = c("Age_within [minmax]", "Beliefs"),
      ci.lvl = 0.95,
      type = "fe",
      typical = "mean",
      back.transform = TRUE,
      ppd = FALSE,
      interval = "confidence"
    )
  return(out)
}



graph_predictions_BD <- function(x,y){
  out <- ggeffects::ggpredict(model = x, terms = c("Age_within [minmax]","Beliefs"),
                              ci.lvl = 0.95,
                              type = "fe",
                              typical = "mean",
                              back.transform = TRUE,
                              ppd = FALSE,
                              interval = "confidence")
  plot(out, facets = T) +  gghighlight::gghighlight()  +  theme_blank() + ggtitle(y) # title to be suppled
}

get_predictions_BD <- function(x){
  out <- ggeffects::ggpredict(model = x, terms = c("Age_within [minmax]","Beliefs"),
                              ci.lvl = 0.95,
                              type = "fe",
                              typical = "mean",
                              back.transform = TRUE,
                              ppd = FALSE,
                              interval = "confidence")
  return(out)
}




# latex model function
table_model_latex_pwi <- function(x){ # x is a model
  xtract <-texreg::extract(
    x,
    level = 0.90,
    include.random = TRUE,
    include.rsquared = F,
    include.nobs = T,
    include.loo.ic = F,
    include.waic = F)
  texreg(list(xtract),
         custom.model.names = c("PWI"),
         caption = "Personal Wellbeing",
         sideways = F,
         scalebox = .5,
         #fontsize= "footnotesize",
         label = "tab:REGRESS_PWI",
         ci.force.level = 0.90, bold = 0.05,
         settingstars = 0,
         booktabs = TRUE,
         custom.note ="")
}
# table function
table_model_latex_ls <- function(x){ # x is a model
  xtract <-texreg::extract(
    x,
    level = 0.90,
    include.random = TRUE,
    include.rsquared = F,
    include.nobs = T,
    include.loo.ic = F,
    include.waic = F)
  texreg(list(xtract),
         custom.model.names = c("Life Sat"),
         caption = "Life Satisfaction",
         sideways = F,
         scalebox = .5,
         #fontsize= "footnotesize",
         label = "tab:REGRESS_LS",
         ci.force.level = 0.90, bold = 0.05,
         settingstars = 0,
         booktabs = TRUE,
         custom.note ="")
}

# dual table
#
table_model_latex_dual <- function(x,y){
  xtable <-texreg::extract(
    x,
    level = 0.90,
    include.random = TRUE,
    include.rsquared = F,
    include.nobs = F,
    include.loo.ic = F,
    include.waic = F)
ytable <-texreg::extract(
  y,
  level = 0.90,
  include.random = TRUE,
  include.rsquared = F,
  include.nobs = F,
  include.loo.ic = F,
  include.waic = F)
texreg(list(xtable,ytable),
       custom.model.names = c("PWI","Life Sat"),
       caption = "",
       sideways = F,
       scalebox = .5,
       #fontsize= "footnotesize",
       label = "tab:REGRESS_LS",
       ci.force.level = 0.90, bold = 0.05,
       settingstars = 0,
       booktabs = TRUE,
       custom.note ="")
}




# imputation functions
amelia_imputation_clean <- function(x){
  set.seed(1234)
  # x is the longitudional dataframe (here 'd_3')
  prep <- x %>% # Remove what we do not need anymore
    dplyr::select(c(Beliefs,   # to predict missinglness
                    Age,
                    Id,
                    Education,
                    NZdep,
                    KESSLER6,
                    LIFESAT,
                    PWI,
                    Political_Conservativism,
                    Relid,
                    Ethnic_Categories,
                    Partner,
                    Employed,
                    Male,
                    Urban,
                    years,
                    Wave))
  # impute missing
  prep <- as.data.frame(prep) # tibble won't run in amelia
  prep2 <- Amelia::amelia(
    prep, #dataset to impute
    m = 10, # number of imputations
    cs= c("Id"),
    ts= c("years"),
    noms = c("Ethnic_Categories",
             "Urban",
             "Partner",
             "Male",
             "Employed",
             "Beliefs"),
    idvars=c("Wave","PWI","LIFESAT"), # not imputing outcomes
    polytime = 3)  #https://stackoverflow.com/questions/56218702/missing-data-warning-r
  prep3<- transform.amelia(prep2,
                           Age.10yrs = (Age/10),
                           yearsC = scale(years, center=TRUE,scale=FALSE),
                           Political_Conservativism_S = scale(Political_Conservativism,center=TRUE,scale=TRUE),
                           Employed = factor(Employed),
                           Ethnic_Categories = as.factor(Ethnic_Categories),
                           Urban = as.factor(Urban),
                           Deprivation_S = scale(NZdep, scale=TRUE, center=TRUE),
                           Education_S = scale(Education, scale =TRUE,center=TRUE),
                           Male = as.factor(Male),
                           PWI = as.numeric(PWI),
                           LIFESAT = as.numeric(LIFESAT),
                           Has_Partner = as.factor(Partner),
                           Beliefs = as.factor(Beliefs),
                           Id =as.factor(Id),
                           yearsC = scale(years, center = TRUE, scale = FALSE))
  # center an d scale age
  out <- transform.amelia(prep3,Age_in_Decades_C = scale(Age.10yrs,scale =FALSE, center=TRUE))
  return(out)
}

# run models iterating over imputed data
loop_lmer_model <- function(x,y){
  m <- 10
  mod <- NULL
  for(i in 1:m) {
    mod[[i]] <- lmer(x, data=y$imputations[[i]])
  }
  return(mod)
}


# table of effects
loop_lmer_model_tab <- function(x){
  mp<-lapply(x, model_parameters)
  out<- parameters::pool_parameters(mp)
  return(out)
}


## coefficient plot
plot_coefs <- function(x,y){
 prm1<-parameters::model_parameters(x, test = "pd",
                                       diagnostic ="ESS",
                                       effects = "fixed",
                                       group_level=FALSE,
                                       verbose=FALSE)
 out1<- plot(prm1) + ggtitle("Personal Well-Being Coefficients")

 prm2<-parameters::model_parameters(y, test = "pd",
                                    diagnostic ="ESS",
                                    effects = "fixed",
                                    group_level=FALSE,
                                    verbose=FALSE)
 out2<- plot(prm2) + ggtitle("Life Satisfaction Coefficients")
 out1/out2
}


## imputation prediction plot
## note we just pick the tenth iteration until there is something better


graph_predictions_imputed <-function( x, y){  # x = model objects
  m<-10
  out<-NULL
  for(i in 1:m) {
    out[[i]] <- ggpredict(x[[i]], terms =c("yearsC [minmax]","Beliefs"))
  }
  plots<-NULL
  for(i in 1:m) {
    plots[[i]] <- plot(out[[i]], facets = T) # + scale_y_continuous(limits=c(6.35,6.85) )
  }
  plots[[10]] + gghighlight() + ggtitle(y)
}


create_hux_table<-function(x){
  as_hux(x)%>%
    select("Parameter", "Coefficient", "CI_low","CI_high", "p") %>%
    set_number_format(3)%>%
    set_left_padding(20)%>%
    set_bold(1,everywhere)#%>% # to create
  #quick_latex()
}

#
# pst_1<-describe_posterior(
#   mod.1_b,
#   centrality = "median",
#   dispersion = FALSE,
#   ci = 0.90,
#   ci_method = "hdi",
#   test = c("p_direction"))
# pst_1%>%tibble::as_tibble()%>%
#   # dplyr::slice(n=1:10)%>%
#   dplyr::select(-c("Rhat","ESS"))%>%
#   mutate_if(is.numeric, ~round(., 2))%>%
#   mutate_all(funs(str_replace(., "b_", "")))%>%
#   mutate_all(funs(str_replace(., "_", " X ")))%>%
#   mutate_all(funs(str_replace(., "d_", "")))%>%
#   ggpubr::ggtexttable(rows = NULL,
#                       theme = ttheme("blank"))


plot_mcmc_intervals <- function(x,y){
 d1 <- x %>%
  bayestestR::estimate_density()%>%
    plot(labels = FALSE)%>%
    ggpar(main = y) + theme_plain()
}



