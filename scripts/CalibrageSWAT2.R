###############################################################################
#############           Mise en oeuvre du Modèle SWAT         #################
###############################################################################
## NETOYAGE 
rm(list=ls())
#LIBRAIRIES
#library(SWATplusR)
library(SWATrunR)
library(DEoptim)
library(fast)
library(sensitivity)
library(hydroPSO)
library(DBI)
library(RSQLite)
library(tidyverse)
library(sf)
library(forcats)
library(hydroGOF)
library(hydroTSM)
library(lhs)
library(lubridate)

## WORK SPACE
setwd("G:/IGEDD/Memoire/Projet_Nowkuy")

## DATA BASE
DataBase <- dbConnect(drv = RSQLite::SQLite(),
                      dbname=file.path("DataBase","ProjectDataBase.sqlite"))

## DATA IMPORTATION
Q_Obs_Data <- dbReadTable(conn = DataBase,name ="QObsData" )%>%
  mutate(DATE=as.Date(DATE))

## CHOIX DES PERIODES DE CALIBRATION ET VALIDATION
calib.periode <- interval(as.Date("2003-01-01"),as.Date("2008-12-31"))
valid.periode <- interval(as.Date("2009-01-01"),as.Date("2014-12-31"))

calib.data <- Q_Obs_Data%>%
  filter(DATE%within%calib.periode)

valid.data <- Q_Obs_Data%>%
  filter(DATE%within%valid.periode)

## SWAT PARAMETERS
SWATParameters<- tibble("GW_DELAY.gw|change = absval" = c(30, 450),
                        "OV_N.hru|change = absval" = c(0.01, 30),
                        "CN2.mgt|change = relchg" = c(-0.2, 0.2),
                        "REVAPMN.gw|change = absval" = c(0, 500),
                        "SOL_AWC.sol|change = absval" = c(0.01, 0.5),
                        "SURLAG.bsn|change = absval" = c(0, 24),
                        "ESCO.hru|change = absval" = c(0, 1),
                        "SHALLST.gw|change = absval" = c(0, 1000),
                        "GWQMN.gw|change = absval" = c(0, 5000),
                        "ALPHA_BF.gw|change = absval" = c(0, 1),
                        "LAT_TIME.hru|change = absval" = c(0, 180),
                        "SLSOIL.hru|change = absval" = c(0, 150),
                        "HRU_SLP.hru|change = absval" = c(0, 0.6),
                        "CH_K2.rte|change = absval" = c(0, 500),
                        "SOL_Z.sol|change = abschg" = c(0, 1000),
                        "CH_K1.sub|change = absval" = c(0, 300),
                        "SLSUBBSN.hru|change = absval" = c(10, 150),
                        "CANMX.hru|change = absval" = c(0, 100),
                        "CH_N2.rte|change = absval" = c(0, 0.3),
                        "CH_N1.sub|change = absval" = c(0.01, 30),
                        "EVRCH.bsn|change = absval" = c(0.5, 1),
                        "GW_REVAP.gw|change = absval" = c(0, 0.3),
                        "RCHRG_DP.gw|change = absval" = c(0, 1),
                        "EPCO.hru|change = absval" = c(0, 1),
                        "PLAPS.sub|change = absval" = c(0, 100))


lower <- as.numeric(SWATParameters[1,]) # Borne inf?rieures des param?tres
upper <- as.numeric(SWATParameters[2,])# Bornes superieures des param?tres

## Parametres de optimum
SWATParametres <- readRDS(file = file.path("DataBase","SWATPARAMETERS.rds"))%>%
  as_tibble%>%
  slice(1)
##--------------------SWAT Modèle Run-----------------------------------------------------
SWATParametres <- readRDS(file = "G:/IGEDD/Memoire/Projet_Nowkuy/OUTPUT/Data/SWAT_PARAMETERSS.rds")%>%
  slice(1)%>%
  select(-nse)

SWATProject <- dir("G:/IGEDD/Memoire/Projet_Nowkuy/SWATProject")[c(5)]
ModelEvaluation <- rbind()
for (p in 1:length(SWATProject)) {
  model_project_path <- paste(getwd(),"SWATProject",SWATProject[p],sep = "/")
  #project_path <- "D:/PROJET/Article/CC_Eau/ProjetNowkuyBassin/TxtInOut2"
  
  modele_run <- SWATrunR::run_swat2012(
    project_path = model_project_path,
    output =define_output(file = "rch",
                          variable = "FLOW_OUT",
                          unit = 1),
    parameter = SWATParametres,
    start_date = "1984-01-01",
    end_date = "2040-12-31",
    years_skip = 3)
  x = SWATProject[p]
  RCP <- substr(x=x,start = nchar(x)-4,stop =nchar(x))
  ClimModel <- substr(x=x,start = 10,stop =nchar(x)-6)
  SimData <- modele_run$simulation%>%
    dplyr::mutate(Model="SWAT",ClimateModel=ClimModel,RCP=RCP)
  ModelEvaluation <- rbind(ModelEvaluation,SimData)
}
ModelEvaluation2 <- ModelEvaluation%>%
  mutate(FLOW_OUT=ifelse(FLOW_OUT>250,NA,FLOW_OUT))


### ANALYSE DES SIMULATIONS
QOBS <- dbReadTable(conn = DataBase,name ="QObsData" )%>%
  mutate(DATE=as.Date(DATE))
calib.periode <- interval(as.Date("2003-01-01"),as.Date("2008-12-31"))
valid.periode <- interval(as.Date("2009-01-01"),as.Date("2014-12-31"))

Calib.Data <-QOBS%>%
  filter(DATE%within%calib.periode)%>%
  select(Debit)%>%unlist

Valid.Data <-QOBS%>%
  filter(DATE%within%valid.periode)%>%
  select(Debit)%>%unlist


Nash_by_Model <-ModelEvaluation%>%
  filter(date%within%calib.periode)%>%
  group_by(ClimateModel,RCP)%>%
  summarize(Calib.Nash=NSE(sim = FLOW_OUT,obs =Calib.Data ))

QMAXperModel <- ModelEvaluation2%>%
  mutate(YEARS=year(date))%>%
  group_by(ClimateModel,RCP)%>%
  summarize(Qmax=max(FLOW_OUT))

SWATQSim <- ModelEvaluation%>%
  dplyr::rename(DATE=date)%>%
  mutate(DATE=as.character(DATE))%>%
   #mutate(ClimateModel=recode(ClimateModel,CNRMCERFACSCNRMCM5=2,CCCmaCanESM2=10))%>%
  dplyr::select(DATE,Model,ClimateModel,RCP,FLOW_OUT)
climateModel <- dbReadTable(DataBase,"ClimateModel")

dbWriteTable(conn = DataBase,name ="SWATQSimCCCma",value = SWATQSim, field.types=c(DATE="DATE") )
#============Etude de la sensibilité des paramètres du modèles============
##-1 Constitution du Jeux de paramètre
ParameterSETS <- fast_parameters(
  minimum =lower,
  maximum =upper,
  names =names(SWATParameters)) %>%
  as_tibble()

 
##-2 Exécution du modèle avec les définis (3555 param?tres)
SensTest<- run_swat2012(
    project_path = project_path,
    output =define_output(file = "rch",
                          variable = "FLOW_OUT",
                          unit = 1),
    parameter = ParameterSETS,
    start_date = "2001-01-01",
    end_date = "2010-12-31",
    years_skip = 2,
    output_interval = "d", 
    keep_folder = T)

#-3 Sauvegarde des donnees simulees
output_path <- "D:/MODELISATION/Mouhoun/Output/ModelCalib/"
saveRDS(object =SensTest,file = paste0(output_path,"//SensDischarge.rds")) # sauvegarde des donn?es de la simulation

##-4 Evaluation des performances du modèles
    
##-1 Formatage de donnees
periode_simulation <- SensTest$simulation$date
sim_data <- SensTest$simulation

q_obs <- Q_Obs_Data%>%
  subset(select(.,DATE)%>%unlist%>%as.Date%in%periode_simulation)

q_sim <- sim_data%>%
  subset(select(.,date)%>%unlist%>%as.Date%in%as.Date(q_obs$DATE))
    
##-2 Calcul de Nash
NSEVal <- sapply(q_sim[,-1],function(x){
  nse <- NSE(x,q_obs$FlowOut2)
  nse
})

##-3 calcul des valeurs de sensibilit?
sensval_fast <- sensitivity(NSEVal, 25)

## Sauvegarde des r?sultats
CalibSWATdata <- data.frame(Nash=NSEVal,SensTest$parameter$values)
CalibSWATdata <- CalibSWATdata[order(CalibSWATdata$Nash,decreasing = T),]
saveRDS(object =CalibSWATdata,file = paste0(output_path,"SWATCalibData.rds")) # sauvegarde des donn?es de la simulation


##-4 Visualisation des r?sultat

##Formatage de donn?es
par_names <- SensTest$parameter$definition$par_name
ResultFast <- tibble(parameter = as.factor(par_names),
                     fast      = sensval_fast)%>%
  mutate(parameter = factor(parameter)%>% 
           fct_reorder(., fast))

#Plots: graphiques donnant les param?tres par ordre de sensibilit?
ggplot(data = ResultFast) +
  geom_col(aes(x = parameter, y = fast),fill="steelblue",color="black")+
  xlab(toupper("Param?ters")) +
  ylab(toupper("Sensibilit?"))+ 
  coord_flip(expand = T) +
  theme_bw()+
  theme(axis.text.x = element_text(colour = "black",size = 10),
        axis.title.x.bottom =element_text(colour = "black",face = "bold",size = 12),
        axis.title.y =element_text(colour = "black",face = "bold",size = 12),
        axis.text.y = element_text(colour = "black",size = 8))

#Plot: graphique montrant l'?volution du crit?re de Nash en fonction des valeurs prises par les param?tres
Nash_Lab <- rep(c("positive","negative"),times=c(911,2644))
CalibSWATdata$Nash_Lab <- Nash_Lab
CalibSWATdata2 <- melt(data = CalibSWATdata,id.vars = c("Nash","Nash_Lab"))
CalibSWATdata3 <- CalibSWATdata2[CalibSWATdata2$Nash>=-1,]

ggplot(data = CalibSWATdata3,aes(x=value,y=Nash))+
  geom_point()+
  facet_wrap(facets = .~variable,scales = "free_x",as.table = F)+
  xlab("Param?tres")+
  ylab("Nash Sutcliffe")+
  theme_bw()+
  theme(strip.background = element_rect(fill = NA,colour = "blue",size =1.5),
        strip.text = element_text(face = "italic",size = 10),
        axis.text.x = element_text(size = 9,angle = 90,face = "bold"))


#=====================OPTIMISATION DES PARAMETRES=================================

##Extraction des param?tres sensibles

ResultFast2 <- ResultFast[order(ResultFast$fast,decreasing = T),]
sens_params <-NULL
for (j in 1:9) {
  sens_params2 <-as.matrix(SWATParameters[,grep(ResultFast2$parameter[j],names(SWATParameters))]) 
  sens_params <- cbind(sens_params,sens_params2)
}
OptimParameters <- as_tibble(sens_params)

lower_vals <- as.numeric(sens_params[1,])# bornes inf?rieures des param?tres
upper_vals <-  as.numeric(sens_params[2,])#bornes superieures des param?tres

    # Fonction d'optimisation
periode_calage <- as.Date(dip("2001-01-01","2010-12-31"))
q_obs2 <- Q_Obs_Data%>%
  subset(DATE%in%periode_calage)
dis_obs <-q_obs2$FlowOut2
##Cr?ation de la fonction d'optimisation
##Initialisation des param?tres
Parametrs_2 <- NULL
k <- 0
OptimSWATplus <- function(ParamOptim) {
  ParamOptim <<- as.numeric(ParamOptim)
  ParamOptim2 <<-as_tibble(t(ParamOptim ))
  params <<- fast_parameters(
    minimum =ParamOptim,
    maximum =ParamOptim,
    names =names(OptimParameters)) %>%
    as_tibble()
  params_2 <<- params[1,]
  k <<-k+1 
  ## Simulation 
  print(k)
  OutputsModel <<- SWATplusR::run_swat2012(
    project_path = project_path,
    output =define_output(file = "rch",
                          variable = "FLOW_OUT",
                          unit = 23),
    parameter = params_2,
    start_date = "1998-01-01",
    end_date = "2010-12-31",
    years_skip = 3,
    output_interval = "d", 
    keep_folder = T,refresh = F)
## Evaluation de la simulation
  q_sim<-OutputsModel$simulation%>%as.data.frame%>%
    subset(select(.,date)%>%unlist%>%as.Date%in%as.Date(q_obs2$DATE))
    
dis_sim <-q_sim$FLOW_OUT
  
  nse <- -NSE(dis_sim,dis_obs)
  Parametrs <- cbind(nse,params_2)
  Parametrs_2 <<-  rbind(Parametrs_2,Parametrs)
  Nash_val <- -min(Parametrs_2$nse)
  print(paste0("Meilleur Nash : ",Nash_val))
  print(-nse)
  return(nse) 
}

## Optimisation avec la m?thode de l'Evolution Diff?rentielle
      
#Premi?re ex?cution
calibDEoptim <- DEoptim::DEoptim(fn = OptimSWATplus,
                                 lower = lower_vals, upper = upper_vals,
                                 control = DEoptim::DEoptim.control(NP = 500, trace = 10,VTR=-1,
                                                                    steptol=500,
                                                                    reltol = 10^-20,
                                                                    strategy = 6,
                                                                    itermax=5000))
#Deuxi?me ex?cution: modification des place de variations
lower_sens1 <- lower_vals*0.1-lower_vals
upper_sens1 <- upper_vals*0.1+upper_vals

dd <- duplicated(Parametrs_2$nse)# suppression des valeurs de Nash identique
ParamsNSE <- Parametrs_2[!dd,]

#d?finition d'une population initiale (jeu de param?tres)
initialpop1 <- subset(ParamsNSE,ParamsNSE$nse<=1)
initialpop <- as.matrix(initialpop1[order(initialpop1$nse),-1])
initialpop <- as_tibble(initialpop)
 
#saveRDS(object = initialpop,file = "Omptim_Q.rds") # sauvedarde de la population initiale
##Deuxi?me ex?cution du mod?le
calibDEoptim <- DEoptim::DEoptim(fn = OptimSWATplus,
                                 lower = lower_vals, upper = upper_vals,
                                 control = DEoptim::DEoptim.control(NP = 395, trace = 10,VTR=-1,
                                                                    steptol=500,
                                                                    reltol = 10^-20,
                                                                    strategy = 6,
                                                                    itermax=5000,
                                                                    initialpop = initialpop))
#=================Analyse des r?sultats de la calibration===========  
CalibSWATdata2 <- CalibSWATdata[order(CalibSWATdata$Nash,decreasing = T),]
best_param <- as.numeric(CalibSWATdata2[1,][,-c(1,27)])
best_param2 <<- fast_parameters(
  minimum =best_param,
  maximum =best_param,
  names =names(SWATParameters)) %>%
  as_tibble()
best_param_2 <- best_param2[1,]

modele_run <- run_swat2012(
  project_path = project_path,
  output =define_output(file = "rch",
                        variable = "FLOW_OUT",
                        unit = 23),
  parameter = best_param_2,
  start_date = "2008-01-01",
  end_date = "2017-06-30",
  years_skip = 3)

Q_sim_SWAT <- modele_run$simulation
Q_sim_SWAT2 <- subset(x = Q_sim_SWAT,subset =date%in%q_obs$DATE)
q_obs <- subset(obs,subset =DATE%in%dip("2011-01-01","2017-06-30"),select =c("DATE","FlowOUT"))
NSEVal <- sapply(Q_sim_SWAT[,-1],function(x){
  nse <- NSE(x,q_obs$FlowOUT)
  nse
})

obs <- subset(obs,subset =DATE%in%Q_sim_SWAT$date)
Data_validation_SWAT_GR6J <- data.frame(DATE=Q_sim_GR6J$DATE,SWAT=Q_sim_SWAT$FLOW_OUT,GR6J=Q_sim_GR6J$q_sim_GR6J,obs=obs$FlowOUT)
saveRDS(object = Data_validation_SWAT_GR6J,file =paste0(output_path,"Data_validation_SWAT_GR6J.rds") )

hydroGOF::NSE(sim = Data_validation_SWAT_GR6J$SWAT,obs=Data_validation_SWAT_GR6J$obs)

