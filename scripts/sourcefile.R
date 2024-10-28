#==================================================================#
#                       FICHIER SOURCE                             #
#==================================================================#

##LIBRARY
library(SWATplusR)
library(DBI)
library(RSQLite)
library(sf)
library(tidyr)
library(tibble)
library(dplyr)
library(fast)
library(forcats)
library(ggplot2)
library(hydroGOF)
library(hydroTSM)
library(lhs)
library(lubridate)
library(purrr)
library(sensitivity)
library(stringr)
library(hydroPSO)
library(DEoptim)
library(reshape2)
library(readxl)
## SWAT MODEL PARAMETERS

SWAT_MODEL_PARAMETERS<- tibble("GW_DELAY.gw|change = absval" = c(30, 450),
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

## CONSTITUTION DE JEU DE PARAMETRES

lower <- as.numeric(SWAT_MODEL_PARAMETERS[1,]) # Borne inf?rieures des param?tres
upper <- as.numeric(SWAT_MODEL_PARAMETERS[2,])# Bornes superieures des param?tres

ParameterFast <- fast_parameters(
  minimum =lower,
  maximum =upper,
  names =names(SWAT_MODEL_PARAMETERS)) %>%
  as_tibble()

## ETUDE SENSIBILITE

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
  
  k<<- k+1 
  ## Simulation given a parameter set
  OutputsModel <<- SWATplusR::run_swat2012(
    project_path = project_path,
    output =define_output(file = "rch",
                          variable = "FLOW_OUT",
                          unit = unit),
    parameter = params_2,
    start_date = start_date,
    end_date = end_date,
    years_skip = years_skip,
    output_interval = "d", 
    keep_folder = T,refresh = T,quiet = T)
  ## Evaluation de la simulation
  q_sim <-as.data.frame(OutputsModel$simulation)%>% 
    subset(date%in%as.Date(Qcalib$DATE))
  dis_sim <- q_sim$FLOW_OUT
  nse <- -NSE(dis_sim,dis_obs)
  Parametrs <- cbind(nse,params_2)
  Parametrs_2 <<-  rbind(Parametrs_2,Parametrs)
  Nash_val <- -min(Parametrs_2$nse)
  print(noquote(paste0("Meilleur Nash : ",round(Nash_val,2))))
  print(noquote(paste0("SIMULATION ", k," : Nash= ",round(-nse,2) )))
  return(nse) 
}
