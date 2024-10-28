## GR6J SOURCE FILE
#LIBRAIRIES
library(airGR)
library(stats)
library(ggplot2)
library(reshape2)
library(hydroTSM)

#-------------------Creation des intants du models-------------------------
Modelinputs <- CreateInputsModel(FUN_MOD = RunModel_GR4J,
                                 DatesR = AirGRData$DATE,
                                 Precip =AirGRData$PCP,
                                 PotEvap = AirGRData$Evap,
                                 TempMean = AirGRData$Tmean)
#--------------------Configuration du modèle----------------------

Ind_Run <- seq(which(format(AirGRData$DATE, format = "%Y-%m-%d")==start_run_date), 
               which(format(AirGRData$DATE, format = "%Y-%m-%d")==end_run_date))

Ind_warmUp <- seq(which(format(AirGRData$DATE, format = "%Y-%m-%d")==start_wramup_date), 
                  which(format(AirGRData$DATE, format = "%Y-%m-%d")==end_wramup_date))

RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                               InputsModel = Modelinputs, IndPeriod_Run = Ind_Run,
                               IniStates = NULL, IniResLevels = NULL,
                               IndPeriod_WarmUp = Ind_warmUp,Outputs_Cal = "Qsim")
#-------------------------D?finition d'une fonction Objective---------------------------------
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, 
                               InputsModel = Modelinputs, 
                               RunOptions = RunOptions,
                               Obs = AirGRData$Qmm[Ind_Run])
#---------------------------Calibration interne-------------------------------------------------
#CalibOptions <- CreateCalibOptions(FUN_MOD = RunModel_GR4J, FUN_CALIB = Calibration_Michel)

#OutputsCalib <- Calibration_Michel(InputsModel = Modelinputs, RunOptions = RunOptions,
                                  # InputsCrit = InputsCrit, CalibOptions = CalibOptions,
                                   #FUN_MOD = RunModel_GR4J)



#---------------------------Calibration Externe: Evolution Diff?rentiele--------------------------
k <- 0
GR6J.PARAMETERS <- rbind()
OptimGR6J <- function(ParamOptim) {
  #ParamOptim2 <<-ParamOptim 
  k <<- k+1
  ## Transformation of the parameter set to real space
  
 RawParamOptim <<-airGR::TransfoParam_GR4J(ParamIn =best.param,
  Direction = "TR")
  ## Simulation given a parameter set
  OutputsModel <<- airGR::RunModel_GR4J(InputsModel = Modelinputs,
                                        RunOptions = RunOptions,
                                        Param =RawParamOptim)# ParamOptim)
  
  ## Computation of the value of the performance criteria
  #Qsim_Extract <- subset(Qsim,as.Date(Qsim$DATE)%in%as.Date(airGrData$DATE))
  GR4J.Qsim <- data.frame(DATE=as.Date(OutputsModel$DatesR),
                          Qsim=OutputsModel$Qsim*coef_conver)%>%  #*coef_conver
    filter(DATE%within%GR6J.calib.periode)%>%
    select(Qsim)%>%unlist

  NSE.Val <- round(hydroGOF::NSE(GR6J.Qsim,GR6J.Calib.Data),2)
  
  ParamOptim2 <-t(ParamOptim)
  run.param <- data.frame(NSE.Val,ParamOptim2)
  GR6J.PARAMETERS <<-rbind(GR6J.PARAMETERS,run.param) 
  print(paste0("Meilleur Nash : ",max(GR6J.PARAMETERS$NSE.Val)))
  print(paste0(k, " : " ,NSE.Val))
  val_nash <- -NSE.Val
  return(val_nash)
}






