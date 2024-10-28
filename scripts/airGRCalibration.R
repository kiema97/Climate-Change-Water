#=================  MISE EN OEUVRE DU MODEL GR6J=================
## NETOYAGE
rm(list = ls())
#--------------------Work Space---------
setwd("D:/IGEDD/Memoire/Projet_Nowkuy")
##------------------ Data Manipulation-------------------------------------------
DataBase <- dbConnect(drv = RSQLite::SQLite(),
                      dbname=file.path("DataBase","ProjectDataBase.sqlite"))

BasinArea <- 14900*10^6 # Basin Area in sqrt meter
Coef <-(10^3*86400)/BasinArea # Coef conversion

Q_Obs_Data <- dbReadTable(conn = DataBase,name ="QObsData" )%>%
  mutate(DATE=as.Date(DATE),Qls=Debit/1000,Qmm=Debit*Coef)%>%
  filter(DATE>=as.Date("1981-01-01"),DATE<=as.Date("2016-12-31"))
ClimDatas <- dbReadTable(conn = DataBase,name ="ObsMeanData" )%>%
  mutate(Tmean=(TMAX+TMIN)/2)%>%
  filter(DATE>=as.Date("1981-01-01"),DATE<=as.Date("2016-12-31"))

AirGRData <-Q_Obs_Data%>%
  mutate(PCP=ClimDatas$RAIN,
         Tmean=ClimDatas$Tmean,
         Evap=ClimDatas$EPT, 
         DATE=as.POSIXct(DATE))#%>%
  #filter(!is.na(Debit))
colnames(AirGRData) <-  c("DATE","Debit","Qls","Qmm","PCP","Tmean","Evap")
## SETS MODEL
save_path <- "D:/IGEDD/Memoire/Projet_Nowkuy/OUTPUT/Data"
start_run_date <-"1983-01-01"
end_run_date <- "2016-12-31"
start_wramup_date <- "1981-01-01"
end_wramup_date <- "1982-12-31"
source("D:/IGEDD/Memoire/Projet_Nowkuy/RScripts/airGRSourceFile.R")

## SAVE PARAMETERS
saveRDS(object = OutputsCalib,file = file.path(save_path,"GRJ6PARAMETERS.rds"))
#-----------------------------Ex?cution du mod?le-----------------------
OutputsModel <- RunModel_GR6J(InputsModel = Modelinputs, 
                              RunOptions = RunOptions, 
                              Param =OutputsCalib$ParamFinalR)

#-----------------------------Evaluation du mod?le-----------------------------------------
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)

coef_conver <- BasinArea/(86400*10^3)# coefficient de conversion
Qsim <- OutputsModel$Qsim*coef_conver
Qobs <- AirGRData$Debit[Ind_Run]
hydroGOF::NSE(sim = Qsim,obs =Qobs)

# Parameters boundaries
GR6J.calib.periode <- lubridate::interval(start =as.Date("2003-01-01") ,
                                     end = as.Date("2008-12-31"))

GR6J.valid.periode <- lubridate::interval(start =as.Date("2009-01-01") ,
                                          end = as.Date("2013-12-31"))

GR6J.Calib.Data <- AirGRData%>%
  filter(DATE%within%GR6J.calib.periode)%>%
  select(Debit)%>%unlist

GR6J.Valid.Data <-AirGRData%>%
  filter(DATE%within%GR6J.valid.periode)%>%
  select(Debit)%>%unlist
  
lowerGR6J <- c(6,-10,7,-8,0,7)
upperGR6J <- c(14,-9,14,12,3,14)

lowerGR4J <- rep(-9.99, times = 4)
upperGR4J <- rep(+9.99, times = 4)
#Calibration
optDE <- DEoptim::DEoptim(fn = OptimGR6J,
                          lower = lowerGR4J, upper = upperGR4J,
                          control = DEoptim::DEoptim.control(NP = 600, 
                                                             trace = 10,
                                                          itermax =2000))

GR4J_PARAMETERS <- readRDS(file = "D:/IGEDD/Memoire/Projet_Nowkuy/OUTPUT/Data/GR4J_PARAMETERS.rds")%>%
  filter(NSE.Val>=-2)%>%
  distinct(NSE.Val,.keep_all = TRUE)%>%
  melt(id.vars=c("NSE.Val"))%>%
  mutate(variable=factor(x = variable,levels = c("X1","X2","X3","X4")))

ggplot(GR4J_PARAMETERS,aes(x=value,y=NSE.Val))+
  geom_point()+
  facet_wrap(facets = .~variable,scales = "free_x")+
  xlab(toupper("Parametres"))+
  ylab(toupper("Nash Sutcliffe"))+
  theme_bw()+
  theme(strip.background = element_rect(fill = NA,colour = "blue",size =1.5),
        strip.text = element_text(face = "italic",size = 10),
        axis.text.x = element_text(size = 9,angle = 90,face = "bold"))

saveRDS(GR4J_PARAMETERS,file = "D:/IGEDD/Memoire/Projet_Nowkuy/OUTPUT/Data/GR4J_PARAMETERS.rds")


 # Evolution du Nash en fonction des param?tres
paramset_ <- paramset
paramset2 <- paramset[paramset$CritValue>=-6,]
paramset2 <- transform(paramset2,CritValue=round(paramset2$CritValue,2))
dd <- duplicated(paramset2$CritValue)
paramset_2 <- paramset2[!dd,]

Plot_data <- melt(data =paramset_2,id.vars ="CritValue"  )
Plot_data <- transform(Plot_data,variable=factor(x = Plot_data$variable,levels = unique(unique(Plot_data$variable))))

ggplot(data = Plot_data,aes(x=value,y=CritValue))+
  geom_point()+
  facet_wrap(facets = .~variable,scales = "free_x")+
  xlab("Param?tres")+
  ylab("Nash Sutcliffe")+
  theme_bw()+
  theme(strip.background = element_rect(fill = NA,colour = "blue",size =1.5),
        strip.text = element_text(face = "italic",size = 10),
        axis.text.x = element_text(size = 9,angle = 90,face = "bold"))


#Analyse des r?sultats
Q_sim_GR6J <- data.frame(DATE=as.Date(OutputsModel$DatesR),q_sim_GR6J=OutputsModel$Qsim*coef_conver)



## SCENARIOS HYDROLOGIQUES
best.param <- readRDS(file = "D:/IGEDD/Memoire/Projet_Nowkuy/OUTPUT/Data/GR4J_PARAMETERS.rds")%>%
  arrange(desc(NSE.Val))%>%
  slice(1)%>%
  select(-NSE.Val)%>%
  as.matrix()


best_param <-airGR::TransfoParam_GR4J(ParamIn =best.param,
                                          Direction = "TR")%>%
  as.vector()
MeanClimateData <-dbGetQuery(conn = DataBase,statement = "SELECT*FROM MeanClimateData WHERE Model_id=1") 
ScenarioHydro <- rbind()
for (j in c(1)) {
  for (r in c("rcp45","rcp85")) {
    ModelAirGRData <-MeanClimateData%>%
      filter(Model_id==j,RCP==r,DATE<=as.Date("2040-12-31"))%>%
      mutate(Tmean=(TMAX+TMIN)/2, 
             DATE=as.POSIXct(DATE))%>%
      dplyr::select(DATE,PCP,Tmean,EPT)
    
    Modelinput2 <- CreateInputsModel(FUN_MOD = RunModel_GR4J,
                                     DatesR = ModelAirGRData$DATE,
                                     Precip =ModelAirGRData$PCP,
                                     PotEvap = ModelAirGRData$EPT,
                                     TempMean = ModelAirGRData$Tmean)
    
    Ind_Run <- seq(which(format(ModelAirGRData$DATE, format = "%Y-%m-%d")=="1984-01-01"), 
                   which(format(ModelAirGRData$DATE, format = "%Y-%m-%d")=="2040-12-31"))
    
    Ind_warmUp <- seq(which(format(ModelAirGRData$DATE, format = "%Y-%m-%d")=="1981-01-01"), 
                      which(format(ModelAirGRData$DATE, format = "%Y-%m-%d")=="1983-12-31"))
    
    RunOption2 <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                                   InputsModel = Modelinput2, IndPeriod_Run = Ind_Run,
                                   IniStates = NULL, IniResLevels = NULL,
                                   IndPeriod_WarmUp = Ind_warmUp,Outputs_Cal = "Qsim")
    
    
    OutputsModel <- RunModel_GR4J(InputsModel = Modelinput2, RunOptions = RunOption2, Param =best_param)
    SimData <- data.frame(DATE=OutputsModel$DatesR,Qsim=OutputsModel$Qsim*coef_conver)%>%
      mutate(Model_id=j,RCP=r)
    
    ScenarioHydro <- rbind(ScenarioHydro,SimData)
  }
} #
ScenarioHydro <-ScenarioHydro%>%
  #rename(FLOW_OUT=Qsim,ClimateModel=Model_id)%>%
  mutate(Model="GR4J",ClimateModel="CCCmaCanESM2")%>%
  select(DATE,Model,ClimateModel,RCP,FLOW_OUT)
SWATQSimCCCma <- dbReadTable(conn = DataBase,name ="SWATQSimCCCma")
SCENARIOSHYDRO <- rbind(ScenarioHydro,SWATQSimCCCma)%>%
  mutate(DATE=as.character(DATE))
#dbWriteTable(conn = DataBase,name = "SCENARIOSHYDRO4",value = SCENARIOSHYDRO,field.types=c(DATE="DATE"))
ScenarioHydro <- dbReadTable(conn = DataBase,name = "SCENARIOSHYDRO4")
ScenarioHydro2 <- ScenarioHydro#%>%
 # mutate()
  rename(FLOW_OUT=Qsim)%>%
  mutate(HydroModel="GR4J",DATE=as.character(DATE))%>%
  dplyr::select(DATE,HydroModel,ClimateModel,RCP,FLOW_OUT)

SWATSimData <- dbReadTable(DataBase,"SWATQSim")%>%
  rename(HydroModel=Model)
  rename(ClimateModel=Model)%>%
  mutate(HydroModel="SWAT",DATE=as.character(DATE),ClimateModel=factor(x=ClimateModel,levels =c("CNRMCERFACSCNRMCM5","NOAAGFDLGFDLESM2M"),
                                               labels =c(2,10)  ))%>%
  dplyr::select(DATE,HydroModel,ClimateModel,RCP,FLOW_OUT)
  
SCENARIOSHYDRO <- rbind(SWATSimData,ScenarioHydro2)

dbWriteTable(DataBase,"SCENARIOSHYDRO2",SCENARIOSHYDRO,field.types=c(DATE="DATE"))

TestSCENARIOSHYDRO <- dbReadTable(DataBase,"SCENARIOSHYDRO")


GR6JHistData <- data.frame(DATE=as.Date(OutputsModel$DatesR),Debit=OutputsModel$Qsim*coef_conver)%>%
  mutate(HydroModel="GR6j",DATE=as.character(DATE))%>%
  dplyr::select(DATE,HydroModel,Debit)

SWATHistData <- dbReadTable(DataBase,"ModelHistData")%>%
  mutate(HydroModel="SWAT",DATE=as.character(DATE))%>%
  rename(Debit=FLOW_OUT)%>%
  dplyr::select(DATE,HydroModel,Debit)

HYDRO_MODEL_HIST_DATA <- rbind(SWATHistData,GR6JHistData)
dbWriteTable(conn = DataBase,name ="HYDRO_MODEL_HIST_DATA",value = HYDRO_MODEL_HIST_DATA,field.types=c(DATE="DATE") )




