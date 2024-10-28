#***************************************************************************#
#               MODELISATION HYDROLOGIQUE AVEC SWAT                         #
#***************************************************************************#

## IMPORTATION DES FICHIERS SOURCES
setwd("D:/IGEDD/Memoire/Projet_Nowkuy")
source(file = "./RScripts/sourcefile.R")
## DATA BASE
DataBase <- dbConnect(drv = RSQLite::SQLite(),dbname=file.path("DataBase","ProjectDataBase.sqlite"))

## CALIBRATION AND VALIDATION DATA SETES
QObs <- dbReadTable(DataBase,"QObsData")
calib.periode <- interval(as.Date("2003-01-01"),as.Date("2008-12-31"))
valid.periode <- interval(as.Date("2009-01-01"),as.Date("2014-12-31"))

Qcalib <- QObs%>%
  mutate(DATE=as.Date(DATE))%>%
  filter(DATE%within%calib.periode,!is.na(Debit))

Qvalid <- QObs%>%
  mutate(DATE=as.Date(DATE))%>%
  filter(DATE%within%valid.periode,!is.na(Debit))
##=======================EXECUTION DU MODEL=======================
## SETING MODEL PARAMETERS
project_path <- "D:/IGEDD/Memoire/Projet_Nowkuy/RefSWATProject/TxtInOut2"
save_path <- "D:/IGEDD/Memoire/Projet_Nowkuy/OUTPUT/Data"
unit <- 1
start_date <- "2000-01-01"
end_date <- "2016-12-31"
years_skip <- 3

## MODEL RUN
SWAT_run <- run_swat2012(
  project_path = project_path,
  output =define_output(file = "rch",
                        variable = "FLOW_OUT",
                        unit = unit),
  start_date = start_date,
  end_date = end_date,
  years_skip = years_skip)

##===========ETUDE DE SENSIBILITE DES PARAMETRES DU MODEL=========
EXCECUT_SENS <-run_swat2012(
  project_path = project_path,
  output =define_output(file = "rch",
                        variable = "FLOW_OUT",
                        unit = unit),
  parameter = ParameterFast,n_thread = 4,
  start_date = start_date,
  end_date = end_date,
  years_skip = years_skip,
  output_interval = "d", 
  keep_folder = TRUE,quiet = FALSE)

#saveRDS(object =EXCECUT_SENS,file = "./DataBase/SENSITIVEData.rds") # sauvegarde des donn?es de la simulation

simulation_data <- EXCECUT_SENS$simulation$FLOW_OUT%>%
  filter(date%in%Qcalib$DATE)

NSEVal <- sapply(simulation_data[,-1],function(x){
  nse <- NSE(x,Qcalib$Debit)
  nse
})
sensval_fast <- sensitivity(NSEVal, 25)

simulation_data2 <- data.frame(Nash=NSEVal,EXCECUT_SENS$parameter$values)
## Graph de sensibilite
### Graph1
par_name<- EXCECUT_SENS$parameter$definition$par_name

ResultFast <- tibble(parameter = as.factor(par_name),
                     fast      = sensval_fast)%>%
  mutate(parameter = factor(parameter)%>% 
           fct_reorder(., fast))

ggplot(data = ResultFast) +
  geom_col(aes(x = parameter, y = fast),fill="steelblue",color="black")+
  xlab("PARAMETRES") +
  ylab("SENSIBILITE")+ 
  coord_flip(expand = T) +
  theme_bw()+
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = NA,
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 12),
        axis.text.x = element_text(size = 10,colour = "black",angle = 0),
        axis.text.y =element_text(size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))

### Graph2
simulation_data3 <- simulation_data2%>%
  melt(id.vars=c("Nash"))

ggplot(data = simulation_data3,aes(x=value,y=Nash))+
  geom_point()+
  facet_wrap(facets = .~variable,scales = "free_x",as.table = F)+
  xlab("Paramètres")+
  ylab("Nash Sutcliffe")+
  theme_bw()+
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = NA,
                                        colour = "blue",
                                        size = 1.5),
        strip.text = element_text(face = "italic",
                                  colour = "black",size = 10),
        axis.text.x = element_text(size = 9,colour = "black",angle = 90),
        axis.text.y =element_text(size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))

# ====================== OPTIMISATION ============================

## EXTRACTION DES PARAMETRES SENSIBLES
ResultFast2 <- ResultFast[order(ResultFast$fast,decreasing = T),]
sens_params <-NULL
for (j in 1:9) {
  sens_params2 <-as.matrix(SWAT_MODEL_PARAMETERS[,grep(ResultFast2$parameter[j],names(SWAT_MODEL_PARAMETERS))]) 
  sens_params<- cbind(sens_params,sens_params2)
}

SWATPARAMETERS <- readRDS(file = "D:/PROJET/Article/CC_Eau/DataBase/SWATPARAMETERS.rds")
sens_params <- SWAT_MODEL_PARAMETERS%>%
  select(colnames(SWATPARAMETERS))

lower_vals <- as.numeric(sens_params[1,])
upper_vals <-  as.numeric(sens_params[2,])

dis_obs <- Qcalib$Debit
OptimParameters <- as_tibble(sens_params)
calibDEoptim <- DEoptim::DEoptim(fn = OptimSWATplus,
                                 lower = lower_vals, upper = upper_vals,
                                 control = DEoptim::DEoptim.control(NP = 500, trace = 10,VTR=-1,
                                                                    steptol=500,
                                                                    reltol = 10^-20,
                                                                    strategy = 6,
                                                                    itermax=5000))
## SAVE PARAMETERES
PARAMETERSS <- Parametrs_2%>%
   mutate(nse=-nse)%>%
   arrange(desc(nse))

 saveRDS(object = PARAMETERSS,file = file.path(save_path,"PARAMETERSS.rds")) 
 
#============================ VALIDATION =========================

best_parameters <- PARAMETERSS%>%
   select(-nse)%>%
   slice(1)

SWAT_run <- run_swat2012(
  project_path = project_path,
  output =define_output(file = "rch",
                        variable = "FLOW_OUT",
                        unit = unit),
  parameter = best_parameters,
  start_date = "1981-01-01",
  end_date = "2016-12-31",
  years_skip = years_skip)

SWATsimDATA <- SWAT_run$simulation%>%
  mutate(DATE=as.character(date))%>%
  select(DATE,FLOW_OUT)

## PARAMETERS TEST
QSim1 <- SWAT_run$simulation%>%
  filter(date%in%Qcalib$DATE)
NSE(QSim1$FLOW_OUT,Qcalib$Debit)

## VALIDATION
QSim2 <- SWAT_run$simulation%>%
  filter(date%in%Qvalid$DATE)
NSE(QSim2$FLOW_OUT,Qvalid$Debit)

