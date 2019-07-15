#           An?lisis Montse Doria         ------------

memory.size(max=160685)

# Directori Font     ==============================  

rm(list=ls())
###
directori.arrel<-c("C:/Users/Jordi/Google Drive", 
                   "C:/Users/usuari/Google Drive",
                   "C:/Users/43728088M/Google Drive",
                   "C:/Users/jreal/Google Drive",
                   "D:/Google Drive",
                   "G:/Google Drive",
                   "E:/Google Drive")

library("dplyr")


directori.arrel[file.exists(directori.arrel)] %>% 
  file.path("Stat_codis/funcions_propies.R") %>% 
  source()

# DIRECTORI DE TREBALL          -------------------    
# setwd en directori de treball 
library(dplyr)

#  Parametres --------------

fitxer_dades<-"mortalitathemodialisi_24052019.xls"

conductor_variables<-"taulavariables_v5.xls"

# Llegir dades ----------------
dades<-readxl::read_excel(fitxer_dades)


# Arreglar noms de variables ------------------
dades<-netejar.noms.variables(dades)

# Calculs  ----------------

# Si any DG es 0 o 1 es recodifica com a missing 

dades<-dades %>% mutate (any_dX_dm=ifelse(any_dX_dm==0 | any_dX_dm==1 ,NA,any_dX_dm)) 

# Convertir dates camps dates 
# Convertir dates UTC a Rdata 
library(rlang)
camps_dates<-extreure.variables(taula="dates",taulavariables = conductor_variables)

dades<-dataUTC_to_Rdata(camps_dates,dades)


#  Etiquetes  -------------------
dades<-etiquetar_valors(dt=dades,variables_factors=conductor_variables,fulla="etiquetes",camp_etiqueta="etiqueta")
dades<-etiquetar(dades, taulavariables=conductor_variables)

# Analisis exploratori -------------------
library(compareGroups)
T1<-descrTable(~.-ID,dades,show.p.overall = F,method = 2,Q1 = 0, Q3 = 1,max.ylev=Inf)

# Calculo Surv mortalitat ------
dades<-dades %>% mutate(temps_seguiment=dfin-data_inclusi,
                        exitus=ifelse(is.na(motiu_exitus),"No","Si"))

dades$exitus_surv<-Surv(dades$temps_seguiment,as.integer(dades$exitus=="Si"))
# Calculo Surv CI ------
# EV1_CI
dades<-dades %>% 
  mutate(temps_fins_CI=case_when(EV1_CI>0 ~ EV1_CI-data_inclusi,
                                 is.na(EV1_CI)~ temps_seguiment)) %>% 
  mutate(EV_CI=ifelse(is.na(EV1_CI),"No","Si")) 

dades$CI_surv<-Surv(dades$temps_fins_CI,as.integer(dades$EV_CI=="Si"))


# Calculo Surv CV -------
# EV1_CV
dades<-dades %>% 
  mutate(temps_fins_CV=case_when(EV1_CV>0 ~ EV1_CV-data_inclusi,
                                 is.na(EV1_CV)~ temps_seguiment)) %>% 
  mutate(EV_CV=ifelse(is.na(EV1_CV),"No","Si")) 

dades$CV_surv<-Surv(dades$temps_fins_CV,as.integer(dades$exitus=="Si"))


# Calculo Surv ART_PER ------
# EV1_ART_PER
dades<-dades %>% 
  mutate(temps_fins_ARTPER=case_when(EV1_ART_PER>=0 ~ EV1_ART_PER-data_inclusi,
                                 is.na(EV1_ART_PER)~ temps_seguiment)) %>% 
  mutate(EV_ART_PER=ifelse(is.na(EV1_ART_PER),"No","Si")) 

# Calculo Surv ULCERES ------
# EV1_ULCERES
dades<-dades %>% 
  mutate(temps_fins_ULCER=case_when(EV1_ULCERES>=0 ~ EV1_ULCERES-data_inclusi,
                                     is.na(EV1_ULCERES)~ temps_seguiment)) %>% 
  mutate(EV_ULCERES=ifelse(is.na(EV1_ULCERES),"No","Si")) 

# Calculo número de ULCERES ------------
# EV1_ULCERES, EV2_ULCERES, EV3_ULCERES, EV4_ULCERES

# Recodifico Events --> Si/No 

ulceres<-c("EV1_ULCERES", "EV2_ULCERES", "EV3_ULCERES", "EV4_ULCERES")

# Convertir dates events a 0/1
dades<-dades %>% 
  purrr::modify_at(ulceres, 
                   ~ as.numeric(!is.na(.x)) %>% factor(levels=c(0,1), labels=c("No","Si")))
# Comptar ulceres
dades<-dades %>% 
  comptar_valors(variables=c("EV1_ULCERES", "EV2_ULCERES", "EV3_ULCERES", "EV4_ULCERES"),valor="Si") %>% 
  mutate(num_ulceres=num_valors) %>% 
  select(-num_valors)
  

# Calculo Surv AMPUTACIO MENOR -------
# EV1_AMP_MENOR
dades<-dades %>% 
  mutate(temps_fins_AMPMENOR=case_when(EV1_AMP_MENOR>=0 ~ EV1_AMP_MENOR-data_inclusi,
                                     is.na(EV1_AMP_MENOR)~ temps_seguiment)) %>% 
  mutate(EV_AMP_MENOR=ifelse(is.na(EV1_AMP_MENOR),"No","Si")) 

# Calculo Surv AMPUTACIO MAJOR-------
# EV1_AMP_MAJOR
dades<-dades %>% 
  mutate(temps_fins_AMPMAJOR=case_when(EV1_AMP_MAJOR>=0 ~ EV1_AMP_MAJOR-data_inclusi,
                                     is.na(EV1_AMP_MAJOR)~ temps_seguiment)) %>% 
  mutate(EV_AMP_MAJOR=ifelse(is.na(EV1_AMP_MAJOR),"No","Si")) 

# Calculo Surv ACFA MENOR-------
# EV1_ACXFA
dades<-dades %>% 
  mutate(temps_fins_ACFA=case_when(EV1_ACXFA>=0 ~ EV1_ACXFA-data_inclusi,
                                     is.na(EV1_ACXFA)~ temps_seguiment)) %>% 
  mutate(EV_ACXFA=ifelse(is.na(EV1_ACXFA),"No","Si"))

# Calculo Surv IC -------
# EV1_IC
dades<-dades %>% 
  mutate(temps_fins_IC=case_when(EV1_IC>=0 ~ EV1_IC-data_inclusi,
                                   is.na(EV1_IC)~ temps_seguiment)) %>% 
  mutate(EV_IC=ifelse(is.na(EV1_IC),"No","Si"))

# Calculo Surv COLITIS_ISQ -------
# EV1_COLITIS_ISQ
dades<-dades %>% 
  mutate(temps_fins_COLITIS_ISQ=case_when(EV1_COLITIS_ISQ>=0 ~ EV1_COLITIS_ISQ-data_inclusi,
                                 is.na(EV1_COLITIS_ISQ)~ temps_seguiment)) %>% 
  mutate(EV_COLITIS_ISQ=ifelse(is.na(EV1_COLITIS_ISQ),"No","Si"))

# Calculo Surv ISQ_MESENTERICA  -------
# EV1_ISQ_MESENTERICA
dades<-dades %>% 
  mutate(temps_fins_ISQ_MESE=case_when(EV1_ISQ_MESENTERICA>=0 ~ EV1_ISQ_MESENTERICA-data_inclusi,
                                          is.na(EV1_ISQ_MESENTERICA)~ temps_seguiment)) %>% 
  mutate(EV_ISQ_MESENTERICA=ifelse(is.na(EV1_ISQ_MESENTERICA),"No","Si"))


# Calculo edat ----------------
dades<-dades %>% mutate(edat=(data_inclusi-data_naixement)/365.25)

# Recodifico antecedents DATA -> si/no -------------
# Variables a recodificar
dates_si_no<-extreure.variables("dates_si_no",conductor_variables)

# Convertir dates antecedents a 0/1  + factoritzar Si/No
dades<-dades %>% 
  purrr::modify_at(dates_si_no, ~ as.numeric(!is.na(.x)) %>% 
                     factor(levels=c(0,1), labels=c("No","Si"))
                   ) 
# Calculo anys DM -------
dades<-dades %>% 
  mutate (anys_DM=data_inclusi-lubridate::ymd(paste0(any_dX_dm,"06","15"))) %>%
  mutate (anys_DM=as.numeric(anys_DM/365.25))

# Calculo Variable exposició peu_diab -------------
# Peu diabertic --> Ulcera + Amputacions + Artper
# Peu diabetic2 --> Ulcera + Amputacions / Artper / No   

vars_peu_diab<-c("ulcera_prvia","ulcera_actual","ANT1_ULCERA","ANT1_AMP_MAJOR","ANT1_AMP_MENOR","ANT1_ARTER_PERI","amputacions")

dades<-dades %>% 
  mutate (peu_diab=case_when(ulcera_prvia=="Si" |
                               ulcera_actual=="Si" | 
                               ANT1_ULCERA=="Si" | 
                               ANT1_AMP_MAJOR=="Si"|
                               ANT1_AMP_MENOR=="Si"|
                               ANT1_ARTER_PERI=="Si"|
                               amputacions!="NO"~"Si",
                               TRUE~"No"))

dades<-dades %>% 
  mutate (peu_diab2=case_when(ulcera_prvia=="Si" |
                               ulcera_actual=="Si" | 
                               ANT1_ULCERA=="Si" | 
                               ANT1_AMP_MAJOR=="Si"|
                               ANT1_AMP_MENOR=="Si"|
                               amputacions!="NO"~"Si",
                             TRUE~"No"))

# Construir/identificar events: 1. Mortalitat CV / 2.ECV (Event CV) / 3.  MACE (Mortalitat o Event CV) / 4. Ulcera o amputacio / 5. Hospitalitzacio ------

# 1. Mortalitat CV: exitusCV / exitusCV_surv
dades<-dades %>% mutate(exitusCV=ifelse(motiu_exitus=="CARDIOVASCULAR","Si","No"))
dades$exitusCV_surv<-Surv(dades$temps_seguiment,as.integer(dades$motiu_exitus=="CARDIOVASCULAR"))
# Verificació
descrTable(exitusCV~EV_CV+motiu_exitus,data=dades,show.p.overall = F)

# 2. 2.ECV (Event CV) (Confirmar quins events: EV_IC, EV_CV etc..??)
dades<-dades %>% mutate (EV_CardV=if_else(EV_IC=="Si" | EV_CV=="Si","Si","No"))
# verificacio
descrTable(EV_CardV~EV_IC + EV_CV,data=dades, show.p.overall = F)
# Calculo el temps_fins_EVCardVs
dades<-dades %>% mutate(temps_fins_EVCardV=pmin(temps_fins_IC,temps_fins_CV)) 
# Surv
dades$EV_CardV_surv<-Surv(dades$temps_fins_EVCardV,as.integer(dades$EV_CardV=="Si"))

# 3. 3.  MACE (Mortalitat CV o Event CV)
dades<-dades %>% mutate (EV_MACE=if_else(EV_CardV=="Si" | exitusCV=="Si","Si","No"))
# verificacio
descrTable(EV_MACE~EV_CardV + exitusCV,data=dades, show.p.overall = F)
# Calculo el temps_MACE
dades<-dades %>% mutate(temps_fins_MACE=pmin(temps_fins_EVCardV,temps_seguiment)) 

# 4. Ulcera o amputacio (EV_ULCERES | EV_AMP_MENOR | EV_AMP_MAJOR)
dades<-dades %>% mutate (EV_ULC_AMP=if_else(EV_ULCERES=="Si" | EV_AMP_MENOR=="Si" | EV_AMP_MAJOR=="Si","Si","No"))
# Calculo el temps_ulcera/temps
dades<-dades %>% mutate(temps_fins_ULCAMP=pmin(temps_fins_AMPMAJOR,temps_fins_AMPMENOR,temps_fins_ULCER)) 
# verificacio
descrTable(EV_ULC_AMP~EV_ULCERES + EV_AMP_MENOR + EV_AMP_MAJOR,data=dades, show.p.overall = F)
# Calculo Surv
dades$EV_ULC_AMP_surv<-Surv(dades$temps_fins_ULCAMP,as.integer(dades$EV_ULC_AMP=="Si"))


# 5. Hospitalització (No trobo la variable)









# 
# FI PREPARACIÓ       -------------

# INICI ANALISIS      -------------

dades<-etiquetar(dades,conductor_variables)

# Descriptiva basal per grups --------------

# Selecciono DMs 
dadestotal<-dades

dades<-dades %>% filter(diabetes=="Si" | diabetes=="Si")

# Descriptiva baseline 0 DM vs No DM
formu<-formula_compare(x="baseline",y="diabetes",taulavariables = conductor_variables)
T0.1<-descrTable(formu,data=dadestotal,show.p.overall = F,max.ylev=Inf,show.n = T,show.all=T)

# Ulcera vs no Ulcera
# Descriptiva baseline  2 (Peu diabetic II) 
formu<-formula_compare(x="baseline",y="peu_diab2",taulavariables = conductor_variables)
T1.2<-descrTable(formu,data=dades,show.p.overall = T,show.n = T)

# Descriptiva baseline  3 (Artper ) 
formu<-formula_compare(x="baseline",y="ANT1_ARTER_PERI",taulavariables = conductor_variables)
T1.3<-descrTable(formu,data=dades,show.p.overall = T,show.n = T)


# Seguiment Incidencia acumulada d'events per grups  ----------------- 
# Seguiment Incidencia acumulada x Peu diabetic (Peu diabetic (Úlcera / amputació)) 
formu<-formula_compare(x="events",y="peu_diab2",taulavariables = conductor_variables)
T2.2<-descrTable(formu,data=dades,show.p.overall = T)

# Seguiment Incidencia acumulada x Artper )  
formu<-formula_compare(x="events",y="ANT1_ARTER_PERI",taulavariables = conductor_variables)
T2.3<-descrTable(formu,data=dades,show.p.overall = T)


# Mortalitat x Baseline  --------
formu<-formula_compare(x="baseline",y="exitus_surv",taulavariables = conductor_variables)
T3.1<-descrTable(formu,data=dades,show.p.overall = T,show.ratio = T,byrow = T)

# Event CV x Baseline -------------
formu<-formula_compare(x="baseline",y="CV_surv",taulavariables = conductor_variables)
T3.2<-descrTable(formu,data=dades,show.p.overall = T,show.ratio = T,byrow = T)



save.image("output_MDORIA_v3.Rdata")




