# Analisis Montse Doria         ------------

memory.size(max=160685)

# Carrega de funcions / llibreries -------------  
rm(list=ls())
library("dplyr")
library(rlang)
library(compareGroups)

link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)

# Parametres --------------
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

camps_dates<-extreure.variables(taula="dates",taulavariables = conductor_variables)

dades<-dataUTC_to_Rdata(camps_dates,dades)

# Etiquetació  -------------------
dades<-etiquetar_valors(dt=dades,variables_factors=conductor_variables,fulla="etiquetes",camp_etiqueta="etiqueta")
dades<-etiquetar(dades, taulavariables=conductor_variables)

# Analisis exploratori -------------------

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

# Construir/identificar events: ------------- 

# 1. Mortalitat CV / 2.ECV (Event CV) / 3.  MACE (Mortalitat o Event CV) / 4. Ulcera o amputacio / 5. Hospitalitzacio ------

# 1. Mortalitat CV: exitusCV / exitusCV_surv --------------
dades<-dades %>% mutate(exitusCV=if_else(motiu_exitus=="CARDIOVASCULAR","Si","No"),
                        exitusCV=if_else(is.na(exitusCV),"No",exitusCV))

dades$exitusCV_surv<-Surv(dades$temps_seguiment,as.integer(dades$exitusCV=="Si"))
# Verificació
descrTable(exitusCV~EV_CV+motiu_exitus,data=dades,show.p.overall = F,show.all = T)

# 2. EV_CardV (Event CardV) ----------------
# (Events considerats: EV_CI, EV_CV, EV_ART_PER, EV_ISQ_MESENTERICA, EV_COLITIS_ISQ)
dades<-dades %>% mutate (EV_CardV=if_else(EV_CI=="Si" | EV_CV=="Si" | EV_ART_PER=="Si" | EV_ISQ_MESENTERICA=="Si" | EV_COLITIS_ISQ=="Si" ,"Si","No"))
dades<-dades %>% mutate(temps_fins_EVCardV=pmin(temps_fins_CI,temps_fins_CV,temps_fins_ARTPER,temps_fins_ISQ_MESE,temps_fins_COLITIS_ISQ,temps_seguiment))
dades$EV_CardV_surv<-Surv(dades$temps_fins_EVCardV,as.integer(dades$EV_CardV=="Si"))

# Verificacio
descrTable(EV_CardV~EV_CI+EV_CV+EV_ART_PER+EV_ISQ_MESENTERICA+EV_COLITIS_ISQ,data=dades,show.p.overall = F,show.all = T)

# 3. MACE (Mortalitat CV o Event CV) ----------------
dades<-dades %>% mutate (EV_MACE=if_else(EV_CV=="Si" | EV_CI=="Si" | exitusCV=="Si","Si","No"),
                         EV_MACE=if_else(is.na(EV_MACE),"No",EV_MACE))
# Temps 
dades<-dades %>% mutate (temps_fins_MACE=pmin(temps_fins_CV,temps_fins_CI,temps_seguiment))
# Surv
dades$EV_MACE_surv<-Surv(dades$temps_fins_MACE,as.integer(dades$EV_MACE=="Si"))
# Verificacio
descrTable(~EV_MACE+EV_MACE_surv,data=dades)

# 4. Ulcera o amputacio (EV_ULCERES | EV_AMP_MENOR | EV_AMP_MAJOR) -----
dades<-dades %>% mutate (EV_ULC_AMP=if_else(EV_ULCERES=="Si" | EV_AMP_MENOR=="Si" | EV_AMP_MAJOR=="Si","Si","No"))
# Calculo el temps_ulcera/temps
dades<-dades %>% mutate(temps_fins_ULCAMP=pmin(temps_fins_AMPMAJOR,temps_fins_AMPMENOR,temps_fins_ULCER,temps_seguiment)) 
# Calculo Surv
dades$EV_ULC_AMP_surv<-Surv(dades$temps_fins_ULCAMP,as.integer(dades$EV_ULC_AMP=="Si"))


# 5. Hospitalització (No trobo la variable)


# FI PREPARACIÓ       -------------

# INICI ANALISIS      -------------

dades<-etiquetar(dades,conductor_variables)

# Descriptiva basal per grups --------------

# Selecciono DMs 
dadestotal<-dades

dades<-dades %>% filter(diabetes=="Si")

# Descriptiva baseline 0 DM vs No DM
formu<-formula_compare(x="baseline",y="diabetes",taulavariables = conductor_variables)
T0.1<-descrTable(formu,data=dadestotal,show.p.overall = F,max.ylev=Inf,show.n = T,show.all=T)

# Ulcera vs no Ulcera
# Descriptiva baseline  2 (Peu diabetic II) 

formu<-formula_compare(x="baseline",y="",taulavariables = conductor_variables)
T1.1<-descrTable(formu,data=dades,show.p.overall = T,show.n = T)

formu<-formula_compare(x="baseline",y="peu_diab2",taulavariables = conductor_variables)
T1.2<-descrTable(formu,data=dades,show.p.overall = T,show.n = T)

# Descriptiva baseline  3 (Artper ) 
formu<-formula_compare(x="baseline",y="ANT1_ARTER_PERI",taulavariables = conductor_variables)
T1.3<-descrTable(formu,data=dades,show.p.overall = T,show.n = T)

# Seguiment Incidencia acumulada d'events per grups  ----------------- 
# Seguiment Incidencia acumulada x Peu diabetic (Peu diabetic (Úlcera / amputació)) 
formu<-formula_compare(x="events_principals",y="peu_diab2",taulavariables = conductor_variables)
T2.2<-descrTable(formu,data=dades,show.p.overall = T,timemax = 1825)


# Seguiment Incidencia acumulada x Artper )  
formu<-formula_compare(x="events_principals",y="ANT1_ARTER_PERI",taulavariables = conductor_variables)
T2.3<-descrTable(formu,data=dades,show.p.overall = T,timemax = 1825)


# Mortalitat CV  x Baseline  --------
formu<-formula_compare(x="baseline",y="exitusCV_surv",taulavariables = conductor_variables)
T3.1<-descrTable(formu,data=dades,show.p.overall = T,show.ratio = T,byrow = T)

# Event CV x Baseline -------------
formu<-formula_compare(x="baseline",y="EV_CardV_surv",taulavariables = conductor_variables)
T3.2<-descrTable(formu,data=dades,show.p.overall = T,show.ratio = T,byrow = T)


# Taula dincidencia d'events ---------------
formula<-formula_compare("events_principals",y="peu_diab2",taulavariables = conductor_variables)
descrTable(formula,Q1 =0,Q3=1, method = 2,timemax = 1825,data=dades,show.all = T)

descrTable(peu_diab2~exitusCV+exitusCV_surv+temps_seguiment,Q1 =0,Q3=1, method = 2,timemax = 1825,data=dades,show.all = T)


#  3 Taula de HR -----
descrTable(exitusCV_surv~peu_diab2,show.ratio = T,data=dades,show.all = T)
descrTable(EV_CardV_surv~peu_diab2,show.ratio = T,data=dades,show.all = T)
descrTable(EV_MACE_surv~peu_diab2,show.ratio = T,data=dades,show.all = T)
descrTable(EV_ULC_AMP_surv~peu_diab2,show.ratio = T,data=dades,show.all = T)

# Curves de supervivencia d'esdeveniments ----

# Funció que Retorna curva K-M -------
plotKM_doria<-function(dt=dades,event="exitusCV",temps="temps_seguiment",titol="Mortalitat cardiovascular",grup="peu_diab2") {

  # dt=dades
  # titol<-"Mortalitat cardiovascular"
  # event="exitusCV"
  # grup="peu_diab2"
  # temps="temps_seguiment"
  
  event<-sym(event)
  grup_sym<-sym(grup)
  temps<-sym(temps)
  
  dt <- dt %>% select(event=!!event,temps=!!temps,grup=!!grup_sym) 
  
  llegenda=c("No","Si")
  # Basic survival curves
  survminer::ggsurvplot(survfit(Surv(temps,as.integer(event=="Si"))~grup,data=dt), data = dt,
                                    main = "Survival curve",
                                    title= titol,
                                    size = 0.5,
                                    ylim = c(0,1),
                                    xlim = c(0,1825),
                                    break.x.by=365,
                                    xlab = "Time in days",
                                    risk.table = F,
                                    censor.shape="|", censor.size = 1,
                                    legend.labs=llegenda,
                                    ggtheme = theme_bw())
  }


# PlotsKM ------
plotKM_doria(dt=dades,event="exitusCV",temps="temps_seguiment",titol="Mortalitat cardiovascular")
plotKM_doria(dt=dades,event="EV_CardV",temps="temps_fins_EVCardV",titol="Event cardiovascular")
plotKM_doria(dt=dades,event="EV_MACE",temps="temps_fins_MACE",titol="MACE")
plotKM_doria(dt=dades,event="EV_ULC_AMP",temps="temps_fins_ULCAMP",titol="Ulcera/Amputació")

# Analisis de supervivencia lliure d'esdeveniments RISCOS COMPETITIUS  -------------
# Funció Riscos competitius Fine & Grey ------------------
# Donat un event, temps de seguiment, grup, eventcompetitiu retorna tibble:
# Beta, SE, p-value, HR, Li95%CI, Ls95%CI

extreure_HRFG=function(event="exitusCV",temps="temps_seguiment",grup="peu_diab2",eventcompetitiu="exitus",dt=dades){

# event="exitusCV"
# temps="temps_seguiment"
event<-sym(event)
temps<-sym(temps)
grup<-sym(grup)
eventcompetitiu<-sym(eventcompetitiu)

# Selecciono variables necessaries
dt<-dt %>% select(grup=!!grup,exitus=!!eventcompetitiu,temps=!!temps,event=!!event)

# Generar variable status (tipo de censuras) ----
dt<-dt %>% mutate(status=case_when(event=="Si" ~"event",
                                         event=="No" & exitus=="Si"~"Mortality",
                                         event=="No" & exitus=="No"~"Censored")) 

# Factor com a numeric 
grup<-matrix(as.numeric(dt$grup=="Si"))

# Codificar riscos competitius 
model<-cmprsk::crr(ftime=dt$temps,
                          fstatus=dt$status,
                          cov1=grup , #  matrix (nobs x ncovs) of fixed covariates
                          failcode = "event", # code of fstatus that denotes the failure type of interest
                          cencode = "Censored") # code of fstatus that denotes censored observations

tab <- summary(model)$coef
x <- round(cbind("beta" = tab[, 1], 
                 "SE" = tab[, 3], 
                 "p-value" = tab[, 5], 
                 "HR" = tab[, 2],
                 "LI" = exp(tab[, 1] - qnorm(1 - (1-0.95)/2)*tab[, 3]),
                 "LS" = exp(tab[, 1] + qnorm(1 - (1-0.95)/2)*tab[, 3])), 4)
colnames(x) <- c("Beta", "SE", "p-value", "HR", "Li95%CI", "Ls95%CI")
rownames(x) <- rownames(tab)

as_tibble(x)

}

# Extreure HR segons riscos competitius ---------------

extreure_HRFG(event="exitusCV",temps="temps_seguiment")
extreure_HRFG(event="EV_CardV",temps="temps_fins_EVCardV")
extreure_HRFG(event="EV_MACE",temps="temps_fins_MACE")
extreure_HRFG(event="EV_ULC_AMP",temps="temps_fins_ULCAMP")


# Salvar objectes -------------

save.image("output/output_MDORIA_v4.Rdata")




