#30.1.2019///[17:17]
#######################################################################
#                                ANALISIS Montse Oria       ###########
#rm(list=ls())

memory.size(max=160685)

#
#####################  Directori Font     ==============================  

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

#----------------------------------------------------
directori.arrel[file.exists(directori.arrel)] %>% 
  file.path("Stat_codis/funcions_propies.R") %>% 
  source()
#----------------------------------------------------

# library(lubridate)
#--------------------------------------------------
NData<-function(ND="20090210"){
  library(lubridate)
  ###   Extreure data!
  reus<- ymd(ND)
  reus
}
#-------------------------------------------------


####    DIRECTORI DE TREBALL          -------------------    
#### setwd en directori de treball 
library(dplyr)

"CIBERDEM/MDORIA" %>% 
  directori_treball(directori.arrel)

###   Parametres --------------

fitxer_dades<-"mortalitathemodialisi_23012019.xls"


conductor_variables<-"taulavariables_v3.xls"

### Llegir dades ----------------
dades<-readxl::read_excel(fitxer_dades)
dades

### Arreglar noms de variables ------------------
names(dades)
dades<-netejar.noms.variables(dades)

#---------------------------------
variables<-readxl::read_excel(conductor_variables)
variables
#----------------------------------

dades<-etiquetar(dades,taulavariables=conductor_variables)

class(dades)
names(dades)
tables(dades$diabetes)

bddoria_nou<-etiquetar_valors(dades_doria,conductor_variables,
                              fulla="etiquetes2",camp_etiqueta = "etiqu")


bddoria_nou$data_naixement 
bddoria_nou$data_inici_HD

## Calculs  ----------------

bddoria_nou$itb_dret




bddoria_nou %>% select(itb_dret,itb_dret.cat4,GIM)
  




#30.01.2019[///]



#id	camp
#1)	ID
#2)	nom_i_cognoms

#3)	sexe
dades_doria$sexe<- factor(dades_doria$sexe,
                    levels = c(1,2),
                    labels = c("HOME", "DONA"))
#4	data_naixement
library("lubridate")
dades_doria$data_naixement<-ymd(dades_doria$data_naixement)

#5	etiol.nefropatia

  #1=NEFROPATIA DIAB?TICA
  #2=MALALTIAVASCULO-RENAL ( HTA/INESPEC?FICA)
  #3=GLOMERULONEFRITIS 
  #4=AMILOIDOSI
  #5=TROMBOSI RENAL
  #6=PIELONEFRITIS
  #7=NO FILIADA 
  #8=TUMOR RENAL
  #9=POLIQUISTOSI
  #10=NEFROANGIOESCLEROSI
  #11=DIPOSITA D'IG A 
  #12=POLIARTERITIS MICROSC?PICA
  #13=ALTRES

dades_doria$etiol.nefropatia<- factor(dades_doria$etiol.nefropatia,
                          levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13),
                          labels = c("NEFROPATIA DIAB?TICA", 
                                     "MALALTIAVASCULO-RENA",
                                     "GLOMERULONEFRITIS",
                                     "AMILOIDOSI",
                                     "TROMBOSI RENAL",
                                      "PIELONEFRITIS",
                                     "NO FILIADA",
                                     "TUMOR RENA",
                                     "POLIQUISTOSI",
                                     "NEFROANGIOESCLEROSI",
                                      "DIPOSITA D'IG A",
                                     "POLIARTERITIS MICROSC?PICA",
                                      "ALTRES"))



#6	data_inici_HD
dades_doria$data_inici_HD<-ymd(dades_doria$data_inici_HD)

#7	diabetes

  #1=SI
  #2=NO

dades_doria$diabetes<- factor(dades_doria$diabetes,
                          levels = c(1,2),
                          labels = c("SI", "NO"))

#8	tipus_diabetes
  
  #0=NO DIABETIS
  #1=TIPUS 1 
  #2=TIPUS 2 
dades_doria$tipus_diabetes<- factor(dades_doria$tipus_diabetes,
                          levels = c(0,1,2),
                          labels = c("NO DIABETIS", "TIPUS 1","TIPUS 2 "))


#9	any_dX_dm
  #0=NO ANY DE DIAGN?STIC PER NO DM 
  #1=ANY DE DIAGN?STIC DE LA DM DESCONEGUT 

###  Si any DG es 0 o 1 es recodifica com a missing 

dades_doria<-dades_doria %>% mutate (any_dX_dm=ifelse(any_dX_dm==0 | any_dX_dm==1 ,NA,any_dX_dm)) 

# 
# 
# dades_doria$any_dX_dm<- factor(dades_doria$any_dX_dm,
#                               levels = c(0,1),
#                               labels = c("NO ANY DE DIAGN?STIC PER NO DM ", "ANY DE DIAGN?STIC DE LA DM DESCONEGUT"))


#10	imc
  
#  0=NO CONEGUT 
#  1=NORMAL: 18,5-24,9
#  2=SOBREPES:25-29,9
#  3=0BESITAT: IGUAL O SUPERIOR A 30 
#  4=BAIX PES: INFERIOR A 18,5
dades_doria$imc<- factor(dades_doria$imc,
                              levels = c(0,1,2,3,4),
                              labels = c("NO CONEGUT", 
                                          "NORMAL: 18,5-24,9", 
                                          "SOBREPES:25-29,9",
                                          "0BESITAT: IGUAL O SUPERIOR A 30" ,
                                           "BAIX PES: INFERIOR A 18,5"))


#11	ESTADI_ATEROMATOSI

# EA0=GRAU 0
# EA1=GRAU 1
# EA2=GRAU 2
# EA3=GRAU3 3
# EP=EVENT PREVI
# DC=NO EXPLORADA 

dades_doria$ESTADI_ATEROMATOSI<- factor(dades_doria$ESTADI_ATEROMATOSI,
                         levels = c("EA0","EA1","EA2","EA3","EP","DC"),
                         labels = c("GRAU 0", 
                                    "GRAU 1", 
                                    "GRAU 2",
                                    "GRAU 3" ,
                                    "EVENT PREVI",
                                    "NO EXPLORADA"))



#12	HTA

  #1=SI
  #2=NO

dades_doria$HTA<- factor(dades_doria$HTA,
                              levels = c(1,2),
                              labels = c("SI", "NO"))


#13	dislipemia

#1=SI
#2=NO

dades_doria$dislipemia<- factor(dades_doria$dislipemia,
                         levels = c(1,2),
                         labels = c("SI", "NO"))


#14	tabaquisme

  #1=SI
  #2=EX
  #3=NO

dades_doria$tabaquisme<- factor(dades_doria$tabaquisme,
                                levels = c(1,2,3),
                                labels = c("SI", "EX","NO"))

#15	antec_CV

  #1=SI 
  #2=NO

dades_doria$antec_CV<- factor(dades_doria$antec_CV,
                                levels = c(1,2),
                                labels = c("SI", "NO"))

#16	ep_cv1

  #1=CARDIOPATIA ISQU?MICA
  #2=EVENT CEREBRO VASCULAR 
  #3=EVENT PERIF?RIC ARTERIOGRAFIA
  #4=EVENT PERIF?RIC REVASCULARITZACIO 
  #5=EVENT PERIF?RIC AMPUTACI? MAJOR 
  #6=EVENT PERIF?RIC AMPUTACI? MENOR
  #7=EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT 
  #8=ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR 
  #9=INSUFICI?NCIA CARD?ACA 


dades_doria$ep_cv1<- factor(dades_doria$ep_cv1,
                              levels = c(1,2,3,4,5,6,7,8,9),
                              labels = c("CARDIOPATIA ISQU?MICA",
                                         "EVENT CEREBRO VASCULAR",
                                         "EVENT PERIF?RIC ARTERIOGRAFIA",
                                         "EVENT PERIF?RIC REVASCULARITZACIO ",
                                         "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                         "EVENT PERIF?RIC AMPUTACI? MENOR",
                                         "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                         "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                         "INSUFICI?NCIA CARD?ACA"))

#17	data_ep_cv1
dades_doria$data_ep_cv1<-ymd(dades_doria$data_ep_cv1)

#18	ep_cv_2
dades_doria$ep_cv2<- factor(dades_doria$ep_cv_2,
                            levels = c(1,2,3,4,5,6,7,8,9),
                            labels = c("CARDIOPATIA ISQU?MICA",
                                       "EVENT CEREBRO VASCULAR",
                                       "EVENT PERIF?RIC ARTERIOGRAFIA",
                                       "EVENT PERIF?RIC REVASCULARITZACIO ",
                                       "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                       "EVENT PERIF?RIC AMPUTACI? MENOR",
                                       "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                       "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                       "INSUFICI?NCIA CARD?ACA"))


#19	data_epcv2
dades_doria$data_epcv2<-ymd(dades_doria$data_epcv2)

#20	ep_cv3
dades_doria$ep_cv3<- factor(dades_doria$ep_cv3,
                            levels = c(1,2,3,4,5,6,7,8,9),
                            labels = c("CARDIOPATIA ISQU?MICA",
                                       "EVENT CEREBRO VASCULAR",
                                       "EVENT PERIF?RIC ARTERIOGRAFIA",
                                       "EVENT PERIF?RIC REVASCULARITZACIO ",
                                       "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                       "EVENT PERIF?RIC AMPUTACI? MENOR",
                                       "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                       "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                       "INSUFICI?NCIA CARD?ACA"))

#21	data_epcv3
dades_doria$data_epcv3<-ymd(dades_doria$data_epcv3)

#22	ep_cv4
dades_doria$ep_cv4<- factor(dades_doria$ep_cv4,
                            levels = c(1,2,3,4,5,6,7,8,9),
                            labels = c("CARDIOPATIA ISQU?MICA",
                                       "EVENT CEREBRO VASCULAR",
                                       "EVENT PERIF?RIC ARTERIOGRAFIA",
                                       "EVENT PERIF?RIC REVASCULARITZACIO ",
                                       "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                       "EVENT PERIF?RIC AMPUTACI? MENOR",
                                       "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                       "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                       "INSUFICI?NCIA CARD?ACA"))
#23	data_epcv
dades_doria$data_epcv<-ymd(dades_doria$data_epcv)

#24	ep_cv5
dades_doria$ep_cv5<- factor(dades_doria$ep_cv5,
                            levels = c(1,2,3,4,5,6,7,8,9),
                            labels = c("CARDIOPATIA ISQU?MICA",
                                       "EVENT CEREBRO VASCULAR",
                                       "EVENT PERIF?RIC ARTERIOGRAFIA",
                                       "EVENT PERIF?RIC REVASCULARITZACIO ",
                                       "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                       "EVENT PERIF?RIC AMPUTACI? MENOR",
                                       "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                       "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                       "INSUFICI?NCIA CARD?ACA"))

#25	data_epcv5
dades_doria$data_epcv5<-ymd(dades_doria$data_epcv5)

#26	ep_cv6
dades_doria$ep_cv6<- factor(dades_doria$ep_cv6,
                            levels = c(1,2,3,4,5,6,7,8,9),
                            labels = c("CARDIOPATIA ISQU?MICA",
                                       "EVENT CEREBRO VASCULAR",
                                       "EVENT PERIF?RIC ARTERIOGRAFIA",
                                       "EVENT PERIF?RIC REVASCULARITZACIO ",
                                       "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                       "EVENT PERIF?RIC AMPUTACI? MENOR",
                                       "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                       "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                       "INSUFICI?NCIA CARD?ACA"))

#27	data_epcv6
dades_doria$data_epcv6<-ymd(dades_doria$data_epcv6)

#28	ep_cv7
dades_doria$ep_cv7<- factor(dades_doria$ep_cv7,
                            levels = c(1,2,3,4,5,6,7,8,9),
                            labels = c("CARDIOPATIA ISQU?MICA",
                                       "EVENT CEREBRO VASCULAR",
                                       "EVENT PERIF?RIC ARTERIOGRAFIA",
                                       "EVENT PERIF?RIC REVASCULARITZACIO ",
                                       "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                       "EVENT PERIF?RIC AMPUTACI? MENOR",
                                       "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                       "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                       "INSUFICI?NCIA CARD?ACA"))

#29	data_epcv7
dades_doria$data_epcv7<-ymd(dades_doria$data_epcv7)

#30	ep_cv8
dades_doria$ep_cv8<- factor(dades_doria$ep_cv8,
                            levels = c(1,2,3,4,5,6,7,8,9),
                            labels = c("CARDIOPATIA ISQU?MICA",
                                       "EVENT CEREBRO VASCULAR",
                                       "EVENT PERIF?RIC ARTERIOGRAFIA",
                                       "EVENT PERIF?RIC REVASCULARITZACIO ",
                                       "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                       "EVENT PERIF?RIC AMPUTACI? MENOR",
                                       "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                       "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                       "INSUFICI?NCIA CARD?ACA"))

#31	data_epcv8
dades_doria$data_epcv8<-ymd(dades_doria$data_epcv8)


#32	retinopaita

  #1=SI DIABETICA 
  #2= NO
  #3= DESCONEGUDA 
  #4= SI HIPERTENSIVA

dades_doria$retinopaita<- factor(dades_doria$retinopaita,
                              levels = c(1,2,3,4),
                              labels = c("SI DIABETICA", "NO","DESCONEGUDA ","SI HIPERTENSIVA"))


#33	neuropatia

  #1=SI DIAB?TICA
  #2=NO
  #3=DESCONEGUDA 
  #4=SI ALCOHOLICA

dades_doria$neuropatia<- factor(dades_doria$neuropatia,
                                 levels = c(1,2,3,4),
                                 labels = c("SI DIABETICA", "NO","DESCONEGUDA ","SI ALCOHOLICA"))


#34	ulcera_prvia

  #1=SI
  #2=NO

dades_doria$ulcera_prvia<- factor(dades_doria$ulcera_prvia,
                              levels = c(1,2),
                              labels = c("SI", "NO"))


#35	ulcera_actual

  #1=SI
  #2=NO

dades_doria$ulcera_actual<- factor(dades_doria$	ulcera_actual,
                                  levels = c(1,2),
                                  labels = c("SI", "NO"))

#36	amputacions

  #1=MAJOR
  #2=MENOR
  #3=NO
  #4=TRAUMATICA 

dades_doria$amputacions<- factor(dades_doria$amputacions,
                                levels = c(1,2,3,4),
                                labels = c("MAJOR", "MENOR","NO","TRAUMATICA"))



#37	mvp

  #0_no explorada 
  #1-present
  #2-no present 
  #3_no valorable 

dades_doria$mvp<- factor(dades_doria$mvp,
                                 levels = c(0,1,2,3),
                                 labels = c("no explorada", "present","no present","no valorable"))


#38	data_itb

dades_doria$data_itb<-ymd(dades_doria$data_itb)



#39	itb_dret


#40	data_ex_vascular
dades_doria$data_ex_vascular<-ymd(dades_doria$data_ex_vascular)



#41	placa_estenosant

  #EN BLANC= NO EXPLORAT 
  #1=PLACA NO ESTENOSANT 
  #2=PLACA AMB ESTENOSI 30-50%
  #3=PLACA AMB ESTENOSI 50-69%
  #4=PLACA AMB ESTENOSI 70-95%
  #5=PLACA OCLUSIVA
  #6=NO PLACA 

dades_doria$placa_estenosant<- factor(dades_doria$placa_estenosant,
                            levels = c(1,2,3,4,5,6),
                            labels = c("PLACA NO ESTENOSANT",
                                       "PLACA AMB ESTENOSI 30-50%",
                                       "PLACA AMB ESTENOSI 50-69%",
                                       "PLACA AMB ESTENOSI 70-95% ",
                                       "PLACA OCLUSIVA",
                                       "NO PLACA "))


#42	GIM

#43	data_stop_HD
dades_doria$data_stop_HD<-ymd(dades_doria$data_stop_HD)


#44	MOTIU_STOP

  #0=P?RDUA SEGUIMENT
  #1=VIU
  #2=EXITUS
  #3=TRANSPLANT

dades_doria$MOTIU_STOP<- factor(dades_doria$MOTIU_STOP,
                                      levels = c(0,1,2,3),
                                      labels = c("P?RDUA SEGUIMENT",
                                                 "VIU",
                                                 "EXITUS",
                                                 "TRANSPLANT"))
                                                 


#45	motiu_exitus

#MOTIU EXITUS 
  #0=NO CONEGUT 
  #1=CARDIOVASCULAR 
  #2=NO CARDIOVASCULAR 
  #3=PERDUA SEGUIMENT 

dades_doria$motiu_exitus<- factor(dades_doria$motiu_exitus,
                                levels = c(0,1,2,3),
                                labels = c("NO CONEGUT",
                                           "CARDIOVASCULAR ",
                                           "NO CARDIOVASCULAR ",
                                           "PERDUA SEGUIMENT"))


#46	DATA_EVENT_1
dades_doria$DATA_EVENT_1<-ymd(dades_doria$DATA_EVENT_1)

#47	descripci_e1
  #EVENT SEGUIMENT N? 1 
  #1=CARDIOPATIA ISQU?MICA
  #2=EVENT CEREBRO VASCULAR 
  #3=EVENT PERIF?RIC ARTERIOGRAFIA
  #4=EVENT PERIF?RIC REVASCULARITZACIO 
  #5=EVENT PERIF?RIC AMPUTACI? MAJOR 
  #6=EVENT PERIF?RIC AMPUTACI? MENOR
  #7=EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT 
  #8=ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR 
  #9=INSUFICI?NCIA CARD?ACA 
  #10=COLITIS ISQU?MICA
  #11=ISQUEMIA MESENT?RICA
  #12=HEMATOMA SUBDURAL
  #13=TRANSPLANT
  #14=MORT CARDIOVASCULAR 
  #15=MORT NO CARDIOVASCULAR 
  #16=MORT AMB ORIGEN NO FILIAT 
  #17=REINICI HEMODIALISI
  #18=MORT RELACIONADA AMB ULCERES , ISQUEMIAEEII, AMPUTACIONS 
  #19=PERDUA SEGUIMENT
  #20=SITUACIO FI DE VIDA 
  #21=PROC?S INFECCIOS 
  #22=NEOPLASIA 
  #23=FIXADOR EXTERN

dades_doria$descripci_e1<- factor(dades_doria$descripci_e1,
                                  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
                                  labels = c("CARDIOPATIA ISQU?MICA",
                                             "EVENT CEREBRO VASCULAR ",
                                             "EVENT PERIF?RIC ARTERIOGRAFIA ",
                                             "EVENT PERIF?RIC REVASCULARITZACIO ",
                                             "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                             "EVENT PERIF?RIC AMPUTACI? MENOR",
                                             "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                             "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                             "INSUFICI?NCIA CARD?ACA",
                                             "COLITIS ISQU?MICA",
                                             "ISQUEMIA MESENT?RICA",
                                             "HEMATOMA SUBDURAL",
                                             "TRANSPLANT",
                                             "MORT CARDIOVASCULAR",
                                             "MORT NO CARDIOVASCULAR",
                                             "MORT AMB ORIGEN NO FILIAT ",
                                             "REINICI HEMODIALISI",
                                             "MORT RELACIONADA AMB ULCERES , ISQUEMIAEEII, AMPUTACIONS ",
                                             "PERDUA SEGUIMENT",
                                             "SITUACIO FI DE VIDA",
                                             "PROC?S INFECCIOS ",
                                             "NEOPLASIA",
                                             "FIXADOR EXTERN"))


#48	data_eveny_2
dades_doria$data_eveny_2<-ymd(dades_doria$data_eveny_2)

#49	descripci_e2
dades_doria$descripci_e2<- factor(dades_doria$descripci_e2,
                                  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
                                  labels = c("CARDIOPATIA ISQU?MICA",
                                             "EVENT CEREBRO VASCULAR ",
                                             "EVENT PERIF?RIC ARTERIOGRAFIA ",
                                             "EVENT PERIF?RIC REVASCULARITZACIO ",
                                             "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                             "EVENT PERIF?RIC AMPUTACI? MENOR",
                                             "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                             "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                             "INSUFICI?NCIA CARD?ACA",
                                             "COLITIS ISQU?MICA",
                                             "ISQUEMIA MESENT?RICA",
                                             "HEMATOMA SUBDURAL",
                                             "TRANSPLANT",
                                             "MORT CARDIOVASCULAR",
                                             "MORT NO CARDIOVASCULAR",
                                             "MORT AMB ORIGEN NO FILIAT ",
                                             "REINICI HEMODIALISI",
                                             "MORT RELACIONADA AMB ULCERES , ISQUEMIAEEII, AMPUTACIONS ",
                                             "PERDUA SEGUIMENT",
                                             "SITUACIO FI DE VIDA",
                                             "PROC?S INFECCIOS ",
                                             "NEOPLASIA",
                                             "FIXADOR EXTERN"))

#50	data_event_3
dades_doria$data_event_3<-ymd(dades_doria$data_event_3)

#51	descripci_e3
dades_doria$descripci_e3<- factor(dades_doria$descripci_e3,
                                  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
                                  labels = c("CARDIOPATIA ISQU?MICA",
                                             "EVENT CEREBRO VASCULAR ",
                                             "EVENT PERIF?RIC ARTERIOGRAFIA ",
                                             "EVENT PERIF?RIC REVASCULARITZACIO ",
                                             "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                             "EVENT PERIF?RIC AMPUTACI? MENOR",
                                             "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                             "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                             "INSUFICI?NCIA CARD?ACA",
                                             "COLITIS ISQU?MICA",
                                             "ISQUEMIA MESENT?RICA",
                                             "HEMATOMA SUBDURAL",
                                             "TRANSPLANT",
                                             "MORT CARDIOVASCULAR",
                                             "MORT NO CARDIOVASCULAR",
                                             "MORT AMB ORIGEN NO FILIAT ",
                                             "REINICI HEMODIALISI",
                                             "MORT RELACIONADA AMB ULCERES , ISQUEMIAEEII, AMPUTACIONS ",
                                             "PERDUA SEGUIMENT",
                                             "SITUACIO FI DE VIDA",
                                             "PROC?S INFECCIOS ",
                                             "NEOPLASIA",
                                             "FIXADOR EXTERN"))

#52	data_event_4
dades_doria$data_event_4<-ymd(dades_doria$data_event_4)

#53	descripci_event_4
dades_doria$descripci_e4<- factor(dades_doria$descripci_event_4,
                                  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
                                  labels = c("CARDIOPATIA ISQU?MICA",
                                             "EVENT CEREBRO VASCULAR ",
                                             "EVENT PERIF?RIC ARTERIOGRAFIA ",
                                             "EVENT PERIF?RIC REVASCULARITZACIO ",
                                             "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                             "EVENT PERIF?RIC AMPUTACI? MENOR",
                                             "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                             "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                             "INSUFICI?NCIA CARD?ACA",
                                             "COLITIS ISQU?MICA",
                                             "ISQUEMIA MESENT?RICA",
                                             "HEMATOMA SUBDURAL",
                                             "TRANSPLANT",
                                             "MORT CARDIOVASCULAR",
                                             "MORT NO CARDIOVASCULAR",
                                             "MORT AMB ORIGEN NO FILIAT ",
                                             "REINICI HEMODIALISI",
                                             "MORT RELACIONADA AMB ULCERES , ISQUEMIAEEII, AMPUTACIONS ",
                                             "PERDUA SEGUIMENT",
                                             "SITUACIO FI DE VIDA",
                                             "PROC?S INFECCIOS ",
                                             "NEOPLASIA",
                                             "FIXADOR EXTERN"))



#54	data_event_5
dades_doria$data_event_5<-ymd(dades_doria$data_event_5)

#55	descripci_event_5
dades_doria$descripci_e5<- factor(dades_doria$descripci_event_5,
                                  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
                                  labels = c("CARDIOPATIA ISQU?MICA",
                                             "EVENT CEREBRO VASCULAR ",
                                             "EVENT PERIF?RIC ARTERIOGRAFIA ",
                                             "EVENT PERIF?RIC REVASCULARITZACIO ",
                                             "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                             "EVENT PERIF?RIC AMPUTACI? MENOR",
                                             "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                             "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                             "INSUFICI?NCIA CARD?ACA",
                                             "COLITIS ISQU?MICA",
                                             "ISQUEMIA MESENT?RICA",
                                             "HEMATOMA SUBDURAL",
                                             "TRANSPLANT",
                                             "MORT CARDIOVASCULAR",
                                             "MORT NO CARDIOVASCULAR",
                                             "MORT AMB ORIGEN NO FILIAT ",
                                             "REINICI HEMODIALISI",
                                             "MORT RELACIONADA AMB ULCERES , ISQUEMIAEEII, AMPUTACIONS ",
                                             "PERDUA SEGUIMENT",
                                             "SITUACIO FI DE VIDA",
                                             "PROC?S INFECCIOS ",
                                             "NEOPLASIA",
                                             "FIXADOR EXTERN"))

#56	data_event_6
dades_doria$data_event_6<-ymd(dades_doria$data_event_6)

#57	descripci_event_6
dades_doria$descripci_e6<- factor(dades_doria$descripci_event_6,
                                  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
                                  labels = c("CARDIOPATIA ISQU?MICA",
                                             "EVENT CEREBRO VASCULAR ",
                                             "EVENT PERIF?RIC ARTERIOGRAFIA ",
                                             "EVENT PERIF?RIC REVASCULARITZACIO ",
                                             "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                             "EVENT PERIF?RIC AMPUTACI? MENOR",
                                             "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                             "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                             "INSUFICI?NCIA CARD?ACA",
                                             "COLITIS ISQU?MICA",
                                             "ISQUEMIA MESENT?RICA",
                                             "HEMATOMA SUBDURAL",
                                             "TRANSPLANT",
                                             "MORT CARDIOVASCULAR",
                                             "MORT NO CARDIOVASCULAR",
                                             "MORT AMB ORIGEN NO FILIAT ",
                                             "REINICI HEMODIALISI",
                                             "MORT RELACIONADA AMB ULCERES , ISQUEMIAEEII, AMPUTACIONS ",
                                             "PERDUA SEGUIMENT",
                                             "SITUACIO FI DE VIDA",
                                             "PROC?S INFECCIOS ",
                                             "NEOPLASIA",
                                             "FIXADOR EXTERN"))

#58	data_event_7
dades_doria$data_event_7<-ymd(dades_doria$data_event_7)




#59	descripcievent_7
dades_doria$descripci_e7<- factor(dades_doria$descripcievent_7,
                                  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
                                  labels = c("CARDIOPATIA ISQU?MICA",
                                             "EVENT CEREBRO VASCULAR ",
                                             "EVENT PERIF?RIC ARTERIOGRAFIA ",
                                             "EVENT PERIF?RIC REVASCULARITZACIO ",
                                             "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                             "EVENT PERIF?RIC AMPUTACI? MENOR",
                                             "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                             "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                             "INSUFICI?NCIA CARD?ACA",
                                             "COLITIS ISQU?MICA",
                                             "ISQUEMIA MESENT?RICA",
                                             "HEMATOMA SUBDURAL",
                                             "TRANSPLANT",
                                             "MORT CARDIOVASCULAR",
                                             "MORT NO CARDIOVASCULAR",
                                             "MORT AMB ORIGEN NO FILIAT ",
                                             "REINICI HEMODIALISI",
                                             "MORT RELACIONADA AMB ULCERES , ISQUEMIAEEII, AMPUTACIONS ",
                                             "PERDUA SEGUIMENT",
                                             "SITUACIO FI DE VIDA",
                                             "PROC?S INFECCIOS ",
                                             "NEOPLASIA",
                                             "FIXADOR EXTERN"))

#60	data_event_8
dades_doria$data_event_8<-ymd(dades_doria$data_event_8)

#61	descripcievent_8
dades_doria$descripci_e8<- factor(dades_doria$descripcievent_8,
                                  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
                                  labels = c("CARDIOPATIA ISQU?MICA",
                                             "EVENT CEREBRO VASCULAR ",
                                             "EVENT PERIF?RIC ARTERIOGRAFIA ",
                                             "EVENT PERIF?RIC REVASCULARITZACIO ",
                                             "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                             "EVENT PERIF?RIC AMPUTACI? MENOR",
                                             "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                             "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                             "INSUFICI?NCIA CARD?ACA",
                                             "COLITIS ISQU?MICA",
                                             "ISQUEMIA MESENT?RICA",
                                             "HEMATOMA SUBDURAL",
                                             "TRANSPLANT",
                                             "MORT CARDIOVASCULAR",
                                             "MORT NO CARDIOVASCULAR",
                                             "MORT AMB ORIGEN NO FILIAT ",
                                             "REINICI HEMODIALISI",
                                             "MORT RELACIONADA AMB ULCERES , ISQUEMIAEEII, AMPUTACIONS ",
                                             "PERDUA SEGUIMENT",
                                             "SITUACIO FI DE VIDA",
                                             "PROC?S INFECCIOS ",
                                             "NEOPLASIA",
                                             "FIXADOR EXTERN"))

#62	data_event_9
dades_doria$data_event_9<-ymd(dades_doria$data_event_9)

#63	descripcievent_9
dades_doria$descripci_e9<- factor(dades_doria$descripcievent_9,
                                  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
                                  labels = c("CARDIOPATIA ISQU?MICA",
                                             "EVENT CEREBRO VASCULAR ",
                                             "EVENT PERIF?RIC ARTERIOGRAFIA ",
                                             "EVENT PERIF?RIC REVASCULARITZACIO ",
                                             "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                             "EVENT PERIF?RIC AMPUTACI? MENOR",
                                             "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                             "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                             "INSUFICI?NCIA CARD?ACA",
                                             "COLITIS ISQU?MICA",
                                             "ISQUEMIA MESENT?RICA",
                                             "HEMATOMA SUBDURAL",
                                             "TRANSPLANT",
                                             "MORT CARDIOVASCULAR",
                                             "MORT NO CARDIOVASCULAR",
                                             "MORT AMB ORIGEN NO FILIAT ",
                                             "REINICI HEMODIALISI",
                                             "MORT RELACIONADA AMB ULCERES , ISQUEMIAEEII, AMPUTACIONS ",
                                             "PERDUA SEGUIMENT",
                                             "SITUACIO FI DE VIDA",
                                             "PROC?S INFECCIOS ",
                                             "NEOPLASIA",
                                             "FIXADOR EXTERN"))

#64	data_event_10
dades_doria$data_event_10<-ymd(dades_doria$data_event_10)

#65	descripcievent_10
dades_doria$descripci_e10<- factor(dades_doria$descripcievent_10,
                                  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
                                  labels = c("CARDIOPATIA ISQU?MICA",
                                             "EVENT CEREBRO VASCULAR ",
                                             "EVENT PERIF?RIC ARTERIOGRAFIA ",
                                             "EVENT PERIF?RIC REVASCULARITZACIO ",
                                             "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                             "EVENT PERIF?RIC AMPUTACI? MENOR",
                                             "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                             "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                             "INSUFICI?NCIA CARD?ACA",
                                             "COLITIS ISQU?MICA",
                                             "ISQUEMIA MESENT?RICA",
                                             "HEMATOMA SUBDURAL",
                                             "TRANSPLANT",
                                             "MORT CARDIOVASCULAR",
                                             "MORT NO CARDIOVASCULAR",
                                             "MORT AMB ORIGEN NO FILIAT ",
                                             "REINICI HEMODIALISI",
                                             "MORT RELACIONADA AMB ULCERES , ISQUEMIAEEII, AMPUTACIONS ",
                                             "PERDUA SEGUIMENT",
                                             "SITUACIO FI DE VIDA",
                                             "PROC?S INFECCIOS ",
                                             "NEOPLASIA",
                                             "FIXADOR EXTERN"))

#66	data_event_11
dades_doria$data_event_11<-ymd(dades_doria$data_event_11)

#67	descripcievent_11
dades_doria$descripci_e11<- factor(dades_doria$descripcievent_11,
                                  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
                                  labels = c("CARDIOPATIA ISQU?MICA",
                                             "EVENT CEREBRO VASCULAR ",
                                             "EVENT PERIF?RIC ARTERIOGRAFIA ",
                                             "EVENT PERIF?RIC REVASCULARITZACIO ",
                                             "EVENT PERIF?RIC AMPUTACI? MAJOR",
                                             "EVENT PERIF?RIC AMPUTACI? MENOR",
                                             "EVENT PERIF?RIC AMPUTACI? ULCERA I/0 DESBRIDAMENT ",
                                             "ARRITMIA CARD?ACA PER FIBRILACI? AURICULAR",
                                             "INSUFICI?NCIA CARD?ACA",
                                             "COLITIS ISQU?MICA",
                                             "ISQUEMIA MESENT?RICA",
                                             "HEMATOMA SUBDURAL",
                                             "TRANSPLANT",
                                             "MORT CARDIOVASCULAR",
                                             "MORT NO CARDIOVASCULAR",
                                             "MORT AMB ORIGEN NO FILIAT ",
                                             "REINICI HEMODIALISI",
                                             "MORT RELACIONADA AMB ULCERES , ISQUEMIAEEII, AMPUTACIONS ",
                                             "PERDUA SEGUIMENT",
                                             "SITUACIO FI DE VIDA",
                                             "PROC?S INFECCIOS ",
                                             "NEOPLASIA",
                                             "FIXADOR EXTERN"))

#68	data_inclusi
dades_doria$data_inclusi<-ymd(dades_doria$data_inclusi)

#69	dfin
dades_doria$dfin<-ymd(dades_doria$dfin)


#observar:[!]


#******
library(compareGroups)
#-----------------------------------------------------------------#

#-----------------------------------------------------------------#
T1<-descrTable(~.-ID,dades_doria,show.p.overall = F,method = 2,Q1 = 0, Q3 = 1,max.ylev=Inf)



save(T1,file="output_MDORIA.Rdata")







