setwd("~/icu_data/")
library(tidyverse)
hr_data = read.csv("mv_data_mimic4_v1.csv")
cohort = read.csv("mimic4_v1/icustays_1.0.csv",header=T)
careunits = read.csv("mimic4_v1/careunits_1.0.csv",header=T)
cohort = merge(cohort,careunits)
sedatives = read.csv("mimic4_v1/sedatives_1.0.csv")
item_ids = read.csv("mimic4_v1/d_items.csv")
sedatives = merge(sedatives,item_ids[,c("itemid","label")])
load("ventilatormode_levels.RData")
names(sedatives)[11] = 'sedative_rate'
names(sedatives)[9] = 'sedative_amt'
names(sedatives)[13] = 'sedative'
sedatives$sedative = as.character(sedatives$sedative)
sedatives$opioid = sedatives$sedative %in% c("Fentanyl (Concentrate)","Hydromorphone (Dilaudid)","Fentanyl","Morphine Sulfate")
sedatives$benzo =sedatives$sedative %in% c("Midazolam (Versed)","Diazepam (Valium)","Lorazepam (Ativan)")
sedatives$propofol = sedatives$sedative =="Propofol"
sedatives$dex = sedatives$sedative =="Dexmedetomidine (Precedex)"
sedatives$ket = sedatives$sedative =="Ketamine"

sedatives = merge(sedatives,cohort[,c("stay_id","intime")])
sedatives$starthour = as.numeric(difftime(strptime(as.character(sedatives$starttime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(sedatives$intime), "%Y-%m-%d %H:%M:%S"),units="hours"))
sedatives$endhour = as.numeric(difftime(strptime(as.character(sedatives$endtime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(sedatives$intime), "%Y-%m-%d %H:%M:%S"),units="hours"))
sedatives$endhour = pmax(sedatives$endhour,sedatives$starthour)

from_to =function(a,b){
  a:b
}
rep_a_b = function(x,a,b){
  rep(x,b-a+1)
}
sedatives_drip = sedatives[sedatives$ordercategoryname=='01-Drips',]
sedatives_drip$duration = sedatives_drip$endhour - sedatives_drip$starthour

sedatives_drip_opioids = sedatives_drip[sedatives_drip$opioid==1,]
sedatives_drip_opioids = sedatives_drip_opioids[order(sedatives_drip_opioids$stay_id,sedatives_drip_opioids$starthour),]
sedatives_drip_opioids$end = 0
for(i in 1:(nrow(sedatives_drip_opioids)-1)){
  if(sedatives_drip_opioids$stay_id[i+1]==sedatives_drip_opioids$stay_id[i]){
    if(sedatives_drip_opioids$starthour[i+1]>=sedatives_drip_opioids$endhour[i]+.5){
      sedatives_drip_opioids$end[i]=1
    }
  }else{
    sedatives_drip_opioids$end[i]=1
  }
}
sedatives_drip_opioids$end[nrow(sedatives_drip_opioids)]=1

sedatives_drip_opioids$start = 0
for(i in 2:(nrow(sedatives_drip_opioids))){
  if(sedatives_drip_opioids$stay_id[i-1]==sedatives_drip_opioids$stay_id[i]){
    if(sedatives_drip_opioids$endhour[i-1]<=sedatives_drip_opioids$starthour[i]-.5){
      sedatives_drip_opioids$start[i]=1
    }
  }else{
    sedatives_drip_opioids$start[i]=1
  }
}
sedatives_drip_opioids$start[1]=1
get_durations = function(stay_ids,starthours,endhours,starts,ends){
  duration_starts = rep(NA,sum(starts))
  duration_ends = rep(NA,sum(starts))
  for(i in 1:sum(starts)){
    start_ind = which(starts==1)[i]
    duration_starts[i] = starthours[start_ind]
    end_ind = min(which(ends==1)[which(ends==1)>=start_ind])
    duration_ends[i] = endhours[end_ind]
  }
  return(list(stay_ids[1:sum(starts)],duration_starts,duration_ends))
}
opioid_stay_ids = unique(sedatives_drip_opioids$stay_id)
opioids_durations_list = sapply(1:length(opioid_stay_ids),function(i)get_durations(stay_ids=sedatives_drip_opioids$stay_id[sedatives_drip_opioids$stay_id==opioid_stay_ids[i]],
                                                                                   starthours=sedatives_drip_opioids$starthour[sedatives_drip_opioids$stay_id==opioid_stay_ids[i]],
                                                                                   endhours=sedatives_drip_opioids$endhour[sedatives_drip_opioids$stay_id==opioid_stay_ids[i]],
                                                                                   starts=sedatives_drip_opioids$start[sedatives_drip_opioids$stay_id==opioid_stay_ids[i]],
                                                                                   ends=sedatives_drip_opioids$end[sedatives_drip_opioids$stay_id==opioid_stay_ids[i]]))
opioids_durations = data.frame(stay_id=unlist(opioids_durations_list[1,]),duration_start=unlist(opioids_durations_list[2,]),duration_end=unlist(opioids_durations_list[3,]))
opioids_durations$duration = opioids_durations$duration_end - opioids_durations$duration_start
opioids_durations$hour = pmax(ceiling(opioids_durations$duration_start),1)

sedatives_drip_benzo = sedatives_drip[sedatives_drip$benzo==1,]
sedatives_drip_benzo = sedatives_drip_benzo[order(sedatives_drip_benzo$stay_id,sedatives_drip_benzo$starthour),]
sedatives_drip_benzo$end = 0
for(i in 1:(nrow(sedatives_drip_benzo)-1)){
  if(sedatives_drip_benzo$stay_id[i+1]==sedatives_drip_benzo$stay_id[i]){
    if(sedatives_drip_benzo$starthour[i+1]>=sedatives_drip_benzo$endhour[i]+.5){
      sedatives_drip_benzo$end[i]=1
    }
  }else{
    sedatives_drip_benzo$end[i]=1
  }
}
sedatives_drip_benzo$end[nrow(sedatives_drip_benzo)]=1

sedatives_drip_benzo$start = 0
for(i in 2:(nrow(sedatives_drip_benzo))){
  if(sedatives_drip_benzo$stay_id[i-1]==sedatives_drip_benzo$stay_id[i]){
    if(sedatives_drip_benzo$endhour[i-1]<=sedatives_drip_benzo$starthour[i]-.5){
      sedatives_drip_benzo$start[i]=1
    }
  }else{
    sedatives_drip_benzo$start[i]=1
  }
}
sedatives_drip_benzo$start[1]=1

benzo_stay_ids = unique(sedatives_drip_benzo$stay_id)
benzo_durations_list = sapply(1:length(benzo_stay_ids),function(i)get_durations(stay_ids=sedatives_drip_benzo$stay_id[sedatives_drip_benzo$stay_id==benzo_stay_ids[i]],
                                                                                starthours=sedatives_drip_benzo$starthour[sedatives_drip_benzo$stay_id==benzo_stay_ids[i]],
                                                                                endhours=sedatives_drip_benzo$endhour[sedatives_drip_benzo$stay_id==benzo_stay_ids[i]],
                                                                                starts=sedatives_drip_benzo$start[sedatives_drip_benzo$stay_id==benzo_stay_ids[i]],
                                                                                ends=sedatives_drip_benzo$end[sedatives_drip_benzo$stay_id==benzo_stay_ids[i]]))
benzo_durations = data.frame(stay_id=unlist(benzo_durations_list[1,]),duration_start=unlist(benzo_durations_list[2,]),duration_end=unlist(benzo_durations_list[3,]))
benzo_durations$duration = benzo_durations$duration_end - benzo_durations$duration_start
benzo_durations$hour = pmax(ceiling(benzo_durations$duration_start),1)


sedatives_drip_dex = sedatives_drip[sedatives_drip$dex==1,]
sedatives_drip_dex = sedatives_drip_dex[order(sedatives_drip_dex$stay_id,sedatives_drip_dex$starthour),]
sedatives_drip_dex$end = 0
for(i in 1:(nrow(sedatives_drip_dex)-1)){
  if(sedatives_drip_dex$stay_id[i+1]==sedatives_drip_dex$stay_id[i]){
    if(sedatives_drip_dex$starthour[i+1]>=sedatives_drip_dex$endhour[i]+.5){
      sedatives_drip_dex$end[i]=1
    }
  }else{
    sedatives_drip_dex$end[i]=1
  }
}
sedatives_drip_dex$end[nrow(sedatives_drip_dex)]=1

sedatives_drip_dex$start = 0
for(i in 2:(nrow(sedatives_drip_dex))){
  if(sedatives_drip_dex$stay_id[i-1]==sedatives_drip_dex$stay_id[i]){
    if(sedatives_drip_dex$endhour[i-1]<=sedatives_drip_dex$starthour[i]-.5){
      sedatives_drip_dex$start[i]=1
    }
  }else{
    sedatives_drip_dex$start[i]=1
  }
}
sedatives_drip_dex$start[1]=1

dex_stay_ids = unique(sedatives_drip_dex$stay_id)
dex_durations_list = sapply(1:length(dex_stay_ids),function(i)get_durations(stay_ids=sedatives_drip_dex$stay_id[sedatives_drip_dex$stay_id==dex_stay_ids[i]],
                                                                            starthours=sedatives_drip_dex$starthour[sedatives_drip_dex$stay_id==dex_stay_ids[i]],
                                                                            endhours=sedatives_drip_dex$endhour[sedatives_drip_dex$stay_id==dex_stay_ids[i]],
                                                                            starts=sedatives_drip_dex$start[sedatives_drip_dex$stay_id==dex_stay_ids[i]],
                                                                            ends=sedatives_drip_dex$end[sedatives_drip_dex$stay_id==dex_stay_ids[i]]))
dex_durations = data.frame(stay_id=unlist(dex_durations_list[1,]),duration_start=unlist(dex_durations_list[2,]),duration_end=unlist(dex_durations_list[3,]))
dex_durations$duration = dex_durations$duration_end - dex_durations$duration_start
dex_durations$hour = pmax(ceiling(dex_durations$duration_start),1)



sedatives_drip_ket = sedatives_drip[sedatives_drip$ket==1,]
sedatives_drip_ket = sedatives_drip_ket[order(sedatives_drip_ket$stay_id,sedatives_drip_ket$starthour),]
sedatives_drip_ket$end = 0
for(i in 1:(nrow(sedatives_drip_ket)-1)){
  if(sedatives_drip_ket$stay_id[i+1]==sedatives_drip_ket$stay_id[i]){
    if(sedatives_drip_ket$starthour[i+1]>=sedatives_drip_ket$endhour[i]+.5){
      sedatives_drip_ket$end[i]=1
    }
  }else{
    sedatives_drip_ket$end[i]=1
  }
}
sedatives_drip_ket$end[nrow(sedatives_drip_ket)]=1

sedatives_drip_ket$start = 0
for(i in 2:(nrow(sedatives_drip_ket))){
  if(sedatives_drip_ket$stay_id[i-1]==sedatives_drip_ket$stay_id[i]){
    if(sedatives_drip_ket$endhour[i-1]<=sedatives_drip_ket$starthour[i]-.5){
      sedatives_drip_ket$start[i]=1
    }
  }else{
    sedatives_drip_ket$start[i]=1
  }
}
sedatives_drip_ket$start[1]=1

ket_stay_ids = unique(sedatives_drip_ket$stay_id)
ket_durations_list = sapply(1:length(ket_stay_ids),function(i)get_durations(stay_ids=sedatives_drip_ket$stay_id[sedatives_drip_ket$stay_id==ket_stay_ids[i]],
                                                                            starthours=sedatives_drip_ket$starthour[sedatives_drip_ket$stay_id==ket_stay_ids[i]],
                                                                            endhours=sedatives_drip_ket$endhour[sedatives_drip_ket$stay_id==ket_stay_ids[i]],
                                                                            starts=sedatives_drip_ket$start[sedatives_drip_ket$stay_id==ket_stay_ids[i]],
                                                                            ends=sedatives_drip_ket$end[sedatives_drip_ket$stay_id==ket_stay_ids[i]]))
ket_durations = data.frame(stay_id=unlist(ket_durations_list[1,]),duration_start=unlist(ket_durations_list[2,]),duration_end=unlist(ket_durations_list[3,]))
ket_durations$duration = ket_durations$duration_end - ket_durations$duration_start
ket_durations$hour = pmax(ceiling(ket_durations$duration_start),1)

potential_violations = unique(do.call(rbind,list(opioids_durations[opioids_durations$duration>=1,c("stay_id","hour")],
                                                 benzo_durations[benzo_durations$duration>=1,c("stay_id","hour")],
                                                 dex_durations[dex_durations$duration>=1,c("stay_id","hour")],
                                                 ket_durations[ket_durations$duration>=1,c("stay_id","hour")])))

start_round = ceiling(sedatives$starthour)
end_round = ceiling(sedatives$endhour)
hr_opioids = unlist(mapply(rep_a_b,sedatives$opioid,start_round,end_round))
hr_benzos = unlist(mapply(rep_a_b,sedatives$benzo,start_round,end_round))
hr_propofol = unlist(mapply(rep_a_b,sedatives$propofol,start_round,end_round))
hr_dex = unlist(mapply(rep_a_b,sedatives$dex,start_round,end_round))
hr_ket = unlist(mapply(rep_a_b,sedatives$ket,start_round,end_round))

drugs_hours = unlist(mapply(from_to,start_round,end_round))
drugs_icustay_id = unlist(mapply(rep_a_b,sedatives$stay_id,start_round,end_round))
drugs = data.frame(cbind('opioid'=hr_opioids,'benzo'=hr_benzos,'propofol'=hr_propofol,'dex'=hr_dex,'ket'=hr_ket, 'stay_id'=drugs_icustay_id,'hour'=drugs_hours))
drugs = drugs %>%
  group_by(stay_id,hour) %>%
  summarise_all(max,na.rm=T)

# sedatives_bolus = sedatives[sedatives$ordercategoryname=='05-Med Bolus',]
# start_round_bolus = ceiling(sedatives_bolus$starthour)
# end_round_bolus = ceiling(sedatives_bolus$endhour)
# hr_opioids_bolus = unlist(mapply(rep_a_b,sedatives_bolus$opioid,start_round_bolus,end_round_bolus))
# hr_benzos_bolus = unlist(mapply(rep_a_b,sedatives_bolus$benzo,start_round_bolus,end_round_bolus))
# hr_propofol_bolus = unlist(mapply(rep_a_b,sedatives_bolus$propofol,start_round_bolus,end_round_bolus))
# hr_dex_bolus = unlist(mapply(rep_a_b,sedatives_bolus$dex,start_round_bolus,end_round_bolus))
# hr_ket_bolus = unlist(mapply(rep_a_b,sedatives_bolus$ket,start_round_bolus,end_round_bolus))
# 
# drugs_hours_bolus = unlist(mapply(from_to,start_round_bolus,end_round_bolus))
# drugs_icustay_id_bolus = unlist(mapply(rep_a_b,sedatives_bolus$stay_id,start_round_bolus,end_round_bolus))
# drugs_bolus = data.frame(cbind('opioid_bolus'=hr_opioids_bolus,'benzo_bolus'=hr_benzos_bolus,'propofol_bolus'=hr_propofol_bolus,'dex_bolus'=hr_dex_bolus,'ket_bolus'=hr_ket_bolus, 'stay_id'=drugs_icustay_id_bolus,'hour'=drugs_hours_bolus))
# drugs_bolus = drugs_bolus %>%
#   group_by(stay_id,hour) %>%
#   summarise_all(max,na.rm=T)

carry_forward = function(x){
  if(length(x)>1){
    for(j in 2:length(x)){
      if(is.na(x[j])|x[j]==''){
        x[j] = x[j-1]
      }
    }
  }
  x
}

get_last = function(x){
  if(length(x)>1){
    x=c(NA,x[1:(length(x)-1)])
  }else{
    x=NA
  }
  x
}

hr_data$ventilator_mode[hr_data$ventilator_mode==1] = NA
hr_data$ventilator_mode = unlist(lapply(split(hr_data$ventilator_mode,hr_data$stay_id),carry_forward))

hr_data$ventilator_mode = ventilatormode_levels[hr_data$ventilator_mode]
hr_data$control = 0
hr_data$control[grep('mv|asv',tolower(hr_data$ventilator_mode))]=1
hr_data$control = ifelse(is.na(hr_data$ventilation_status),0,ifelse(hr_data$ventilation_status %in% c(2,5),hr_data$control,0))
# base_hour = data[data$study_hour==1,c('stay_id','hour')]
# names(base_hour)[2] = 'base_hour'
# data = merge(data,base_hour)

hr_data = merge(hr_data,drugs,all.x=T)
hr_data$opioid[is.na(hr_data$opioid)] = 0
hr_data$propofol[is.na(hr_data$propofol)] = 0
hr_data$benzo[is.na(hr_data$benzo)] = 0
hr_data$ket[is.na(hr_data$ket)] = 0
hr_data$dex[is.na(hr_data$dex)] = 0

potential_violations$potential_violation = 1
hr_data = merge(hr_data,potential_violations,all.x=T)
hr_data$potential_violation[is.na(hr_data$potential_violation)] = 0

# hr_data = merge(hr_data,drugs_bolus,all.x=T)
# hr_data$opioid_bolus[is.na(hr_data$opioid_bolus)] = 0
# hr_data$propofol_bolus[is.na(hr_data$propofol_bolus)] = 0
# hr_data$benzo_bolus[is.na(hr_data$benzo_bolus)] = 0
# hr_data$ket_bolus[is.na(hr_data$ket_bolus)] = 0
# hr_data$dex_bolus[is.na(hr_data$dex_bolus)] = 0

propofol_drip = sedatives_drip[sedatives_drip$sedative=='Propofol',]
start_round_prop = ceiling(propofol_drip$starthour)
end_round_prop = ceiling(propofol_drip$endhour)
hr_propofol_rate = unlist(mapply(rep_a_b,propofol_drip$sedative_rate,start_round_prop,end_round_prop))
prop_rate_hours = unlist(mapply(from_to,start_round_prop,end_round_prop))
prop_icustay_id = unlist(mapply(rep_a_b,propofol_drip$stay_id,start_round_prop,end_round_prop))
prop_rate = data.frame(cbind('propofol_rate'=hr_propofol_rate,'stay_id'=prop_icustay_id,'hour'=prop_rate_hours))
prop_rate = prop_rate %>%
  group_by(stay_id,hour) %>%
  summarise_all(max,na.rm=T)
hr_data = merge(hr_data,prop_rate,all.x=T)
hr_data$propofol_rate[is.na(hr_data$propofol_rate)] = 0

# ventilator = read.csv("mimic4_v1/ventilator.csv")
# ventilator = merge(ventilator,careunits[,c("stay_id","intime","outtime")])
# ventilator$starthour = ceiling(as.numeric(difftime(strptime(as.character(ventilator$starttime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(ventilator$intime), "%Y-%m-%d %H:%M:%S"),units="hours")))
# ventilator$endhour = ceiling(as.numeric(difftime(strptime(as.character(ventilator$endtime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(ventilator$intime), "%Y-%m-%d %H:%M:%S"),units="hours")))
# ventilator = ventilator[ventilator$ventilation_status=='InvasiveVent',]
# ventilator$ventilation_status = 1
# hr_vent = unlist(mapply(rep_a_b,ventilator$ventilation_status,ventilator$starthour,ventilator$endhour))
# vent_hours = unlist(mapply(from_to,ventilator$starthour,ventilator$endhour))
# vent_icustay_id = unlist(mapply(rep_a_b,ventilator$stay_id,ventilator$starthour,ventilator$endhour))
# vents = data.frame(cbind('ventilation_status'=hr_vent,'stay_id'=vent_icustay_id,'hour'=vent_hours))
# vents = vents %>%
#   group_by(stay_id,hour) %>%
#   summarise_all(max,na.rm=T)
# hr_data = merge(hr_data,vents,all.x=T)
# hr_data$ventilation_status[is.na(hr_data$ventilation_status)] = 0

hr_data$first_control = unlist(lapply(split(hr_data$control,hr_data$stay_id),function(x)cumsum(cumsum(x))==1))

hr_data$study_start = hr_data$ventilation_status==2 & (hr_data$propofol_rate > 0)
hr_data$study_started = unlist(sapply(split(hr_data$study_start,hr_data$stay_id),function(x)cumsum(x)>0))
hr_data$study_hour = unlist(sapply(split(hr_data$study_started,hr_data$stay_id),cumsum))
vitals = c('sofa_24hours','spo2','resp_rate','mbp','dbp','sbp','heart_rate','rate_std','amount','liver_24hours','cardiovascular_24hours',
           'cns_24hours','coagulation_24hours','respiration_24hours','renal_24hours')
for(var in vitals){
  hr_data[,paste0('last_',var)] = unlist(sapply(split(hr_data[,var],hr_data$stay_id),get_last))
}
hr_data = hr_data[hr_data$study_started==1,]
hr_data = hr_data[!is.na(hr_data$stay_id),]
#figure out which patients were ventilated for over a day
stay_ids = unique(hr_data$stay_id)
vent_duration = aggregate(hr_data$ventilation_status,by=list(hr_data$stay_id),function(x)min(which(x!=2)))
vent24hr = vent_duration[,2]>=24
vent_dur = cbind(stay_id = stay_ids,vent_duration = vent_duration[,2],vent24hr=vent24hr)
# save(vent_dur,file='ventilation_durations_sedative.RData')
# 
# 
# load("ventilation_durations_sedative.RData")
hr_data = merge(hr_data,vent_dur[,c("stay_id","vent24hr")])
hr_data = hr_data[order(hr_data$stay_id,hr_data$hour),]

cohort = read.csv("mimic4_v1/icustays_1.0.csv",header=T)
careunits = read.csv("mimic4_v1/careunits_1.0.csv",header=T)
cohort = merge(cohort,careunits)
admissions = read.csv("mimic4_v1/admissions.csv",header=T)
cohort = merge(cohort,admissions)
cohort$death_hour = as.numeric(difftime(strptime(as.character(cohort$deathtime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(cohort$intime), "%Y-%m-%d %H:%M:%S"),units="hours"))
hr_data = merge(hr_data,cohort[,c("stay_id","death_hour")])
early_deaths = unique(hr_data$stay_id[hr_data$study_start==1 & hr_data$death_hour <= (hr_data$hour+24)])
early_deaths = early_deaths[!is.na(early_deaths)]
hr_data$vent24hr[hr_data$stay_id%in%early_deaths]=1
# L = names(hr_data)[c(2,76:98,132:150,172:177,185:197,212:215,218:248)]
# X24 = model.matrix(as.formula(paste('vent24hr~',paste(L,collapse = '+'),sep='')),model.frame(~.,hr_data[hr_data$study_hour==1,],na.action=na.pass))
# library("xgboost")
# A_mod_cv = xgb.cv(verbose=0,data = X24, label = hr_data$vent24hr[hr_data$study_hour==1], max_depth = 2, eta=.3, nrounds = 500,objective = "binary:logistic",metrics = 'rmse',missing=NA,nfold=10,early_stopping_rounds = 10,prediction = T)
# niters = A_mod_cv$best_iteration
# A_mod = xgboost(verbose=0,data = X24, label = hr_data$vent24hr[hr_data$study_hour==1], max_depth = 2, eta=.3, nrounds = niters,objective = "binary:logistic",metrics = 'rmse',missing=NA)
# imp = xgb.importance(A_mod,feature_names = colnames(X24))
# phat_xgb = predict(A_mod,newdata = X24,missing=NA)
# cal_mod = isoreg(y=hr_data$vent24hr[hr_data$study_hour==1],x=phat_xgb)
# yf = cal_mod$yf
# yf[order(phat_xgb)] = cal_mod$yf
# phat_xgb_cal = yf
# # calibrate.plot(hr_data$vent24hr[hr_data$study_hour==1],phat_xgb_cal)
# prob24hour = cbind(prob24hr=phat_xgb,stay_id = hr_data$stay_id[hr_data$study_hour==1])
# prob24hour_cal = cbind(prob24hr_cal=phat_xgb_cal,stay_id = hr_data$stay_id[hr_data$study_hour==1])
# 
# hr_data = merge(hr_data,prob24hour)
# hr_data = merge(hr_data,prob24hour_cal)

cam = read.csv('mimic4_v1/CAM.csv')
cam = cam[cam$stay_id %in% unique(hr_data$stay_id),]
cam = merge(cam,item_ids)
cam = merge(cam,cohort[,c("stay_id","intime")])
cam$hour = ceiling(as.numeric(difftime(strptime(as.character(cam$charttime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(cam$intime), "%Y-%m-%d %H:%M:%S"),units="hours")))
names(cam)[7] = 'cam'

cam_yes = cam[grep('yes',tolower(cam$cam)),]
cam_yes$cam = 1
names(cam_yes)[7] = 'cam_yes'
cam_yes = unique(cam_yes[,c("stay_id","hour","cam_yes")])
hr_data = merge(hr_data,cam_yes,all.x=T)

cam_no = cam[grep('no',tolower(cam$cam)),]
cam_no$cam = 1
names(cam_no)[7] = 'cam_no'
cam_no = unique(cam_no[,c("stay_id","hour","cam_no")])
hr_data = merge(hr_data,cam_no,all.x=T)

cam_unable = cam[grep('unable',tolower(cam$cam)),]
cam_unable$cam = 1
names(cam_unable)[7] = 'cam_unable'
cam_unable = unique(cam_unable[,c("stay_id","hour","cam_unable")])
hr_data = merge(hr_data,cam_unable,all.x=T)

cam_dementia = cam[grep('dementia',tolower(cam$cam)),]
cam_dementia$cam = 1
names(cam_dementia)[7] = 'cam_dementia'
cam_dementia = unique(cam_dementia[,c("stay_id","cam_dementia")])
hr_data = merge(hr_data,cam_dementia,all.x=T)

cam_language = cam[grep('language',tolower(cam$cam)),]
cam_language$language = 1
names(cam_language)[7] = 'cam_language'
cam_language = unique(cam_language[,c("stay_id","hour","cam_language")])
hr_data = merge(hr_data,cam_language,all.x=T)

demented = unique(hr_data$stay_id[hr_data$cam_dementia==1])
demented = demented[!is.na(demented)]
hr_data = hr_data[!hr_data$stay_id %in% demented,]
hr_data = hr_data[order(hr_data$stay_id,hr_data$hour),]

hr_data$cam = ifelse(!is.na(hr_data$cam_yes),"yes",ifelse(!is.na(hr_data$cam_no),'no',ifelse(!is.na(hr_data$cam_unable),'unable',NA)))
hr_data$cam_meas = !is.na(hr_data$cam)
hr_data$cam = unlist(lapply(split(hr_data$cam,hr_data$stay_id),carry_forward))
hr_data$cam = as.character(hr_data$cam)
hr_data$cam[is.na(hr_data$cam)]='NA'
hr_data$cam = as.factor(hr_data$cam)
mean(hr_data$cam[hr_data$study_hour==48]=='NA')

rass = read.csv("mimic4_v1/rass_1.0.csv")
rass = merge(rass,cohort[,c("stay_id","intime")])
rass$hour = ceiling(as.numeric(difftime(strptime(as.character(rass$charttime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(rass$intime), "%Y-%m-%d %H:%M:%S"),units="hours")))
library(tidyverse)
rass_max = rass[,c("stay_id","hour",'rass')] %>%
  group_by(stay_id,hour) %>%
  summarise_all(max,na.rm=T)
names(rass_max)[3] = 'rass_max'

rass_min = rass[,c("stay_id","hour",'rass')] %>%
  group_by(stay_id,hour) %>%
  summarise_all(min,na.rm=T)
names(rass_min)[3] = 'rass_min'

rass_mean = rass[,c("stay_id","hour",'rass')] %>%
  group_by(stay_id,hour) %>%
  summarise_all(mean,na.rm=T)
names(rass_mean)[3] = 'rass_mean'

hr_data = merge(hr_data,rass_max,all.x=T)
hr_data = merge(hr_data,rass_min,all.x=T)
hr_data = merge(hr_data,rass_mean,all.x=T)
hr_data$rass_meas = !is.na(hr_data$rass_max)

hr_data = hr_data[order(hr_data$stay_id,hr_data$hour),]
hr_data$rass_max = unlist(lapply(split(hr_data$rass_max,hr_data$stay_id),carry_forward))
hr_data$rass_min = unlist(lapply(split(hr_data$rass_min,hr_data$stay_id),carry_forward))
hr_data$rass_mean = unlist(lapply(split(hr_data$rass_mean,hr_data$stay_id),carry_forward))

hr_data$last_rass_max=unlist(lapply(split(hr_data$rass_max,hr_data$stay_id),get_last))
hr_data$last_rass_min=unlist(lapply(split(hr_data$rass_min,hr_data$stay_id),get_last))
hr_data$last_rass_mean=unlist(lapply(split(hr_data$rass_mean,hr_data$stay_id),get_last))
hr_data$last_rass_meas=unlist(lapply(split(hr_data$rass_meas,hr_data$stay_id),get_last))

#add riker
riker = read.csv("mimic4_v1/riker_sas.csv")
riker = merge(riker,cohort[,c("stay_id","intime")])
names(riker)[8] = 'riker_score'
riker$hour = ceiling(as.numeric(difftime(strptime(as.character(riker$charttime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(riker$intime), "%Y-%m-%d %H:%M:%S"),units="hours")))

riker_max = riker[,c("stay_id","hour",'riker_score')] %>%
  group_by(stay_id,hour) %>%
  summarise_all(max,na.rm=T)
names(riker_max)[3] = 'riker_max'

riker_min = riker[,c("stay_id","hour",'riker_score')] %>%
  group_by(stay_id,hour) %>%
  summarise_all(min,na.rm=T)
names(riker_min)[3] = 'riker_min'

riker_mean = riker[,c("stay_id","hour",'riker_score')] %>%
  group_by(stay_id,hour) %>%
  summarise_all(mean,na.rm=T)
names(riker_mean)[3] = 'riker_mean'

hr_data = merge(hr_data,riker_max,all.x=T)
hr_data = merge(hr_data,riker_min,all.x=T)
hr_data = merge(hr_data,riker_mean,all.x=T)
hr_data$riker_meas = !is.na(hr_data$riker_max)

hr_data$riker_max = unlist(lapply(split(hr_data$riker_max,hr_data$stay_id),carry_forward))
hr_data$riker_min = unlist(lapply(split(hr_data$riker_min,hr_data$stay_id),carry_forward))
hr_data$riker_mean = unlist(lapply(split(hr_data$riker_mean,hr_data$stay_id),carry_forward))

hr_data$last_riker_max=unlist(lapply(split(hr_data$riker_max,hr_data$stay_id),get_last))
hr_data$last_riker_min=unlist(lapply(split(hr_data$riker_min,hr_data$stay_id),get_last))
hr_data$last_riker_mean=unlist(lapply(split(hr_data$riker_mean,hr_data$stay_id),get_last))
hr_data$last_riker_meas=unlist(lapply(split(hr_data$riker_meas,hr_data$stay_id),get_last))

# table(hr_data$cam[hr_data$study_hour==48])
# table(hr_data$cam[hr_data$study_hour==48],hr_data$rass_min[hr_data$study_hour==48])
# table(hr_data$cam[hr_data$study_hour==48],hr_data$rass_max[hr_data$study_hour==48])
# 
# table(hr_data$rass_min[hr_data$study_hour==72])
# mean(is.na(hr_data$rass_min[hr_data$study_hour==72 & hr_data$vent24hr==1]))
# mean(is.na(hr_data$rass_min[hr_data$study_hour==72]))



# missing_stay_ids = unique(hr_data$stay_id[hr_data$study_hour==48 & hr_data$cam=='NA'])
# hr_data$missing_cam = hr_data$stay_id %in% missing_stay_ids
# 
# Xmiss = model.matrix(as.formula(paste('missing_cam~',paste(L,collapse = '+'),sep='')),model.frame(~.,hr_data[hr_data$study_hour==1,],na.action=na.pass))
# miss_mod_cv = xgb.cv(verbose=0,data = Xmiss, label = hr_data$missing_cam[hr_data$study_hour==1], max_depth = 2, eta=.3, nrounds = 500,objective = "binary:logistic",metrics = 'rmse',missing=NA,nfold=10,early_stopping_rounds = 10,prediction = T)
# niters = miss_mod_cv$best_iteration
# miss_mod = xgboost(verbose=0,data = Xmiss, label = hr_data$missing_cam[hr_data$study_hour==1], max_depth = 2, eta=.3, nrounds = niters,objective = "binary:logistic",metrics = 'rmse',missing=NA)
# imp = xgb.importance(miss_mod,feature_names = colnames(X24))
# mhat_xgb = predict(miss_mod,newdata = Xmiss,missing=NA)
# cal_mod = isoreg(y=hr_data$vent24hr[hr_data$study_hour==1],x=phat_xgb)
# yf = cal_mod$yf
# yf[order(phat_xgb)] = cal_mod$yf
# phat_xgb_cal = yf
# # calibrate.plot(hr_data$vent24hr[hr_data$study_hour==1],phat_xgb_cal)
# prob24hour = cbind(prob24hr=phat_xgb,stay_id = hr_data$stay_id[hr_data$study_hour==1])
# prob24hour_cal = cbind(prob24hr_cal=phat_xgb_cal,stay_id = hr_data$stay_id[hr_data$study_hour==1])
# # 
# hr_data = merge(hr_data,prob24hour)
# hr_data = merge(hr_data,prob24hour_cal)

cpot = read.csv('mimic4_v1/cpot.csv')
cpot = cpot[cpot$stay_id %in% unique(hr_data$stay_id),]
cpot = merge(cpot,item_ids)
cpot = merge(cpot,cohort[,c("stay_id","intime")])
cpot$hour = ceiling(as.numeric(difftime(strptime(as.character(cpot$charttime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(cpot$intime), "%Y-%m-%d %H:%M:%S"),units="hours")))
cpot_facial = cpot[grep('Facial',cpot$label),]
names(cpot_facial)[8] = 'cpot_facial'
cpot_facial = cpot_facial[,c("stay_id","cpot_facial","hour")]
cpot_facial = cpot_facial %>%
  group_by(stay_id,hour) %>%
  summarise_all(max,na.rm=T)
cpot_body = cpot[grep('Body',cpot$label),]
names(cpot_body)[8] = 'cpot_body'
cpot_body = cpot_body[,c("stay_id","cpot_body","hour")]
cpot_body = cpot_body %>%
  group_by(stay_id,hour) %>%
  summarise_all(max,na.rm=T)
cpot_muscle = cpot[grep('Muscle',cpot$label),]
names(cpot_muscle)[8] = 'cpot_muscle'
cpot_muscle = cpot_muscle[,c("stay_id","cpot_muscle","hour")]
cpot_muscle = cpot_muscle %>%
  group_by(stay_id,hour) %>%
  summarise_all(max,na.rm=T)
cpot_vocal = cpot[grep('Vocal',cpot$label),]
names(cpot_vocal)[8] = 'cpot_vocal'
cpot_vocal = cpot_vocal[,c("stay_id","cpot_vocal","hour")]
cpot_vocal = cpot_vocal %>%
  group_by(stay_id,hour) %>%
  summarise_all(max,na.rm=T)

hr_data = merge(hr_data,cpot_facial,all.x=T)
hr_data = merge(hr_data,cpot_body,all.x=T)
hr_data = merge(hr_data,cpot_muscle,all.x=T)
hr_data = merge(hr_data,cpot_vocal,all.x=T)

hr_data$cpot_facial = unlist(sapply(split(hr_data$cpot_facial,hr_data$stay_id),carry_forward))
hr_data$cpot_body = unlist(sapply(split(hr_data$cpot_body,hr_data$stay_id),carry_forward))
hr_data$cpot_muscle = unlist(sapply(split(hr_data$cpot_muscle,hr_data$stay_id),carry_forward))
hr_data$cpot_vocal = unlist(sapply(split(hr_data$cpot_vocal,hr_data$stay_id),carry_forward))

hr_data$last_cpot_facial = unlist(sapply(split(hr_data$cpot_facial,hr_data$stay_id),get_last))
hr_data$last_cpot_body = unlist(sapply(split(hr_data$cpot_body,hr_data$stay_id),get_last))
hr_data$last_cpot_muscle = unlist(sapply(split(hr_data$cpot_muscle,hr_data$stay_id),get_last))
hr_data$last_cpot_vocal = unlist(sapply(split(hr_data$cpot_vocal,hr_data$stay_id),get_last))

# hr_data = merge(hr_data,cam[,c("stay_id","hour","cpot")])


# arms = hr_data[hr_data$study_start,c("stay_id","opioid","propofol","dex","ket","benzo")]
# sum(arms$opioid & !arms$benzo & !arms$propofol & !arms$dex)
# sum(arms$opioid & arms$benzo & !arms$propofol & !arms$dex)
# sum(arms$opioid & !arms$benzo & arms$propofol & !arms$dex)
# sum(arms$opioid & arms$benzo & arms$propofol & !arms$dex)
# sum(!arms$opioid & !arms$benzo & arms$propofol & !arms$dex)
# sum(!arms$opioid & arms$benzo & !arms$propofol & !arms$dex)
# sum(arms$opioid & arms$benzo & arms$propofol)
# sum(!arms$opioid & arms$benzo & !arms$propofol & !arms$dex)
# sum(!arms$opioid & !arms$benzo & !arms$propofol & arms$dex)
# sum(!arms$opioid & !arms$benzo & !arms$propofol & arms$dex)
# sum(arms$dex)
# sum(arms$dex & arms$benzo)
# length(unique(hr_data$stay_id[hr_data$dex & hr_data$benzo]))

hr_data$study_end = ifelse(is.na(hr_data$ventilation_status),hr_data$study_started,hr_data$study_started & hr_data$ventilation_status != 2) 
hr_data$study_ended = unlist(sapply(split(hr_data$study_end,hr_data$stay_id),function(x)cumsum(x)>0))


#ITT
#Arms: 
####Propofol only
# prop_only_pats = unique(arms$stay_id[!arms$opioid & !arms$benzo & arms$propofol & !arms$dex & !arms$ket])
# hr_data$prop_only = hr_data$stay_id %in% prop_only_pats
# mean(is.na(hr_data$rass_min[hr_data$study_hour==48 & hr_data$prop_only]) & hr_data$cam[hr_data$study_hour==48 & hr_data$prop_only]=='NA' & is.na(hr_data$riker_min[hr_data$study_hour==48 & hr_data$prop_only]))
# mean(hr_data$cam[hr_data$study_hour==48 & hr_data$prop_only]=='NA')
# mean(is.na(hr_data$rass_min[hr_data$study_hour==48 & hr_data$prop_only]))
# mean(is.na(hr_data$riker_min[hr_data$study_hour==48 & hr_data$prop_only]))
# 
# missing_prop_only_pats = hr_data$stay_id[hr_data$study_hour==48 & is.na(hr_data$rass_min)]
# rass_meas_times = aggregate(hr_data$rass_meas,by=list(hr_data$stay_id),function(x) which(x==1)) 
# names(rass_meas_times) = c('stay_id','rass_meas_times')
# obs_times = aggregate(hr_data$study_hour,by=list(hr_data$stay_id),max) 
# names(obs_times) = c('stay_id','followup')
# rass_meas_times = merge(rass_meas_times,obs_times)
# 
# ####Opioid only
# opioid_only_pats = unique(arms$stay_id[arms$opioid & !arms$benzo & !arms$propofol & !arms$dex & !arms$ket])
# hr_data$opioid_only = hr_data$stay_id %in% opioid_only_pats
# mean(is.na(hr_data$rass_min[hr_data$study_hour==48 & hr_data$opioid_only]) & hr_data$cam[hr_data$study_hour==48 & hr_data$opioid_only]=='NA' & is.na(hr_data$riker_min[hr_data$study_hour==48 & hr_data$opioid_only]))
# mean(hr_data$cam[hr_data$study_hour==48 & hr_data$opioid_only]=='NA')
# mean(is.na(hr_data$rass_min[hr_data$study_hour==48 & hr_data$opioid_only]))
# 
# ####Opioid + Propofol
# prop_opioid_pats = unique(arms$stay_id[arms$opioid & !arms$benzo & arms$propofol & !arms$dex & !arms$ket])
# hr_data$prop_opioid = hr_data$stay_id %in% prop_opioid_pats
# mean(is.na(hr_data$rass_min[hr_data$study_hour==48 & hr_data$prop_opioid]) & is.na(hr_data$riker_min[hr_data$study_hour==48 & hr_data$prop_opioid]))
# mean(hr_data$cam[hr_data$study_hour==48 & hr_data$prop_opioid]=='NA')
# mean(is.na(hr_data$rass_min[hr_data$study_hour==48 & hr_data$prop_opioid]))
# 
# ####Opioid + Benzo
# benzo_opioid_pats = unique(arms$stay_id[arms$opioid & arms$benzo & !arms$propofol & !arms$dex & !arms$ket])
# hr_data$benzo_opioid = hr_data$stay_id %in% benzo_opioid_pats
# mean(is.na(hr_data$rass_min[hr_data$study_hour==48 & hr_data$benzo_opioid]) & hr_data$cam[hr_data$study_hour==48 & hr_data$benzo_opioid]=='NA' & is.na(hr_data$riker_min[hr_data$study_hour==48 & hr_data$benzo_opioid]))
# mean(hr_data$cam[hr_data$study_hour==48 & hr_data$benzo_opioid]=='NA')
# mean(is.na(hr_data$rass_min[hr_data$study_hour==48 & hr_data$benzo_opioid]))
# 
# ####Opioid + Benzo
# prop_benzo_opioid_pats = unique(arms$stay_id[arms$opioid & arms$benzo & arms$propofol & !arms$dex & !arms$ket])
# hr_data$prop_benzo_opioid = hr_data$stay_id %in% prop_benzo_opioid_pats
# 
# ####Opioid + Benzo
# benzo_pats = unique(arms$stay_id[!arms$opioid & arms$benzo & !arms$propofol & !arms$dex & !arms$ket])
# hr_data$benzo_only = hr_data$stay_id %in% benzo_pats
# 
# #Outcomes at time t
# ####Dead, not ventilated and alive, ventilated and most recent RASS
# hr_data$coma_meas = pmax(hr_data$riker_meas,hr_data$rass_meas)
# hr_data$coma = ifelse(hr_data$coma_meas,ifelse(hr_data$riker_meas,hr_data$riker_min<=2,ifelse(hr_data$rass_meas,hr_data$rass_min<=-4,NA)),NA)
# hr_data$coma = unlist(lapply(split(hr_data$coma,hr_data$stay_id),carry_forward))
# prop_coma_prevs = aggregate(hr_data$coma[hr_data$prop_only],by=list(hr_data$study_hour[hr_data$prop_only]),mean,na.rm=T)
# opioid_coma_prevs = aggregate(hr_data$coma[hr_data$opioid_only],by=list(hr_data$study_hour[hr_data$opioid_only]),mean,na.rm=T)
# prop_opioid_coma_prevs = aggregate(hr_data$coma[hr_data$prop_opioid],by=list(hr_data$study_hour[hr_data$prop_opioid]),mean,na.rm=T)
# benzo_opioid_coma_prevs = aggregate(hr_data$coma[hr_data$benzo_opioid],by=list(hr_data$study_hour[hr_data$benzo_opioid]),mean,na.rm=T)
# 
# death_cohort = cohort[cohort$deathtime!='',]
# death_cohort = merge(death_cohort,hr_data[hr_data$study_hour==1,c("stay_id","hour")])
# death_cohort$study_hour = pmax(1,ceiling(death_cohort$death_hour) - death_cohort$hour)
# 
# stay_ids = unique(hr_data$stay_id[hr_data$prop_only | hr_data$opioid_only | hr_data$prop_opioid | hr_data$benzo_opioid | hr_data$prop_benzo_opioid | hr_data$benzo_only])
# outcome_table = data.frame(stay_id = rep(stay_ids,each = 720), study_hour = rep(1:720,length(stay_ids)))
# outcome_table = merge(outcome_table,hr_data[hr_data$coma_meas==T,c("stay_id","study_hour","coma")],all.x=T)
# outcome_table$coma_recent = unlist(lapply(split(outcome_table$coma,outcome_table$stay_id),carry_forward))
# 
# 
# death_cohort$death = 1
# outcome_table = merge(outcome_table,death_cohort[,c("stay_id","study_hour","death")],all.x=T)
# outcome_table = merge(outcome_table,hr_data[,c('stay_id','study_hour','ventilation_status')],all.x=T)
# outcome_table$day = ceiling(outcome_table$study_hour/24)
# outcome_table$death = unlist(lapply(split(outcome_table$death,outcome_table$stay_id),carry_forward))
# outcome_table$death[is.na(outcome_table$death)] = 0
# 
# out_cohort = cohort
# out_cohort$out_hour = as.numeric(difftime(strptime(as.character(cohort$outtime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(cohort$intime), "%Y-%m-%d %H:%M:%S"),units="hours"))
# out_cohort = merge(out_cohort,hr_data[hr_data$study_hour==1,c("stay_id","hour")])
# out_cohort$study_hour = pmax(1,ceiling(out_cohort$out_hour) - out_cohort$hour)
# out_cohort$release = 1
# outcome_table = merge(outcome_table,out_cohort[,c("stay_id","study_hour","release")],all.x=T)
# outcome_table$release = unlist(lapply(split(outcome_table$release,outcome_table$stay_id),carry_forward))
# outcome_table$release[is.na(outcome_table$release)] = 0
# 
# outcome_day = outcome_table[,c("stay_id","day","coma","death","release")] %>%
#   group_by(stay_id,day) %>%
#   summarise_all(max,na.rm=T)
# 
# outcome_day$coma_free_day = ifelse(outcome_day$death==1 | outcome_day$coma==1,0,1)
# hist(aggregate(outcome_day$coma_free_day,by=list(outcome_day$stay_id),sum)[,2])
# table(aggregate(outcome_day$coma_free_day,by=list(outcome_day$stay_id),sum)[,2])
# 
# outcome_day = merge(outcome_day,hr_data[hr_data$study_hour==1,c("stay_id","prop_only")],all.x=T)
# outcome_day = merge(outcome_day,hr_data[hr_data$study_hour==1,c("stay_id","opioid_only")],all.x=T)
# outcome_day = merge(outcome_day,hr_data[hr_data$study_hour==1,c("stay_id","prop_opioid")],all.x=T)
# outcome_day = merge(outcome_day,hr_data[hr_data$study_hour==1,c("stay_id","benzo_opioid")],all.x=T)
# outcome_day = merge(outcome_day,hr_data[hr_data$study_hour==1,c("stay_id","prop_benzo_opioid")],all.x=T)
# outcome_day = merge(outcome_day,hr_data[hr_data$study_hour==1,c("stay_id","benzo_only")],all.x=T)
# 
# mean(outcome_day$coma_free_day[outcome_day$prop_only==1])
# mean(outcome_day$coma_free_day[outcome_day$opioid_only==1])
# mean(outcome_day$coma_free_day[outcome_day$prop_opioid==1])
# mean(outcome_day$coma_free_day[outcome_day$benzo_opioid==1])
# 
# #fit propensity models
# #add earliest pf_ratio
# library(tidyverse)
# hr_data$first_pfratio = hr_data$pao2fio2ratio
# hr_data = hr_data %>% 
#   group_by(stay_id) %>%
#   fill(first_pfratio,.direction = "up")
# hr_data = data.frame(hr_data)
# hr_data$first_pfratio = ifelse(is.na(hr_data$first_pfratio),0,hr_data$first_pfratio<=100)
# baseline_data = hr_data[hr_data$study_hour==1 & (hr_data$prop_only | hr_data$opioid_only | hr_data$prop_opioid | hr_data$benzo_opioid | hr_data$benzo_only | hr_data$prop_benzo_opioid),]
# 
# L0 = c('heart_rate','sbp','dbp',
#        'mbp','resp_rate','spo2','sofa_24hours','rate_std','amount','weight','elixhauser_score',
#        'age','gender','admission_type','admission_location','insurance','marital_status',
#        'ethnicity','language','first_careunit',"CONGESTIVE_HEART_FAILURE",
#        "VALVULAR_DISEASE","PERIPHERAL_VASCULAR","HYPERTENSION","PARALYSIS",
#        "CHRONIC_PULMONARY","LIVER_DISEASE","OBESITY","ALCOHOL_ABUSE","DRUG_ABUSE","DEPRESSION","first_pfratio")
# 
# apply(baseline_data[,L0],2,function(x)mean(is.na(x)))
# baseline_data$gender = as.numeric(baseline_data$gender)
# baseline_data$first_pfratio = as.numeric(baseline_data$first_pfratio)
# 
# for(col in L0){
#   if(is.numeric(baseline_data[,col])){
#     baseline_data[is.na(baseline_data[,col]),col] = median(baseline_data[,col],na.rm=T)
#   }
# }
# 
# prop_model = glm(prop_only~.,data=baseline_data[,c(L0,'prop_only')],family='binomial')
# opioid_model = glm(opioid_only~.,data=baseline_data[,c(L0,'opioid_only')],family='binomial')
# prop_opioid_model = glm(prop_opioid~.,data=baseline_data[,c(L0,'prop_opioid')],family='binomial')
# benzo_opioid_model = glm(benzo_opioid~.,data=baseline_data[,c(L0,'benzo_opioid')],family='binomial')
# prop_benzo_opioid_model = glm(prop_benzo_opioid~.,data=baseline_data[,c(L0,'prop_benzo_opioid')],family='binomial')
# benzo_model = glm(benzo_only~.,data=baseline_data[,c(L0,'benzo_only')],family='binomial')
# 
# baseline_data$prop_pscore = predict(prop_model,baseline_data,type='response')
# baseline_data$opioid_pscore = predict(opioid_model,baseline_data,type='response')
# baseline_data$prop_opioid_pscore = predict(prop_opioid_model,baseline_data,type='response')
# baseline_data$benzo_opioid_pscore = predict(benzo_opioid_model,baseline_data,type='response')
# baseline_data$prop_benzo_opioid_pscore = predict(prop_benzo_opioid_model,baseline_data,type='response')
# baseline_data$benzo_pscore = predict(benzo_model,baseline_data,type='response')
# 
# baseline_data$prop_weight = 1/baseline_data$prop_pscore
# baseline_data$opioid_weight = 1/baseline_data$opioid_pscore
# baseline_data$prop_opioid_weight = 1/baseline_data$prop_opioid_pscore
# baseline_data$benzo_opioid_weight = 1/baseline_data$benzo_opioid_pscore
# baseline_data$prop_benzo_opioid_weight = 1/baseline_data$prop_benzo_opioid_pscore
# baseline_data$benzo_weight = 1/baseline_data$benzo_pscore
# 
# baseline_data$prop_weight_stabilized = mean(baseline_data$prop_only)/baseline_data$prop_pscore
# baseline_data$opioid_weight_stabilized = mean(baseline_data$opioid_only)/baseline_data$opioid_pscore
# baseline_data$prop_opioid_weight_stabilized = mean(baseline_data$prop_opioid)/baseline_data$prop_opioid_pscore
# baseline_data$benzo_opioid_weight_stabilized = mean(baseline_data$benzo_opioid)/baseline_data$benzo_opioid_pscore
# baseline_data$prop_benzo_opioid_weight_stabilized = mean(baseline_data$prop_benzo_opioid)/baseline_data$prop_benzo_opioid_pscore
# baseline_data$benzo_weight_stabilized = mean(baseline_data$benzo_only)/baseline_data$benzo_pscore
# 
# baseline_data$prop_weight_trimmed = pmin(baseline_data$prop_weight_stabilized,50)
# baseline_data$opioid_weight_trimmed = pmin(baseline_data$opioid_weight_stabilized,50)
# baseline_data$prop_opioid_weight_trimmed = pmin(baseline_data$prop_opioid_weight_stabilized,50)
# baseline_data$benzo_opioid_weight_trimmed = pmin(baseline_data$benzo_opioid_weight_stabilized,50)
# baseline_data$prop_benzo_opioid_weight_trimmed = pmin(baseline_data$prop_benzo_opioid_weight_stabilized,50)
# baseline_data$benzo_weight_trimmed = pmin(baseline_data$benzo_weight_stabilized,50)
# 
# itt_cfd_table = aggregate(outcome_day$coma_free_day,by=list(outcome_day$stay_id),sum)
# names(itt_cfd_table) = c('stay_id','coma_free_days')
# itt_cfd_table = merge(itt_cfd_table,baseline_data[,c('stay_id',"prop_only","prop_opioid","opioid_only","benzo_opioid","prop_benzo_opioid","benzo_only", "prop_pscore","opioid_pscore","prop_opioid_pscore","benzo_opioid_pscore","prop_benzo_opioid_pscore","benzo_pscore","prop_weight_trimmed","opioid_weight_trimmed","prop_opioid_weight_trimmed","benzo_opioid_weight_trimmed","prop_benzo_opioid_weight_trimmed","benzo_weight_trimmed")])
# 
# mean(itt_cfd_table$coma_free_days[itt_cfd_table$prop_only])
# prop_est = (1/sum(itt_cfd_table$prop_only*itt_cfd_table$prop_weight_trimmed))*sum(itt_cfd_table$coma_free_days*itt_cfd_table$prop_only*itt_cfd_table$prop_weight_trimmed)
# hist(itt_cfd_table$prop_weight_trimmed[itt_cfd_table$prop_only])
# 
# mean(itt_cfd_table$coma_free_days[itt_cfd_table$opioid_only])
# opioid_est = (1/sum(itt_cfd_table$opioid_only*itt_cfd_table$opioid_weight_trimmed))*sum(itt_cfd_table$coma_free_days*itt_cfd_table$opioid_only*itt_cfd_table$opioid_weight_trimmed)
# hist(itt_cfd_table$opioid_weight_trimmed[itt_cfd_table$opioid_only])
# 
# mean(itt_cfd_table$coma_free_days[itt_cfd_table$prop_opioid])
# prop_opioid_est = 1/sum(itt_cfd_table$prop_opioid*itt_cfd_table$prop_opioid_weight_trimmed)*sum(itt_cfd_table$coma_free_days*itt_cfd_table$prop_opioid*itt_cfd_table$prop_opioid_weight_trimmed)
# hist(itt_cfd_table$prop_opioid_weight_trimmed[itt_cfd_table$prop_opioid])
# 
# mean(itt_cfd_table$coma_free_days[itt_cfd_table$benzo_opioid])
# benzo_opioid_est = 1/sum(itt_cfd_table$benzo_opioid*itt_cfd_table$benzo_opioid_weight_trimmed)*sum(itt_cfd_table$coma_free_days*itt_cfd_table$benzo_opioid*itt_cfd_table$benzo_opioid_weight_trimmed)
# hist(itt_cfd_table$benzo_opioid_weight_trimmed[itt_cfd_table$benzo_opioid])
# 
# mean(itt_cfd_table$coma_free_days[itt_cfd_table$prop_benzo_opioid])
# prop_benzo_opioid_est = 1/sum(itt_cfd_table$prop_benzo_opioid*itt_cfd_table$prop_benzo_opioid_weight_trimmed)*sum(itt_cfd_table$coma_free_days*itt_cfd_table$prop_benzo_opioid*itt_cfd_table$prop_benzo_opioid_weight_trimmed)
# hist(itt_cfd_table$prop_benzo_opioid_weight_trimmed[itt_cfd_table$prop_benzo_opioid])
# 
# mean(itt_cfd_table$coma_free_days[itt_cfd_table$benzo_only])
# benzo_est = 1/sum(itt_cfd_table$benzo_only*itt_cfd_table$benzo_weight_trimmed)*sum(itt_cfd_table$coma_free_days*itt_cfd_table$benzo_only*itt_cfd_table$benzo_weight_trimmed)
# hist(itt_cfd_table$benzo_weight_trimmed[itt_cfd_table$benzo_only])
# 
# nboot=100
# prop_boot = rep(NA,nboot)
# opioid_boot = rep(NA,nboot)
# prop_opioid_boot = rep(NA,nboot)
# benzo_opioid_boot = rep(NA,nboot)
# prop_benzo_opioid_boot = rep(NA,nboot)
# benzo_boot = rep(NA,nboot)
# 
# 
# for(b in 1:nboot){
#   set.seed(b)
#   pats = sample(1:nrow(baseline_data),nrow(baseline_data),replace=T)  
#   baseline_data_boot = baseline_data[pats,]
#   
#   prop_model_boot = glm(prop_only~.,data=baseline_data_boot[,c(L0,'prop_only')],family='binomial')
#   opioid_model_boot = glm(opioid_only~.,data=baseline_data_boot[,c(L0,'opioid_only')],family='binomial')
#   prop_opioid_model_boot = glm(prop_opioid~.,data=baseline_data_boot[,c(L0,'prop_opioid')],family='binomial')
#   benzo_opioid_model_boot = glm(benzo_opioid~.,data=baseline_data_boot[,c(L0,'benzo_opioid')],family='binomial')
#   prop_benzo_opioid_model_boot = glm(prop_benzo_opioid~.,data=baseline_data_boot[,c(L0,'prop_benzo_opioid')],family='binomial')
#   benzo_model_boot = glm(benzo_only~.,data=baseline_data_boot[,c(L0,'benzo_only')],family='binomial')
#   
#   baseline_data_boot$prop_pscore = predict(prop_model_boot,baseline_data_boot,type='response')
#   baseline_data_boot$opioid_pscore = predict(opioid_model_boot,baseline_data_boot,type='response')
#   baseline_data_boot$prop_opioid_pscore = predict(prop_opioid_model_boot,baseline_data_boot,type='response')
#   baseline_data_boot$benzo_opioid_pscore = predict(benzo_opioid_model_boot,baseline_data_boot,type='response')
#   baseline_data_boot$prop_benzo_opioid_pscore = predict(prop_benzo_opioid_model_boot,baseline_data_boot,type='response')
#   baseline_data_boot$benzo_pscore = predict(benzo_model_boot,baseline_data_boot,type='response')
#   
#   baseline_data_boot$prop_weight = 1/baseline_data_boot$prop_pscore
#   baseline_data_boot$opioid_weight = 1/baseline_data_boot$opioid_pscore
#   baseline_data_boot$prop_opioid_weight = 1/baseline_data_boot$prop_opioid_pscore
#   baseline_data_boot$benzo_opioid_weight = 1/baseline_data_boot$benzo_opioid_pscore
#   baseline_data_boot$prop_benzo_opioid_weight = 1/baseline_data_boot$prop_benzo_opioid_pscore
#   baseline_data_boot$benzo_weight = 1/baseline_data_boot$benzo_pscore
#   
#   baseline_data_boot$prop_weight_stabilized = mean(baseline_data_boot$prop_only)/baseline_data_boot$prop_pscore
#   baseline_data_boot$opioid_weight_stabilized = mean(baseline_data_boot$opioid_only)/baseline_data_boot$opioid_pscore
#   baseline_data_boot$prop_opioid_weight_stabilized = mean(baseline_data_boot$prop_opioid)/baseline_data_boot$prop_opioid_pscore
#   baseline_data_boot$benzo_opioid_weight_stabilized = mean(baseline_data_boot$benzo_opioid)/baseline_data_boot$benzo_opioid_pscore
#   baseline_data_boot$prop_benzo_opioid_weight_stabilized = mean(baseline_data_boot$prop_benzo_opioid)/baseline_data_boot$prop_benzo_opioid_pscore
#   baseline_data_boot$benzo_weight_stabilized = mean(baseline_data_boot$benzo_only)/baseline_data_boot$benzo_pscore
#   
#   baseline_data_boot$prop_weight_trimmed = pmin(baseline_data_boot$prop_weight_stabilized,50)
#   baseline_data_boot$opioid_weight_trimmed = pmin(baseline_data_boot$opioid_weight_stabilized,50)
#   baseline_data_boot$prop_opioid_weight_trimmed = pmin(baseline_data_boot$prop_opioid_weight_stabilized,50)
#   baseline_data_boot$benzo_opioid_weight_trimmed = pmin(baseline_data_boot$benzo_opioid_weight_stabilized,50)
#   baseline_data_boot$prop_benzo_opioid_weight_trimmed = pmin(baseline_data_boot$prop_benzo_opioid_weight_stabilized,50)
#   baseline_data_boot$benzo_weight_trimmed = pmin(baseline_data_boot$benzo_weight_stabilized,50)
#   
#   itt_cfd_table_boot = itt_cfd_table[pats,]
#   itt_cfd_table_boot$prop_weight_trimmed = baseline_data_boot$prop_weight_trimmed
#   itt_cfd_table_boot$opioid_weight_trimmed = baseline_data_boot$opioid_weight_trimmed
#   itt_cfd_table_boot$prop_opioid_weight_trimmed = baseline_data_boot$prop_opioid_weight_trimmed
#   itt_cfd_table_boot$benzo_opioid_weight_trimmed = baseline_data_boot$benzo_opioid_weight_trimmed
#   itt_cfd_table_boot$prop_benzo_opioid_weight_trimmed = baseline_data_boot$prop_benzo_opioid_weight_trimmed
#   itt_cfd_table_boot$benzo_weight_trimmed = baseline_data_boot$benzo_weight_trimmed
#   
#   # mean(itt_cfd_table_boot$coma_free_days[itt_cfd_table_boot$prop_only])
#   prop_boot[b] = (1/sum(itt_cfd_table_boot$prop_only*itt_cfd_table_boot$prop_weight_trimmed))*sum(itt_cfd_table_boot$coma_free_days*itt_cfd_table_boot$prop_only*itt_cfd_table_boot$prop_weight_trimmed)
#   # hist(itt_cfd_table$prop_weight_trimmed[itt_cfd_table$prop_only])
#   
#   # mean(itt_cfd_table$coma_free_days[itt_cfd_table$opioid_only])
#   opioid_boot[b] = (1/sum(itt_cfd_table_boot$opioid_only*itt_cfd_table_boot$opioid_weight_trimmed))*sum(itt_cfd_table_boot$coma_free_days*itt_cfd_table_boot$opioid_only*itt_cfd_table_boot$opioid_weight_trimmed)
#   # hist(itt_cfd_table$opioid_weight_trimmed[itt_cfd_table$opioid_only])
#   
#   # mean(itt_cfd_table$coma_free_days[itt_cfd_table$prop_opioid])
#   prop_opioid_boot[b] = 1/sum(itt_cfd_table_boot$prop_opioid*itt_cfd_table_boot$prop_opioid_weight_trimmed)*sum(itt_cfd_table_boot$coma_free_days*itt_cfd_table_boot$prop_opioid*itt_cfd_table_boot$prop_opioid_weight_trimmed)
#   # hist(itt_cfd_table_boot$prop_opioid_weight_trimmed[itt_cfd_table_boot$prop_opioid])
#   
#   # mean(itt_cfd_table_boot$coma_free_days[itt_cfd_table_boot$benzo_opioid])
#   benzo_opioid_boot[b] = 1/sum(itt_cfd_table_boot$benzo_opioid*itt_cfd_table_boot$benzo_opioid_weight_trimmed)*sum(itt_cfd_table_boot$coma_free_days*itt_cfd_table_boot$benzo_opioid*itt_cfd_table_boot$benzo_opioid_weight_trimmed)
#   # hist(itt_cfd_table_boot$benzo_opioid_weight_trimmed[itt_cfd_table_boot$opioid_only])
#   
#   prop_benzo_opioid_boot[b] = 1/sum(itt_cfd_table_boot$prop_benzo_opioid*itt_cfd_table_boot$prop_benzo_opioid_weight_trimmed)*sum(itt_cfd_table_boot$coma_free_days*itt_cfd_table_boot$prop_benzo_opioid*itt_cfd_table_boot$prop_benzo_opioid_weight_trimmed)
#   
#   benzo_boot[b] = (1/sum(itt_cfd_table_boot$benzo_only*itt_cfd_table_boot$benzo_weight_trimmed))*sum(itt_cfd_table_boot$coma_free_days*itt_cfd_table_boot$benzo_only*itt_cfd_table_boot$benzo_weight_trimmed)
#   
# }
# 
# 
# effect_ests = c(prop_est,opioid_est,prop_opioid_est,benzo_opioid_est,prop_benzo_opioid_est,benzo_est)
# sds = c(sd(prop_boot),sd(opioid_boot),sd(prop_opioid_boot),sd(benzo_opioid_boot),sd(prop_benzo_opioid_boot),sd(benzo_boot))
# lowers = effect_ests - 1.96*sds
# uppers = effect_ests + 1.96*sds
# 
# ticks = c(1:6)
# plot(ticks,effect_ests,ylim=c(15,26),xlab='',ylab="Coma Free Days",xaxt="n",main="Effects of Initial Sedative on Coma Free Days",col=c(1:4))
# arrows(ticks,lowers,ticks,uppers,length=0.05,angle=90,code=3)
# axis(1, at = 1:6,
#      labels = c("Propofol", "Opioid", "Propofol + Opioid", "BZD + Opioid","Propofol + BZD + Opioid","BZD"))
# 
# 

#time varying case
hr_data$coma_meas = pmax(hr_data$riker_meas,hr_data$rass_meas)
hr_data$coma = ifelse(hr_data$coma_meas,ifelse(hr_data$riker_meas,hr_data$riker_min<=2,ifelse(hr_data$rass_meas,hr_data$rass_min<=-4,NA)),NA)
hr_data$coma = unlist(lapply(split(hr_data$coma,hr_data$stay_id),carry_forward))
hr_data$last_coma = unlist(sapply(split(hr_data$coma,hr_data$stay_id),get_last))

prop_thresh = 45
hr_data$prop_only_follow = ifelse(hr_data$propofol_rate>=prop_thresh,1,!hr_data$potential_violation)

# hr_data$strike_one = ifelse(!hr_data$opioid & !hr_data$benzo & !hr_data$dex & !hr_data$ket,1,hr_data$propofol_rate>=50 | hr_data$study_ended)
# hr_data$strike_two = unlist(aggregate(hr_data$strike_one,by=list(hr_data$stay_id),function(x) if(length(x)>=2){c(x[2:length(x)],1)}else{1})[,2])
# hr_data$prop_only_follow = ifelse(hr_data$strike_one,1,as.numeric(hr_data$strike_two))
hr_data$prop_only_censored = unlist(sapply(split(hr_data$prop_only_follow,hr_data$stay_id),function(x)cumsum(cumsum(x==0))>1))

hr_data$agitated_meas = pmax(hr_data$riker_meas,hr_data$rass_meas)
hr_data$agitated = ifelse(hr_data$coma_meas,ifelse(hr_data$riker_meas,hr_data$riker_max>=5,ifelse(hr_data$rass_meas,hr_data$rass_max>=4,NA)),NA)
hr_data$agitated = unlist(lapply(split(hr_data$agitated,hr_data$stay_id),carry_forward))
hr_data$last_agitated = unlist(lapply(split(hr_data$agitated,hr_data$stay_id),get_last))
hr_data$last_agitated_meas = unlist(lapply(split(hr_data$agitated_meas,hr_data$stay_id),get_last))

hr_data$coma[is.na(hr_data$coma)]=0
hr_data$last_coma[is.na(hr_data$last_coma)]=0
hr_data$total_coma = unlist(sapply(split(hr_data$coma,hr_data$stay_id),cumsum))
hr_data$mean_coma = hr_data$total_coma/hr_data$study_hour
hr_data$last_total_coma = unlist(sapply(split(hr_data$total_coma,hr_data$stay_id),get_last))
hr_data$last_mean_coma = unlist(sapply(split(hr_data$mean_coma,hr_data$stay_id),get_last))


hr_data$agitated[is.na(hr_data$agitated)]=0
hr_data$last_agitated[is.na(hr_data$last_agitated)]=0
hr_data$total_agitated = unlist(sapply(split(hr_data$agitated,hr_data$stay_id),cumsum))
hr_data$mean_agitated = hr_data$total_agitated/hr_data$study_hour
hr_data$last_total_agitated = unlist(sapply(split(hr_data$total_agitated,hr_data$stay_id),get_last))
hr_data$last_mean_agitated = unlist(sapply(split(hr_data$mean_agitated,hr_data$stay_id),get_last))

hr_data$opioid[is.na(hr_data$opioid)]=0
hr_data$last_opioid = unlist(lapply(split(hr_data$opioid,hr_data$stay_id),get_last))
hr_data$last_opioid[is.na(hr_data$last_opioid)]=0
hr_data$ever_opioid = unlist(sapply(split(hr_data$opioid,hr_data$stay_id),function(x)cumsum(x)>0))
hr_data$last_ever_opioid = unlist(lapply(split(hr_data$ever_opioid,hr_data$stay_id),get_last))
hr_data$total_opioid = unlist(sapply(split(hr_data$opioid,hr_data$stay_id),cumsum))
hr_data$mean_opioid = hr_data$total_opioid/hr_data$study_hour
hr_data$last_total_opioid = unlist(sapply(split(hr_data$total_opioid,hr_data$stay_id),get_last))
hr_data$last_mean_opioid = unlist(sapply(split(hr_data$mean_opioid,hr_data$stay_id),get_last))

hr_data$dex[is.na(hr_data$dex)]=0
hr_data$last_dex = unlist(lapply(split(hr_data$dex,hr_data$stay_id),get_last))
hr_data$last_dex[is.na(hr_data$last_dex)]=0
hr_data$ever_dex = unlist(sapply(split(hr_data$dex,hr_data$stay_id),function(x)cumsum(x)>0))
hr_data$last_ever_dex = unlist(lapply(split(hr_data$ever_dex,hr_data$stay_id),get_last))
hr_data$total_dex = unlist(sapply(split(hr_data$dex,hr_data$stay_id),cumsum))
hr_data$mean_dex = hr_data$total_dex/hr_data$study_hour
hr_data$last_total_dex = unlist(sapply(split(hr_data$total_dex,hr_data$stay_id),get_last))
hr_data$last_mean_dex = unlist(sapply(split(hr_data$mean_dex,hr_data$stay_id),get_last))

hr_data$ket[is.na(hr_data$ket)]=0
hr_data$last_ket = unlist(lapply(split(hr_data$ket,hr_data$stay_id),get_last))
hr_data$last_ket[is.na(hr_data$last_ket)]=0
hr_data$ever_ket = unlist(sapply(split(hr_data$ket,hr_data$stay_id),function(x)cumsum(x)>0))
hr_data$last_ever_ket = unlist(lapply(split(hr_data$ever_ket,hr_data$stay_id),get_last))
hr_data$total_ket = unlist(sapply(split(hr_data$ket,hr_data$stay_id),cumsum))
hr_data$mean_ket = hr_data$total_ket/hr_data$study_hour
hr_data$last_total_ket = unlist(sapply(split(hr_data$total_ket,hr_data$stay_id),get_last))
hr_data$last_mean_ket = unlist(sapply(split(hr_data$mean_ket,hr_data$stay_id),get_last))

hr_data$benzo[is.na(hr_data$benzo)]=0
hr_data$last_benzo = unlist(lapply(split(hr_data$benzo,hr_data$stay_id),get_last))
hr_data$last_benzo[is.na(hr_data$last_benzo)]=0
hr_data$ever_benzo = unlist(sapply(split(hr_data$benzo,hr_data$stay_id),function(x)cumsum(x)>0))
hr_data$last_ever_benzo = unlist(lapply(split(hr_data$ever_benzo,hr_data$stay_id),get_last))
hr_data$total_benzo = unlist(sapply(split(hr_data$benzo,hr_data$stay_id),cumsum))
hr_data$mean_benzo = hr_data$total_benzo/hr_data$study_hour
hr_data$last_total_benzo = unlist(sapply(split(hr_data$total_benzo,hr_data$stay_id),get_last))
hr_data$last_mean_benzo = unlist(sapply(split(hr_data$mean_benzo,hr_data$stay_id),get_last))


hr_data$first_pfratio = hr_data$pao2fio2ratio
hr_data = hr_data %>% 
  group_by(stay_id) %>%
  fill(first_pfratio,.direction = "up")
hr_data$first_pfratio[is.na(hr_data$first_pfratio)] = 300
hr_data$first_pfratio = unlist(sapply(split(hr_data$first_pfratio,hr_data$stay_id),function(x)rep(x[1],length(x))))
hr_data = data.frame(hr_data)

hr_data = hr_data %>% 
  group_by(stay_id) %>%
  fill(weight,.direction = "up")

hr_data = hr_data %>% 
  group_by(stay_id) %>%
  fill(weight,.direction = "down")
hr_data = data.frame(hr_data)

neuroblock = read.csv('mimic4_v1/neuroblock.csv')
neuroblock = merge(neuroblock,cohort[,c("stay_id","intime")])
neuroblock$starthour = as.numeric(difftime(strptime(as.character(neuroblock$starttime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(neuroblock$intime), "%Y-%m-%d %H:%M:%S"),units="hours"))
neuroblock$endhour = as.numeric(difftime(strptime(as.character(neuroblock$endtime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(neuroblock$intime), "%Y-%m-%d %H:%M:%S"),units="hours"))

start_round = ceiling(neuroblock$starthour)
end_round = ceiling(neuroblock$endhour)
neuroblock$neuroblock=1
hr_neuro = unlist(mapply(rep_a_b,neuroblock$neuroblock,start_round,end_round))

neuro_hours = unlist(mapply(from_to,start_round,end_round))
neuro_icustay_id = unlist(mapply(rep_a_b,neuroblock$stay_id,start_round,end_round))
neuro = data.frame(cbind('neuroblock'=hr_neuro,'stay_id'=neuro_icustay_id,'hour'=neuro_hours))
neuro = neuro %>%
  group_by(stay_id,hour) %>%
  summarise_all(max,na.rm=T)

hr_data = merge(hr_data,neuro,all.x=T)
hr_data$neuroblock[is.na(hr_data$neuroblock)] = 0
hr_data = hr_data[order(hr_data$stay_id,hr_data$hour),]
hr_data$last_neuroblock = unlist(lapply(split(hr_data$neuroblock,hr_data$stay_id),get_last))
hr_data$last_neuroblock[is.na(hr_data$last_neuroblock)] = 0

hr_data$study_hour_sq = hr_data$study_hour^2
hr_data$hour_sq = hr_data$hour^2

hr_data$last_total_fluids = unlist(sapply(split(hr_data$last_amount,hr_data$stay_id),cumsum))
hr_data$last_total_fluids = pmin(hr_data$last_total_fluids,25)



V0 = c('elixhauser_score','age','gender','marital_status','admission_location','admission_type',
       'first_careunit','language','ethnicity',
       'CONGESTIVE_HEART_FAILURE','VALVULAR_DISEASE','PERIPHERAL_VASCULAR',
       'HYPERTENSION','PARALYSIS','CHRONIC_PULMONARY',"DIABETES_UNCOMPLICATED","DIABETES_COMPLICATED",'LIVER_DISEASE',
       'OBESITY','ALCOHOL_ABUSE','DRUG_ABUSE','DEPRESSION',"METASTATIC_CANCER","SOLID_TUMOR","RHEUMATOID_ARTHRITIS","COAGULOPATHY",'study_hour','weight')
V = c('elixhauser_score','age','gender','marital_status','admission_location','admission_type',
      'first_careunit','language','ethnicity','insurance',
      'CONGESTIVE_HEART_FAILURE','VALVULAR_DISEASE','PERIPHERAL_VASCULAR',
      'HYPERTENSION','PARALYSIS','CHRONIC_PULMONARY',"DIABETES_UNCOMPLICATED","DIABETES_COMPLICATED",'LIVER_DISEASE',
      'OBESITY','ALCOHOL_ABUSE','DRUG_ABUSE','DEPRESSION',"METASTATIC_CANCER","SOLID_TUMOR","RHEUMATOID_ARTHRITIS","COAGULOPATHY",'study_hour','hour','study_hour_sq','hour_sq','weight')
vitals = c('sofa_24hours','spo2','resp_rate','mbp','dbp','sbp','heart_rate','rate_std','amount','total_fluids', 'liver_24hours','cardiovascular_24hours',
           'cns_24hours','coagulation_24hours','respiration_24hours','renal_24hours')
tv_vars = c('coma','agitated','mean_coma','mean_agitated','opioid','ever_opioid','mean_opioid','ket','ever_ket','mean_ket','dex','ever_dex','mean_dex','benzo','ever_benzo','mean_benzo',
            'amount','rate_std','sofa_24hours','pao2fio2ratio','gcs','total_fluids',
            'spo2','resp_rate','mbp','dbp','sbp','heart_rate','peep_set','tidal_volume_set','fio2_chartevents',
            'cpot_facial','cpot_body','cpot_muscle','cpot_vocal','neuroblock','liver_24hours','cardiovascular_24hours',
            'cns_24hours','coagulation_24hours','respiration_24hours','renal_24hours')

for(var in setdiff(tv_vars,vitals)){
  hr_data[,paste0('last_',var)] = unlist(sapply(split(hr_data[,var],hr_data$stay_id),get_last))
}
L = c(V,paste0('last_',tv_vars))


hr_data$last_coma = as.numeric(hr_data$last_coma)
hr_data$last_agitated = as.numeric(hr_data$last_agitated)
hr_data$last_opioid = as.numeric(hr_data$last_opioid)
hr_data$last_ket = as.numeric(hr_data$last_ket)
hr_data$last_dex = as.numeric(hr_data$last_dex)
hr_data$last_benzo = as.numeric(hr_data$last_benzo)
hr_data$last_ever_opioid = as.numeric(hr_data$last_ever_opioid)
hr_data$last_ever_ket = as.numeric(hr_data$last_ever_ket)
hr_data$last_ever_dex = as.numeric(hr_data$last_ever_dex)
hr_data$last_ever_benzo = as.numeric(hr_data$last_ever_benzo)
hr_data$rate_std = hr_data$rate_std>0
hr_data$last_rate_std = hr_data$last_rate_std>0
hr_data$last_rate_std = as.numeric(hr_data$last_rate_std)
hr_data$ever_vaso = as.numeric(unlist(sapply(split(hr_data$last_rate_std,hr_data$stay_id),function(x)cumsum(x)>0)))
hr_data$tidal_volume_set_min = unlist(sapply(split(hr_data$last_tidal_volume_set,hr_data$stay_id),cummin))
hr_data$peep_set_max = unlist(sapply(split(hr_data$last_peep_set,hr_data$stay_id),cummax))
hr_data$pfratio_min = unlist(sapply(split(hr_data$last_pao2fio2ratio,hr_data$stay_id),cummin))
hr_data$sofa_max = unlist(sapply(split(hr_data$last_sofa_24hours,hr_data$stay_id),cummax))
hr_data$cardiovascular_max = unlist(sapply(split(hr_data$last_cardiovascular_24hours,hr_data$stay_id),cummax))
hr_data$liver_max = unlist(sapply(split(hr_data$last_liver_24hours,hr_data$stay_id),cummax))
hr_data$coagulation_max = unlist(sapply(split(hr_data$last_coagulation_24hours,hr_data$stay_id),cummax))
hr_data$respiration_max = unlist(sapply(split(hr_data$last_respiration_24hours,hr_data$stay_id),cummax))
hr_data$cns_max = unlist(sapply(split(hr_data$last_cns_24hours,hr_data$stay_id),cummax))
hr_data$renal_max = unlist(sapply(split(hr_data$last_renal_24hours,hr_data$stay_id),cummax))
hr_data$gcs_min = unlist(sapply(split(hr_data$last_gcs,hr_data$stay_id),cummin))
sepsis3 = read.csv('mimic4_v1/sepsis3.csv')
sepsis3$sepsis = 1
sepsis3 = merge(sepsis3,careunits)
sepsis3$sepsis_hour = pmax(1,ceiling(as.numeric(difftime(strptime(as.character(sepsis3$suspected_infection_time), "%Y-%m-%d %H:%M:%S"), strptime(as.character(sepsis3$intime), "%Y-%m-%d %H:%M:%S"),units="hours"))))
sepsis3 = merge(sepsis3,hr_data[hr_data$study_hour==1,c("stay_id","hour")])
sepsis3$hour = pmax(sepsis3$sepsis_hour,sepsis3$hour)
hr_data = merge(hr_data,sepsis3[,c("stay_id","sepsis","hour")],all.x=T)
hr_data = hr_data[order(hr_data$stay_id,hr_data$hour),]
hr_data$sepsis = unlist(lapply(split(hr_data$sepsis,hr_data$stay_id),carry_forward))
hr_data$sepsis[is.na(hr_data$sepsis)]=0

hr_data$mbp_min = unlist(sapply(split(hr_data$last_mbp,hr_data$stay_id),cummin))

apsiii = read.csv('mimic4_v1/apsiii')
hr_data = merge(hr_data,apsiii[,c("stay_id","apsiii_prob")],all.x=T)
hr_data = hr_data[order(hr_data$stay_id,hr_data$hour),]

oasis = read.csv('mimic4_v1/oasis')
hr_data = merge(hr_data,oasis[,c("stay_id","oasis_prob")],all.x=T)
hr_data = hr_data[order(hr_data$stay_id,hr_data$hour),]

hr_data$mbp_min = unlist(sapply(split(hr_data$last_mbp,hr_data$stay_id),cummin))

hr_data$amount = hr_data$amount>0
hr_data$last_amount = hr_data$last_amount>0
hr_data$last_amount = as.numeric(hr_data$last_amount)

for(col in L){
  if(is.numeric(hr_data[,col])){
    hr_data[is.na(hr_data[,col]),col] = median(hr_data[,col],na.rm=T)
  }
}
apply(hr_data[,L],2,function(x)mean(is.na(x)))
hr_data$propofol_rate = pmin(hr_data$propofol_rate,250)
hr_data$propofol_rate_sq = hr_data$propofol_rate^2
# hr_data$last_propofol_rate = unlist(sapply(split(hr_data$propofol_rate,hr_data$stay_id),get_last))
# hr_data$last_propofol_rate_sq = hr_data$last_propofol_rate^2
# hr_data$last_propofol_rate[is.na(hr_data$last_propofol_rate)] = hr_data$propofol_rate[is.na(hr_data$last_propofol_rate)]
# hr_data$last_propofol_rate_sq[is.na(hr_data$last_propofol_rate_sq)] = hr_data$propofol_rate_sq[is.na(hr_data$last_propofol_rate_sq)]

hr_data$baseline_propofol_rate = unlist(sapply(split(hr_data$propofol_rate,hr_data$stay_id),function(x)rep(x[1],length(x))))
hr_data$baseline_propofol_rate_sq = hr_data$baseline_propofol_rate^2
hr_data$propofol_start = hr_data$baseline_propofol_rate>0

hr_data$admission_location[hr_data$admission_location=="INTERNAL TRANSFER TO OR FROM PSYCH"]="EMERGENCY ROOM"
hr_data$admission_location[hr_data$admission_location=="AMBULATORY SURGERY TRANSFER"]="EMERGENCY ROOM"
hr_data$admission_type[hr_data$admission_type=="DIRECT OBSERVATION"]="EW EMER."
hr_data$admission_type[hr_data$admission_type=="EU OBSERVATION"]="EW EMER."
hr_data$first_careunit[hr_data$first_careunit=="Neuro Stepdown"]="Medical Intensive Care Unit (MICU)"
hr_data$first_careunit[hr_data$first_careunit=="Neuro Intermediate"]="Medical Intensive Care Unit (MICU)"

# prop_only_follow_mod = glm(prop_only_follow~.,data=hr_data[!hr_data$study_ended & !hr_data$prop_only_censored & hr_data$propofol_rate<=50 & hr_data$propofol_start==1,c(L,"propofol_rate","propofol_rate_sq","prop_only_follow")],family='binomial')
# prop_only_follow_mod_num = glm(prop_only_follow~.,data=hr_data[!hr_data$study_ended & !hr_data$prop_only_censored & hr_data$propofol_start==1,c(V,"baseline_propofol_rate","baseline_propofol_rate_sq","prop_only_follow")],family='binomial')
library(xgboost)
X24 = model.matrix(as.formula(paste('vent24hr~',paste(c(V,paste0('last_',vitals),'oasis_prob','apsiii_prob'),collapse = '+'),sep='')),model.frame(~.,hr_data[hr_data$study_hour==1,],na.action=na.pass))
vent24_mod_cv = xgb.cv(verbose=0,data = X24, label = hr_data$vent24hr[hr_data$study_hour==1], max_depth = 2, eta=.3, nrounds = 500,objective = "binary:logistic",metrics = 'rmse',missing=NA,nfold=10,early_stopping_rounds = 10,prediction = T)
# niters = vent24_mod_cv$best_iteration
pdf(file='../vent24probs.pdf')
hist(vent24_mod_cv$pred)
dev.off()
# vent24_mod = xgboost(verbose=0,data = X24, label = hr_data$vent24hr[hr_data$study_hour==1], max_depth = 2, eta=.3, nrounds = niters,objective = "binary:logistic",metrics = 'rmse',missing=NA)
vent24_prob_table = data.frame(stay_id=hr_data$stay_id[hr_data$study_hour==1],vent24_prob=vent24_mod_cv$pred)
# hr_data = hr_data[,-grep('vent24_prob',names(hr_data))]
hr_data = merge(hr_data,vent24_prob_table)
hr_data = hr_data[order(hr_data$stay_id,hr_data$study_hour),]

vent24_prob_cutoff = 1/2
# pf_cutoff = 200

# L=L[-grep('sq',L)]
prop_only_follow_mod_24 = glm(prop_only_follow~.,data=hr_data[!hr_data$study_ended & !hr_data$prop_only_censored & hr_data$propofol_rate<=prop_thresh & hr_data$vent24_prob>=vent24_prob_cutoff & hr_data$propofol_start==1,c(L,"propofol_rate","control", "prop_only_follow","ever_vaso")],family='binomial')
prop_only_follow_mod_24_baseline = glm(prop_only_follow~.,data=hr_data[hr_data$study_hour==1 & hr_data$propofol_rate<=prop_thresh & hr_data$vent24_prob>=vent24_prob_cutoff & hr_data$propofol_start==1,c(L,"prop_only_follow","control","ever_vaso","pfratio_min","first_pfratio","sofa_max","mbp_min","cardiovascular_max","respiration_max","renal_max","liver_max","coagulation_max","cns_max","sepsis","apsiii_prob","oasis_prob")],family='binomial')
prop_only_follow_mod_24_tv = glm(prop_only_follow~.,data=hr_data[!hr_data$study_ended & hr_data$study_hour > 1 & !hr_data$prop_only_censored & hr_data$propofol_rate<=prop_thresh & hr_data$vent24_prob>=vent24_prob_cutoff & hr_data$propofol_start==1,c(L,"propofol_rate","propofol_rate_sq","control","ever_vaso","prop_only_follow","pfratio_min","sofa_max","mbp_min","cardiovascular_max","respiration_max","renal_max","liver_max","coagulation_max","cns_max","sepsis","apsiii_prob","oasis_prob")],family='binomial')

# hist(predict(prop_only_follow_mod,hr_data[!hr_data$study_ended & !hr_data$prop_only_censored,],type='response'))

# hist(predict(prop_only_follow_mod_num,hr_data[!hr_data$study_ended & !hr_data$prop_only_censored,],type='response'))
pdf(file = "../follow_probs.pdf")
hist(predict(prop_only_follow_mod_24,hr_data[!hr_data$study_ended & !hr_data$prop_only_censored & hr_data$first_pfratio<=pf_cutoff,],type='response'))
dev.off()
# hist(predict(prop_only_follow_mod_num_24,hr_data[!hr_data$study_ended & !hr_data$prop_only_censored & hr_data$first_pfratio<=pf_cutoff,],type='response'))

# hr_data$follow_prob = ifelse(hr_data$propofol_rate>=50|hr_data$study_ended,1,predict(prop_only_follow_mod,hr_data,type='response'))
# hr_data$follow_prob_num = ifelse(hr_data$propofol_rate>=50|hr_data$study_ended,1,predict(prop_only_follow_mod_num,hr_data,type='response'))

# hr_data$follow_prob_24 = ifelse(hr_data$propofol_rate>=prop_thresh|hr_data$study_ended,1,predict(prop_only_follow_mod_24,hr_data,type='response'))
hr_data$follow_prob_24 = ifelse(hr_data$propofol_rate>=prop_thresh|hr_data$study_ended,1,ifelse(hr_data$study_hour==1,predict(prop_only_follow_mod_24_baseline,hr_data,type='response'),predict(prop_only_follow_mod_24_tv,hr_data,type='response')))

# hr_data$follow_prob_num_24 = ifelse(hr_data$propofol_rate>=50|hr_data$study_ended,1,predict(prop_only_follow_mod_num_24,hr_data,type='response'))

disobeyers = unique(hr_data$stay_id[hr_data$prop_only_follow==0])
follower_data = hr_data[!hr_data$stay_id%in%disobeyers,]
follower_data = follower_data[order(follower_data$stay_id,follower_data$study_hour),]
# follower_data_24 = follower_data[follower_data$first_pfratio<=pf_cutoff,]
follower_data_24 = follower_data[follower_data$vent24_prob>=vent24_prob_cutoff,]
follower_data_24 = follower_data_24[order(follower_data_24$stay_id,follower_data_24$study_hour),]

stay_ids = unique(hr_data$stay_id)
outcome_table = data.frame(stay_id = rep(stay_ids,each = 720), study_hour = rep(1:720,length(stay_ids)))
outcome_table = merge(outcome_table,hr_data[hr_data$coma_meas==T,c("stay_id","study_hour","coma")],all.x=T)
outcome_table$coma_recent = unlist(lapply(split(outcome_table$coma,outcome_table$stay_id),carry_forward))

death_cohort = cohort[cohort$deathtime!='',]
death_cohort$death = 1
death_cohort = merge(death_cohort,hr_data[hr_data$study_hour==1,c("stay_id","hour")])
death_cohort$study_hour = pmax(1,ceiling(death_cohort$death_hour) - death_cohort$hour)

outcome_table = merge(outcome_table,death_cohort[,c("stay_id","study_hour","death")],all.x=T)
outcome_table = merge(outcome_table,hr_data[,c('stay_id','study_hour','ventilation_status')],all.x=T)
outcome_table$day = ceiling(outcome_table$study_hour/24)
outcome_table$death = unlist(lapply(split(outcome_table$death,outcome_table$stay_id),carry_forward))
outcome_table$death[is.na(outcome_table$death)] = 0

out_cohort = cohort
out_cohort$out_hour = as.numeric(difftime(strptime(as.character(out_cohort$outtime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(out_cohort$intime), "%Y-%m-%d %H:%M:%S"),units="hours"))
out_cohort = merge(out_cohort,hr_data[hr_data$study_hour==1,c("stay_id","hour")])
out_cohort$study_hour = pmax(1,ceiling(out_cohort$out_hour) - out_cohort$hour)
out_cohort$release = 1
outcome_table = merge(outcome_table,out_cohort[,c("stay_id","study_hour","release")],all.x=T)
outcome_table$release = unlist(lapply(split(outcome_table$release,outcome_table$stay_id),carry_forward))
outcome_table$release[is.na(outcome_table$release)] = 0

outcome_day = outcome_table[,c("stay_id","day","coma","death","release")] %>%
  group_by(stay_id,day) %>%
  summarise_all(max,na.rm=T)

outcome_day$coma_free_day = ifelse(outcome_day$death==1 | outcome_day$coma==1,0,1)
pdf(file="../coma_free_days.pdf")
hist(aggregate(outcome_day$coma_free_day,by=list(outcome_day$stay_id),sum)[,2])
dev.off()
table(aggregate(outcome_day$coma_free_day,by=list(outcome_day$stay_id),sum)[,2])
summary(aggregate(outcome_day$coma_free_day,by=list(outcome_day$stay_id),sum)[,2])
# save(outcome_day,file="sedative_outcome_coma_table.RData")
load("sedative_outcome_coma_table.RData")

cfd_table = aggregate(outcome_day$coma_free_day,by=list(outcome_day$stay_id),sum)
names(cfd_table) = c('stay_id','coma_free_days')
# cfd_table = merge(cfd_table,hr_data[hr_data$study_hour==1,c("stay_id","prob24hr")],all.x=T)
cfd_table = merge(cfd_table,hr_data[hr_data$study_hour==1,c("stay_id","propofol_start")],all.x=T)
cfd_table = merge(cfd_table,hr_data[hr_data$study_hour==1,c("stay_id","first_pfratio")],all.x=T)
cfd_table = merge(cfd_table,hr_data[hr_data$study_hour==1,c("stay_id","vent24_prob")],all.x=T)

followers = unique(follower_data$stay_id)
# followers_itt = unique(hr_data$stay_id[hr_data$study_hour==1 & hr_data$prop_only_follow==1])

mean(cfd_table$coma_free_days[cfd_table$stay_id %in% followers & cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1])
mean(cfd_table$coma_free_days[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1])
sum(cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1)

# mean(cfd_table$coma_free_days[cfd_table$stay_id %in% followers & cfd_table$first_pfratio<=pf_cutoff & cfd_table$propofol_start==1])
# mean(cfd_table$coma_free_days[cfd_table$first_pfratio<=pf_cutoff & cfd_table$propofol_start==1])

# mean(cfd_table$coma_free_days[cfd_table$stay_id %in% followers_itt & cfd_table$first_pfratio<=pf_cutoff & cfd_table$propofol_start==1])

# mean(cfd_table$coma_free_days[cfd_table$stay_id %in% followers & cfd_table$propofol_start==1])
# mean(cfd_table$coma_free_days[cfd_table$propofol_start==1])
# mean(cfd_table$coma_free_days[cfd_table$stay_id %in% followers_itt & cfd_table$propofol_start==1])

# Y = cfd_table[cfd_table$stay_id %in% follower_data$stay_id,c("stay_id","coma_free_days")]
# Y = Y[order(Y$stay_id),]
# weights_den = aggregate(follower_data$follow_prob,by=list(follower_data$stay_id),function(x) prod(x))
# names(weights_den) = c('stay_id','weights_den')
# weights_num = aggregate(follower_data$follow_prob_num,by=list(follower_data$stay_id),function(x) prod(x))
# names(weights_num) = c('stay_id','weights_num')
# Y = merge(Y,weights_den)
# Y = merge(Y,weights_num)
# Y$stab_weights = Y$weights_num/Y$weights_den
# Y$weights = 1/Y$weights_den
# Y$trunc_stab_weights = pmin(Y$stab_weights,min(quantile(Y$stab_weights,.999),50))
# Y$trunc_weights = pmin(Y$weights,min(quantile(Y$weights,.99),25))
# est_stab = sum(Y$coma_free_days*Y$trunc_stab_weights)/sum(Y$trunc_stab_weights)
# est = sum(Y$coma_free_days*Y$trunc_weights)/sum(Y$trunc_weights)

Y_24 = cfd_table[cfd_table$stay_id %in% follower_data_24$stay_id,c("stay_id","coma_free_days")]
Y_24 = Y_24[order(Y_24$stay_id),]
weights_den_24 = aggregate(follower_data_24$follow_prob_24,by=list(follower_data_24$stay_id),function(x) prod(x))
names(weights_den_24) = c('stay_id','weights_den_24')
# weights_num_24 = aggregate(follower_data_24$follow_prob_num_24,by=list(follower_data_24$stay_id),function(x) prod(x))
# names(weights_num_24) = c('stay_id','weights_num_24')
Y_24 = merge(Y_24,weights_den_24)
# Y_24 = merge(Y_24,weights_num_24)
# Y_24$stab_weights_24 = Y_24$weights_num_24/Y_24$weights_den_24
Y_24$weights_24 = 1/Y_24$weights_den_24
# Y_24$trunc_stab_weights_24 = pmin(Y_24$stab_weights_24,min(quantile(Y_24$stab_weights_24,.99),50))
Y_24$trunc_weights_24 = pmin(Y_24$weights_24,min(quantile(Y_24$weights_24,.99),100))
# est_stab_24 = sum(Y_24$coma_free_days*Y_24$trunc_stab_weights_24)/sum(Y_24$trunc_stab_weights_24)
est_24 = sum(Y_24$coma_free_days*Y_24$trunc_weights_24)/sum(Y_24$trunc_weights_24)
mean(cfd_table$coma_free_days[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1])

# itt_data = hr_data[hr_data$study_hour==1 & hr_data$propofol_start,]
# itt_data = itt_data[order(itt_data$stay_id),]
# prop_only_follow_mod_itt = glm(prop_only_follow~.,data=itt_data[itt_data$propofol_rate<=50 & itt_data$propofol_start==1,c(L,"prop_only_follow","baseline_propofol_rate","baseline_propofol_rate_sq")],family='binomial')
# itt_data$pscores = ifelse(itt_data$propofol_rate>50,1,predict(prop_only_follow_mod_itt,newdata=itt_data,type='response'))
# 
# Y_itt = cfd_table[cfd_table$stay_id %in% followers_itt,c("stay_id","coma_free_days")]
# Y_itt = merge(Y_itt,itt_data[,c("stay_id","pscores")])
# est_itt = sum(Y_itt$coma_free_days*(1/Y_itt$pscores))/sum(1/Y_itt$pscores)
# 
# itt_data_24 = hr_data[hr_data$study_hour==1 & hr_data$propofol_start & hr_data$first_pfratio<=pf_cutoff,]
# itt_data_24 = itt_data_24[order(itt_data_24$stay_id),]
# prop_only_follow_mod_itt_24 = glm(prop_only_follow~.,data=itt_data_24[itt_data_24$propofol_rate<=50 & itt_data_24$propofol_start==1,c(L,"prop_only_follow","baseline_propofol_rate","baseline_propofol_rate_sq")],family='binomial')
# itt_data_24$pscores = ifelse(itt_data_24$propofol_rate>50,1,predict(prop_only_follow_mod_itt_24,newdata=itt_data_24,type='response'))
# 
# followers_itt_24 = unique(itt_data_24$stay_id[itt_data_24$prop_only_follow==1])
# Y_itt_24 = cfd_table[cfd_table$stay_id %in% followers_itt_24,c("stay_id","coma_free_days")]
# Y_itt_24 = merge(Y_itt_24,itt_data_24[,c("stay_id","pscores")])
# est_itt_24 = sum(Y_itt_24$coma_free_days*(1/Y_itt_24$pscores))/sum(1/Y_itt_24$pscores)


#ADD VENTILATOR FREE DAYS
cohort = read.csv("mimic4_v1/icustays_1.0.csv",header=T)
careunits = read.csv("mimic4_v1/careunits_1.0.csv",header=T)
cohort = merge(cohort,careunits)
admissions = read.csv("mimic4_v1/admissions.csv",header=T)
cohort = merge(cohort,admissions)
vent = read.csv("mimic4_v1/ventilator.csv")

baseline_hour_table = merge(cohort[,c("stay_id","intime","outtime","subject_id")],hr_data[hr_data$study_hour==1,c("stay_id","study_hour")])
baseline_hour_table$baseline_time = 3600*baseline_hour_table$study_hour + strptime(as.character(baseline_hour_table$intime), "%Y-%m-%d %H:%M:%S")
vent_duration_table = merge(vent,baseline_hour_table[,c("stay_id","subject_id","baseline_time")])
vent_duration_table = vent_duration_table[vent_duration_table$ventilation_status %in% c("InvasiveVent","Trach"),]
vent_duration_table$max_time = vent_duration_table$baseline_time + 3600*24*30

vent_duration_table = vent_duration_table[as.numeric(difftime(strptime(as.character(vent_duration_table$endtime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(vent_duration_table$baseline_time), "%Y-%m-%d %H:%M:%S"),units="hours"))>0,]
vent_duration_table = vent_duration_table[as.numeric(difftime(strptime(as.character(vent_duration_table$max_time), "%Y-%m-%d %H:%M:%S"), strptime(as.character(vent_duration_table$starttime), "%Y-%m-%d %H:%M:%S"),units="hours"))>0,]
vent_duration_table$startday = as.numeric(difftime(strptime(as.character(vent_duration_table$starttime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(vent_duration_table$baseline_time), "%Y-%m-%d %H:%M:%S"),units="days"))
vent_duration_table$endday = as.numeric(difftime(strptime(as.character(vent_duration_table$endtime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(vent_duration_table$baseline_time), "%Y-%m-%d %H:%M:%S"),units="days"))
vent_duration_table$startday_ceiling = pmax(1,ceiling(vent_duration_table$startday))
vent_duration_table$endday_ceiling = pmin(30,ceiling(vent_duration_table$endday))
from_to = function(a,b){
  a:b
}
vent_duration_table$vent_days = mapply(from_to,vent_duration_table$startday_ceiling,vent_duration_table$endday_ceiling)
death_cohort = cohort[cohort$deathtime!='',]

baseline_hour_table = merge(baseline_hour_table,unique(death_cohort[,c("subject_id","deathtime")]),all.x=T)
baseline_hour_table$death_day = ceiling(pmax(1,as.numeric(difftime(strptime(as.character(baseline_hour_table$deathtime), "%Y-%m-%d %H:%M:%S"), strptime(as.character(baseline_hour_table$baseline_time), "%Y-%m-%d %H:%M:%S"),units="days"))))
baseline_hour_table$death_day[is.na(baseline_hour_table$death_day)] = 10000000


pats = unique(vent_duration_table$stay_id)

vent_days = vector(mode='list',length=length(pats))
for(i in 1:length(pats)){
  vent_days[[i]] = unique(unlist(c(vent_duration_table$vent_days[vent_duration_table$stay_id==pats[i]])))
}

vent_days_table = cbind(stay_id = pats,vent_days=vent_days)
baseline_hour_table = merge(baseline_hour_table,vent_days_table,all.x=T)
from_to_death = function(a){
  if(a<=30){
    a:30
  }else{
    NA
  }
}
baseline_hour_table$death_days = lapply(baseline_hour_table$death_day,from_to_death)
get_vent_free_days = function(i){
  30-ifelse(baseline_hour_table$death_day[i]<=30,length(unique(c(baseline_hour_table$vent_days[[i]],baseline_hour_table$death_days[[i]]))),length(unique(baseline_hour_table$vent_days[[i]])))
}
baseline_hour_table$vent_free_days = sapply(1:nrow(baseline_hour_table),get_vent_free_days)
baseline_hour_table = baseline_hour_table[order(baseline_hour_table$stay_id),]
cfd_table = merge(cfd_table,baseline_hour_table[,c("stay_id","vent_free_days")],all.x=T)

Y_24_vfd = cfd_table[cfd_table$stay_id %in% follower_data_24$stay_id,c("stay_id","vent_free_days")]
Y_24_vfd = Y_24_vfd[order(Y_24_vfd$stay_id),]
weights_den_24 = aggregate(follower_data_24$follow_prob_24,by=list(follower_data_24$stay_id),function(x) prod(x))
names(weights_den_24) = c('stay_id','weights_den_24')
# weights_num_24 = aggregate(follower_data_24$follow_prob_num_24,by=list(follower_data_24$stay_id),function(x) prod(x))
# names(weights_num_24) = c('stay_id','weights_num_24')
Y_24_vfd = merge(Y_24_vfd,weights_den_24)
# Y_24_vfd = merge(Y_24_vfd,weights_num_24)
# Y_24_vfd$stab_weights_24 = Y_24_vfd$weights_num_24/Y_24_vfd$weights_den_24
Y_24_vfd$weights_24 = 1/Y_24_vfd$weights_den_24
# Y_24_vfd$trunc_stab_weights_24 = pmin(Y_24_vfd$stab_weights_24,min(quantile(Y_24_vfd$stab_weights_24,.99),50))
Y_24_vfd$trunc_weights_24 = pmin(Y_24_vfd$weights_24,min(quantile(Y_24_vfd$weights_24,.99),50))
# est_stab_24_vfd = sum(Y_24_vfd$vent_free_days*Y_24_vfd$trunc_stab_weights_24)/sum(Y_24_vfd$trunc_stab_weights_24)
est_24_vfd = sum(Y_24_vfd$vent_free_days*Y_24_vfd$trunc_weights_24)/sum(Y_24_vfd$trunc_weights_24)
mean(cfd_table$vent_free_days[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1])
# mean(cfd_table$vent_free_days[cfd_table$first_pfratio<=pf_cutoff & cfd_table$propofol_start==1])

cfd_table = merge(cfd_table,hr_data[hr_data$study_hour==1,c("stay_id","hospital_expire_flag")],all.x=T)
Y_24_mort = cfd_table[cfd_table$stay_id %in% follower_data_24$stay_id,c("stay_id","hospital_expire_flag")]
Y_24_mort = Y_24_mort[order(Y_24_mort$stay_id),]
weights_den_24 = aggregate(follower_data_24$follow_prob_24,by=list(follower_data_24$stay_id),function(x) prod(x))
names(weights_den_24) = c('stay_id','weights_den_24')
# weights_num_24 = aggregate(follower_data_24$follow_prob_num_24,by=list(follower_data_24$stay_id),function(x) prod(x))
# names(weights_num_24) = c('stay_id','weights_num_24')
Y_24_mort = merge(Y_24_mort,weights_den_24)
# Y_24_mort = merge(Y_24_mort,weights_num_24)
# Y_24_mort$stab_weights_24 = Y_24_mort$weights_num_24/Y_24_mort$weights_den_24
Y_24_mort$weights_24 = 1/Y_24_mort$weights_den_24
# Y_24_mort$trunc_stab_weights_24 = pmin(Y_24_mort$stab_weights_24,min(quantile(Y_24_mort$stab_weights_24,.99),50))
Y_24_mort$trunc_weights_24 = pmin(Y_24_mort$weights_24,100)
# est_stab_24_mort = sum(Y_24_mort$hospital_expire_flag*Y_24_mort$trunc_stab_weights_24)/sum(Y_24_mort$trunc_stab_weights_24)
est_24_mort = sum(Y_24_mort$hospital_expire_flag*Y_24_mort$trunc_weights_24)/sum(Y_24_mort$trunc_weights_24)
mean(cfd_table$hospital_expire_flag[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1])
mean(cfd_table$hospital_expire_flag[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1 & cfd_table$stay_id %in% follower_data_24$stay_id])
# mean(cfd_table$hospital_expire_flag[cfd_table$first_pfratio<=pf_cutoff & cfd_table$propofol_start==1])
# mean(cfd_table$hospital_expire_flag[cfd_table$first_pfratio<=pf_cutoff & cfd_table$propofol_start==1 & cfd_table$stay_id %in% follower_data_24$stay_id])



nboot=250
# ests_itt = rep(NA,nboot)
# ests = rep(NA,nboot)
ests_itt_24 = rep(NA,nboot)
ests_24 = rep(NA,nboot)
ests_mort_24 = rep(NA,nboot)
ests_vent_24 = rep(NA,nboot)
# usual_cares = rep(NA,nboot)
usual_cares_24 = rep(NA,nboot)
usual_cares_mort_24 = rep(NA,nboot)
usual_cares_vent_24 = rep(NA,nboot)

lengths = aggregate(hr_data$stay_id,by=list(hr_data$stay_id),length)[,2]
ind_cuts = c(0,cumsum(lengths))
inds = lapply(1:length(lengths),function(i) (ind_cuts[i]+1):ind_cuts[i+1])

for(b in 1:nboot){
  set.seed(b)
  pat_inds = sample(1:length(unique(hr_data$stay_id)),replace=T)  
  boot_inds = unlist(inds[pat_inds])
  data = hr_data[boot_inds,] 
  data$stay_id = factor(rep(1:length(pat_inds),lengths[pat_inds]))
  cfd_table_boot = cfd_table[pat_inds,]
  cfd_table_boot$stay_id = factor(1:length(pat_inds))
  # prop_only_follow_mod_boot = glm(prop_only_follow~.,data=data[!data$study_ended & !data$prop_only_censored & data$propofol_rate<=50 & data$propofol_start==1,c(L,"propofol_rate","propofol_rate_sq","prop_only_follow")],family='binomial')
  # prop_only_follow_mod_num_boot = glm(prop_only_follow~.,data=data[!data$study_ended & !data$prop_only_censored & data$propofol_start==1,c(V,"baseline_propofol_rate","baseline_propofol_rate_sq","prop_only_follow")],family='binomial')
  
  # prop_only_follow_mod_24_boot = glm(prop_only_follow~.,data=data[!data$study_ended & !data$prop_only_censored & data$propofol_rate<=prop_thresh & data$vent24_prob>=vent24_prob_cutoff & data$propofol_start==1,c(L,"propofol_rate","propofol_rate_sq","prop_only_follow")],family='binomial')
  prop_only_follow_mod_24_baseline_boot = glm(prop_only_follow~.,data=data[data$study_hour==1 & data$propofol_rate<=prop_thresh & data$vent24_prob>=vent24_prob_cutoff & data$propofol_start==1,c(L,"prop_only_follow","control","ever_vaso","pfratio_min","first_pfratio","sofa_max","mbp_min","cardiovascular_max","respiration_max","renal_max","liver_max","coagulation_max","cns_max","sepsis","apsiii_prob","oasis_prob")],family='binomial')
  prop_only_follow_mod_24_tv_boot = glm(prop_only_follow~.,data=data[!data$study_ended & data$study_hour > 1 & !data$prop_only_censored & data$propofol_rate<=prop_thresh & data$vent24_prob>=vent24_prob_cutoff & data$propofol_start==1,c(L,"propofol_rate","propofol_rate_sq","control","ever_vaso","prop_only_follow","pfratio_min","sofa_max","mbp_min","cardiovascular_max","respiration_max","renal_max","liver_max","coagulation_max","cns_max","sepsis","apsiii_prob","oasis_prob")],family='binomial')
  
 
  # hr_data$follow_prob_24 = ifelse(hr_data$propofol_rate>=prop_thresh|hr_data$study_ended,1,predict(prop_only_follow_mod_24,hr_data,type='response'))
  data$follow_prob_24 = ifelse(data$propofol_rate>=prop_thresh|data$study_ended,1,ifelse(data$study_hour==1,predict(prop_only_follow_mod_24_baseline_boot,data,type='response'),predict(prop_only_follow_mod_24_tv_boot,data,type='response')))
  
  # data$follow_prob_24 = ifelse(data$propofol_rate>=prop_thresh|data$study_ended,1,predict(prop_only_follow_mod_24_boot,data,type='response'))
  # data$follow_prob_num_24 = ifelse(data$propofol_rate>=50|data$study_ended,1,predict(prop_only_follow_mod_num_24_boot,data,type='response'))
  
  disobeyers = unique(data$stay_id[data$prop_only_follow==0])
  follower_data_boot = data[!data$stay_id%in%disobeyers,]
  follower_data_boot = follower_data_boot[order(follower_data_boot$stay_id,follower_data_boot$study_hour),]
  follower_data_24_boot = follower_data_boot[follower_data_boot$vent24_prob>=vent24_prob_cutoff,]
  # follower_data_24_boot = follower_data_boot[follower_data_boot$first_pfratio<=pf_cutoff,]
  follower_data_24_boot = follower_data_24_boot[order(follower_data_24_boot$stay_id,follower_data_24_boot$study_hour),]
  
  followers_boot = unique(follower_data_boot$stay_id)
  followers_itt_boot = unique(data$stay_id[data$study_hour==1 & data$prop_only_follow==1])
  usual_cares_24[b] = mean(cfd_table_boot$coma_free_days[cfd_table_boot$vent24_prob>=vent24_prob_cutoff & cfd_table_boot$propofol_start==1])
  # usual_cares_24[b] = mean(cfd_table_boot$coma_free_days[cfd_table_boot$first_pfratio<=pf_cutoff & cfd_table_boot$propofol_start==1])
  usual_cares_vent_24[b] = mean(cfd_table_boot$vent_free_days[cfd_table_boot$vent24_prob>=vent24_prob_cutoff & cfd_table_boot$propofol_start==1])
  # usual_cares_vent_24[b] = mean(cfd_table_boot$vent_free_days[cfd_table_boot$first_pfratio<=pf_cutoff & cfd_table_boot$propofol_start==1])
  
  # usual_cares[b] = mean(cfd_table_boot$coma_free_days[cfd_table_boot$propofol_start==1])
  
  # Y_boot = cfd_table_boot[cfd_table_boot$stay_id %in% follower_data_boot$stay_id,c("stay_id","coma_free_days")]
  # Y_boot = Y_boot[order(Y_boot$stay_id),]
  # weights_den_boot = aggregate(follower_data_boot$follow_prob,by=list(follower_data_boot$stay_id),function(x) prod(x))
  # names(weights_den_boot) = c('stay_id','weights_den')
  # weights_num_boot = aggregate(follower_data_boot$follow_prob_num,by=list(follower_data_boot$stay_id),function(x) prod(x))
  # names(weights_num_boot) = c('stay_id','weights_num')
  # Y_boot = merge(Y_boot,weights_den_boot)
  # Y_boot = merge(Y_boot,weights_num_boot)
  # Y_boot$stab_weights = Y_boot$weights_num/Y_boot$weights_den
  # Y_boot$weights = 1/Y_boot$weights_den
  # Y_boot$trunc_stab_weights = pmin(Y_boot$stab_weights,50)
  # Y_boot$trunc_weights = pmin(Y_boot$weights,50)
  # ests[b] = sum(Y_boot$coma_free_days*Y_boot$trunc_weights)/sum(Y_boot$trunc_weights)
  # 
  Y_24_boot = cfd_table_boot[cfd_table_boot$stay_id %in% follower_data_24_boot$stay_id,c("stay_id","coma_free_days")]
  Y_24_boot = Y_24_boot[order(Y_24_boot$stay_id),]
  weights_den_24_boot = aggregate(follower_data_24_boot$follow_prob_24,by=list(follower_data_24_boot$stay_id),function(x) prod(x))
  names(weights_den_24_boot) = c('stay_id','weights_den_24')
  # weights_num_24_boot = aggregate(follower_data_24_boot$follow_prob_num_24,by=list(follower_data_24_boot$stay_id),function(x) prod(x))
  # names(weights_num_24_boot) = c('stay_id','weights_num_24')
  Y_24_boot = merge(Y_24_boot,weights_den_24_boot)
  # Y_24_boot = merge(Y_24_boot,weights_num_24_boot)
  # Y_24_boot$stab_weights_24 = Y_24_boot$weights_num_24/Y_24_boot$weights_den_24
  Y_24_boot$weights_24 = 1/Y_24_boot$weights_den_24
  # Y_24_boot$trunc_stab_weights_24 = pmin(Y_24_boot$stab_weights_24,50)
  Y_24_boot$trunc_weights_24 = pmin(Y_24_boot$weights_24,min(quantile(Y_24_boot$weights_24,.99),100))
  ests_24[b] = sum(Y_24_boot$coma_free_days*Y_24_boot$trunc_weights_24)/sum(Y_24_boot$trunc_weights_24)
  
  Y_24_vfd_boot = cfd_table_boot[cfd_table_boot$stay_id %in% follower_data_24_boot$stay_id,c("stay_id","vent_free_days")]
  Y_24_vfd_boot = Y_24_vfd_boot[order(Y_24_vfd_boot$stay_id),]
  weights_den_24_boot = aggregate(follower_data_24_boot$follow_prob_24,by=list(follower_data_24_boot$stay_id),function(x) prod(x))
  names(weights_den_24_boot) = c('stay_id','weights_den_24')
  # weights_num_24_boot = aggregate(follower_data_24_boot$follow_prob_num_24,by=list(follower_data_24_boot$stay_id),function(x) prod(x))
  # names(weights_num_24_boot) = c('stay_id','weights_num_24')
  Y_24_vfd_boot = merge(Y_24_vfd_boot,weights_den_24_boot)
  # Y_24_vfd_boot = merge(Y_24_vfd_boot,weights_num_24_boot)
  # Y_24_vfd_boot$stab_weights_24 = Y_24_vfd_boot$weights_num_24/Y_24_vfd_boot$weights_den_24
  Y_24_vfd_boot$weights_24 = 1/Y_24_vfd_boot$weights_den_24
  # Y_24_vfd_boot$trunc_stab_weights_24 = pmin(Y_24_vfd_boot$stab_weights_24,50)
  Y_24_vfd_boot$trunc_weights_24 = pmin(Y_24_vfd_boot$weights_24,min(quantile(Y_24_vfd_boot$weights_24,.99),100))
  ests_vent_24[b] = sum(Y_24_vfd_boot$vent_free_days*Y_24_vfd_boot$trunc_weights_24)/sum(Y_24_vfd_boot$trunc_weights_24)
  
  # itt_data_boot = data[data$study_hour==1 & data$propofol_start,]
  # itt_data_boot = itt_data_boot[order(itt_data_boot$stay_id),]
  # prop_only_follow_mod_itt_boot = glm(prop_only_follow~.,data=itt_data_boot[itt_data_boot$propofol_rate<=50 & itt_data_boot$propofol_start==1,c(L,"prop_only_follow","baseline_propofol_rate","baseline_propofol_rate_sq")],family='binomial')
  # itt_data_boot$pscores = ifelse(itt_data_boot$propofol_rate>50,1,predict(prop_only_follow_mod_itt_boot,newdata=itt_data_boot,type='response'))
  # 
  # Y_itt_boot = cfd_table_boot[cfd_table_boot$stay_id %in% followers_itt_boot,c("stay_id","coma_free_days")]
  # Y_itt_boot = merge(Y_itt_boot,itt_data_boot[,c("stay_id","pscores")])
  # ests_itt[b] = sum(Y_itt_boot$coma_free_days*(1/Y_itt_boot$pscores))/sum(1/Y_itt_boot$pscores)
  # 
  # itt_data_24_boot = data[data$study_hour==1 & data$propofol_start & data$first_pfratio<=pf_cutoff,]
  # itt_data_24_boot = itt_data_24_boot[order(itt_data_24_boot$stay_id),]
  # prop_only_follow_mod_itt_24_boot = glm(prop_only_follow~.,data=itt_data_24_boot[itt_data_24_boot$propofol_rate<=50 & itt_data_24_boot$propofol_start==1,c(L,"prop_only_follow","baseline_propofol_rate","baseline_propofol_rate_sq")],family='binomial')
  # itt_data_24_boot$pscores = ifelse(itt_data_24_boot$propofol_rate>50,1,predict(prop_only_follow_mod_itt_24_boot,newdata=itt_data_24_boot,type='response'))
  # 
  # followers_itt_24_boot = unique(itt_data_24_boot$stay_id[itt_data_24_boot$prop_only_follow==1])
  # Y_itt_24_boot = cfd_table_boot[cfd_table_boot$stay_id %in% followers_itt_24_boot,c("stay_id","coma_free_days")]
  # Y_itt_24_boot = merge(Y_itt_24_boot,itt_data_24_boot[,c("stay_id","pscores")])
  # ests_itt_24[b] = sum(Y_itt_24_boot$coma_free_days*(1/Y_itt_24_boot$pscores))/sum(1/Y_itt_24_boot$pscores)
  # 
  Y_24_mort_boot = cfd_table_boot[cfd_table_boot$stay_id %in% follower_data_24_boot$stay_id,c("stay_id","hospital_expire_flag")]
  Y_24_mort_boot = Y_24_mort_boot[order(Y_24_mort_boot$stay_id),]
  weights_den_24_boot = aggregate(follower_data_24_boot$follow_prob_24,by=list(follower_data_24_boot$stay_id),function(x) prod(x))
  names(weights_den_24_boot) = c('stay_id','weights_den_24')
  # weights_num_24 = aggregate(follower_data_24$follow_prob_num_24,by=list(follower_data_24$stay_id),function(x) prod(x))
  # names(weights_num_24) = c('stay_id','weights_num_24')
  Y_24_mort_boot = merge(Y_24_mort_boot,weights_den_24_boot)
  # Y_24_mort = merge(Y_24_mort,weights_num_24)
  # Y_24_mort$stab_weights_24 = Y_24_mort$weights_num_24/Y_24_mort$weights_den_24
  Y_24_mort_boot$weights_24 = 1/Y_24_mort_boot$weights_den_24
  # Y_24_mort$trunc_stab_weights_24 = pmin(Y_24_mort$stab_weights_24,min(quantile(Y_24_mort$stab_weights_24,.99),50))
  Y_24_mort_boot$trunc_weights_24 = pmin(Y_24_mort_boot$weights_24,100)
  # est_stab_24_mort = sum(Y_24_mort$hospital_expire_flag*Y_24_mort$trunc_stab_weights_24)/sum(Y_24_mort$trunc_stab_weights_24)
  ests_mort_24[b] = sum(Y_24_mort_boot$hospital_expire_flag*Y_24_mort_boot$trunc_weights_24)/sum(Y_24_mort_boot$trunc_weights_24)
  usual_cares_mort_24[b] = mean(cfd_table_boot$hospital_expire_flag[cfd_table_boot$vent24_prob>=vent24_prob_cutoff & cfd_table_boot$propofol_start==1])
  # usual_cares_mort_24[b] = mean(cfd_table_boot$hospital_expire_flag[cfd_table_boot$first_pfratio<=pf_cutoff & cfd_table_boot$propofol_start==1])
}

hist(ests_24)
points(est_24,0,col=2)

hist(ests_24-usual_cares_24)
points(est_24 - mean(cfd_table$coma_free_days[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]),0,col=2)
est_24 - mean(cfd_table$coma_free_days[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]) 
est_24 - mean(cfd_table$coma_free_days[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]) +1.96*sd(ests_24-usual_cares_24,na.rm=T)
est_24 - mean(cfd_table$coma_free_days[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]) -1.96*sd(ests_24-usual_cares_24,na.rm=T)
est_24
est_24 + 1.96*sd(ests_24)
est_24 - 1.96*sd(ests_24)

mean(cfd_table$coma_free_days[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]) + 1.96*sd(usual_cares_24)
mean(cfd_table$coma_free_days[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]) - 1.96*sd(usual_cares_24)

est_24_vfd - mean(cfd_table$vent_free_days[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1])
hist(ests_vent_24-usual_cares_vent_24)
points(est_24_vfd - mean(cfd_table$vent_free_days[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]),0,col=2)
est_24_vfd - mean(cfd_table$vent_free_days[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]) 
est_24_vfd - mean(cfd_table$vent_free_days[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]) +1.96*sd(ests_vent_24-usual_cares_vent_24,na.rm=T)
est_24_vfd - mean(cfd_table$vent_free_days[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]) -1.96*sd(ests_vent_24-usual_cares_vent_24,na.rm=T)
mean(cfd_table$vent_free_days[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]) -1.96*sd(usual_cares_vent_24,na.rm=T)
mean(cfd_table$vent_free_days[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]) +1.96*sd(usual_cares_vent_24,na.rm=T)
est_24_vfd  +1.96*sd(ests_vent_24)
est_24_vfd -1.96*sd(ests_vent_24)


hist(ests_mort_24 - usual_cares_mort_24)
points(est_24_mort - mean(cfd_table$hospital_expire_flag[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]),0,col=2)
est_24_mort - mean(cfd_table$hospital_expire_flag[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1])
est_24_mort - mean(cfd_table$hospital_expire_flag[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]) +1.96*sd(ests_mort_24-usual_cares_mort_24,na.rm=T)
est_24_mort - mean(cfd_table$hospital_expire_flag[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]) -1.96*sd(ests_mort_24-usual_cares_mort_24,na.rm=T)
est_24_mort  +1.96*sd(ests_mort_24)
est_24_mort -1.96*sd(ests_mort_24)
mean(cfd_table$hospital_expire_flag[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]) +1.96*sd(usual_cares_mort_24)
mean(cfd_table$hospital_expire_flag[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]) -1.96*sd(usual_cares_mort_24)


sds = c(sd(usual_cares_24,na.rm=T),sd(ests_24,na.rm=T),sd(usual_cares_mort_24),sd(ests_mort_24),sd(usual_cares_vent_24),sd(ests_vent_24))
point_ests = c(mean(cfd_table$coma_free_days[cfd_table$first_pfratio<=pf_cutoff & cfd_table$propofol_start==1]),est_24,
               mean(cfd_table$hospital_expire_flag[cfd_table$first_pfratio<=pf_cutoff & cfd_table$propofol_start==1]), est_24_mort,
               mean(cfd_table$vent_free_days[cfd_table$first_pfratio<=pf_cutoff & cfd_table$propofol_start==1]),est_24_vfd)
uppers = point_ests+1.96*sds
lowers = point_ests-1.96*sds

effect_pes = c(mean(cfd_table$coma_free_days[cfd_table$first_pfratio<=pf_cutoff & cfd_table$propofol_start==1])-est_24,
               mean(cfd_table$hospital_expire_flag[cfd_table$first_pfratio<=pf_cutoff & cfd_table$propofol_start==1]) - est_24_mort)
effect_sds = c(sd(usual_cares_24-ests_24,na.rm=T),sd(usual_cares_mort_24-ests_mort_24))
effect_uppers = effect_pes + 1.96*effect_sds
effect_lowers = effect_pes - 1.96*effect_sds

ticks = c(1:3)
plot(ticks,point_ests[1:3],ylim=c(23,26),xlab='',ylab="Coma Free Days",xaxt="n",main="Effects of Avoiding Polypharmacy on Coma Free Days",col=c(1:6))
arrows(ticks,lowers[1:3],ticks,uppers[1:3],length=0.05,angle=90,code=3)
axis(1, at = ticks,
     labels = c("Usual Care", "Per-protocol", "ITT"))

save(hr_data,cfd_table,vent24_prob_cutoff,file='sedatives.RData')
save(usual_cares_24,ests_24,ests_itt_24,usual_cares,ests,ests_itt,est_24,ests_mort_24,est_24_mort,file="sedative_results.RData")

load('sedatives.RData')
load("sedative_results.RData")

table1_data = hr_data[hr_data$study_hour==1,c('stay_id',L,'oasis_prob','apsiii_prob','sepsis')]
table1_data = data.frame(table1_data)
cohort_stay_ids = cfd_table$stay_id[cfd_table$vent24_prob>=vent24_prob_cutoff & cfd_table$propofol_start==1]
cohort_stay_ids = cohort_stay_ids[!is.na(cohort_stay_ids)]

cohort_follower_stay_ids = cfd_table$stay_id[cfd_table$stay_id %in% followers & cfd_table$first_pfratio<=pf_cutoff & cfd_table$propofol_start==1]
cohort_follower_stay_ids = cohort_follower_stay_ids[!is.na(cohort_follower_stay_ids)]

apply(table1_data[table1_data$stay_id %in% cohort_stay_ids,L],2,mean,na.rm=T)
cohort_table1 = table1_data[table1_data$stay_id %in% cohort_stay_ids,c(L,'oasis_prob','apsiii_prob','sepsis')]
mean(cohort_table1$last_heart_rate)
sd(cohort_table1$last_heart_rate)

mean(cohort_table1$last_sbp)
sd(cohort_table1$last_sbp)

mean(cohort_table1$last_dbp)
sd(cohort_table1$last_dbp)

mean(cohort_table1$last_mbp)
sd(cohort_table1$last_mbp)

mean(cohort_table1$last_resp_rate)
sd(cohort_table1$last_resp_rate)

mean(cohort_table1$age)
sd(cohort_table1$age)

table(cohort_table1$gender)

table(cohort_table1$admission_location)

table(cohort_table1$admission_type)

table(cohort_table1$first_careunit)/sum(table(cohort_table1$first_careunit))

mean(cohort_table1$OBESITY)
mean(cohort_table1$CONGESTIVE_HEART_FAILURE)
mean(cohort_table1$PERIPHERAL_VASCULAR|cohort_table1$HYPERTENSION)
mean(cohort_table1$DRUG_ABUSE)
mean(cohort_table1$CHRONIC_PULMONARY)
mean(cohort_table1$LIVER_DISEASE)
mean(cohort_table1$DRUG_ABUSE|cohort_table1$ALCOHOL_ABUSE)



mean(cohort_table1$last_sofa_24hours)
sd(cohort_table1$last_sofa_24hours)

mean(cfd_table$first_pfratio[cfd_table$stay_id %in% cohort_stay_ids])
sd(cfd_table$first_pfratio[cfd_table$stay_id %in% cohort_stay_ids])

mean(hr_data$rate_std[hr_data$stay_id %in% cohort_stay_ids & hr_data$study_hour==1])
mean(hr_data$sepsis[hr_data$stay_id %in% cohort_stay_ids & hr_data$study_hour==1])

