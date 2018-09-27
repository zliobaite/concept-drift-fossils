# I.Zliobaite 2017 11 03
# analyzing fossil data
# Last updated: 2018 09 12


input_file_fossils_all <- 'data_fossils/data_fossil_occurence.csv'
input_file_fossils_teeth<- 'data_fossils/data_fossil_teeth.csv'

out_file <- 'outputs_fossils/data_fossil_sites.csv'
out_file2 <- 'outputs_fossils/ComLocs_all.csv'
out_file3 <- 'outputs_fossils/ComLocs_selected.csv'
out_concept_drift <- 'outputs_fossils/concept_drift.csv'

fig_concept_drift <- 'plots_fossils/fig_concept_drift.pdf'

do_habitat_predictions <- TRUE

mycol <- c('#016c59','#a65628','gold','#b2df8a','#1f78b4','#e41a1c')
 

param_minFossils <- 5
dg <- 3

load('fit_taxon_fossils_pls.RData')
equation_taxon_pls <- coef(fit_taxon_fossils_pls, intercept = TRUE)
equation_variables <- rownames(equation_taxon_pls)
load('mean_NPP_gen_train.RData')


data_all <- read.csv(input_file_fossils_all, header = TRUE, sep = '\t')
data_FCT <- read.csv(input_file_fossils_teeth, header = TRUE, sep = '\t')

print('select four orders')
include <- which(data_all[,'Order']=='Artiodactyla')
include <- c(include,which(data_all[,'Order']=='Perissodactyla'))
include <- c(include,which(data_all[,'Order']=='Primates'))
include <- c(include,which(data_all[,'Order']=='Proboscidea'))

data_all <- data_all[include,]

print('select species genus')

include <- which(!is.na(data_all[,'Genus']))

data_all <- data_all[include,]

data_all[,'FCT_HYP'] <- NA
data_all[,'FCT_HOR'] <- NA
data_all[,'FCT_LOP'] <- NA
data_all[,'FCT_AL'] <- NA
data_all[,'FCT_OL'] <- NA
data_all[,'FCT_SF'] <- NA
data_all[,'FCT_OT'] <- NA
data_all[,'FCT_CM'] <- NA
data_all[,'Living'] <- NA
data_all[,'LivingGenus'] <- NA

for (sk in 1:dim(data_FCT)[1]){
  tax_now <- as.vector(data_FCT[sk,'TaxonString'])
  ind <- which(data_all[,'TaxonString']==tax_now)
  data_all[ind,'FCT_HYP'] <- data_FCT[sk,'FCT_HYP']
  data_all[ind,'FCT_HOR'] <- data_FCT[sk,'FCT_HOR']
  data_all[ind,'FCT_LOP'] <- data_FCT[sk,'FCT_LOP']
  data_all[ind,'FCT_AL'] <- data_FCT[sk,'FCT_AL']
  data_all[ind,'FCT_OL'] <- data_FCT[sk,'FCT_OL']
  data_all[ind,'FCT_SF'] <- data_FCT[sk,'FCT_SF']
  data_all[ind,'FCT_OT'] <- data_FCT[sk,'FCT_OT']
  data_all[ind,'FCT_CM'] <- data_FCT[sk,'FCT_CM']
  data_all[ind,'Living'] <- data_FCT[sk,'Living']
  data_all[ind,'LivingGenus'] <- as.vector(data_FCT[sk,'LivingGenus'])
}

include <- which(!is.na(data_all[,'LivingGenus']))
data_all <- data_all[include,]

un_comloc <- unique(data_all[,'ComLoc'])
un_comloc <-un_comloc[!is.na(un_comloc)]

data_comloc <- c()

for (sk in 1:length(un_comloc)){
  comloc_now <- un_comloc[sk]
  
  ind <- which(data_all[,'ComLoc'] == comloc_now)
  ind2 <- which(data_all[ind,'unique_species_used'] ==1)
  
  if (is.na(data_all[ind[1],'EastWest'])){
    ew <- NA
  }else{
    if ((data_all[ind[1],'EastWest'])=='E'){
      ew <- 1  
    }else{
      ew <- 2
    }
  }
  data_comloc <- rbind(data_comloc,cbind(as.vector(data_all[ind[1],'ComLoc']),as.vector(data_all[ind[1],'Member']),data_all[ind[1],'MidMemberAge'],length(ind),length(unique(data_all[ind,'Genus'])),length(ind2),ew,as.vector(data_all[ind[1],'Formation'])))
}




colnames(data_comloc) <- c('ComLoc','Member','MidMemberAge','noFossils','noGenus','noSpecies','EastWest','Formation')
write.table(data_comloc,file = out_file2,quote = FALSE,row.names = FALSE,col.names = TRUE,sep='\t')


print('filtering out min number of fossils')
ind <- which(as.numeric(data_comloc[,'noFossils'])>=param_minFossils)
comlocs_select <- data_comloc[ind,'ComLoc']

include <- c()
for (sk in 1:length(comlocs_select)){
  comloc_now <- comlocs_select[sk]
  ind <- which(as.vector(data_all[,'ComLoc']) == comloc_now)
  include <- c(include,ind)
}

data_all <- data_all[include,]

un_comloc <- unique(data_all[,'ComLoc'])

data_comloc <- c()

n_all <- dim(data_all)[1]

data_all[,'un_gen'] <- NA

data_all[,'MeanHYP'] <- NA
data_all[,'MeanLOP'] <- NA
data_all[,'MeanHOR'] <- NA
data_all[,'MeanAL'] <- NA
data_all[,'MeanOL'] <- NA
data_all[,'MeanSF'] <- NA
data_all[,'MeanOT'] <- NA
data_all[,'MeanCM'] <- NA
for (sk in 1:length(un_comloc)){
  comloc_now <- un_comloc[sk]
  #print(comloc_now)
  ind <- which(data_all[,'ComLoc'] == comloc_now)
  #print(data_all[ind[1],'EastWest'])
  if (is.na(data_all[ind[1],'EastWest'])){
    ew <- NA
  }else{
    if ((data_all[ind[1],'EastWest'])=='E'){
      ew <- 1  
    }else{
      ew <- 2
    }
  }
  
  #averages
  un_genera <- unique(data_all[ind,'Genus'])
  ind_un_gen <- c()
  for (sk3 in 1:length(un_genera)){
    ind3 <- which(data_all[ind,'Genus'] == un_genera[sk3])
    ind_un_gen <- c(ind_un_gen,ind[ind3[1]])
  }
  data_all[ind_un_gen,'un_gen'] <- 1
  
  meanHYP <- round(mean(data_all[ind_un_gen,'FCT_HYP'],na.rm=TRUE),digits = dg)
  meanHOR <- round(mean(data_all[ind_un_gen,'FCT_HOR'],na.rm=TRUE),digits = dg)
  meanAL <- round(mean(data_all[ind_un_gen,'FCT_AL'],na.rm=TRUE),digits = dg)
  meanOL <- round(mean(data_all[ind_un_gen,'FCT_OL'],na.rm=TRUE),digits = dg)
  meanSF <- round(mean(data_all[ind_un_gen,'FCT_SF'],na.rm=TRUE),digits = dg)
  meanOT <- round(mean(data_all[ind_un_gen,'FCT_OT'],na.rm=TRUE),digits = dg)
  meanCM <- round(mean(data_all[ind_un_gen,'FCT_CM'],na.rm=TRUE),digits = dg)
  meanLOP <- round(mean(data_all[ind_un_gen,'FCT_LOP'],na.rm=TRUE),digits = dg)
  
  temp_high <- 55.2 - 16.8*meanAL - 20.7*meanOL - 67.9*meanSF
  pred_npp_ecomI <- 2980 - 1323*meanHYP + 3146*meanSF + 1939*meanOT + 198*meanCM + 51*meanLOP
  #pred_npp_ecom <- 2870 - 357*meanHYP -741*meanLOP  
  pred_npp_ecom <- 1251.9 - 460.9*meanHYP + 2237.1*meanLOP - 823.7*meanHYP*meanLOP
  if (pred_npp_ecom<0) {pred_npp_ecom <- 0}
  pred_npp_ecom <- 3000*(1 - exp(-0.000664*pred_npp_ecom))
  #pred_npp_ecom <- 4050 - 1340*meanHYP + 1157*meanAL + 1111*meanOT + 2229*meanCM - 534*meanHOR
  #pred_npp_base_ecom <- 3150 - 912*meanHYP
  #pred_npp_base_ecom <- 1482 - 1116*meanOT
  pred_npp_base_ecom <- 1404 + 2171*meanAL - 1253*meanOT
  
  
  data_all[ind_un_gen,'meanHYP'] <- meanHYP
  data_all[ind_un_gen,'meanHOR'] <- meanHOR
  data_all[ind_un_gen,'meanAL'] <- meanAL
  data_all[ind_un_gen,'meanOL'] <- meanOL
  data_all[ind_un_gen,'meanSF'] <- meanSF
  data_all[ind_un_gen,'meanOT'] <- meanOT
  data_all[ind_un_gen,'meanCM'] <- meanCM
  data_all[ind_un_gen,'meanLOP'] <- meanLOP
  data_all[ind_un_gen,'temp_high'] <- temp_high
  data_all[ind_un_gen,'pred_npp_ecom'] <- pred_npp_ecom
  data_all[ind_un_gen,'pred_npp_ecomI'] <- pred_npp_ecomI
  data_all[ind_un_gen,'pred_npp_base_ecom'] <- pred_npp_base_ecom
  
  
    genera_all <- unique(data_all[ind_un_gen,'LivingGenus'])
    pred_all <- c()
    for (sk5 in 1:length(genera_all)){
      gen_now <- genera_all[sk5]
      ind_temp <- which(mean_NPP_gen_train[,'gen_now'] == gen_now)
      pred_all <- c(pred_all,mean_NPP_gen_train[ind_temp,'mn_NPP'])
    }
    pred_habitat <- mean(as.numeric(pred_all),na.rm = TRUE)
    data_all[ind_un_gen,'pred_habitat'] <- pred_habitat
  
    pred_taxa <- equation_taxon_pls[1]
    for (sk5 in 1:length(genera_all)){
      gen_now <- genera_all[sk5]
      ind_temp <- which(equation_variables == gen_now)
      pred_taxa <- pred_taxa + equation_taxon_pls[ind_temp]
    }
    data_all[ind_un_gen,'pred_taxa'] <- pred_taxa
  
  data_comloc <- rbind(data_comloc,cbind(as.vector(data_all[ind[1],'ComLoc']),as.vector(data_all[ind[1],'Member']),data_all[ind[1],'MidMemberAge'],length(ind),length(un_genera),ew,as.vector(data_all[ind[1],'Formation']),meanHYP,meanHOR,meanAL,meanOL,meanSF,meanOT,meanCM,temp_high,pred_npp_ecom,pred_npp_ecomI,pred_npp_base_ecom,meanLOP,pred_habitat,pred_taxa))
}

colnames(data_comloc) <- c('ComLoc','Member','MidMemberAge','noFossils','noGenus','EastWest','Formation','meanHYP','meanHOR','meanAL','meanOL','meanSF','meanOT','meanCM','temp_high','pred_npp_ecom','pred_npp_ecomI','pred_npp_base_ecom','meanLOP','pred_habitat','pred_taxa')

print(dim(data_comloc))
write.table(data_comloc,file = out_file3,quote = FALSE,row.names = FALSE,col.names = TRUE,sep='\t')

write.table(data_all,file = out_file,quote = FALSE,row.names = FALSE,col.names = TRUE,sep='\t')


un_living <- unique(data_all[,'LivingGenus'])
save(un_living, file = "un_living.RData")

un_age <- unique(data_all[,'MidMemberAge'])
ord <- order(un_age)
un_age <- un_age[ord]

rr <- c()

for (sk in 1:length(un_age)){
  age_now <- un_age[sk]
  ind <- which(data_all[,'MidMemberAge']==age_now)
  un_spec <- unique(data_all[ind,'Genus'])
  if (length(un_spec)>5){
    all_liv <- c()
    for (sk2 in 1:length(un_spec)){
      ind_match <- which(data_all[ind,'Genus']==un_spec[sk2])
      all_liv <- c(all_liv,data_all[ind[ind_match],'Living'])
    }
    rr <- rbind(rr,c(age_now,mean(all_liv,na.rm=TRUE)))  
  }
}
rr <- as.data.frame(rr)
colnames(rr) <- c('age','alive')
fit <- lm(alive ~ age,data=rr)
write.table(rr,file = out_concept_drift,quote = FALSE,row.names = FALSE,col.names = TRUE,sep='\t')

pdf(fig_concept_drift,width = 7, height = 4.5)
plot(rr[,'age'], rr[,'alive'],ylim=c(0,0.4), xlim = c(0.8,7.8), pch = 19, xlab="Age of fossil sites, MA", ylab="Fraction of genera alive today", col='black')
lines(rr[,'age'], rr[,'alive'],pch = 18, lwd = 3)
#abline(fit,lwd = 2)
dev.off()

