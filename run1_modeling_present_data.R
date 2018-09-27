# 2017 11 06 I.Zliobaite
# modeling on present day data
# Last updated: 2018 09 10

input_sites <- 'data_present/data_present_climate.csv'
input_teeth_raw <- 'data_present/data_present_teeth.csv'
input_occrence_raw <- 'data_present/data_present_occurence.csv'
input_living_relatives <- 'data_present/data_nearest_relatives.csv'

out_occurence <- 'outputs_present/occurence_processed.csv'
out_occurence_counts <- 'outputs_present/occurence_counts.csv'
out_teeth_all <- 'outputs_present/dental_traits_all.csv'
out_teeth <- 'outputs_present/dental_traits.csv'
out_sites <- 'outputs_present/data_all.csv'
out_sites_test <- 'outputs_present/data_predictions_test.csv'
out_sites_train <- 'outputs_present/data_predictions_train.csv'
out_occurence_pseudo <- 'outputs_present/occurence_pseudo.csv'
out_occurence_living <- 'outputs_present/occurence_living.csv'
out_plot_NPP <- 'plots_present/fig_hist_NPP.pdf'
out_plot_spec_NPP <- 'plots_present/fig_spec_NPP.pdf'

# these are data aggregation operations, they can be done once and during subsequent runs are loaded from saved csv files
do_occ_into_gen <- FALSE
do_mean_traits <- FALSE
do_select_living_relatives <- TRUE

#alternative options for model run and parameter settings
do_filter_NPP <- FALSE #add cap on NPP
param_round_dg <- 3
param_lim_NPP <- 2000

param_training_data <- c('East','South','Diag','All')
#param_training_data <- c('East','South','Diag','Half1','Half2','All') #alternative option for training data
#param_training_data <- 'All' #alternative option
fets_teeth <- c('HYP','LOP','FCT_HOD','FCT_AL','FCT_OL','FCT_SF','FCT_OT','FCT_CM')
mycolors <-c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
mycolors_NPP <- c('#edf8e9','#c7e9c0','#74c476','#005a32')
mycolors_cross <- c('#f0f0f0','#fdb863','#542788')

# main

for (cik in 1:length(param_training_data)){
  select_training_data <- param_training_data[cik]
  print(select_training_data)

  data_sites <- read.csv(input_sites, header = TRUE, sep = '\t')
  if (do_filter_NPP){
    print('filtering NPP')
    ind <- which(data_sites[,'NPP']<=param_lim_NPP)
    data_sites <- data_sites[ind,]
  }else{
    print('not filtering NPP')
  }
  pdf(out_plot_NPP, width = 7, height = 7)
  hist(data_sites[,'NPP'],breaks = 20)
  dev.off()
  
  
  if (do_occ_into_gen){
    data_occ_raw <- read.csv(input_occrence_raw, header = TRUE, sep = ',')
    data_occ <- c()
    for (sk in 1:dim(data_sites)[1]){
      ind <- which(as.vector(data_occ_raw[,'SITE']) == data_sites[sk,'SITE'])
      data_occ <- rbind(data_occ,data_occ_raw[ind,])
    }
    sm <- apply(data_occ,2,sum)
    ind_take <- which(sm>0)
    data_occ <- data_occ[,ind_take]
    
    taxa_occ <- gsub("\\.", " ", colnames(data_occ))
    #print(length(taxa_occ))
    genera <- c()
    for (sk2 in 1:length(taxa_occ)){
      str_now <- strsplit(taxa_occ[sk2],' ')
      genera <- rbind(genera,str_now[[1]][1])
    }
    #print(length(genera))
    un_genera <- unique(genera)
    data_occ_gen <- data_occ[,c('SITE','SITE')]
    colnames(data_occ_gen)[2] <- 'SITEcheck'
    data_occ_gen[,un_genera[2:length(un_genera)]] <- NA
    data_occ_gen_counts <- data_occ_gen
    for (sk2 in 2:length(un_genera)){
      gen_now <- un_genera[sk2]
      ind <- which(genera == gen_now)
      for (sk3 in 1:dim(data_occ)[1]){
        data_occ_gen[sk3,'SITEcheck'] <- data_occ[sk3,'SITE']
        sm <- sum(data_occ[sk3,ind])
        data_occ_gen_counts [sk3,gen_now] <- sm
        if (sm>0){
          data_occ_gen[sk3,gen_now] <- 1
        }else{
          data_occ_gen[sk3,gen_now] <- 0
        }
      }
    }
    
    print('manually adding Taurotragus and Camelus')
    data_occ_gen[,'Taurotragus'] <- 0
    data_occ_gen[,'Camelus'] <- 0
    print('removing Capra')
    ind <- which(colnames(data_occ_gen) == 'Capra')
    print(dim(data_occ_gen))
    data_occ_gen <- data_occ_gen[,-ind]
    print(dim(data_occ_gen))
    
    write.table(data_occ_gen,file = out_occurence,quote = FALSE,row.names = FALSE,col.names = TRUE,sep='\t')
    write.table(data_occ_gen_counts,file = out_occurence_counts,quote = FALSE,row.names = FALSE,col.names = TRUE,sep='\t')
  }
  
  data_occ_gen <- read.csv(out_occurence, header = TRUE, sep = '\t')
  
  
  if (do_mean_traits){
    data_teeth_raw <- read.csv(input_teeth_raw, header = TRUE, sep = '\t')
    gen_all <- c()
    for (sk in 1:dim(data_teeth_raw)[1]){
      gen_now <- as.vector(data_teeth_raw[sk,'TAXON'])
      gen_all <- rbind(gen_all,strsplit(gen_now,' ')[[1]][1])
    }
    un_gen <- unique(gen_all)
    data_teeth_all <- c()
    for (sk in 1:length(un_gen)){
      gen_now <- un_gen[sk]
      ind <- which(gen_all == gen_now)
      if (length(ind)>1){
        mn <- round(apply(data_teeth_raw[ind,fets_teeth],2,mean,na.rm=TRUE))
      }else{
        mn <- data_teeth_raw[ind,fets_teeth]
      }
      data_teeth_all <- rbind(data_teeth_all,c(gen_now,as.vector(data_teeth_raw[ind[1],'TAXON']),as.vector(data_teeth_raw[ind[1],'FAMILY']),as.vector(data_teeth_raw[ind[1],'ORDER']),mn))
    }
    colnames(data_teeth_all)[1:4] <- c('Genus','TAXON','FAMILY','ORDER')
    write.table(data_teeth_all,file = out_teeth_all,quote = FALSE,row.names = FALSE,col.names = TRUE,sep='\t')
    data_teeth <- c()
    for (sk in 3:dim(data_occ_gen)[1]){
      gen_now <- colnames(data_occ_gen)[sk]
      ind <- which(as.vector(data_teeth_all[,'Genus']) == gen_now)
      data_teeth <- rbind(data_teeth,data_teeth_all[ind,])
      ### stopped here 
    }
    write.table(data_teeth,file = out_teeth,quote = FALSE,row.names = FALSE,col.names = TRUE,sep='\t')
    
  }
  data_teeth <- read.csv(out_teeth, header = TRUE, sep = '\t')
  
  
  load('data_present/un_living.RData')
  ind_select_living_genera <- c(1,2)
  for (sk in 1:length(un_living)){
    liv_now <- un_living[sk]
    #print(liv_now)
    ind <- which(colnames(data_occ_gen) == liv_now)
    if (length(ind) == 0){
      #print(liv_now)
    }else{
      ind_select_living_genera <- c(ind_select_living_genera,ind)
    }
  }
  data_occ_living_as_fosils_gen <- data_occ_gen[,ind_select_living_genera]
  
  
  cxax <- 1
  pdf('plots_present/fig_map_NPP.pdf',width = 9,height = 7)
  rbPal <- colorRampPalette(mycolors_NPP)
  pcol <- rbPal(12)[as.numeric(cut(data_sites[,'NPP'],breaks = 12))]
  plot(data_sites[,'lon_bio'],data_sites[,'lat_bio'],pch=20,cex = 1,col = pcol,xlab = 'longitude',ylab = 'latitude',cex.axis=cxax)
  #text(-17,21,'(c)',cex=0.8)
  legend("bottomleft",legend=seq(0,2300,by=450),col=rbPal(6),pch=15,cex = 1.5,pt.cex = 2.5)
  dev.off()
  
  data_sites[,'genCount'] <- NA
  data_sites[,'occCount'] <- NA
  data_sites[,'occCount2'] <- NA
  data_sites[,fets_teeth] <- NA
  for (sk in 1:dim(data_sites)[1]){
    site_now <- data_sites[sk,'SITE']
    ind <- which(as.vector(data_occ_gen[,'SITE']) == site_now)
    data_sites[sk,'genCount'] <- sum(data_occ_gen[ind,3:dim(data_occ_gen)[2]])
    gen_now <- colnames(data_occ_gen)[which(data_occ_gen[ind,]==1)]
    data_sites[sk,'occCount'] <- length(gen_now)
    ind_gen <- c()
    for (sk2 in 1:length(gen_now)){
      identified <- which(data_teeth[,'Genus']==gen_now[sk2])
      if (length(identified)==0){
        print(gen_now[sk2])
      }else{
        ind_gen <- c(ind_gen,identified)  
      }
    }
    data_sites[sk,'occCount2'] <- length(ind_gen)
    mn <- round(apply(data_teeth[ind_gen,fets_teeth],2,mean,na.rm = TRUE),digits = 3)
    data_sites[sk,fets_teeth] <- mn
  }
  write.table(data_sites,file = out_sites,quote = FALSE,row.names = FALSE,col.names = TRUE,sep='\t')
  
  pdf('plots_present/fig_hist_HYP.pdf', width = 7, height = 7)
  hist(data_sites[,'HYP'],breaks = 20)
  dev.off()
  
  pdf('plots_present/fig_hist_OT.pdf', width = 7, height = 7)
  hist(data_sites[,'FCT_OT'],breaks = 20)
  dev.off()
  
  pdf('plots_present/fig_hist_SF.pdf', width = 7, height = 7)
  hist(data_sites[,'FCT_SF'],breaks = 20)
  dev.off()
  
  pdf('plots_present/fig_hist_LOP.pdf', width = 7, height = 7)
  hist(data_sites[,'LOP'],breaks = 20)
  dev.off()
  
  pdf('plots_present/fig_hist_OL.pdf', width = 7, height = 7)
  hist(data_sites[,'FCT_OL'],breaks = 20)
  dev.off()
  
  pdf('plots_present/fig_hist_AL.pdf', width = 7, height = 7)
  hist(data_sites[,'FCT_AL'],breaks = 20)
  dev.off()
  
  pdf('plots_present/fig_hist_HOD.pdf', width = 7, height = 7)
  hist(data_sites[,'FCT_HOD'],breaks = 20)
  dev.off()
  
  pdf('plots_present/fig_hist_CM.pdf', width = 7, height = 7)
  hist(data_sites[,'FCT_CM'],breaks = 20)
  dev.off()
  
  
  #division into trainign and testing data, important!
  ind_remove_vertical <- intersect(which(data_sites[,'lon_bio']>25),which(data_sites[,'lon_bio']<27))
  ind_remove_horizontal <- intersect(which(data_sites[,'lat_bio']<1),which(data_sites[,'lat_bio']>(-1)))
  
  ind_east <- which(data_sites[,'lon_bio']>=26)
  ind_south <- which(data_sites[,'lat_bio']<=0)
  
  ind_northeast <- setdiff(ind_east,ind_south)
  ind_southwest <- setdiff(ind_south,ind_east)
  ind_southeast <- intersect(ind_south,ind_east)
  ind_all <- union(ind_south,ind_east)
  if (select_training_data == 'East'){
    ind_test <- setdiff(setdiff(ind_south,ind_east),ind_remove_vertical)
    ind_train <- setdiff(ind_east,ind_remove_vertical)
    file_name_test_train <- 'plots_present/fig_map_train_east.pdf'
  }else{
    if (select_training_data == 'South'){
      ind_test <- setdiff(setdiff(ind_east,ind_south),ind_remove_horizontal)
      ind_train <- setdiff(ind_south,ind_remove_horizontal)
      file_name_test_train <- 'plots_present/fig_map_train_south.pdf'
    }else{
      if (select_training_data == 'Diag'){
        ind_test <- setdiff(setdiff(intersect(ind_east,ind_south),ind_remove_horizontal),ind_remove_vertical)
        ind_train <- setdiff(setdiff(setdiff(union(ind_south,ind_east),ind_test),ind_remove_horizontal),ind_remove_vertical)
        file_name_test_train <- 'plots_present/fig_map_train_diag.pdf'
      }else{
        if (select_training_data == 'Half1'){
          ind_test <- setdiff(setdiff(ind_south,ind_east),ind_remove_vertical)
          ind_train <- setdiff(setdiff(ind_south,ind_test),ind_remove_vertical)
          file_name_test_train <- 'plots_present/fig_map_train_half1.pdf'
        }else{
          if (select_training_data == 'Half2'){
            ind_train <- setdiff(setdiff(ind_south,ind_east),ind_remove_vertical)
            ind_test <- setdiff(setdiff(ind_south,ind_train),ind_remove_vertical)
            file_name_test_train <- 'plots_present/fig_map_train_half2.pdf'
          }else{
            ind_test <- union(ind_east,ind_south)
            ind_train <- ind_test
            file_name_test_train <- 'plots_present/fig_map_train_all.pdf'     
          }
        }
      }
    }
  }
  pdf(file_name_test_train,width = 9,height = 7)
  plot(data_sites[,'lon_bio'],data_sites[,'lat_bio'],pch=20,col = mycolors_cross[1],xaxt='n', ann=FALSE,yaxt='n')
  points(data_sites[ind_train,'lon_bio'],data_sites[ind_train,'lat_bio'],pch=20,col = mycolors_cross[2])
  points(data_sites[ind_test,'lon_bio'],data_sites[ind_test,'lat_bio'],pch=20,col = mycolors_cross[3])
  legend("bottomleft",legend = c('training','testing','discarded'),col=mycolors_cross[c(2,3,1)],pch=15,cex = 2,pt.cex = 3.5,lty=NULL)
  dev.off()
  
  
  data_sites_train <- data_sites[ind_train,]
  data_sites_test <- data_sites[ind_test,]
  
  
  if (do_select_living_relatives){
    data_rel <- read.csv(input_living_relatives, header = TRUE, sep = '\t')
    un_pseudo_gen <- unique(data_rel[,'Relative'])
    data_occ_pseudo_gen <- data_occ_gen[,c(1,2)]
    data_occ_living_gen <- data_occ_gen[,c(1,2)]
    for (sk in 1:length(un_pseudo_gen)){
      pseudo_gen_now <- un_pseudo_gen[sk]
      ind_rel <- which(data_rel[,'Relative'] == pseudo_gen_now)
      gen_rel <- data_rel[ind_rel,'Genus']
      ind_in_mod <- c()
      for (sk2 in 1:length(gen_rel)){
        ind_in_mod <- c(ind_in_mod,which(colnames(data_occ_gen)==gen_rel[sk2]))
      }
      ind_liv <- which(colnames(data_occ_gen) == pseudo_gen_now)
      if (length(gen_rel) == 1){
        moc_occurence <- data_occ_gen[,ind_in_mod]
      }else{
        moc_occurence <- apply(data_occ_gen[ind_in_mod],1,sum)
        ind_moc <- which(moc_occurence>0)
        moc_occurence[ind_moc] <- 1
      }
      true_occurence <- data_occ_gen[,ind_liv]
      data_occ_pseudo_gen <- cbind(data_occ_pseudo_gen,moc_occurence)
      data_occ_living_gen <- cbind(data_occ_living_gen,true_occurence)
      colnames(data_occ_pseudo_gen)[sk+2] <- as.vector(pseudo_gen_now)
      colnames(data_occ_living_gen)[sk+2] <- colnames(data_occ_gen)[ind_liv]
    }
    write.table(data_occ_pseudo_gen,file = out_occurence_pseudo,quote = FALSE,row.names = FALSE,col.names = TRUE,sep='\t')
    write.table(data_occ_living_gen,file = out_occurence_living,quote = FALSE,row.names = FALSE,col.names = TRUE,sep='\t')
  }
  data_occ_pseudo_gen <- read.csv(out_occurence_pseudo, header = TRUE, sep = '\t')
  data_occ_living_gen <- read.csv(out_occurence_living, header = TRUE, sep = '\t')
  
  # check sites fit
  if ((sum(data_sites[,'SITE'] != data_occ_gen[,'SITE']))>0){
    print('site mismatch')
  }else{
    data_occ_gen_train <- data_occ_gen[ind_train,]
    data_occ_gen_test <- data_occ_gen[ind_test,]
    data_occ_pseudo_gen_train <- data_occ_pseudo_gen[ind_train,]
    data_occ_pseudo_gen_test <- data_occ_pseudo_gen[ind_test,]
    data_occ_living_gen_train <- data_occ_living_gen[ind_train,]
    data_occ_living_gen_test <- data_occ_living_gen[ind_test,]
    data_occ_living_as_fosils_gen_train <-  data_occ_living_as_fosils_gen[ind_train,]
    data_occ_living_as_fosils_gen_test <-  data_occ_living_as_fosils_gen[ind_test,]
    print('no genera')
    print(dim(data_occ_gen_train))
    sm <- apply(data_occ_gen_train,2,sum)[2:dim(data_occ_gen_train)[2]]
    ind_occ <- which(sm>0)
    print(length(ind_occ))
    sm <- apply(data_occ_gen_test,2,sum)[2:dim(data_occ_gen_test)[2]]
    ind_occ <- which(sm>0)
    print(length(ind_occ))
    sm <- apply(data_occ_living_gen_train,2,sum)[2:dim(data_occ_living_gen_train)[2]]
    ind_occ <- which(sm>0)
    print(length(ind_occ))
    sm <- apply(data_occ_pseudo_gen_test,2,sum)[2:dim(data_occ_pseudo_gen_test)[2]]
    ind_occ <- which(sm>0)
    print(length(ind_occ))
  }
  
  mean_NPP_gen_train <- c()
  col_gen_train <- colnames(data_occ_gen_train)
  for (sk in 3:length(col_gen_train)){
    gen_now <- col_gen_train[sk]
    ind <- which(data_occ_gen_train[,gen_now] == 1)
    no_occ <- length(ind)
    if (no_occ>0){
      sites_occ_now <- data_occ_gen_train[ind,'SITE']  
      ind_match <- match(sites_occ_now, data_sites_train[,'SITE'])
      mn_NPP <- round(mean(data_sites_train[ind_match,'NPP']) )
    }else{
      mn_NPP <- NA
    }
    mean_NPP_gen_train <- rbind(mean_NPP_gen_train,cbind(gen_now,no_occ,mn_NPP))
  }
  
  print(dim(data_sites_test))
  
  save(mean_NPP_gen_train, file = "mean_NPP_gen_train.RData")
  
  #predict by relatives
  data_sites_test[,'pred_habitat'] <- NA
  col_gen_test <- colnames(data_occ_gen_test)
  for (sk in 1:dim(data_sites_test)[1]){
    site_now <- data_sites_test[sk,'SITE']
    ind <- which(as.vector(data_occ_gen_test[,'SITE'])==site_now)
    gen_occ <- col_gen_test[which(data_occ_gen_test[ind,] == 1)]
    all_NPP <- c()
    for (sk2 in 1:length(gen_occ)){
      ind_animal <- which(mean_NPP_gen_train[,'gen_now'] == gen_occ[sk2])
      all_NPP <- c(all_NPP,mean_NPP_gen_train[ind_animal,'mn_NPP'])
    }
    data_sites_test[sk,'pred_habitat'] <- round(mean(as.numeric(all_NPP),na.rm=TRUE))
  }
  
  R2_habitat  <- 1 - sum((data_sites_test[,'NPP'] - data_sites_test[,'pred_habitat'])^2)/sum((data_sites_test[,'NPP'] - mean(data_sites_test[,'NPP']))^2)
  R2_habitat <- round(R2_habitat,digits = param_round_dg)
  
  
  data_sites_test[,'pred_habitat_relative'] <- NA
  col_gen_test <- colnames(data_occ_pseudo_gen_test)
  for (sk in 1:dim(data_sites_test)[1]){
    site_now <- data_sites_test[sk,'SITE']
    ind <- which(as.vector(data_occ_pseudo_gen_test[,'SITE'])==site_now)
    gen_occ <- col_gen_test[which(data_occ_pseudo_gen_test[ind,] == 1)]
    all_NPP <- c()
    for (sk2 in 1:length(gen_occ)){
      ind_animal <- which(mean_NPP_gen_train[,'gen_now'] == gen_occ[sk2])
      all_NPP <- c(all_NPP,mean_NPP_gen_train[ind_animal,'mn_NPP'])
    }
    data_sites_test[sk,'pred_habitat_relative'] <- round(mean(as.numeric(all_NPP),na.rm=TRUE))
  }
  
  R2_habitat_relative  <- 1 - sum((data_sites_test[,'NPP'] - data_sites_test[,'pred_habitat_relative'])^2)/sum((data_sites_test[,'NPP'] - mean(data_sites_test[,'NPP']))^2)
  R2_habitat_relative <- round(R2_habitat_relative,digits = param_round_dg)
  
  
  data_sites_test[,'pred_habitat_fossils'] <- NA
  col_gen_test <- colnames(data_occ_living_as_fosils_gen_test)
  for (sk in 1:dim(data_sites_test)[1]){
    site_now <- data_sites_test[sk,'SITE']
    ind <- which(as.vector(data_occ_living_as_fosils_gen_test[,'SITE'])==site_now)
    gen_occ <- col_gen_test[which(data_occ_living_as_fosils_gen_test[ind,] == 1)]
    all_NPP <- c()
    for (sk2 in 1:length(gen_occ)){
      ind_animal <- which(mean_NPP_gen_train[,'gen_now'] == gen_occ[sk2])
      all_NPP <- c(all_NPP,mean_NPP_gen_train[ind_animal,'mn_NPP'])
    }
    data_sites_test[sk,'pred_habitat_fossils'] <- round(mean(as.numeric(all_NPP),na.rm=TRUE))
  }
  
  R2_habitat_fossils  <- 1 - sum((data_sites_test[,'NPP'] - data_sites_test[,'pred_habitat_fossils'])^2)/sum((data_sites_test[,'NPP'] - mean(data_sites_test[,'NPP']))^2)
  R2_habitat_fossils <- round(R2_habitat_fossils,digits = param_round_dg)
  
  
  data_sites_train[,'fit_habitat'] <- NA
  col_gen_train <- colnames(data_occ_gen_train)
  for (sk in 1:dim(data_sites_train)[1]){
    site_now <- data_sites_train[sk,'SITE']
    ind <- which(as.vector(data_occ_gen_train[,'SITE'])==site_now)
    gen_occ <- col_gen_train[which(data_occ_gen_train[ind,] == 1)]
    all_NPP <- c()
    for (sk2 in 1:length(gen_occ)){
      ind_animal <- which(mean_NPP_gen_train[,'gen_now'] == gen_occ[sk2])
      all_NPP <- c(all_NPP,mean_NPP_gen_train[ind_animal,'mn_NPP'])
    }
    data_sites_train[sk,'fit_habitat'] <- round(mean(as.numeric(all_NPP),na.rm=TRUE))
  }
  
  R2_habitat_fit  <- 1 - sum((data_sites_train[,'NPP'] - data_sites_train[,'fit_habitat'])^2)/sum((data_sites_train[,'NPP'] - mean(data_sites_train[,'NPP']))^2)
  R2_habitat_fit <- round(R2_habitat_fit,digits = param_round_dg)
  
  
  data_sites_train[,'fit_habitat_relative'] <- NA
  col_gen_train <- colnames(data_occ_living_gen_train)
  for (sk in 1:dim(data_sites_train)[1]){
    site_now <- data_sites_train[sk,'SITE']
    ind <- which(as.vector(data_occ_living_gen_train[,'SITE'])==site_now)
    gen_occ <- col_gen_train[which(data_occ_living_gen_train[ind,] == 1)]
    all_NPP <- c()
    for (sk2 in 1:length(gen_occ)){
      ind_animal <- which(mean_NPP_gen_train[,'gen_now'] == gen_occ[sk2])
      all_NPP <- c(all_NPP,mean_NPP_gen_train[ind_animal,'mn_NPP'])
    }
    data_sites_train[sk,'fit_habitat_relative'] <- round(mean(as.numeric(all_NPP),na.rm=TRUE))
  }
  
  R2_habitat_relative_fit  <- 1 - sum((data_sites_train[,'NPP'] - data_sites_train[,'fit_habitat_relative'])^2)/sum((data_sites_train[,'NPP'] - mean(data_sites_train[,'NPP']))^2)
  R2_habitat_relative_fit <- round(R2_habitat_relative_fit,digits = param_round_dg)
  
  
  
  data_sites_train[,'fit_habitat_fossils'] <- NA
  col_gen_train <- colnames(data_occ_living_as_fosils_gen_train)
  for (sk in 1:dim(data_sites_train)[1]){
    site_now <- data_sites_train[sk,'SITE']
    ind <- which(as.vector(data_occ_living_as_fosils_gen_train[,'SITE'])==site_now)
    gen_occ <- col_gen_train[which(data_occ_living_as_fosils_gen_train[ind,] == 1)]
    all_NPP <- c()
    for (sk2 in 1:length(gen_occ)){
      ind_animal <- which(mean_NPP_gen_train[,'gen_now'] == gen_occ[sk2])
      all_NPP <- c(all_NPP,mean_NPP_gen_train[ind_animal,'mn_NPP'])
    }
    data_sites_train[sk,'fit_habitat_fossils'] <- round(mean(as.numeric(all_NPP),na.rm=TRUE))
  }
  
  R2_habitat_fossils_fit  <- 1 - sum((data_sites_train[,'NPP'] - data_sites_train[,'fit_habitat_fossils'])^2)/sum((data_sites_train[,'NPP'] - mean(data_sites_train[,'NPP']))^2)
  R2_habitat_fossils_fit <- round(R2_habitat_fossils_fit,digits = param_round_dg)
  
  
  
  
  pdf(out_plot_spec_NPP,width = 4, height = 4)
  plot(data_sites_test[,'genCount'],data_sites_test[,'NPP'])
  dev.off()
  

  #taxon based methods
  
  #PLS regression
  library('pls')
  
  #checksum
  if (sum(data_occ_gen_train[,'SITE'] != data_sites_train[,'SITE'])>0){
    print('troblem 3')
  }else{
    if (sum(data_occ_gen_test[,'SITE'] != data_sites_test[,'SITE'])>0){
      print('troblem 4')
    }else{
      ncomp_taxon <- 4
      
      data_taxon_train <- cbind(data_sites_train[,'NPP'],data_occ_gen_train[,3:dim(data_occ_gen_train)[2]])
      data_taxon_test <- cbind(data_sites_test[,'NPP'],data_occ_gen_test[,3:dim(data_occ_gen_test)[2]])
      colnames(data_taxon_train)[1] <- 'NPP'
      colnames(data_taxon_test)[1] <- 'NPP'
      #fit_taxon_pls <- pcr(NPP ~ ., ncomp = 1, data = data_taxon_train)
      fit_taxon_pls <- plsr(NPP ~ ., ncomp = ncomp_taxon, data = data_taxon_train)
      data_sites_test[,'pred_taxon_pls'] <- round(predict(fit_taxon_pls,ncomp = ncomp_taxon, newdata = data_taxon_test))
      
      R2_taxon_pls  <- 1 - sum((data_sites_test[,'NPP'] - data_sites_test[,'pred_taxon_pls'])^2)/sum((data_sites_test[,'NPP'] - mean(data_sites_test[,'NPP']))^2)
      R2_taxon_pls <- round(R2_taxon_pls,digits = param_round_dg)
      R2_taxon_pls_fit  <- 1 - sum((data_sites_train[,'NPP'] - round(predict(fit_taxon_pls,ncomp = ncomp_taxon)))^2)/sum((data_sites_train[,'NPP'] - mean(data_sites_train[,'NPP']))^2)
      R2_taxon_pls_fit <- round(R2_taxon_pls_fit,digits = param_round_dg)
      
      
      data_taxon_living_train <- cbind(data_sites_train[,'NPP'],data_occ_living_gen_train[,3:dim(data_occ_living_gen_train)[2]])
      data_taxon_pseudo_test <- cbind(data_sites_test[,'NPP'],data_occ_pseudo_gen_test[,3:dim(data_occ_pseudo_gen_test)[2]])
      colnames(data_taxon_living_train)[1] <- 'NPP'
      colnames(data_taxon_pseudo_test)[1] <- 'NPP'
      
      fit_taxon_pseudo_pls <- plsr(NPP ~ ., ncomp = ncomp_taxon, data = data_taxon_living_train)
      data_sites_test[,'pred_taxon_pseudo_pls'] <- round(predict(fit_taxon_pseudo_pls,ncomp = ncomp_taxon, newdata = data_taxon_pseudo_test))
      
      R2_taxon_pseudo_pls  <- 1 - sum((data_sites_test[,'NPP'] - data_sites_test[,'pred_taxon_pseudo_pls'])^2)/sum((data_sites_test[,'NPP'] - mean(data_sites_test[,'NPP']))^2)
      R2_taxon_pseudo_pls <- round(R2_taxon_pseudo_pls,digits = param_round_dg)
      R2_taxon_pseudo_pls_fit  <- 1 - sum((data_sites_train[,'NPP'] - round(predict(fit_taxon_pseudo_pls,ncomp = ncomp_taxon)))^2)/sum((data_sites_train[,'NPP'] - mean(data_sites_train[,'NPP']))^2)
      R2_taxon_pseudo_pls_fit <- round(R2_taxon_pseudo_pls_fit,digits = param_round_dg)
      
      
      data_taxon_fossils_train <- cbind(data_sites_train[,'NPP'],data_occ_living_as_fosils_gen_train[,3:dim(data_occ_living_as_fosils_gen_train)[2]])
      data_taxon_fossils_test <- cbind(data_sites_test[,'NPP'],data_occ_living_as_fosils_gen_test[,3:dim(data_occ_living_as_fosils_gen_test)[2]])
      colnames(data_taxon_fossils_train)[1] <- 'NPP'
      colnames(data_taxon_fossils_test)[1] <- 'NPP'
      
      fit_taxon_fossils_pls <- plsr(NPP ~ ., ncomp = ncomp_taxon, data = data_taxon_fossils_train)
      data_sites_test[,'pred_taxon_fossils_pls'] <- round(predict(fit_taxon_fossils_pls,ncomp = ncomp_taxon, newdata = data_taxon_fossils_test))
      
      save(fit_taxon_fossils_pls, file = "fit_taxon_fossils_pls.RData")
      
      R2_taxon_fossils_pls  <- 1 - sum((data_sites_test[,'NPP'] - data_sites_test[,'pred_taxon_fossils_pls'])^2)/sum((data_sites_test[,'NPP'] - mean(data_sites_test[,'NPP']))^2)
      R2_taxon_fossils_pls <- round(R2_taxon_fossils_pls,digits = param_round_dg)
      R2_taxon_fossils_pls_fit  <- 1 - sum((data_sites_train[,'NPP'] - round(predict(fit_taxon_fossils_pls,ncomp = ncomp_taxon)))^2)/sum((data_sites_train[,'NPP'] - mean(data_sites_train[,'NPP']))^2)
      R2_taxon_fossils_pls_fit <- round(R2_taxon_fossils_pls_fit,digits = param_round_dg)
      
      }
  }
  
  pdf('plots_present/fig_R2_taxon_pls.pdf',width = 5, height = 5)
  plot(data_sites_test[,'NPP'],data_sites_test[,'pred_taxon_pls'], xlim = c(0,2400), ylim = c(0,2400))
  dev.off()
  
  #pdf('outputs/fig_R2_taxon_ols.pdf',width = 5, height = 5)
  #plot(data_sites_test[,'NPP'],data_sites_test[,'pred_taxon_ols'], xlim = c(0,2400), ylim = c(0,2400))
  #dev.off()
  
  ncomp_ecom <- 4
  
  fit_ecom_pls <- plsr(NPP ~ HYP + LOP + FCT_SF + FCT_OT + FCT_CM, ncomp = ncomp_ecom, data = data_sites_train)
  #fit_ecom_pls <- plsr(NPP ~ HYP + FCT_AL + FCT_HOD + FCT_OT + FCT_CM, ncomp = ncomp_ecom, data = data_sites_train)
  data_sites_test[,'pred_ecom_pls'] <- round(predict(fit_ecom_pls,ncomp = ncomp_ecom,newdata = data_sites_test))
  
  R2_ecom_pls  <- 1 - sum((data_sites_test[,'NPP'] - data_sites_test[,'pred_ecom_pls'])^2)/sum((data_sites_test[,'NPP'] - mean(data_sites_test[,'NPP']))^2)
  R2_ecom_pls <- round(R2_ecom_pls,digits = param_round_dg)
  
  R2_ecom_pls_fit  <- 1 - sum((data_sites_train[,'NPP'] - round(predict(fit_ecom_pls, ncomp = ncomp_ecom)))^2)/sum((data_sites_train[,'NPP'] - mean(data_sites_train[,'NPP']))^2)
  R2_ecom_pls_fit <- round(R2_ecom_pls_fit,digits = param_round_dg)
  
  pdf('plots_present/fig_validation_ecom_plsr.pdf',width = 8, height = 4)
  plot(RMSEP(fit_ecom_pls), legendpos = "topright")
  dev.off()
  
  pdf('plots_present/fig_R2_ecom_pls.pdf',width = 5, height = 5)
  plot(data_sites_test[,'NPP'],data_sites_test[,'pred_ecom_pls'], xlim = c(0,2400), ylim = c(0,2400))
  dev.off()
  
  print('training')
  print(dim(data_sites_train))
  print('testing')
  print(dim(data_sites_test))
  
  fit_ecom_ols <- lm(NPP ~ FCT_OT, data = data_sites_train)
  #fit_ecom_ols <- lm(NPP ~ HYP + LOP + FCT_SF + FCT_OT + FCT_CM, data = data_sites_train)
  data_sites_test[,'pred_ecom_ols'] <- round(predict(fit_ecom_ols,newdata = data_sites_test))
  
  R2_ecom_ols  <- 1 - sum((data_sites_test[,'NPP'] - data_sites_test[,'pred_ecom_ols'])^2)/sum((data_sites_test[,'NPP'] - mean(data_sites_test[,'NPP']))^2)
  R2_ecom_ols <- round(R2_ecom_ols,digits = param_round_dg)
  
  R2_ecom_ols_fit  <- 1 - sum((data_sites_train[,'NPP'] - round(predict(fit_ecom_ols)))^2)/sum((data_sites_train[,'NPP'] - mean(data_sites_train[,'NPP']))^2)
  R2_ecom_ols_fit <- round(R2_ecom_ols_fit,digits = param_round_dg)
  
  
  fit_ecom_basic_ols <- lm(NPP ~ FCT_OT + FCT_AL, data = data_sites_train)
  data_sites_test[,'pred_ecom_basic_ols'] <- round(predict(fit_ecom_basic_ols,newdata = data_sites_test))
  
  R2_ecom_basic_ols  <- 1 - sum((data_sites_test[,'NPP'] - data_sites_test[,'pred_ecom_basic_ols'])^2)/sum((data_sites_test[,'NPP'] - mean(data_sites_test[,'NPP']))^2)
  R2_ecom_basic_ols <- round(R2_ecom_basic_ols,digits = param_round_dg)
  
  R2_ecom_basic_ols_fit  <- 1 - sum((data_sites_train[,'NPP'] - round(predict(fit_ecom_basic_ols)))^2)/sum((data_sites_train[,'NPP'] - mean(data_sites_train[,'NPP']))^2)
  R2_ecom_basic_ols_fit <- round(R2_ecom_basic_ols_fit,digits = param_round_dg)
  
  
  
  write.table(data_sites_test,file = out_sites_test,quote = FALSE,row.names = FALSE,col.names = TRUE,sep='\t')
  write.table(data_sites_train,file = out_sites_train,quote = FALSE,row.names = FALSE,col.names = TRUE,sep='\t')
  
  results <- c()
  results <- rbind(results,c(select_training_data,'train',R2_habitat_fit,R2_habitat_relative_fit,R2_habitat_fossils_fit,R2_taxon_pls_fit,R2_taxon_pseudo_pls_fit,R2_ecom_pls_fit,R2_ecom_ols_fit,R2_ecom_basic_ols_fit,ncomp_taxon,ncomp_ecom))
  results <- rbind(results,c(select_training_data,'test',R2_habitat,R2_habitat_relative,R2_habitat_fossils,R2_taxon_pls,R2_taxon_pseudo_pls,R2_ecom_pls,R2_ecom_ols,R2_ecom_basic_ols,ncomp_taxon,ncomp_ecom))
  colnames(results) <- c('REG','mode','R2hab','R2habrel','R2habfossils','R2taxonPLS','R2taxonpseudoPLS','R2ecomPLS','R2ecomOLS','R2ecomBAS','NcompTaxon','NcompEcom')
  
  formulas_ecom_pls <- c()
  formulas_ecom_pls <- rbind(formulas_ecom_pls,c(select_training_data,'ecom OLS',ncomp_ecom,round(coef(fit_ecom_ols))))
  formulas_ecom_pls <- rbind(formulas_ecom_pls,c(select_training_data,'ecom PLS',ncomp_ecom,round(as.vector(coef(fit_ecom_pls,intercept = TRUE)))))
  
  formulas_fossils_pls <- c()
  formulas_fossils_pls <- rbind(formulas_fossils_pls,c(select_training_data,'fossils PLS',ncomp_taxon,round(as.vector(coef(fit_taxon_fossils_pls,intercept = TRUE)))))
  colnames(formulas_fossils_pls) <- c('mode','name','ncomp',rownames(coef(fit_taxon_fossils_pls,intercept = TRUE)))
  
  if (select_training_data == 'East'){
    ap_file <- FALSE
    ap_col_names <- TRUE
  }else{
    ap_file <- TRUE
    ap_col_names <- FALSE
  }
  write.table(results,file='outputs_present/results.csv',append=ap_file,quote = FALSE,row.names = FALSE,col.names = ap_col_names,sep='\t')
  write.table(formulas_ecom_pls,file='outputs_present/formulas_ecom_pls.csv',append=ap_file,quote = FALSE,row.names = FALSE,col.names = ap_col_names,sep='\t')
  write.table(formulas_fossils_pls,file='outputs_present/formulas_fossils_taxon_pls.csv',append=ap_file,quote = FALSE,row.names = FALSE,col.names = ap_col_names,sep='\t')
}
