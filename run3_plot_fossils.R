# 2017 11 05 I.Zliobaite
# run plot

input_file <- 'outputs_fossils/ComLocs_selected.csv'

out_data_E <- 'outputs_fossils/dataE.csv'
out_data_W <- 'outputs_fossils/dataW.csv'

data_sum <- read.csv(input_file, header = TRUE, sep = '\t')

dg <- 3
fets_mean <- c('meanHYP','meanHOR','meanAL','meanOL','meanSF','meanOT','meanCM')

#mycolors <-c('#0072bd','#d95319','#edb120','#7e2f8e','#77ac30','#4dbeee','#a2142f')
mycolors_ew <-c('#542788','#fdb863','#e0e0e0')

#ind <- which(data_sum[,'Member'] == "Lokalalei")
#data_sum[ind,'Member'] <- NA

indW <- which(data_sum[,'EastWest'] == 2)
un_mem_W <- unique(data_sum[indW,'Member'])
indNA <- which(is.na(data_sum[indW,'Member']))
levels(data_sum[,'Member']) <- c(levels(data_sum[,'Member']),'Kanapoi')
data_sum[indW[indNA],'Member'] <- 'Kanapoi'
un_mem_W <- unique(data_sum[indW,'Member'])

ind <- which(data_sum[,'Member'] == "Lokochot \226 Tulu Bor")
data_sum[ind,'Member'] <- NA

ind <- which(data_sum[,'Member'] == "Moiti \226 Lokochot")
data_sum[ind,'Member'] <- NA

ind <- which(data_sum[,'Member'] == "KBS \226 Okote")
data_sum[ind,'Member'] <- NA

ind <- which(data_sum[,'Member'] == "Lokochot \357\276\226 Tulu Bor")
data_sum[ind,'Member'] <- NA

ind <- which(data_sum[,'Member'] == "Moiti \357\276\226 Lokochot")
data_sum[ind,'Member'] <- NA

ind <- which(data_sum[,'Member'] == "KBS \357\276\226 Okote")
data_sum[ind,'Member'] <- NA


indE <- which(data_sum[,'EastWest'] == 1)
ind_take <- which(!is.na(data_sum[indE,'Member']))
un_mem_E <- unique(data_sum[indE[ind_take],'Member'])

data_E <- c()
for (sk in 1:length(un_mem_E)){
  ind <- which(as.vector(data_sum[,'Member']) == un_mem_E[sk])
  mnHYP <- round(mean(data_sum[ind,'meanHYP']), digits = dg)
  mnHOR <- round(mean(data_sum[ind,'meanHOR']), digits = dg)
  mnAL <- round(mean(data_sum[ind,'meanAL']), digits = dg)
  mnOL <- round(mean(data_sum[ind,'meanOL']), digits = dg)
  mnSF <- round(mean(data_sum[ind,'meanSF']), digits = dg)
  mnOT <- round(mean(data_sum[ind,'meanOT']), digits = dg)
  mnCM <- round(mean(data_sum[ind,'meanCM']), digits = dg)
  mnLOP <- round(mean(data_sum[ind,'meanLOP']), digits = dg)
  mn_temp_high <- round(mean(data_sum[ind,'temp_high']), digits = dg)
  mn_pred_npp_ecom <- round(mean(data_sum[ind,'pred_npp_ecom']), digits = dg)
  mn_pred_npp_ecomI <- round(mean(data_sum[ind,'pred_npp_ecomI']), digits = dg)
  mn_pred_npp_base_ecom <- round(mean(data_sum[ind,'pred_npp_base_ecom']), digits = dg)
  mn_pred_habitat <- round(mean(data_sum[ind,'pred_habitat']), digits = dg)
  mn_pred_taxa <- round(mean(data_sum[ind,'pred_taxa']), digits = dg)
  
  sdHYP <- round(sd(data_sum[ind,'meanHYP']), digits = dg)
  sdHOR <- round(sd(data_sum[ind,'meanHOR']), digits = dg)
  sdAL <- round(sd(data_sum[ind,'meanAL']), digits = dg)
  sdOL <- round(sd(data_sum[ind,'meanOL']), digits = dg)
  sdSF <- round(sd(data_sum[ind,'meanSF']), digits = dg)
  sdOT <- round(sd(data_sum[ind,'meanOT']), digits = dg)
  sdCM <- round(sd(data_sum[ind,'meanCM']), digits = dg)
  sdLOP <- round(sd(data_sum[ind,'meanLOP']), digits = dg)
  sd_temp_high <- round(sd(data_sum[ind,'temp_high']), digits = dg)
  sd_pred_npp_ecom <- round(sd(data_sum[ind,'pred_npp_ecom']), digits = dg)
  sd_pred_npp_ecomI <- round(sd(data_sum[ind,'pred_npp_ecomI']), digits = dg)
  sd_pred_npp_base_ecom <- round(sd(data_sum[ind,'pred_npp_base_ecom']), digits = dg)
  sd_pred_habitat <- round(sd(data_sum[ind,'pred_habitat']), digits = dg)
  sd_pred_taxa <- round(sd(data_sum[ind,'pred_taxa']), digits = dg)
  
  data_E <- rbind(data_E,cbind(as.vector(un_mem_E[sk]),data_sum[ind[1],'MidMemberAge'],as.vector(data_sum[ind[1],'Formation']),length(ind),mnHYP,mnHOR,mnAL,mnOL,mnSF,mnOT,mnCM,sdHYP,sdHOR,sdAL,sdOL,sdSF,sdOT,sdCM,mn_temp_high,sd_temp_high,mn_pred_npp_ecom,mn_pred_npp_ecomI,sd_pred_npp_ecom,sd_pred_npp_ecomI,mn_pred_npp_base_ecom,sd_pred_npp_base_ecom,mnLOP,sdLOP,mn_pred_habitat,sd_pred_habitat,mn_pred_taxa,sd_pred_taxa))
}
colnames(data_E)[1:4] <- c('Member','MidMemberAge','Formation','noComLocs')

ord <- order(data_E[,'MidMemberAge'],decreasing = TRUE)
data_E <- data_E[ord,]

write.table(data_E,file = out_data_E,quote = FALSE,row.names = FALSE,col.names = TRUE,sep='\t')
data_E <- read.csv(out_data_E, header = TRUE, sep = '\t')


data_W <- c()
for (sk in 1:length(un_mem_W)){
  ind <- which(as.vector(data_sum[,'Member']) == un_mem_W[sk])
  mnHYP <- round(mean(data_sum[ind,'meanHYP']), digits = dg)
  mnHOR <- round(mean(data_sum[ind,'meanHOR']), digits = dg)
  mnAL <- round(mean(data_sum[ind,'meanAL']), digits = dg)
  mnOL <- round(mean(data_sum[ind,'meanOL']), digits = dg)
  mnSF <- round(mean(data_sum[ind,'meanSF']), digits = dg)
  mnOT <- round(mean(data_sum[ind,'meanOT']), digits = dg)
  mnCM <- round(mean(data_sum[ind,'meanCM']), digits = dg)
  mnLOP <- round(mean(data_sum[ind,'meanLOP']), digits = dg)
  mn_temp_high <- round(mean(data_sum[ind,'temp_high']), digits = dg)
  mn_pred_npp_ecom <- round(mean(data_sum[ind,'pred_npp_ecom']), digits = dg)
  mn_pred_npp_ecomI <- round(mean(data_sum[ind,'pred_npp_ecomI']), digits = dg)
  mn_pred_npp_base_ecom <- round(mean(data_sum[ind,'pred_npp_base_ecom']), digits = dg)
  mn_pred_habitat <- round(mean(data_sum[ind,'pred_habitat']), digits = dg)
  mn_pred_taxa <- round(mean(data_sum[ind,'pred_taxa']), digits = dg)
  
  sdHYP <- round(sd(data_sum[ind,'meanHYP']), digits = dg)
  sdHOR <- round(sd(data_sum[ind,'meanHOR']), digits = dg)
  sdAL <- round(sd(data_sum[ind,'meanAL']), digits = dg)
  sdOL <- round(sd(data_sum[ind,'meanOL']), digits = dg)
  sdSF <- round(sd(data_sum[ind,'meanSF']), digits = dg)
  sdOT <- round(sd(data_sum[ind,'meanOT']), digits = dg)
  sdCM <- round(sd(data_sum[ind,'meanCM']), digits = dg)
  sdLOP <- round(sd(data_sum[ind,'meanLOP']), digits = dg)
  sd_temp_high <- round(sd(data_sum[ind,'temp_high']), digits = dg)
  sd_pred_npp_ecom <- round(sd(data_sum[ind,'pred_npp_ecom']), digits = dg)
  sd_pred_npp_ecomI <- round(sd(data_sum[ind,'pred_npp_ecomI']), digits = dg)
  sd_pred_npp_base_ecom <- round(sd(data_sum[ind,'pred_npp_base_ecom']), digits = dg)
  sd_pred_habitat <- round(sd(data_sum[ind,'pred_habitat']), digits = dg)
  sd_pred_taxa <- round(sd(data_sum[ind,'pred_taxa']), digits = dg)
  
  
  data_W <- rbind(data_W,cbind(as.vector(un_mem_W[sk]),data_sum[ind[1],'MidMemberAge'],as.vector(data_sum[ind[1],'Formation']),length(ind),mnHYP,mnHOR,mnAL,mnOL,mnSF,mnOT,mnCM,sdHYP,sdHOR,sdAL,sdOL,sdSF,sdOT,sdCM,mn_temp_high,sd_temp_high,mn_pred_npp_ecom,mn_pred_npp_ecomI,sd_pred_npp_ecom,sd_pred_npp_ecomI,mn_pred_npp_base_ecom,sd_pred_npp_base_ecom,mnLOP,sdLOP,mn_pred_habitat,sd_pred_habitat,mn_pred_taxa,sd_pred_taxa))
}
colnames(data_W)[1:4] <- c('Member','MidMemberAge','Formation','noComLocs')

ord <- order(data_W[,'MidMemberAge'],decreasing = TRUE)
data_W <- data_W[ord,]

write.table(data_W,file = out_data_W,quote = FALSE,row.names = FALSE,col.names = TRUE,sep='\t')
data_W <- read.csv(out_data_W, header = TRUE, sep = '\t')

ht <- 3.5
ht2 <- 4.2
wd <- 7
lmlo <- 0
lmhi <- 2500
xlo <- 1
xhi <- 8
cx <- 0.8
cxtx <- 0.5
cxmn <- 0.8
  
pdf('plots_fossils/fig_HYP.pdf',width = wd, height = ht)
plot(data_E[,'MidMemberAge'],data_E[,'mnHYP'], ylim=c(1,3), xlim = c(xlo,xhi), xlab='',xaxt='n', pch=19, ylab="mean hypsodonty (HYP)", col=mycolors_ew[3], cex.lab=cx, cex.axis = cx)
arrows(data_E[,'MidMemberAge'], data_E[,'mnHYP']-data_E[,'sdHYP'], data_E[,'MidMemberAge'], data_E[,'mnHYP']+data_E[,'sdHYP'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
arrows(data_W[,'MidMemberAge'], data_W[,'mnHYP']-data_W[,'sdHYP'], data_W[,'MidMemberAge'], data_W[,'mnHYP']+data_W[,'sdHYP'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
points(data_E[,'MidMemberAge'],data_E[,'mnHYP'], pch=19,col=mycolors_ew[1])
lines(data_E[,'MidMemberAge'],data_E[,'mnHYP'],col=mycolors_ew[1],lwd = 3)
points(data_W[,'MidMemberAge'],data_W[,'mnHYP'], pch=19,col=mycolors_ew[2])
lines(data_W[,'MidMemberAge'],data_W[,'mnHYP'],col=mycolors_ew[2],lwd = 3)
text(data_E[,'MidMemberAge'],data_E[,'mnHYP'],data_E[,'Member'],cex = cxtx, pos = 3,col=mycolors_ew[1])
text(data_W[,'MidMemberAge'],data_W[,'mnHYP'],data_W[,'Member'],cex = cxtx, pos = 1,col=mycolors_ew[2])
dev.off()


pdf('plots_fossils/fig_HOR.pdf',width = wd, height = ht)
plot(data_E[,'MidMemberAge'],data_E[,'mnHOR'], ylim=c(1,2), xlim = c(xlo,xhi), pch=19, xlab='',xaxt='n', ylab="mean horizodonty (HOR)", col=mycolors_ew[3], cex.lab=cx, cex.axis = cx)
arrows(data_E[,'MidMemberAge'], data_E[,'mnHOR']-data_E[,'sdHOR'], data_E[,'MidMemberAge'], data_E[,'mnHOR']+data_E[,'sdHOR'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
arrows(data_W[,'MidMemberAge'], data_W[,'mnHOR']-data_W[,'sdHOR'], data_W[,'MidMemberAge'], data_W[,'mnHOR']+data_W[,'sdHOR'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
points(data_E[,'MidMemberAge'],data_E[,'mnHOR'], pch=19,col=mycolors_ew[1])
lines(data_E[,'MidMemberAge'],data_E[,'mnHOR'],col=mycolors_ew[1],lwd = 3)
points(data_W[,'MidMemberAge'],data_W[,'mnHOR'], pch=19,col=mycolors_ew[2])
lines(data_W[,'MidMemberAge'],data_W[,'mnHOR'],col=mycolors_ew[2],lwd = 3)
text(data_E[,'MidMemberAge'],data_E[,'mnHOR'],data_E[,'Member'],cex = cxtx, pos = 3,col=mycolors_ew[1])
text(data_W[,'MidMemberAge'],data_W[,'mnHOR'],data_W[,'Member'],cex = cxtx, pos = 1,col=mycolors_ew[2])
dev.off()

pdf('plots_fossils/fig_AL.pdf',width = wd, height = ht)
plot(data_E[,'MidMemberAge'],data_E[,'mnAL'], ylim=c(0,0.2), xlim = c(xlo,xhi), pch=19, xlab="",xaxt='n', ylab="mean acute lophs (AL)", col=mycolors_ew[3], cex.lab=cx, cex.axis = cx)
arrows(data_E[,'MidMemberAge'], data_E[,'mnAL']-data_E[,'sdAL'], data_E[,'MidMemberAge'], data_E[,'mnAL']+data_E[,'sdAL'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
arrows(data_W[,'MidMemberAge'], data_W[,'mnAL']-data_W[,'sdAL'], data_W[,'MidMemberAge'], data_W[,'mnAL']+data_W[,'sdAL'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
points(data_E[,'MidMemberAge'],data_E[,'mnAL'], pch=19,col=mycolors_ew[1])
lines(data_E[,'MidMemberAge'],data_E[,'mnAL'],col=mycolors_ew[1],lwd = 3)
points(data_W[,'MidMemberAge'],data_W[,'mnAL'], pch=19,col=mycolors_ew[2])
lines(data_W[,'MidMemberAge'],data_W[,'mnAL'],col=mycolors_ew[2],lwd = 3)
text(data_E[,'MidMemberAge'],data_E[,'mnAL'],data_E[,'Member'],cex = cxtx, pos = 3,col=mycolors_ew[1])
text(data_W[,'MidMemberAge'],data_W[,'mnAL'],data_W[,'Member'],cex = cxtx, pos = 1,col=mycolors_ew[2])
dev.off()

pdf('plots_fossils/fig_OL.pdf',width = wd, height = ht)
plot(data_E[,'MidMemberAge'],data_E[,'mnOL'], ylim=c(0.4,1.1), xlim = c(xlo,xhi), pch=19, xlab="",xaxt='n',ylab="mean obtuse lophs (OL)", col=mycolors_ew[3], cex.lab=cx, cex.axis = cx)
arrows(data_E[,'MidMemberAge'], data_E[,'mnOL']-data_E[,'sdOL'], data_E[,'MidMemberAge'], data_E[,'mnOL']+data_E[,'sdOL'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
arrows(data_W[,'MidMemberAge'], data_W[,'mnOL']-data_W[,'sdOL'], data_W[,'MidMemberAge'], data_W[,'mnOL']+data_W[,'sdOL'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
points(data_E[,'MidMemberAge'],data_E[,'mnOL'], pch=19,col=mycolors_ew[1])
lines(data_E[,'MidMemberAge'],data_E[,'mnOL'],col=mycolors_ew[1],lwd = 3)
points(data_W[,'MidMemberAge'],data_W[,'mnOL'], pch=19,col=mycolors_ew[2])
lines(data_W[,'MidMemberAge'],data_W[,'mnOL'],col=mycolors_ew[2],lwd = 3)
text(data_E[,'MidMemberAge'],data_E[,'mnOL'],data_E[,'Member'],cex = cxtx, pos = 3,col=mycolors_ew[1])
text(data_W[,'MidMemberAge'],data_W[,'mnOL'],data_W[,'Member'],cex = cxtx, pos = 1,col=mycolors_ew[2])
dev.off()

pdf('plots_fossils/fig_SF.pdf',width = wd, height = ht)
plot(data_E[,'MidMemberAge'],data_E[,'mnSF'], ylim=c(0,0.8), xlim = c(xlo,xhi), pch=19, xlab="",xaxt='n', ylab="mean st. fortification (SF)", col=mycolors_ew[3], cex.lab=cx, cex.axis = cx)
arrows(data_E[,'MidMemberAge'], data_E[,'mnSF']-data_E[,'sdSF'], data_E[,'MidMemberAge'], data_E[,'mnSF']+data_E[,'sdSF'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
arrows(data_W[,'MidMemberAge'], data_W[,'mnSF']-data_W[,'sdSF'], data_W[,'MidMemberAge'], data_W[,'mnSF']+data_W[,'sdSF'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
points(data_E[,'MidMemberAge'],data_E[,'mnSF'], pch=19,col=mycolors_ew[1])
lines(data_E[,'MidMemberAge'],data_E[,'mnSF'],col=mycolors_ew[1],lwd = 3)
points(data_W[,'MidMemberAge'],data_W[,'mnSF'], pch=19,col=mycolors_ew[2])
lines(data_W[,'MidMemberAge'],data_W[,'mnSF'],col=mycolors_ew[2],lwd = 3)
text(data_E[,'MidMemberAge'],data_E[,'mnSF'],data_E[,'Member'],cex = cxtx, pos = 3,col=mycolors_ew[1])
text(data_W[,'MidMemberAge'],data_W[,'mnSF'],data_W[,'Member'],cex = cxtx, pos = 1,col=mycolors_ew[2])
dev.off()

pdf('plots_fossils/fig_OT.pdf',width = wd, height = ht)
plot(data_E[,'MidMemberAge'],data_E[,'mnOT'], ylim=c(0,0.8), xlim = c(xlo,xhi), pch=19, xlab="",xaxt='n', ylab="mean flat topography (OT)", col=mycolors_ew[3], cex.lab=cx, cex.axis = cx)
arrows(data_E[,'MidMemberAge'], data_E[,'mnOT']-data_E[,'sdOT'], data_E[,'MidMemberAge'], data_E[,'mnOT']+data_E[,'sdOT'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
arrows(data_W[,'MidMemberAge'], data_W[,'mnOT']-data_W[,'sdOT'], data_W[,'MidMemberAge'], data_W[,'mnOT']+data_W[,'sdOT'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
points(data_E[,'MidMemberAge'],data_E[,'mnOT'], pch=19,col=mycolors_ew[1])
lines(data_E[,'MidMemberAge'],data_E[,'mnOT'],col=mycolors_ew[1],lwd = 3)
points(data_W[,'MidMemberAge'],data_W[,'mnOT'], pch=19,col=mycolors_ew[2])
lines(data_W[,'MidMemberAge'],data_W[,'mnOT'],col=mycolors_ew[2],lwd = 3)
text(data_E[,'MidMemberAge'],data_E[,'mnOT'],data_E[,'Member'],cex = cxtx, pos = 3,col=mycolors_ew[1])
text(data_W[,'MidMemberAge'],data_W[,'mnOT'],data_W[,'Member'],cex = cxtx, pos = 1,col=mycolors_ew[2])
dev.off()


pdf('plots_fossils/fig_CM.pdf',width = wd, height = (ht+0.05))
plot(data_E[,'MidMemberAge'],data_E[,'mnCM'], ylim=c(0,1), xlim = c(xlo,xhi), pch=19, xlab="Age, MA", ylab="mean cement (CM)", col=mycolors_ew[3], cex.lab=cx, cex.axis = cx)
arrows(data_E[,'MidMemberAge'], data_E[,'mnCM']-data_E[,'sdCM'], data_E[,'MidMemberAge'], data_E[,'mnCM']+data_E[,'sdCM'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
arrows(data_W[,'MidMemberAge'], data_W[,'mnCM']-data_W[,'sdCM'], data_W[,'MidMemberAge'], data_W[,'mnCM']+data_W[,'sdCM'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
points(data_E[,'MidMemberAge'],data_E[,'mnCM'], pch=19,col=mycolors_ew[1])
lines(data_E[,'MidMemberAge'],data_E[,'mnCM'],col=mycolors_ew[1],lwd = 3)
points(data_W[,'MidMemberAge'],data_W[,'mnCM'], pch=19,col=mycolors_ew[2])
lines(data_W[,'MidMemberAge'],data_W[,'mnCM'],col=mycolors_ew[2],lwd = 3)
text(data_E[,'MidMemberAge'],data_E[,'mnCM'],data_E[,'Member'],cex = cxtx, pos = 3,col=mycolors_ew[1])
text(data_W[,'MidMemberAge'],data_W[,'mnCM'],data_W[,'Member'],cex = cxtx, pos = 1,col=mycolors_ew[2])
dev.off()

pdf('plots_fossils/fig_temp_high.pdf',width = wd, height = ht)
plot(data_E[,'MidMemberAge'],data_E[,'mn_temp_high'], ylim=c(0,50), xlim = c(xlo,xhi), pch=19, xlab="Age, MA", main = 'Average temperature of the hottest day of a month', ylab="mean _temp_high", col=mycolors_ew[3], cex.lab=cx, cex.axis = cx)
arrows(data_E[,'MidMemberAge'], data_E[,'mn_temp_high']-data_E[,'sd_temp_high'], data_E[,'MidMemberAge'], data_E[,'mn_temp_high']+data_E[,'sd_temp_high'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
arrows(data_W[,'MidMemberAge'], data_W[,'mn_temp_high']-data_W[,'sd_temp_high'], data_W[,'MidMemberAge'], data_W[,'mn_temp_high']+data_W[,'sd_temp_high'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
points(data_E[,'MidMemberAge'],data_E[,'mn_temp_high'], pch=19,col=mycolors_ew[1])
lines(data_E[,'MidMemberAge'],data_E[,'mn_temp_high'],col=mycolors_ew[1],lwd = 3)
points(data_W[,'MidMemberAge'],data_W[,'mn_temp_high'], pch=19,col=mycolors_ew[2])
lines(data_W[,'MidMemberAge'],data_W[,'mn_temp_high'],col=mycolors_ew[2],lwd = 3)
text(data_E[,'MidMemberAge'],data_E[,'mn_temp_high'],data_E[,'Member'],cex = cxtx, pos = 3,col=mycolors_ew[1])
text(data_W[,'MidMemberAge'],data_W[,'mn_temp_high'],data_W[,'Member'],cex = cxtx, pos = 1,col=mycolors_ew[2])
dev.off()

pdf('plots_fossils/fig_pred_npp_ecom.pdf',width = wd, height = (ht2+0.07))
op <- par(mar=c(5, 6, 4, 2) + 0.1)
plot(data_E[,'MidMemberAge'],data_E[,'mn_pred_npp_ecom'], ylim=c(lmlo,lmhi), xlim = c(xlo,xhi), pch=19, xlab="Age, MA", ylab="Ecometric, Fortelius et al 2016\nPredicted NPP", col=mycolors_ew[3], cex.lab=cx, cex.axis = cx,cex.main = cxmn)
arrows(data_E[,'MidMemberAge'], data_E[,'mn_pred_npp_ecom']-data_E[,'sd_pred_npp_ecom'], data_E[,'MidMemberAge'], data_E[,'mn_pred_npp_ecom']+data_E[,'sd_pred_npp_ecom'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
arrows(data_W[,'MidMemberAge'], data_W[,'mn_pred_npp_ecom']-data_W[,'sd_pred_npp_ecom'], data_W[,'MidMemberAge'], data_W[,'mn_pred_npp_ecom']+data_W[,'sd_pred_npp_ecom'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
points(data_E[,'MidMemberAge'],data_E[,'mn_pred_npp_ecom'], pch=19,col=mycolors_ew[1])
lines(data_E[,'MidMemberAge'],data_E[,'mn_pred_npp_ecom'],col=mycolors_ew[1],lwd = 3)
points(data_W[,'MidMemberAge'],data_W[,'mn_pred_npp_ecom'], pch=19,col=mycolors_ew[2])
lines(data_W[,'MidMemberAge'],data_W[,'mn_pred_npp_ecom'],col=mycolors_ew[2],lwd = 3)
text(data_E[,'MidMemberAge'],data_E[,'mn_pred_npp_ecom'],data_E[,'Member'],cex = cxtx, pos = 3,col=mycolors_ew[1])
text(data_W[,'MidMemberAge'],data_W[,'mn_pred_npp_ecom'],data_W[,'Member'],cex = cxtx, pos = 1,col=mycolors_ew[2])
par(op)
dev.off()

pdf('plots_fossils/fig_pred_npp_ecomI.pdf',width = wd, height = ht2)
op <- par(mar=c(5, 6, 4, 2) + 0.1)
plot(data_E[,'MidMemberAge'],data_E[,'mn_pred_npp_ecomI'], ylim=c(lmlo,lmhi), xlim = c(xlo,xhi), pch=19, xlab="", xaxt='n', ylab="Ecometric model I\n Predicted NPP", col=mycolors_ew[3], cex.lab=cx, cex.axis = cx)
arrows(data_E[,'MidMemberAge'], data_E[,'mn_pred_npp_ecomI']-data_E[,'sd_pred_npp_ecomI'], data_E[,'MidMemberAge'], data_E[,'mn_pred_npp_ecomI']+data_E[,'sd_pred_npp_ecomI'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
arrows(data_W[,'MidMemberAge'], data_W[,'mn_pred_npp_ecomI']-data_W[,'sd_pred_npp_ecomI'], data_W[,'MidMemberAge'], data_W[,'mn_pred_npp_ecomI']+data_W[,'sd_pred_npp_ecomI'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
points(data_E[,'MidMemberAge'],data_E[,'mn_pred_npp_ecomI'], pch=19,col=mycolors_ew[1])
lines(data_E[,'MidMemberAge'],data_E[,'mn_pred_npp_ecomI'],col=mycolors_ew[1],lwd = 3)
points(data_W[,'MidMemberAge'],data_W[,'mn_pred_npp_ecomI'], pch=19,col=mycolors_ew[2])
lines(data_W[,'MidMemberAge'],data_W[,'mn_pred_npp_ecomI'],col=mycolors_ew[2],lwd = 3)
text(data_E[,'MidMemberAge'],data_E[,'mn_pred_npp_ecomI'],data_E[,'Member'],cex = cxtx, pos = 3,col=mycolors_ew[1])
text(data_W[,'MidMemberAge'],data_W[,'mn_pred_npp_ecomI'],data_W[,'Member'],cex = cxtx, pos = 1,col=mycolors_ew[2])
par(op)
dev.off()


pdf('plots_fossils/fig_pred_npp_base_ecom.pdf',width = wd, height = (ht2+0.07))
op <- par(mar=c(5, 6, 4, 2) + 0.1)
#plot(data_E[,'MidMemberAge'],data_E[,'mn_pred_npp_base_ecom'], ylim=c(lmlo,lmhi), xlim = c(xlo,xhi), pch=19, xlab="", main = 'Ecometric approach: hypsodonty (HYP) only', ylab="predicted NPP", col=mycolors_ew[3], cex.lab=cx, cex.axis = cx,cex.main = cxmn)
#plot(data_E[,'MidMemberAge'],data_E[,'mn_pred_npp_base_ecom'], ylim=c(lmlo,lmhi), xlim = c(xlo,xhi), pch=19, xlab="", main = 'Ecometric approach: flat topography (OT) only', ylab="predicted NPP", col=mycolors[1], cex.lab=cx, cex.axis = cx,cex.main = cxmn)
plot(data_E[,'MidMemberAge'],data_E[,'mn_pred_npp_base_ecom'], ylim=c(lmlo,lmhi), xlim = c(xlo,xhi), pch=19, xlab="Age, MA", ylab="Ecometric model OT and AL\nPredicted NPP", col=mycolors_ew[1], cex.lab=cx, cex.axis = cx,cex.main = cxmn)
arrows(data_E[,'MidMemberAge'], data_E[,'mn_pred_npp_base_ecom']-data_E[,'sd_pred_npp_base_ecom'], data_E[,'MidMemberAge'], data_E[,'mn_pred_npp_base_ecom']+data_E[,'sd_pred_npp_base_ecom'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
arrows(data_W[,'MidMemberAge'], data_W[,'mn_pred_npp_base_ecom']-data_W[,'sd_pred_npp_base_ecom'], data_W[,'MidMemberAge'], data_W[,'mn_pred_npp_base_ecom']+data_W[,'sd_pred_npp_base_ecom'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
points(data_E[,'MidMemberAge'],data_E[,'mn_pred_npp_base_ecom'], pch=19,col=mycolors_ew[1])
lines(data_E[,'MidMemberAge'],data_E[,'mn_pred_npp_base_ecom'],col=mycolors_ew[1],lwd = 3)
points(data_W[,'MidMemberAge'],data_W[,'mn_pred_npp_base_ecom'], pch=19,col=mycolors_ew[2])
lines(data_W[,'MidMemberAge'],data_W[,'mn_pred_npp_base_ecom'],col=mycolors_ew[2],lwd = 3)
text(data_E[,'MidMemberAge'],data_E[,'mn_pred_npp_base_ecom'],data_E[,'Member'],cex = cxtx, pos = 3,col=mycolors_ew[1])
text(data_W[,'MidMemberAge'],data_W[,'mn_pred_npp_base_ecom'],data_W[,'Member'],cex = cxtx, pos = 1,col=mycolors_ew[2])
par(op)
dev.off()

pdf('plots_fossils/fig_LOP.pdf',width = wd, height = ht)
plot(data_E[,'MidMemberAge'],data_E[,'mnLOP'], ylim=c(0,2), xlim = c(xlo,xhi), pch=19, xlab="",xaxt='n', ylab="mean lophedness (LOP)", col=mycolors_ew[3], cex.lab=cx, cex.axis = cx)
arrows(data_E[,'MidMemberAge'], data_E[,'mnLOP']-data_E[,'sdLOP'], data_E[,'MidMemberAge'], data_E[,'mnLOP']+data_E[,'sdLOP'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
arrows(data_W[,'MidMemberAge'], data_W[,'mnLOP']-data_W[,'sdLOP'], data_W[,'MidMemberAge'], data_W[,'mnLOP']+data_W[,'sdLOP'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
points(data_E[,'MidMemberAge'],data_E[,'mnLOP'], pch=19,col=mycolors_ew[1])
lines(data_E[,'MidMemberAge'],data_E[,'mnLOP'],col=mycolors_ew[1],lwd = 3)
points(data_W[,'MidMemberAge'],data_W[,'mnLOP'], pch=19,col=mycolors_ew[2])
lines(data_W[,'MidMemberAge'],data_W[,'mnLOP'],col=mycolors_ew[2],lwd = 3)
text(data_E[,'MidMemberAge'],data_E[,'mnLOP'],data_E[,'Member'],cex = cxtx, pos = 3,col=mycolors_ew[1])
text(data_W[,'MidMemberAge'],data_W[,'mnLOP'],data_W[,'Member'],cex = cxtx, pos = 1,col=mycolors_ew[2])
dev.off()

pdf('plots_fossils/fig_pred_habitat.pdf',width = wd, height = ht2)
op <- par(mar=c(5, 6, 4, 2) + 0.1)
plot(data_E[,'MidMemberAge'],data_E[,'mn_pred_habitat'], ylim=c(lmlo,lmhi), xlim = c(xlo,xhi), pch=19, xlab="", xaxt='n', ylab="Mean habitat approach\nPredicted NPP", col=mycolors_ew[3], cex.lab=cx, cex.axis = cx, cex.main = cxmn)
arrows(data_E[,'MidMemberAge'], data_E[,'mn_pred_habitat']-data_E[,'sd_pred_habitat'], data_E[,'MidMemberAge'], data_E[,'mn_pred_habitat']+data_E[,'sd_pred_habitat'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
arrows(data_W[,'MidMemberAge'], data_W[,'mn_pred_habitat']-data_W[,'sd_pred_habitat'], data_W[,'MidMemberAge'], data_W[,'mn_pred_habitat']+data_W[,'sd_pred_habitat'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
points(data_E[,'MidMemberAge'],data_E[,'mn_pred_habitat'], pch=19,col=mycolors_ew[1])
lines(data_E[,'MidMemberAge'],data_E[,'mn_pred_habitat'],col=mycolors_ew[1],lwd = 3)
points(data_W[,'MidMemberAge'],data_W[,'mn_pred_habitat'], pch=19,col=mycolors_ew[2])
lines(data_W[,'MidMemberAge'],data_W[,'mn_pred_habitat'],col=mycolors_ew[2],lwd = 3)
text(data_E[,'MidMemberAge'],data_E[,'mn_pred_habitat'],data_E[,'Member'],cex = cxtx, pos = 3,col=mycolors_ew[1])
text(data_W[,'MidMemberAge'],data_W[,'mn_pred_habitat'],data_W[,'Member'],cex = cxtx, pos = 1,col=mycolors_ew[2])
par(op)
dev.off()

pdf('plots_fossils/fig_pred_taxa.pdf',width = wd, height = ht2)
op <- par(mar=c(5, 6, 4, 2) + 0.1)
plot(data_E[,'MidMemberAge'],data_E[,'mn_pred_taxa'], ylim=c(lmlo,lmhi), xlim = c(xlo,xhi), pch=19, xlab="",  xaxt='n', ylab="Taxon assemblage approach\nPredicted NPP", col=mycolors_ew[3], cex.lab=cx, cex.axis = cx, cex.main = cxmn)
arrows(data_E[,'MidMemberAge'], data_E[,'mn_pred_taxa']-data_E[,'sd_pred_taxa'], data_E[,'MidMemberAge'], data_E[,'mn_pred_taxa']+data_E[,'sd_pred_taxa'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
arrows(data_W[,'MidMemberAge'], data_W[,'mn_pred_taxa']-data_W[,'sd_pred_taxa'], data_W[,'MidMemberAge'], data_W[,'mn_pred_taxa']+data_W[,'sd_pred_taxa'], length=0.05, angle=90, code=3,col=mycolors_ew[3])
points(data_E[,'MidMemberAge'],data_E[,'mn_pred_taxa'], pch=19,col=mycolors_ew[1])
lines(data_E[,'MidMemberAge'],data_E[,'mn_pred_taxa'],col=mycolors_ew[1],lwd = 3)
points(data_W[,'MidMemberAge'],data_W[,'mn_pred_taxa'], pch=19,col=mycolors_ew[2])
lines(data_W[,'MidMemberAge'],data_W[,'mn_pred_taxa'],col=mycolors_ew[2],lwd = 3)
text(data_E[,'MidMemberAge'],data_E[,'mn_pred_taxa'],data_E[,'Member'],cex = cxtx, pos = 3,col=mycolors_ew[1])
text(data_W[,'MidMemberAge'],data_W[,'mn_pred_taxa'],data_W[,'Member'],cex = cxtx, pos = 1,col=mycolors_ew[2])
par(op)
dev.off()
