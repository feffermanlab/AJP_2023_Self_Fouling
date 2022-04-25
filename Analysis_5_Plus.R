library(ggplot2)

ggplot() + geom_smooth(data=Results[Results$Md == 0.0,], 
                       method="loess", aes(x=as.numeric(Dis), y=(ParaMX/(max(ParaMX))))) + 
  geom_smooth(data=Results[Results$Md == 0.0,], method="loess", 
              aes(x=as.numeric(Dis), y=(TravT/(max(TravT))))) + 
  geom_point(data=Results[Results$Md == 0.0,], 
             aes(x=as.numeric(Dis), y=(ParaMX/(max(ParaMX))))) + 
  geom_point(data=Results[Results$Md == 0.0,], 
             aes(x=as.numeric(Dis), y=(TravT/(max(TravT))))) + 
  facet_grid(. ~ Catch, labeller = labeller(Catch = catch.labs)) 

Fit <- as.data.frame(rbind(c(8.27, 9.39, 7.25, 16), c(8.11, 8.49, 7.60, 32), c(8.21, 8.53, 7.81, 64)))
names(Fit) <- c("Def", "Ttwice", "Ptwice", "Catch")

ggplot() + geom_smooth(data=Results[Results$Md == 0.0 & Results$Catch == 16,], method = "gam", formula = y ~ s(x, k=9), aes(x=as.numeric(Dis), y=normalize(ParaMX), color='blue')) + 
  geom_smooth(data=Results[Results$Md == 0.0 & Results$Catch == 16,], method = "gam", formula = y ~ s(x, k=9), 
              aes(x=as.numeric(Dis), y=normalize(TravT), color='red'))  + geom_smooth(data=Results[Results$Md == 0.0 & Results$Catch == 32,], method = "gam", formula = y ~ s(x, k=9), aes(x=as.numeric(Dis), y=normalize(ParaMX), color='blue')) + 
  geom_smooth(data=Results[Results$Md == 0.0 & Results$Catch == 32,], method = "gam", formula = y ~ s(x, k=9), 
              aes(x=as.numeric(Dis), y=normalize(TravT), color='red')) + geom_smooth(data=Results[Results$Md == 0.0 & Results$Catch == 64,], method = "gam", formula = y ~ s(x, k=9), aes(x=as.numeric(Dis), y=normalize(ParaMX), color='blue')) + 
  geom_smooth(data=Results[Results$Md == 0.0 & Results$Catch == 64,], method = "gam", formula = y ~ s(x, k=9), 
              aes(x=as.numeric(Dis), y=normalize(TravT), color='red'))+ 
  facet_grid(Catch ~ ., labeller = labeller(Catch = catch.labs)) + geom_vline(xintercept=8.5, lwd=1, linetype="dashed", color="red") +  labs(color="Variable", labels = c("Parasites", "Travel"), size=21, x = "Tolerance", y = "Percent, Relative to Maximum") + 
  theme_classic() + theme(text=element_text(size=20)) +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),  panel.grid.major.y = element_line(color = "grey60", linetype='dashed')) + theme(panel.spacing = unit(1, "lines"))

ggplot() + geom_smooth(data=Results[Results$Md == 0.0 & Results$Catch == 16,], method = "gam", formula = y ~ s(x, k=9), aes(x=as.numeric(Dis), y=normalize(ParaMX), color='blue')) + 
  geom_smooth(data=Results[Results$Md == 0.0 & Results$Catch == 16,], method = "gam", formula = y ~ s(x, k=9), 
              aes(x=as.numeric(Dis), y=normalize(TravT), color='red'))  + geom_smooth(data=Results[Results$Md == 0.0 & Results$Catch == 32,], method = "gam", formula = y ~ s(x, k=9), aes(x=as.numeric(Dis), y=normalize(ParaMX), color='blue')) + 
  geom_smooth(data=Results[Results$Md == 0.0 & Results$Catch == 32,], method = "gam", formula = y ~ s(x, k=9), 
              aes(x=as.numeric(Dis), y=normalize(TravT), color="red")) + geom_smooth(data=Results[Results$Md == 0.0 & Results$Catch == 64,], method = "gam", formula = y ~ s(x, k=9), aes(x=as.numeric(Dis), y=normalize(ParaMX), color='blue')) + 
  geom_smooth(data=Results[Results$Md == 0.0 & Results$Catch == 64,], method = "gam", formula = y ~ s(x, k=9), 
              aes(x=as.numeric(Dis), y=normalize(TravT), color='red'))+ 
  facet_grid(Catch ~ ., labeller = labeller(Catch = catch.labs)) + geom_vline(data=Fit, aes(xintercept=Def), lwd=1, linetype="dashed", color="black") + geom_vline(data=Fit, aes(xintercept=Ttwice), lwd=1, linetype="dotted", color="blue") + geom_vline(data=Fit, aes(xintercept=Ptwice), lwd=1, linetype="dotted", color="red") +  labs(color="Variable", size=21, x = "Tolerance", y = "Normalized Variables") +  scale_color_manual(labels = c("Parasites", "Travel"), values = c("red", "blue")) +
  theme_classic() + theme(text=element_text(size=20)) +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),  panel.grid.major.y = element_line(color = "grey60", linetype='dashed')) + theme(panel.spacing = unit(1, "lines"))  + coord_cartesian(xlim =c(0, 30))


#to extract ideal values for Fit dataframe: insert ", outfit=fitT<<-..y.."  in aesthetics (without quotes) and add "n = 10000" without quotes before "method="

Results$r <- rep(c(1:10), 108)

DungVec <- as.vector(NA)
for(i in 2:10){ DungVec[i] <- sd(Results$DungMX[Results$r <= i])/mean(Results$DungMX[Results$r <= i]) }

TraVec <- as.vector(NA)
for(i in 2:10){ TraVec[i] <- sd(Results$TravT[Results$r <= i])/mean(Results$TravT[Results$r <= i]) }
ParaVec <- as.vector(NA)
for(i in 2:10){ ParaVec[i] <- sd(Results$ParaMX[Results$r <= i])/mean(Results$ParaMX[Results$r <= i]) }
LocVec <- as.vector(NA)
for(i in 2:10){ LocVec[i] <- sd(Results$LocMX[Results$r <= i])/mean(Results$LocMX[Results$r <= i]) }
RLEVec <- as.vector(NA)
for(i in 2:10){ RLEVec[i] <- sd(Results$RLE[Results$r <= i])/mean(Results$RLE[Results$r <= i]) }
VisitVec <- as.vector(NA)
for(i in 2:10){ VisitVec[i] <- sd(Results$Visits[Results$r <= i])/mean(Results$Visits[Results$r <= i]) }

Hm <- as.data.frame(cbind(c(1:10), DungVec, TraVec, ParaVec, LocVec, RLEVec, VisitVec))

names(Hm) <- c("Realizations", "Dung", "Travel", "Parasites", "Aggregate", "Continuous", "Unique")

library(data.table)
long <- melt(setDT(Hm), id.vars="Realizations")
long <- long[!is.na(long$value),]

ggplot() + geom_line(data=long, aes(x=Realizations, y=value)) + facet_grid(variable ~ ., scale = "free_y")+
  theme_classic() + theme(text=element_text(size=20)) +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),  panel.grid.major.y = element_line(color = "grey60", linetype='dashed')) + theme(panel.spacing = unit(1, "lines"))


ggplot() + geom_smooth(data=long, method="loess", se = TRUE, aes(x=Realizations, y=value)) + facet_grid(variable ~ ., scale = "free_y")+
  theme_classic() + theme(text=element_text(size=20)) +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),  panel.grid.major.y = element_line(color = "grey60", linetype='dashed')) + theme(panel.spacing = unit(1, "lines")) + labs(y="Coefficients of Variation")

MOD <- readRDS("C:/Data/Buff/OUTPUT/MODWatDisALL.RDS")

catch.labs <- c("16x Catchment", "32x Catchment", "64x Catchment")
names(catch.labs) <- c("16", "32", "64")

Md.labs <- c("0.01 Habituation", "0.10 Habituation", "1.00 Habituation")
names(Md.labs) <- c("0.01", "0.1", "1")

ggplot() + geom_jitter(data=MOD[MOD$Md == 0.01,], height=0.01, aes(x=W2, y=as.numeric(D2), col=as.factor(Dis)), size=0.5)+ geom_jitter(data=MOD[MOD$Md == 0.1,], height=0.1, aes(x=W2, y=as.numeric(D2), col=as.factor(Dis)), size=0.5)+ geom_jitter(data=MOD[MOD$Md == 1,], height=1, aes(x=W2, y=as.numeric(D2), col=as.factor(Dis)), size=0.5) + scale_color_manual(values=My.Pal)  + facet_grid(Md ~ Catch, scale="free_y", labeller = labeller(Catch = catch.labs, Md = Md.labs)) + 
  labs(color="Tolerance", x = "Proportion of Total Water, per Time-Step", y = "Increment of Disgust Modifier, per Time-Step") + 
  theme_classic() + theme(text=element_text(size=20)) + 
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'), legend.position = "bottom",
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),  panel.grid.major.y = element_line(color = "grey60", linetype='dashed')) +
  guides(color = guide_legend(override.aes = list(size = 5) ) ) + theme(panel.spacing = unit(1.5, "lines"))

 #To obtain water: 
   MODW <- data.frame(matrix(ncol = 6, nrow = 0))
 for(i in 721:nrow(Results)){
   OUT <-  readRDS(paste0(getwd(),"/OUTPUT/","Disgust_",Results$Dis[i],"_mods_",Results$Md[i],"_catch_",Results$Catch[i],"_R_",rep(Results$r, 3)[i],".RDS"))
   rm(Water, tmp)
   Water <- rbindlist(lapply(OUT$Water, as.data.frame.list), fill=TRUE)
   Water$Amnt <- Water$VoLiter/Water$OGLiter
   tmp <- aggregate(Water$Amnt, by=list(Water$t), FUN=mean)
   colnames(tmp) <- c("t", "Water")
   tmp$Dis <- Results$Dis[i]
   tmp$Md <- Results$Md[i]
   tmp$r <- Results$r[i]
   tmp$Catch <- Results$Catch[i]
   MODW <- rbind(MODW, tmp)
   print(i)
 }
 
 MODD <- data.frame(matrix(ncol = 6, nrow = 0))
 for(i in 721:nrow(Results)){
   OUT <-  readRDS(paste0(getwd(),"/OUTPUT/","Disgust_",Results$Dis[i],"_mods_",Results$Md[i],"_catch_",Results$Catch[i],"_R_",rep(Results$r, 3)[i],".RDS"))
   rm(Buff, tmp)
   Buff <- rbindlist(lapply(OUT$Buff, as.data.frame.list), fill=TRUE)
   tmp <- aggregate(Buff$Dgst_Md, by=list(Buff$t), FUN=sum)
   tmp2 <- as.vector("NA")
   for(j in 2:length(tmp$x)){tmp2[j] <- tmp$x[j] - tmp$x[j-1]}
   tmp$x <- tmp2
   colnames(tmp) <- c("t", "Dgst_Md")
   tmp$Dis <- Results$Dis[i]
   tmp$Md <- Results$Md[i]
   tmp$r <- Results$r[i]
   tmp$Catch <- Results$Catch[i]
   MODD <- rbind(MODD, tmp)
   print(i)
 }
