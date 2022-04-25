library(ggplot2)
library(data.table)
library(RColorBrewer)
library(dplyr)

Dis_params <- c(rep(0.1, 4), rep(1.0, 4), rep(10, 4), rep(100, 4), rep(4, 4), rep(7, 4), rep(25, 4), rep(50, 4), rep(75, 4))
Md_params <- rep(c(0.0, 0.01, 0.1, 1.0), 9)
Params <- as.data.frame(cbind(rep(Dis_params, 10), rep(Md_params, 10)))
Params$r <- c(rep(1, 36), rep(2, 36), rep(3, 36), rep(4, 36), rep(5, 36), rep(6, 36), rep(7, 36), rep(8, 36), rep(9, 36), rep(10, 36))
Params <- Params[order(Params$V1, Params$V2),]
colnames(Params) <- c("Dis","Md","r")

catchment <- 16
Params$Catch <- catchment

Travelmn<-numeric()
Parasitesmn<-numeric()
Dungmn <- numeric()
volmn <- numeric()

Travelsd<-numeric()
Parasitessd<-numeric()
Dungsd <- numeric()
volsd <- numeric()

Parasitesmx<-numeric()
Dungmx <- numeric()

TravelT<-numeric()
ParasitesT<-numeric()
DungT <- numeric()
volP <- numeric()
volT <- numeric()

LocMN <- numeric()
LocMD <- numeric()
LocMX <- numeric()
LocSD <- numeric()
DgstMN <- numeric()
DgstMX <- numeric()

for(i in 1:nrow(Params)){
OUT <-  readRDS(paste0(getwd(),"/OUTPUT/","Disgust_",Params$Dis[i],"_mods_",Params$Md[i],"_catch_",catchment,"_R_",Params$r[i],".RDS"))
rm(Dungs, Paras, Buff, Water)
Buff <- rbindlist(lapply(OUT$Buff, as.data.frame.list), fill=TRUE)
Paras <- rbindlist(lapply(OUT$Paras, as.data.frame.list), fill=TRUE)
Dungs <- rbindlist(lapply(OUT$Dung, as.data.frame.list), fill=TRUE)
Water <- rbindlist(lapply(OUT$Water, as.data.frame.list), fill=TRUE)

  Travelmn[i]<- mean(Buff$Travel)
  Parasitesmn[i] <-mean(Paras$X0)
  Dungmn[i] <- mean(apply(Dungs[,2:36], 1, sum))
  volmn[i] <- mean(Water$VoLiter/Water$OGLiter)
  
  Travelsd[i]<-sd(Buff$Travel)
  Parasitessd[i]<-sd(Paras$X0)
  Dungsd[i] <- sd(apply(Dungs[,2:36], 1, sum))
  volsd[i] <- sd(Water$VoLiter/Water$OGLiter)
  
  Parasitesmx[i] <- max(Paras$X0)
  Dungmx[i] <- max(apply(Dungs[,2:36], 1, sum))
  
  TravelT[i]<-sum(Buff$Travel)
  ParasitesT[i]<-sum(Paras$X0)
  DungT[i] <- sum(apply(Dungs[,2:36], 1, sum))
  volP[i] <- sum(Water$VoLiter/Water$OGLiter)
  volT[i] <- mean(Water$VoLiter)

  LocMN[i] <- mean(as.matrix(count(Buff, Buff$loc)[,2]))
  LocMD[i] <- median(as.matrix(count(Buff, Buff$loc)[,2]))
  LocMX[i] <- max(as.matrix(count(Buff, Buff$loc)[,2]))
  LocSD[i] <- sd(as.matrix(count(Buff, Buff$loc)[,2]))
  DgstMN[i] <- mean(Buff$Dgst_Md)
  DgstMX[i] <- max(Buff$Dgst_Md)
  
print(i)
}

RLE <- as.numeric()
tmp <- as.numeric()

for(i in 1:nrow(Params)){
  OUT <-  readRDS(paste0(getwd(),"/OUTPUT/","Disgust_",Params$Dis[i],"_mods_",Params$Md[i],"_catch_",catchment,"_R_",Params$r[i],".RDS"))
  rm(Dungs, Paras, Buff, Water)
  Buff <- rbindlist(lapply(OUT$Buff, as.data.frame.list), fill=TRUE)
  
  for(j in 1:length(unique(Buff$In_Group[Buff$t == 1]))){
    id <- unique(Buff$In_Group[Buff$t == 1])[j]
    tmp[j] <- max(rle(Buff$loc[Buff$In_Group == unique(Buff$In_Group)[j]])$lengths)
  }
  RLE[i] <- max(tmp)
  tmp <- as.numeric()
  print(i)
}

Visits <- as.numeric()

for(i in 1:nrow(Params)){
  OUT <-  readRDS(paste0(getwd(),"/OUTPUT/","Disgust_",Params$Dis[i],"_mods_",Params$Md[i],"_catch_",catchment,"_R_",Params$r[i],".RDS"))
  rm(Buff)
  Buff <- rbindlist(lapply(OUT$Buff, as.data.frame.list), fill=TRUE)
  
  Visits[i] <- length(unique(Buff$loc))
  
  print(i)
}

Results <- as.data.frame(cbind(Params$Dis, Params$Md, Travelsd, Parasitessd, Dungsd, volsd, Travelmn, Parasitesmn, Dungmn, volmn, Parasitesmx, Dungmx, TravelT, ParasitesT, DungT, volP, volT, LocMN, LocMD, LocMX, LocSD, DgstMN, DgstMX))
colnames(Results) <- c("Dis", "Md", "TravSD", "ParaSD", "DungSD", "VolSD", "TravMN", "ParaMN", "DungMN", "VolMN", "ParaMX", "DungMX", "TravT", "ParaT", "DungT", "VolP", "VolT", "LocMN", "LocMD", "LocMX", "LocSD", "DgstMN", "DgstMX")

Results$RLE <- RLE

Results$Visits <- Visits

MOD <- data.frame(matrix(ncol = 5, nrow = 0))

for(i in 1:nrow(Results)){
  OUT <-  readRDS(paste0(getwd(),"/OUTPUT/","Disgust_",Results$Dis[i],"_mods_",Results$Md[i],"_catch_",Results$Catch[i],"_R_",rep(Params$r, 3)[i],".RDS"))
  rm(Buff, tmp)
  Buff <- rbindlist(lapply(OUT$Buff, as.data.frame.list), fill=TRUE)
  
  tmp <- aggregate(Buff$Dgst_Md, by=list(Buff$t), FUN=mean)
  colnames(tmp) <- c("t", "Dgst_Md")
  tmp$Dis <- Results$Dis[i]
  tmp$Md <- Results$Md[i]
  tmp$r <- rep(Params$r, 3)[i]
  
  MOD <- rbind(MOD, tmp)
  
  print(i)
}

VoPara <- data.frame(matrix(ncol = 7, nrow = 0))

for(i in 1:nrow(Params)){
  OUT <-  readRDS(paste0(getwd(),"/OUTPUT/","Disgust_",Params$Dis[i],"_mods_",Params$Md[i],"_catch_",catchment,"_R_",Params$r[i],".RDS"))
  rm(Paras, Dungs, Water, tmp, tmp2, tmp3)
  Paras <- rbindlist(lapply(OUT$Paras, as.data.frame.list), fill=TRUE)
  Dungs <- rbindlist(lapply(OUT$Dung, as.data.frame.list), fill=TRUE)
  Water <- rbindlist(lapply(OUT$Water, as.data.frame.list), fill=TRUE)
  
  tp <- as.data.frame(cbind(Water$VoLiter/Water$OGLiter, Water$t))
  colnames(tp) <- c("VoL", "t")
  tmp <- aggregate(tp$VoL, by=list(tp$t), FUN=mean)
  colnames(tmp) <- c("t", "VoL")
  
  tmp2 <- aggregate(apply(Dungs[,2:36], 1, sum), by=list(Dungs$t), FUN=max)
  colnames(tmp2) <- c("t", "Dung")
  
  tmp3 <- aggregate(Paras$X0, by=list(Paras$t), FUN=max)
  colnames(tmp3) <- c("t", "Paras")
  
  tmp$Dis <- Params$Dis[i]
  tmp$Md <- Params$Md[i]
  tmp$r <- Params$r[i]
  
  o <- left_join(tmp, tmp2, by="t")
  ok <- left_join(o, tmp3, by="t")
  
  VoPara <- rbind(VoPara, ok)
  
  print(i)
}