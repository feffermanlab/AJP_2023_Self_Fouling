  library(data.table)
  library(ggplot2)
  library(igraph)
  library(dplyr)
  library(VFS)
  library(gdata)
  #library(ragg)
  
  set.seed(120)
  setwd("C:/Data/Buff")
  #set duration of each run
  TIMESTEPS <- 1095
  
  Dis_params <- c(rep(0.1, 4), rep(1.0, 4), rep(10, 4), rep(100, 4), rep(4, 4), rep(7, 4), rep(25, 4), rep(50, 4), rep(75, 4))
  Md_params <- rep(c(0.0, 0.01, 0.1, 1.0), 9)
  Params <- as.data.frame(cbind(Dis_params, Md_params))
  colnames(Params) <- c("Dis", "Md")
  
  for(j in 25:nrow(Params)){
  #set sensitivity of fecal avoidance (lower values are more sensitive)
  Disgust <- Params$Dis[j]
  #set adjustments based on duress of travelling and relative conditions of waterholes
  MOD <- Params$Md[j]
  r <- 1
  while(r < 11){
  keep(TIMESTEPS, Disgust, Params, MOD, j, r)
  #Populate groups of semi-random size
  m <- 350
  s <- 200
  location <- log(m^2 / sqrt(s^2 + m^2))
  shape <- sqrt(log(1 + (s^2 / m^2)))/2
  Groups <- round(rlnorm(n=1000, location, 0.3))
  
  #Draw a number of groups that approximates carrying capacity of relevant region - here: Laikipia, Kenya
  
  carry_cap <- 3906
  Avg_Grps <- round(carry_cap/mean(Groups))
  In_Group <- sample(Groups, Avg_Grps, replace=FALSE)
  while(sum(In_Group) > carry_cap){In_Group <- sample(Groups, Avg_Grps, replace=FALSE)}
  
  #set water requirement per ID: 3.42 l/d per ID
  ld <- 3.42
  
  #Set this as you see fit sqkm - here: Laikipia, Kenya; set total area of permanent/seasonal water holes: https://www.sdg661.app/
  total_area <- 8696.1 #km^2
  diamArea <- sqrt(total_area/pi)*2
  PermH2O <- 2.30 #km^2
  SeasH2O <- 5.80 #km^2
  
  #Didn't use
  # ShortSeas <- 0.4375
  # MedSeas <- 0.3125
  # LongSeas <- 0.25
  
  #from total areas: draw the number of water holes
  m <- 50
  s <- 30
  location <- log(m^2 / sqrt(s^2 + m^2))
  shape <- sqrt(log(1 + (s^2 / m^2)))/2
  PH <- round(rlnorm(n=1000, location, shape))
  PermHoles <- (PermH2O*1000)/mean(PH)
  PHSamp <- sample(PH, PermHoles, replace=FALSE)
  while(sum(PHSamp) > PermH2O*1000){PHSamp <- sample(PH, PermHoles, replace=FALSE)}
  
  SH <- round(rnorm(n=1000, 15, 10))
  SH <- SH[SH < 30 & SH > 0]
  SeasHoles <- (SeasH2O*1000)/mean(SH)
  SESamp <- sample(SH, SeasHoles, replace=FALSE)
  while(sum(SESamp) > SeasH2O*1000){SESamp <- sample(SH, SeasHoles, replace=FALSE)}
  
  #tidy waterhole specs into a neater data frame
  
  H <- as.data.frame(rbind(cbind(PHSamp, "PH"), cbind(SESamp, "SH")))
  
  #this is no longer used
  H$Cat <- NA
  
  colnames(H) <- c("SuArea", "PermSea", "Cat")
  H$SuArea <- as.numeric(H$SuArea)
  
  #https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2016GL071378
  #H$VoluM3 <- (10^(1.204*log10(H$SuArea) - 0.629 + rnorm(1, 0, 0.292)))
  #without error
  
  #Calculate volume from surface area and then convert to liters
  H$VoluM3 <- (10^(1.204*log10(H$SuArea) - 0.629))
  H$VoLiter <- H$VoluM3 * 1000
  
  #https://pdf.sciencedirectassets.com/271238/1-s2.0-S0378377405X00877/1-s2.0-S0378377404002677/main.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjED4aCXVzLWVhc3QtMSJHMEUCIQDA0b%2F%2FdpiTTZq1xjItneYL1Fq7mzfcva%2F4byPfxXrpLwIgQJinpNK5YDKKneD1ZCLqBVBXrScPxNjGpCLGMK0gJqMq%2BgMIFxAEGgwwNTkwMDM1NDY4NjUiDATlmR1TgM27%2F5SiPyrXAwmNem9m0WqFoL177Pp%2B2dOm68L2gGjYmXzNlqR120O8nm724rA0HtHKxGrIWSY4ppK%2B%2FaeRwU0OJd2Z3%2BkzK5LsPZmje6OrpEa4nymXJ1VszGMmpDzcXwVQQigD5RLLwufx0YOam%2BbVnJO%2FCMv%2FqAzCrORaXlXQcQFRRVGQgS8as%2FaRWOE7ZH7u1qif3K3UYiq3%2FIm8vuKAsHzRRUswLe186Bxvd6EiPuAgsH2xPZt%2BSrsCP%2BTH1nrlfzmIsS0LsgbivWeNyVgFROytPEaREvHyQ3HSFCLeSc4dZv9xmtVPQLKGUGVj6HDnqTcNvNQlGAfrr33hFhzHWu2ssbXbyyVCDIlDMDbDeFXM5YyaswsLsBaBfnUJKgMbejKelxowQDZR9fdy4qA%2FYCMZ9xlkBmzPTO4cCiHzx024v3Y%2BqOgqLgUSAFQNile1Oyb%2BfiGvx7%2BmNV8llLTKqHMn1qlKwJMCpdtXEC2wic%2F9e3uJjmgwSDHRwdzLoFVbb5l8j4UsXyen7s3HeC5fDWdhKb2sFVEyHyBUG15DGLS4k642i5V5BNFuOAIkRna24A4AyWaPqmPVmaFrPynMe4hJRdPjiKSbziXGGptyiiBOXY08bvKisJ5ej3FccDDS%2FJ2NBjqlAYTU2fwFpd23hg7B4tEltJSAQjRZnk4wHGEW6SgwBd0KCBXWDFh08VQhdXXVpv7UrAGVPQKC9E%2FfnUrpg%2BiFDBQ4oWCiE40MgiVXs5quI%2FAVSMvab%2FOnLJKrtXyv39YKXbCyvbiY5gls4L0MU%2BVAsafDLOCtfkaoQx%2BWNBn8J7hob%2BbBnOBpdI3MH8duN9NqgJvALKkfABS1rx5QOlWeXVuEsvSvSQ%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20211201T144524Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAQ3PHCVTY5IBKLVPN%2F20211201%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=d3d9af9113eaee7d19466b9e782c8df16fd08db897e0b19876898c71f3cc4a98&hash=930b607be1e13826c3fb1af2f77ec7dc84434d8b821b3278bae8f2a11d3f0b4b&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=S0378377404002677&tid=spdf-a0619078-539c-4622-b2ac-0f806d79c844&sid=ec58c34a8b5b944ce629c7131755c7cec2acgxrqa&type=client
  
  #relative size of rainfall catchment, compared to waterhole
  catchment <- 16
  
  #rain from El Doret used to approximate annual rainfalls, either directly or via a modelled sampling
  dly <- read.dly("C:/Data/Buff/KEM00063686.dly")
  sim <- wth.param(dly[dly$YEAR > 1990 & dly$YEAR < 1998,], llim = 0, method = "markov")
  rain <- rainfall(TIMESTEPS, sim)
  
  #if you prefer real values
  # rain <- dly$PRCP.VALUE[dly$YEAR > 1990 & dly$YEAR < 1998]
  # rain[is.na(rain)] <- 0
  
  #if you prefer using monthly rainfall from laikipia  
  # rain <- rep(c(rep(52/31, 31), rep(59/28, 28), rep(85/31, 31), rep(116/30, 30), 
  #           rep(50/31, 31), rep(49/30, 30), rep(42/31, 31), rep(46/31, 31), 
  #           rep(31/30, 30), rep(83/31, 31), rep(137/30, 30), rep(81/31, 31)), 200)
  
  #add these original area values, necessary for preventing 'overflow' when refilling
  H$OGArea <- H$SuArea
  H$OGLiter <- H$VoLiter
  
  #create IDs for location based movement 
  H$ids <- c(1:nrow(H))
  H$t <- 1
  
  #assemble a fully-connected graph
  g <- make_full_graph(
    nrow(H),
    directed = FALSE,
    loops = FALSE
  )
  
  #extract edge distances from the existing layout
  distance <- layout.fruchterman.reingold(g)
  
  w <- as.data.frame(get.edgelist(g))
  
  #extract relative distances from the randomly drawn layout to create edge weight distances defined by the area of the region
  for(i in 1:nrow(w)){
    w$weight[i] <- diff(c(distance[w$V1[i],1], distance[w$V2[i],1]))^2 + diff(c(distance[w$V1[i],2], distance[w$V2[i],2]))^2
  }
  w$weight <- abs(w$weight - max(w$weight + 1))
  w$weight <- ((w$weight - min(w$weight)) / (max(w$weight) - min(w$weight)))
  w$weight <- diamArea*w$weight
  g <- graph_from_data_frame(w, directed=FALSE)
  
  #add node attributes
  V(g)$ids <- H$ids
  V(g)$VoL <- H$VoLiter
  
  #add travel and water requirements and inset in a buffalo's data frame
  loc <- sample(H$ids, length(In_Group), replace = FALSE)
  Dest <- loc 
  WaterReq <- In_Group*ld
  Dist <- rep(0, length(In_Group))
  Travel <- rep(0, length(In_Group))
  Dgst_Md <- rep(0, length(In_Group))
  frame <- as.data.frame(cbind(In_Group, WaterReq, loc, Dist, Dest, Travel, Dgst_Md))
  
  #create a distribution for parasite egg hatch times
  hatch_draw <- round(rnorm(1000000, 10.5, 1.2))
  hatch_draw <- hatch_draw[hatch_draw > 6 & hatch_draw < 15]
  
  #dust and sweep
  keep(r, j, Params, MOD, Disgust, TIMESTEPS, catchment, H, g, frame, hatch_draw, diamArea, rain, distance, MOD, sure = TRUE)
  
  #create data frame for parasite development
  Hatching <- data.frame(matrix(0, nrow(H), 18))
  colnames(Hatching) <- c("loc", -1:14, "t")
  Hatching$loc <- H$ids
  #Lifespan weeks * days in a week
  parasitelife <- 6*7
  #list for parasite output per timestep
  Hatchls <- list()
  Hatchls[[1]] <- Hatching
  
  #create data frame for dung odor
  Dunging <- data.frame(matrix(0, nrow(H), 37))
  colnames(Dunging) <- c("loc", 1:35, "t")
  Dunging$loc <- H$ids
  #list for Dung output per timestep
  Dungls <- list()
  Dungls[[1]] <- Dunging
  
  #list for water output per timestep
  H2Output <- list()
  H2Output[[1]] <- H
  
  #list for buffalo output per timestep
  BuffOut <- list()
  frame$t <- 1
  BuffOut[[1]] <- frame
  
  #Buffalo leave expended/contaminated water (contingent on disgust) 
  #then seek water sources that are viable to support their group
  SeekAndDestroy <- function(loc, WaterReq, DgstM, g, diamArea, H, Dunging, Disgust){
    #water hole distance relative to current location node
    tmp <- g[V(g)$ids == loc]
    #bind in a dataframe with volume
    Temp <- as.data.frame(cbind(tmp, c(1:length(tmp)), V(g)$VoL))
    colnames(Temp) <- c("Dist", "Dest", "VoL")
    #if current hole has enough clean water, do nothing
    if(H$VoLiter[H$ids == loc] >= WaterReq && (sum(Dunging[Dunging$loc == loc, c(-1, -37)]) < (Disgust+DgstM))){
      Temp <- Temp[Temp$Dest == loc,]
      Temp <- Temp[Temp$Dist == 0,]
    #if current water hole is expended/contaminated, then move to the closest viable water source
    } else { 
    # if(H$VoLiter[H$ids == loc] < WaterReq){
    Temp <- Temp[Temp$VoL > WaterReq,]
    Temp <- Temp[Temp$Dist == max(Temp$Dist),]
    Temp$Dist <- abs(Temp$Dist-diamArea)}
    return(Temp[,1:2])
  }
  
  #buffalo defecate at their current location
  Defecation <- function(frameTrav, H, Dunging)
  {
    #Age dung
    for(i in 3:36){Dunging[,i-1] <- Dunging[,i] + Dunging[,i-1]; Dunging[,i] <- 0}
    
    #assuming 20 kgs per day, 1440 minutes in a day and 10 minutes at a water hole
    dung <- round(((10*frameTrav$In_Group)/1440)*20)
    
    #normalize by area of current circumferance + 5m of the water hole
    dung2 <- mapply(FUN=function(x, y, z) (x/(pi*((sqrt(z$SuArea[z$ids == y]/(pi)))+5)^2-z$SuArea[z$ids == y])), dung, frameTrav$loc, MoreArgs = list(H))
    if(sum(dung2) > 0)
    {
      #generate dung to add
      dung_tmp <- as.data.frame(cbind(frameTrav$loc, as.matrix(dung2)))
      colnames(dung_tmp) <- c("loc", 35)
      #merge with current dung
      Dunging <- bind_rows(Dunging, dung_tmp)
      Dunging[is.na(Dunging)] <- 0
      Dunging <- aggregate(Dunging, by=list(Dunging$loc), FUN="sum", drop=FALSE)
      Dunging <- Dunging[,-2]
      colnames(Dunging) <- c("loc", 1:35, "t")
    }
    return(Dunging)}  
  
  #generate parasites
  Parasites <- function(frameTrav, H, Hatching)
  {
    #age parasites
    Hatch_old <- Hatching
    for(i in 4:17){Hatching[,i-1] <- Hatching[,i] + Hatching[,i-1]; Hatching[,i] <- 0}
    for(i in 1:nrow(Hatch_old)){if(Hatching$`0`[Hatching$loc==Hatch_old$loc[i]] > Hatch_old$`0`[i]){Hatching$`-1`[Hatching$loc==Hatch_old$loc[i]] <- round(Hatching$`0`[Hatching$loc==Hatch_old$loc[i]]/parasitelife)}}
    Hatching$`-1`[Hatching$`-1` == 0] <- round(Hatching$`0`[Hatching$`-1` == 0]/parasitelife)
  
    #assuming 20 kgs per day, 1440 minutes in a day and 10 minutes at a water hole
    dung <- round(((10*frameTrav$In_Group)/1440)*20)
   
    #new parasites
    #epg with 0.85 IDs shedding * 2kg(in grams) assuming (one defecation) per ID, quarter of the group defecates at water hole
    paras <- sapply(dung, FUN=function(x) sum(rnorm(((x * 0.85)), 349, 54)))
    
    #normalize by area of circumferance + 5m of waterholes
    paras2 <- mapply(FUN=function(x, y, z) round(x/(pi*((sqrt(z$SuArea[z$ids == y]/(pi)))+5)^2-z$SuArea[z$ids == y])), paras, frameTrav$loc, MoreArgs = list(H))
    if(sum(paras2) > 0)
    {
      #generate parasite hatching
      tmp <- mapply(FUN=function(x, y, z) as.data.frame(cbind(x, t(as.matrix(table(sample(z, y, replace=TRUE)))))), frameTrav$loc, paras2, MoreArgs = list(hatch_draw), SIMPLIFY = FALSE)
      Hatch_tmp <- rbindlist((tmp), use.names=TRUE, fill=TRUE)
      colnames(Hatch_tmp) <- c("loc", colnames(Hatch_tmp[,-1]))
      #merge with current Hatching events
      Hatching <- bind_rows(Hatching, Hatch_tmp)
      Hatching[is.na(Hatching)] <- 0
      Hatching <- aggregate(Hatching, by=list(Hatching$loc), FUN="sum", drop=FALSE)
      Hatching <- Hatching[,-2]
      colnames(Hatching) <- c("loc", -1:14, "t")
    }
    return(Hatching)}
  
  for(t in 2:TIMESTEPS){
    if(0 %in% frame$Travel){
    Travel <- as.data.frame(t(mapply(FUN = SeekAndDestroy, frame$loc[frame$Travel == 0], frame$WaterReq[frame$Travel == 0], frame$Dgst_Md[frame$Travel == 0], MoreArgs = list(g, diamArea, H, Dunging, Disgust), SIMPLIFY=TRUE)))}
    frame$Dist[frame$Travel == 0] <- as.numeric(Travel$Dist)
    frame$Dest[frame$Travel == 0] <- as.numeric(Travel$Dest)
    if(NA %in% frame$Dest){print("NO WATER") 
      t <- TIMESTEPS + 1 
      break()}
    if(nrow(frame[frame$Travel == 0 & frame$Dist > 0,]) > 0){
    Relative_Dgst <- mapply(FUN=function(x, y, z) sum(z[z$loc == x, c(-1, -37)]) - sum(z[z$loc == y, c(-1, -37)]), frame$Dest[frame$Travel == 0 & frame$Dist > 0], frame$loc[frame$Travel == 0 & frame$Dist > 0], MoreArgs = list(Dunging))
    Relative_Dgst[Relative_Dgst > 0] <- MOD
    Relative_Dgst[Relative_Dgst < 0] <- -MOD
    frame$Dgst_Md[frame$Travel == 0 & frame$Dist > 0] <- frame$Dgst_Md[frame$Travel == 0 & frame$Dist > 0] + Relative_Dgst
    }
    frame$Travel[frame$Dist > 0] <- 1
    frame$Dgst_Md[frame$Travel == 1] <- frame$Dgst_Md[frame$Travel == 1] + MOD
    frame$Dist[frame$Travel == 1] <- frame$Dist[frame$Travel == 1] - rnorm(length(frame$Dist[frame$Travel == 1]), 4.6, 2.6)
    frame$Dist[frame$Dist < 0] <- 0
    frame$loc[frame$Dist == 0] <- frame$Dest[frame$Dist == 0]
    frame$Travel[frame$Dist == 0] <- 0
    
    #Calculate evaporation based on surface area
    evap <- sapply(H$SuArea, FUN = function(x) ((0.0065*x)*1000))
    #Remove evaporation from current volume
    H$VoLiter <- H$VoLiter - evap
    
    #if buffalo are at waterholes
    if(0 %in% frame$Travel){
    
    #Defecate
    Dunging[,2] <- 0
    Dunging <- Defecation(frame[frame$Travel == 0,], H, Dunging)
    
    #Add parasite eggs/hatch current ones
    Hatching$`0` <- Hatching$`0`-Hatching$`-1`
    Hatching$`0`[Hatching$`0` < 0] <- 0
    Hatching <- Parasites(frame[frame$Travel == 0,], H, Hatching)
    
    #Drink from waterhole
    drank <- aggregate(frame$WaterReq[frame$Travel == 0], by=list(frame$loc[frame$Travel == 0]), FUN=sum)
    H$VoLiter[H$ids %in% drank$Group.1] <- H$VoLiter[H$ids %in% drank$Group.1] - drank$x
    }
    
    #add rainfall to waterholes
    H$VoLiter <- H$VoLiter + ((H$OGArea*catchment) * rain[t])
    H$VoLiter[H$VoLiter > H$OGLiter]  <- H$OGLiter[H$VoLiter > H$OGLiter]
    
    #waterhole cannot be negative
    H$VoLiter[H$VoLiter < 0] <- 0
    
    #update surface area
    H$SuArea <- 10^((log10(H$VoLiter/1000) + 0.629)/1.204)
    V(g)$VoL <- H$VoLiter
    
    #log everything for current timestep
    Dunging$t <- t
    Dungls[[t]] <- Dunging
    Hatching$t <- t
    Hatchls[[t]] <- Hatching
    H$t <- t
    H2Output[[t]] <- H
    frame$t <- t
    BuffOut[[t]] <- frame
  }
  print(c(r,j))
  if(t == TIMESTEPS){
  OUT <- list(H2Output, BuffOut, Dungls, Hatchls)
  names(OUT) <- c("Water", "Buff", "Dung", "Paras")
  saveRDS(OUT, paste0(getwd(),"/OUTPUT/","Disgust_",Disgust,"_mods_",MOD,"_catch_",catchment,"_R_",r,".RDS"))
  r <- r + 1
  }
  if(t > TIMESTEPS){print("Rerun")}
  }
  }
  
  
  k <- rbindlist(lapply(H2Output, as.data.frame.list), fill=TRUE)
  b <- rbindlist(lapply(BuffOut, as.data.frame.list), fill=TRUE)
  c <- rbindlist(lapply(Hatchls, as.data.frame.list), fill=TRUE)
  c <- c[c$t != "NA",]
  c$tally <- apply(c[,3:17], 1, FUN = "sum")
   
  d <- rbindlist(lapply(Dungls, as.data.frame.list), fill=TRUE)
  d <- d[d$t != "NA",]
  d$tally <- apply(d[,2:36], 1, FUN = "sum")
  
  #plot parasites per water hole and whether buffalo visited
  e <- aggregate(c$tally, by=list(c$loc), FUN="sum")
  V(g)$Para <- e$x
  V(g)$Buff <- ifelse(H$ids %in% b$loc, 1, 0)
  plot(g, layout=distance, vertex.color=V(g)$Buff, vertex.size=scale(V(g)$Para, center=FALSE)*10)
  
  #plot parasite prevalence over time
  WaterVol <- ggplot(data=k, aes(x = t, y = VoLiter, col = as.factor(ids))) + geom_point() + theme(legend.position = "none")
  
  #plot water volume per water hole and whether buffalo visited
  plot(g, layout=distance, vertex.color=V(g)$Buff, vertex.size=scale(H$OGLiter, center=FALSE)*10)
  
  #plot parasite prevalence over time
  ParaVol <- ggplot(data=c, aes(x = t, y = X0, col = as.factor(loc))) + geom_point() + theme(legend.position = "none")