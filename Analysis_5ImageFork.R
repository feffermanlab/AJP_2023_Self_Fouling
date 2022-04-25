library(ggplot2)
library(data.table)
library(RColorBrewer)
library(dplyr)

Results <- readRDS("C:/Data/Buff/OUTPUT/ResultsALL.RDS")

MOD <- readRDS("C:/Data/Buff/OUTPUT/ModALL.RDS")

catch.labs <- c("16x Catchment", "32x Catchment", "64x Catchment")
names(catch.labs) <- c("16", "32", "64")

My.Pal <- brewer.pal(name="Blues",n=9)[c(2,4,6,8)]

ggplot() + geom_boxplot(data = Results, lwd=0.8, aes(y=TravT, x=interaction(Md), fill=as.factor(Dis), color=as.factor(Md))) + facet_grid(Catch ~ ., scale="free_y", labeller = labeller(Catch = catch.labs)) +
theme_classic()  + scale_fill_manual(values=My.Pal) + scale_color_manual(values=c("#CCCCCC", "#999999", "#666666","#000000")) + 
  labs(fill = "Tolerance (Disgust)", color="Disgust Plasticity", size=21, x = "Disgust Plasticity", y = "Travel (Total)") + 
  theme(text=element_text(size=20))

ggplot() + geom_boxplot(data = Results, lwd=0.8, aes(y=LocMX, x=interaction(Dis), fill=as.factor(Dis), color=as.factor(Md))) + facet_grid(Catch ~ ., scale="free_y", labeller = labeller(Catch = catch.labs)) + 
  theme_classic() + scale_fill_manual(values=My.Pal) + scale_color_manual(values=c("#CCCCCC", "#999999", "#666666","#000000")) + 
  labs(fill = "Tolerance (Disgust)", color="Disgust Plasticity", size=21, x = "Tolerance (Disgust)", y = "Max time at a node") + 
  theme(text=element_text(size=20))

#Figure 2

ggplot() + geom_boxplot(data = Results, lwd=0.8, aes(y=ParaMX, x=interaction(Dis), fill=as.factor(Md))) + facet_grid(Catch ~ ., labeller = labeller(Catch = catch.labs)) + 
  theme_classic() + scale_fill_manual(values=My.Pal) + scale_color_manual(values=c("#CCCCCC", "#999999", "#666666","#000000")) + 
  labs(fill="Habituation", size=21, x = "Tolerance", y = "Max Parasites Load (in a node, per run)") + 
  theme(text=element_text(size=20)) + theme(panel.spacing = unit(1.5, "lines")) + theme(legend.position="bottom") +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5), linetype="dotted", size=0.25)

#Figure S1

ggplot() + geom_boxplot(data = Results, lwd=0.8, aes(y=ParaMN, x=interaction(Dis), fill=as.factor(Md))) + facet_grid(Catch ~ ., labeller = labeller(Catch = catch.labs)) + 
  theme_classic() + scale_fill_manual(values=My.Pal) + scale_color_manual(values=c("#CCCCCC", "#999999", "#666666","#000000")) + 
  labs(fill="Habituation", size=21, x = "Tolerance", y = "Mean Parasites Load (per run)") + 
  theme(text=element_text(size=20)) + theme(panel.spacing = unit(1.5, "lines")) + theme(legend.position="bottom") +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5), linetype="dotted", size=0.25)

#Figure S2

ggplot() + geom_boxplot(data = Results, lwd=0.8, aes(y=ParaSD, x=interaction(Dis), fill=as.factor(Md))) + facet_grid(Catch ~ ., labeller = labeller(Catch = catch.labs)) + 
  theme_classic() + scale_fill_manual(values=My.Pal) + scale_color_manual(values=c("#CCCCCC", "#999999", "#666666","#000000")) + 
  labs(fill = "Habituation", size=21, x = "Tolerance", y = "SD Parasites Load (per run)") + 
  theme(text=element_text(size=20)) + theme(panel.spacing = unit(1.5, "lines")) + theme(legend.position="bottom") +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5), linetype="dotted", size=0.25)

ggplot() + geom_boxplot(data = Results, lwd=0.8, aes(y=DungMX, x=interaction(Dis), fill=as.factor(Dis), color=as.factor(Md))) + facet_grid(Catch ~ ., labeller = labeller(Catch = catch.labs)) + 
  theme_classic() + scale_fill_manual(values=My.Pal) + scale_color_manual(values=c("#CCCCCC", "#999999", "#666666","#000000")) + 
  labs(fill = "Disgust Strength", color="Disgust Plasticity", size=21, x = "Tolerance (Disgust)", y = "Max Dung Load (in a node, per run)") + 
  theme(text=element_text(size=20))

ggplot() + geom_boxplot(data = Results, lwd=0.8, aes(y=DungSD, x=interaction(Dis), fill=as.factor(Dis), color=as.factor(Md))) + facet_grid(Catch ~ ., labeller = labeller(Catch = catch.labs)) + 
  theme_classic() + scale_fill_manual(values=My.Pal) + scale_color_manual(values=c("#CCCCCC", "#999999", "#666666","#000000")) + 
  labs(fill = "Tolerance (Disgust)", color="Disgust Plasticity", size=21, x = "Tolerance (Disgust)", y = "SD Dung Load (in a node, per run)") + 
  theme(text=element_text(size=20))

My.Pal <- brewer.pal(name="Reds",n=9)[c(9,8,7,6,5,4,3,2,1)]

#Figure 3

ggplot() + geom_point(data=Results[Results$Md == 0.0,], aes(x=ParaMX, y=(TravT), size=(DungMX), group=Dis, color=as.factor(Dis))) + facet_grid(. ~ Catch, labeller = labeller(Catch = catch.labs)) +
  labs(fill = "Parasites (max)", color="Tolerance", size=21, size="Dung", x = "Parasites (max)", y = "Total Travel (per run)") + 
  theme_classic() + theme(text=element_text(size=20)) +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),  panel.grid.major.y = element_line(color = "grey60", linetype='dashed')) + scale_color_manual(values=My.Pal, labels = c("0.1", "1.0", "4", "7", "10", "25", "50", "75", "100")) + scale_size_continuous(name = "Dung (max)") +
  guides(color = guide_legend(override.aes = list(size = 5) ) ) + theme(panel.spacing = unit(1, "lines"))

My.Pal <- brewer.pal(name="Reds",n=9)[c(1,2,3,9,4,5,6,7,8)]

#Figure 4a

ggplot() + geom_point(data=Results[Results$Md == 0.0,], aes(x=ParaMX, y=(LocMX), size=(DungMX), group=Dis, color=as.factor(Dis)))  + facet_grid(. ~ Catch, scale="free_x", labeller = labeller(Catch = catch.labs)) + 
  labs(fill = "Parasites (max)", color="Tolerance", size=21, size="Dung", x = "Parasites (max)", y = "Aggregate Time at Waterhole (max per run)") + 
  theme_classic() + theme(text=element_text(size=20)) +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),  panel.grid.major.y = element_line(color = "grey60", linetype='dashed')) + scale_color_manual(values=My.Pal, labels = c("0.1", "1.0", "4", "7", "10", "25", "50", "75", "100")) + scale_size_continuous(name = "Dung (max)") +
  guides(color = guide_legend(override.aes = list(size = 5) ) )+ theme(panel.spacing = unit(1, "lines"))

#Figure 4b

ggplot() + geom_point(data=Results[Results$Md == 0.0,], aes(x=ParaMX, y=RLE, size=(DungMX), group=Dis, color=as.factor(Dis)))  + facet_grid(. ~ Catch, scale="free_x", labeller = labeller(Catch = catch.labs)) + 
  labs(fill = "Parasites (max)", color="Tolerance", size=21, size="Dung", x = "Parasites (max)", y = "Continuous Waterhole Occupation (max per run)") + 
  theme_classic() + theme(text=element_text(size=20)) +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),  panel.grid.major.y = element_line(color = "grey60", linetype='dashed')) + scale_color_manual(values=My.Pal, labels = c("0.1", "1.0", "4", "7", "10", "25", "50", "75", "100")) + scale_size_continuous(name = "Dung (max)") +
  guides(color = guide_legend(override.aes = list(size = 5) ) )+ theme(panel.spacing = unit(1, "lines"))

#Figure 4c

ggplot() + geom_point(data=Results[Results$Md == 0.0,], aes(x=ParaMX, y=Visits, size=(DungMX), group=Dis, color=as.factor(Dis)))   + facet_grid(. ~ Catch, scale="free_x", labeller = labeller(Catch = catch.labs)) + 
  labs(fill = "Parasites (max)", color="Tolerance", size=21, size="Dung", x = "Parasites (max)", y = "Unique Waterholes Visited (per run)") + 
  theme_classic() + theme(text=element_text(size=20)) + 
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),  panel.grid.major.y = element_line(color = "grey60", linetype='dashed')) + scale_color_manual(values=My.Pal, labels = c("0.1", "1.0", "4", "7", "10", "25", "50", "75", "100")) + scale_size_continuous(name = "Dung (max)") +
  guides(color = guide_legend(override.aes = list(size = 5) ) )+ theme(panel.spacing = unit(1, "lines"))

ggplot() + geom_point(data=Results, aes(x=ParaMX, y=(DgstMN), size=sqrt(Md), group=Dis, color=as.factor(Dis))) + facet_grid(. ~ Catch, labeller = labeller(Catch = catch.labs)) + 
  labs(fill = "Parasites (max)", color="Tolerance (Disgust)", size=21, size="Dung", x = "Parasites", y = "Maximum Disgust Modifier") + 
  theme_classic() + theme(text=element_text(size=20)) + 
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),  panel.grid.major.y = element_line(color = "grey60", linetype='dashed'),  panel.grid.minor.y = element_line(color = "grey30", linetype='dotted')) + scale_color_manual(values=My.Pal, labels = c("0.1", "1.0", "4", "7", "10.0", "100.0")) + scale_size_continuous(name = "Disgust Plasticity", breaks=c(sqrt(0.00), sqrt(0.01), sqrt(0.10), sqrt(1.00)), labels = c("0.00", "0.01", "0.10", "1.00")) +
  guides(color = guide_legend(override.aes = list(size = 5) ) )+ theme(panel.spacing = unit(1, "lines"))

#Figure 5

ggplot() + geom_point(data=MOD[MOD$Md != 0.0,], aes(x=t, y=Dgst_Md, col=as.factor(Dis)), size=0.5) + scale_color_manual(values=My.Pal)  + facet_grid(Md ~ Catch, scale="free_y", labeller = labeller(Catch = catch.labs, Md = Md.labs)) + 
  labs(color="Tolerance", x = "Time-Steps", y = "Disgust Modifier") + 
  theme_classic() + theme(text=element_text(size=20)) + 
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'), legend.position = "bottom",
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),  panel.grid.major.y = element_line(color = "grey60", linetype='dashed')) +
  guides(color = guide_legend(override.aes = list(size = 5) ) ) + theme(panel.spacing = unit(1.5, "lines"))

# + facet_grid(. ~ Catch, scale="free_y", labeller = labeller(Catch = catch.labs)) 
# My.Pal <- brewer.pal(name="Reds",n=9)[c(2,3,4,8,9,5)]