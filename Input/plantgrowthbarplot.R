
####Hello this is my help with Sophie on 6/18/25#####
 ####Getting set up######
#make sure to open the R environment from the microbiome "completed_pipeline.RData
setwd("~/Documents/GitHub/Cotton-Soil-Micro/Input")
read.csv("./plantgrowthup.csv")
PlantGrowth= read.csv("./plantgrowthup.csv")

######## looking to make the table in long format######
##Making a merge
Pbarfunc = merge(PlantGrowth[,c(1,2,3)],pseq_barplot_slice, all.y = TRUE)

Pbarpgr = Pbarfunc %>%group_by(PGR)%>%summarise(Csum= sum(cumulative_sums))

##Making a new long data frame
Pbarfunc = merge(PlantGrowth[,c(1,2,3)],pseq_barplot_slice, all.y = TRUE)

pbarfunclong = Pbarfunc[,c(1:28)] %>%pivot_longer(cols = c(10:28), names_to = "sample", values_to = "reads")
view(pbarfunclong)


##reading and modifying the metadata table##
metadatapgr=read_tsv("./metadataPhyloseqFormatted.tsv")

##making the metadata merge with the original graph##
metadatapgr$sample=metadatapgr$old_ID

pgrfunclong = merge(pbarfunclong, metadatapgr[,c(3,8)], all.x = TRUE)
View(pgrfunclong)

Pbarpgr = pgrfunclong %>%
  group_by(PGR, Irrigation)%>%summarise(Csum= sum(reads))

###Summarizing it######

Pbarpgr$rf=(Pbarpgr$Csum/sum(Pbarpgr$Csum))*100

Pbarpgr= Pbarpgr%>%
  group_by(Irrigation)%>%
  mutate(rf=(Csum/sum(Csum))*100)

######Results from that should be
#PGR "N"+ Irrigation "D" rf = 6.443259
#PGR "N"+ Irrigation "I" rf = 4.530576
#PGR "Y"+ Irrigation "D" rf = 93.556741
#PGR "Y"+ Irrigation "I" rf = 95.469424

#####Graphing possible####
library(ggplot2)


Pbarpgrup = Pbarpgr

ggplot(data = Pbarpgr, aes(x = Irrigation, y = rf, fill = Irrigation)) +
  geom_boxplot()

ggplot(data = Pbarpgr, aes(x = Irrigation, y = rf, fill = Irrigation)) +
  geom_boxplot()+
  labs(title = "PGR and Irrigation", x="Irrigation", y="Proportion Found")+
  scale_x_discrete(labels=c("Non-Irrigated", "Irrigated"))+
  theme_classic()
###Doesnt look like much :( Sad ######   

######Chi square and fisher test#####
dat <- Pbarpgrup

testsq <- chisq.test(table(dat$Irrigation, dat$PGR))

testfis <- fisher.test(dat)

testsq <- chisq.test(table(dat$Irrigation, dat$Csum))

testfis <- fisher.test(table(dat$PGR, dat$Csum))

testfis <- fisher.test(table(dat$Irrigation, dat$Csum))

fisher.test(table(dat$Irrigation, dat$Csum))

fisher.test(table(dat$rf, dat$PGR))


