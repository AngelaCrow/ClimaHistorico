library("raster", quietly = TRUE)
library("dplyr", quietly = TRUE)
library("ggplot2", quietly = TRUE)
library("tidyr", quietly = TRUE)


setwd("E:/MK_articuloP")

#Bio1####
climat1<-raster("F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Bioclimas/Bioclimas_1910-1949/bio13_t1.tif")
climat2<-raster("F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Bioclimas/Bioclimas_1950-1979/bio13_t2.tif")
climat3<-raster("F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Bioclimas/Bioclimas_1980-2009/bio13_t3.tif")

nea.df<-read.csv("neartic.csv")
nea.df<-dplyr::select(nea.df, "POINT_X", "POINT_Y")
nea_df_t1<-data.frame(raster::extract(climat1,nea.df))
names(nea_df_t1)<-c("Bio13")
nea_df_t1$Region<-"Neartic region"
nea_df_t1$Periods<-"1910-1949"

nea_df_t2<-data.frame(raster::extract(climat2,nea.df))
names(nea_df_t2)<-c("Bio13")
nea_df_t2$Region<-"Neartic region"
nea_df_t2$Periods<-"1950-1979"

nea_df_t3<-data.frame(raster::extract(climat3,nea.df))
names(nea_df_t3)<-c("Bio13")
nea_df_t3$Region<-"Neartic region"
nea_df_t3$Periods<-"1980-2009"

neartic<-rbind(nea_df_t1,nea_df_t2,nea_df_t3)

#ZOnadeTransicion
trans.df<-read.csv("transicion.csv")
trans.df<-dplyr::select(trans.df, "POINT_X", "POINT_Y")
trans_df_t1<-data.frame(raster::extract(climat1,trans.df))
names(trans_df_t1)<-c("Bio13")
trans_df_t1$Region<-"Transition zone"
trans_df_t1$Periods<-"1910-1949"

trans_df_t2<-data.frame(raster::extract(climat2,trans.df))
names(trans_df_t2)<-c("Bio13")
trans_df_t2$Region<-"Transition zone"
trans_df_t2$Periods<-"1950-1979"

trans_df_t3<-data.frame(raster::extract(climat3,trans.df))
names(trans_df_t3)<-c("Bio13")
trans_df_t3$Region<-"Transition zone"
trans_df_t3$Periods<-"1980-2009"

Ztrans<-rbind(trans_df_t1,trans_df_t2,trans_df_t3)

#Neotropical
neo.df<-read.csv("neotropico.csv")
neo.df<-dplyr::select(neo.df, "POINT_X", "POINT_Y")
neo_df_t1<-data.frame(raster::extract(climat1,neo.df))
names(neo_df_t1)<-c("Bio13")
neo_df_t1$Region<-"Neotropical region"
neo_df_t1$Periods<-"1910-1949"

neo_df_t2<-data.frame(raster::extract(climat2,neo.df))
names(neo_df_t2)<-c("Bio13")
neo_df_t2$Region<-"Neotropical region"
neo_df_t2$Periods<-"1950-1979"

neo_df_t3<-data.frame(raster::extract(climat3,neo.df))
names(neo_df_t3)<-c("Bio13")
neo_df_t3$Region<-"Neotropical region"
neo_df_t3$Periods<-"1980-2009"

neotropic <-rbind(neo_df_t1,neo_df_t2,neo_df_t3)


mexico<-rbind(neartic, Ztrans, neotropic)
mexico[2]<-c("Mexico")
mexicoAll<-rbind(neartic, Ztrans, neotropic, mexico)

write.csv(mexicoAll,"bio01y12/mexicoAll_Bio13.csv")

tb_bio13<-mexicoAll %>% split(.$Region) %>%  
  map(~ group_by(.x,.$Periods) %>% summarise(Promedio=mean(Bio13, na.rm = TRUE), 
                                             Mediana = median(Bio13, na.rm = TRUE),
                                             Variance = var(Bio13, na.rm = TRUE),
                                             Quar = IQR(Bio13, na.rm = TRUE),
                                             Minimum = min(Bio13, na.rm = TRUE), 
                                             Maximum = max(Bio13, na.rm = TRUE, 
                                                           StandarDev = sd(Bio13, na.rm = TRUE))))

write.csv(tb_bio13[[1]],"tb_Bio13_Mexico.csv")
write.csv(tb_bio13[[2]],"tb_Bio13_Nea.csv")
write.csv(tb_bio13[[3]],"tb_Bio13_Neo.csv")
write.csv(tb_bio13[[4]],"tb_Bio13_Ztrans.csv")

#graficas por temperatura
sp_t<-cbind(mexicoAll[c("Periods")],mexicoAll[c("Region")],mexicoAll[c("Bio13")])
head(sp_t)
unique(sp_t$Periods)
sp_t$Region_f = factor(sp_t$Region, levels = c("Mexico","Neartic region", "Transition zone", "Neotropical region"))

p <- ggplot(data = sp_t, aes(x=Periods, y=Bio13)) + 
  geom_boxplot(alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2.5, color="red", fill="red") 
p  + facet_grid(.~Region_f)+theme(legend.position="bottom")  + labs( y = "Precipitation of Wettest Month (mm)")

ggsave("bio01y12/bio13.jpg", dpi = 300)

rm(mexicoAll, climat1,climat2,climat3)

#bio14####

climat1<-raster("F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Bioclimas/Bioclimas_1910-1949/bio14_t1.tif")
climat2<-raster("F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Bioclimas/Bioclimas_1950-1979/bio14_t2.tif")
climat3<-raster("F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Bioclimas/Bioclimas_1980-2009/bio14_t3.tif")

nea.df<-read.csv("neartic.csv")
nea.df<-dplyr::select(nea.df, "POINT_X", "POINT_Y")
nea_df_t1<-data.frame(raster::extract(climat1,nea.df))
names(nea_df_t1)<-c("bio14")
nea_df_t1$Region<-"Neartic region"
nea_df_t1$Periods<-"1910-1949"

nea_df_t2<-data.frame(raster::extract(climat2,nea.df))
names(nea_df_t2)<-c("bio14")
nea_df_t2$Region<-"Neartic region"
nea_df_t2$Periods<-"1950-1979"

nea_df_t3<-data.frame(raster::extract(climat3,nea.df))
names(nea_df_t3)<-c("bio14")
nea_df_t3$Region<-"Neartic region"
nea_df_t3$Periods<-"1980-2009"

neartic<-rbind(nea_df_t1,nea_df_t2,nea_df_t3)

#ZOnadeTransicion
trans.df<-read.csv("transicion.csv")
trans.df<-dplyr::select(trans.df, "POINT_X", "POINT_Y")
trans_df_t1<-data.frame(raster::extract(climat1,trans.df))
names(trans_df_t1)<-c("bio14")
trans_df_t1$Region<-"Transition zone"
trans_df_t1$Periods<-"1910-1949"

trans_df_t2<-data.frame(raster::extract(climat2,trans.df))
names(trans_df_t2)<-c("bio14")
trans_df_t2$Region<-"Transition zone"
trans_df_t2$Periods<-"1950-1979"

trans_df_t3<-data.frame(raster::extract(climat3,trans.df))
names(trans_df_t3)<-c("bio14")
trans_df_t3$Region<-"Transition zone"
trans_df_t3$Periods<-"1980-2009"

Ztrans<-rbind(trans_df_t1,trans_df_t2,trans_df_t3)

#Neotropical
neo.df<-read.csv("neotropico.csv")
neo.df<-dplyr::select(neo.df, "POINT_X", "POINT_Y")
neo_df_t1<-data.frame(raster::extract(climat1,neo.df))
names(neo_df_t1)<-c("bio14")
neo_df_t1$Region<-"Neotropical region"
neo_df_t1$Periods<-"1910-1949"

neo_df_t2<-data.frame(raster::extract(climat2,neo.df))
names(neo_df_t2)<-c("bio14")
neo_df_t2$Region<-"Neotropical region"
neo_df_t2$Periods<-"1950-1979"

neo_df_t3<-data.frame(raster::extract(climat3,neo.df))
names(neo_df_t3)<-c("bio14")
neo_df_t3$Region<-"Neotropical region"
neo_df_t3$Periods<-"1980-2009"

neotropic <-rbind(neo_df_t1,neo_df_t2,neo_df_t3)


mexico<-rbind(neartic, Ztrans, neotropic)
mexico[2]<-c("Mexico")
mexicoAll<-rbind(neartic, Ztrans, neotropic, mexico)


write.csv(mexicoAll,"bio01y12/mexicoAll_bio14.csv")

tb_bio14<-mexicoAll %>% split(.$Region) %>%  
  map(~ group_by(.x,.$Periods) %>% summarise(Promedio=mean(bio14, na.rm = TRUE), 
                                             Mediana = median(bio14, na.rm = TRUE),
                                             Variance = var(bio14, na.rm = TRUE),
                                             Quar = IQR(bio14, na.rm = TRUE),
                                             Minimum = min(bio14, na.rm = TRUE), 
                                             Maximum = max(bio14, na.rm = TRUE, 
                                                           StandarDev = sd(bio14, na.rm = TRUE))))

write.csv(tb_bio14[[1]],"tb_bio14_Mexico.csv")
write.csv(tb_bio14[[2]],"tb_bio14_Nea.csv")
write.csv(tb_bio14[[3]],"tb_bio14_Neo.csv")
write.csv(tb_bio14[[4]],"tb_bio14_Ztrans.csv")


#graficas por temperatura
sp_t<-cbind(mexicoAll[c("Periods")],mexicoAll[c("Region")],mexicoAll[c("bio14")])
head(sp_t)
unique(sp_t$Periods)
sp_t$Region_f = factor(sp_t$Region, levels = c("Mexico","Neartic region", "Transition zone", "Neotropical region"))

p <- ggplot(data = sp_t, aes(x=Periods, y=bio14)) + 
  geom_boxplot(alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2.5, color="red", fill="red") 
p  + facet_grid(.~Region_f)+theme(legend.position="bottom")  + labs( y = "Precipitation of Driest Month (mm)")

ggsave("bio01y12/bio14.jpg", dpi = 300)

