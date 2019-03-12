library("raster", quietly = TRUE)
library("dplyr", quietly = TRUE)
library("ggplot2", quietly = TRUE)
library("tidyr", quietly = TRUE)
library("purrr", quietly = TRUE)


setwd("E:/MK_articuloP")

#Bio5####
nea.df<-read.csv("neartic.csv")
nea.df<-dplyr::select(nea.df, "POINT_X", "POINT_Y")
climat1<-raster("F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Bioclimas/Bioclimas_1910-1949/bio05_t1.tif")
nea_df_t1<-data.frame(raster::extract(climat1,nea.df))
names(nea_df_t1)<-c("Bio5")
nea_df_t1$Region<-"Neartic region"
nea_df_t1$Periods<-"1910-1949"

climat2<-raster("F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Bioclimas/Bioclimas_1950-1979/bio05_t2.tif")
nea_df_t2<-data.frame(raster::extract(climat2,nea.df))
names(nea_df_t2)<-c("Bio5")
nea_df_t2$Region<-"Neartic region"
nea_df_t2$Periods<-"1950-1979"

climat3<-raster("F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Bioclimas/Bioclimas_1980-2009/bio05_t3.tif")
nea_df_t3<-data.frame(raster::extract(climat3,nea.df))
names(nea_df_t3)<-c("Bio5")
nea_df_t3$Region<-"Neartic region"
nea_df_t3$Periods<-"1980-2009"

neartic<-rbind(nea_df_t1,nea_df_t2,nea_df_t3)

#ZOnadeTransicion
trans.df<-read.csv("transicion.csv")
trans.df<-dplyr::select(trans.df, "POINT_X", "POINT_Y")
trans_df_t1<-data.frame(raster::extract(climat1,trans.df))
names(trans_df_t1)<-c("Bio5")
trans_df_t1$Region<-"Transition zone "
trans_df_t1$Periods<-"1910-1949"

trans_df_t2<-data.frame(raster::extract(climat2,trans.df))
names(trans_df_t2)<-c("Bio5")
trans_df_t2$Region<-"Transition zone "
trans_df_t2$Periods<-"1950-1979"

trans_df_t3<-data.frame(raster::extract(climat3,trans.df))
names(trans_df_t3)<-c("Bio5")
trans_df_t3$Region<-"Transition zone "
trans_df_t3$Periods<-"1980-2009"

Ztrans<-rbind(trans_df_t1,trans_df_t2,trans_df_t3)

#Neotropical
neo.df<-read.csv("neotropico.csv")
neo.df<-dplyr::select(neo.df, "POINT_X", "POINT_Y")
neo_df_t1<-data.frame(raster::extract(climat1,neo.df))
names(neo_df_t1)<-c("Bio5")
neo_df_t1$Region<-"Neotropical region"
neo_df_t1$Periods<-"1910-1949"

neo_df_t2<-data.frame(raster::extract(climat2,neo.df))
names(neo_df_t2)<-c("Bio5")
neo_df_t2$Region<-"Neotropical region"
neo_df_t2$Periods<-"1950-1979"

neo_df_t3<-data.frame(raster::extract(climat3,neo.df))
names(neo_df_t3)<-c("Bio5")
neo_df_t3$Region<-"Neotropical region"
neo_df_t3$Periods<-"1980-2009"

neotropic <-rbind(neo_df_t1,neo_df_t2,neo_df_t3)


mexico<-rbind(neartic, Ztrans, neotropic)
mexico[2]<-c("Mexico")
mexicoAll<-rbind(neartic, Ztrans, neotropic, mexico)

write.csv(mexicoAll,"Bio01y12/mexicoAll_Bio5.csv")

tb_Bio5<-mexicoAll %>% split(.$Region) %>%  
  map(~ group_by(.x,.$Periods) %>% summarise(Promedio=mean(Bio5, na.rm = TRUE), 
                                             Mediana = median(Bio5, na.rm = TRUE),
                                             Variance = var(Bio5, na.rm = TRUE),
                                             Quar = IQR(Bio5, na.rm = TRUE),
                                             Minimum = min(Bio5, na.rm = TRUE), 
                                             Maximum = max(Bio5, na.rm = TRUE, 
                                             StandarDev = sd(Bio5, na.rm = TRUE))))

write.csv(tb_Bio6[[1]],"tb_Bio5_Mexico.csv")
write.csv(tb_Bio6[[2]],"tb_Bio5_Nea.csv")
write.csv(tb_Bio6[[3]],"tb_Bio5_Neo.csv")
write.csv(tb_Bio6[[4]],"tb_Bio5_Ztrans.csv")

#mexicoAll %>% split(.$Region) %>%  
 # map(~ group_by(.x,.$Periods)
      
  #mexicoAll %>%
  #tbl_df() %>%
  #nest(-Region) %>%
  #mutate(Quantiles = map(data, ~ quantile(.$Bio5)),
   #      Quantiles = map(Quantiles, ~ bind_rows(.) %>% gather())) %>% 
  #unnest(Quantiles))

#graficas por temperatura
sp_t<-cbind(mexicoAll[c("Periods")],mexicoAll[c("Region")],mexicoAll[c("Bio5")])
head(sp_t)
unique(sp_t$Periods)

sp_t$Region_f = factor(sp_t$Region, levels = c("Mexico","Neartic region", "Transition zone ", "Neotropical region"))

p <- ggplot(data = sp_t, aes(x=Periods, y=Bio5)) + 
  geom_boxplot(alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2.5, color="red", fill="red") 
p  + facet_grid(. ~ Region_f)+theme(legend.position="bottom")  + labs( y = "Maximum temperature of the warmest month (°C)")

ggsave("Bio01y12/Bio5.jpg", dpi = 300)

rm(mexicoAll, climat1, climat2, climat3)

#Bio6####

nea.df<-read.csv("neartic.csv")
nea.df<-dplyr::select(nea.df, "POINT_X", "POINT_Y")
climat1<-raster("F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Bioclimas/Bioclimas_1910-1949/bio06_t1.tif")
nea_df_t1<-data.frame(raster::extract(climat1,nea.df))
names(nea_df_t1)<-c("Bio6")
nea_df_t1$Region<-"Neartic region"
nea_df_t1$Periods<-"1910-1949"

climat2<-raster("F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Bioclimas/Bioclimas_1950-1979/bio06_t2.tif")
nea_df_t2<-data.frame(raster::extract(climat2,nea.df))
names(nea_df_t2)<-c("Bio6")
nea_df_t2$Region<-"Neartic region"
nea_df_t2$Periods<-"1950-1979"

climat3<-raster("F:/COBERTURAS/CLIMA_ANGELA/PeriodosClimaticos_tif/Bioclimas/Bioclimas_1980-2009/bio06_t3.tif")
nea_df_t3<-data.frame(raster::extract(climat3,nea.df))
names(nea_df_t3)<-c("Bio6")
nea_df_t3$Region<-"Neartic region"
nea_df_t3$Periods<-"1980-2009"

neartic<-rbind(nea_df_t1,nea_df_t2,nea_df_t3)

#ZOnadeTransicion
trans.df<-read.csv("transicion.csv")
trans.df<-dplyr::select(trans.df, "POINT_X", "POINT_Y")
trans_df_t1<-data.frame(raster::extract(climat1,trans.df))
names(trans_df_t1)<-c("Bio6")
trans_df_t1$Region<-"Transition zone "
trans_df_t1$Periods<-"1910-1949"

trans_df_t2<-data.frame(raster::extract(climat2,trans.df))
names(trans_df_t2)<-c("Bio6")
trans_df_t2$Region<-"Transition zone "
trans_df_t2$Periods<-"1950-1979"

trans_df_t3<-data.frame(raster::extract(climat3,trans.df))
names(trans_df_t3)<-c("Bio6")
trans_df_t3$Region<-"Transition zone "
trans_df_t3$Periods<-"1980-2009"

Ztrans<-rbind(trans_df_t1,trans_df_t2,trans_df_t3)

#Neotropical
neo.df<-read.csv("neotropico.csv")
neo.df<-dplyr::select(neo.df, "POINT_X", "POINT_Y")
neo_df_t1<-data.frame(raster::extract(climat1,neo.df))
names(neo_df_t1)<-c("Bio6")
neo_df_t1$Region<-"Neotropical region"
neo_df_t1$Periods<-"1910-1949"

neo_df_t2<-data.frame(raster::extract(climat2,neo.df))
names(neo_df_t2)<-c("Bio6")
neo_df_t2$Region<-"Neotropical region"
neo_df_t2$Periods<-"1950-1979"

neo_df_t3<-data.frame(raster::extract(climat3,neo.df))
names(neo_df_t3)<-c("Bio6")
neo_df_t3$Region<-"Neotropical region"
neo_df_t3$Periods<-"1980-2009"

neotropic <-rbind(neo_df_t1,neo_df_t2,neo_df_t3)


mexico<-rbind(neartic, Ztrans, neotropic)
mexico[2]<-c("Mexico")
mexicoAll<-rbind(neartic, Ztrans, neotropic, mexico)

write.csv(mexicoAll,"Bio01y12/mexicoAll_Bio6.csv")

tb_Bio6<-mexicoAll %>% split(.$Region) %>%  
  map(~ group_by(.x,.$Periods) %>% summarise(Promedio=mean(Bio6, na.rm = TRUE), 
                                             Mediana = median(Bio6, na.rm = TRUE),
                                             Variance = var(Bio6, na.rm = TRUE),
                                             Quar = IQR(Bio6, na.rm = TRUE),
                                             Minimum = min(Bio6, na.rm = TRUE), 
                                             Maximum = max(Bio6, na.rm = TRUE, 
                                                           StandarDev = sd(Bio6, na.rm = TRUE))))

write.csv(tb_Bio6[[1]],"tb_Bio6_Mexico.csv")
write.csv(tb_Bio6[[2]],"tb_Bio6_Nea.csv")
write.csv(tb_Bio6[[3]],"tb_Bio6_Neo.csv")
write.csv(tb_Bio6[[4]],"tb_Bio6_Ztrans.csv")

#mexicoAll %>% split(.$Region) %>%  
#  map(~ group_by(.x,.$Periods)
      
#      mexicoAll %>%
#       tbl_df() %>%
#        nest(-Region) %>%
#        mutate(Quantiles = map(data, ~ quantile(.$Bio62)),
#               Quantiles = map(Quantiles, ~ bind_rows(.) %>% gather())) %>% 
#        unnest(Quantiles))

#graficas por temperatura
sp_t<-cbind(mexicoAll[c("Periods")],mexicoAll[c("Region")],mexicoAll[c("Bio6")])
head(sp_t)
unique(sp_t$Periods)
sp_t$Region_f = factor(sp_t$Region, levels = c("Mexico","Neartic region", "Transition zone ", "Neotropical region"))

p <- ggplot(data = sp_t, aes(x=Periods, y=Bio6)) + 
  geom_boxplot(alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2.5, color="red", fill="red") 
p  + facet_grid(. ~ Region_f)+theme(legend.position="bottom")  + labs( y = "Minimun Temperature of Coldest Month (°C)")

ggsave("Bio01y12/Bio6.jpg", dpi = 300)