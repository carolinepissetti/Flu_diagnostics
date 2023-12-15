


table(data$SwabResult,data$WipeResult,data$Population)



results

stable<-read.table(here("Outputs","Table.txt"),header = T)


stable<-stable%>%
  filter(!Parameter %in% c("Prev(1)","Prev(2)"))%>%
  dplyr::mutate_if(is.numeric, ~ round(., 4)*100)%>%
  arrange(desc(Median))

names(stable)<-c("Parameter", "2.5 %","50 % (median)","97.5 %", "Mean %", "Std. Dev. %")


dist_res<-data.frame(combine.mcmc(results))

dist_res%>%
  gather(key="measu", value="Perc")%>%
  filter(measu %in%c("se.1.","se.2.","sp.1.","sp.2."))%>%
  mutate(carac=ifelse(measu=="se.1." | measu=="se.2.","Sensitivity","Specificity"),
         test=ifelse(grepl(".1",measu),"Nasal \n Wipe","Nasal \n Swab"))%>%
  ggplot()+  
  theme_minimal()+
  theme(legend.title = element_blank(),
        legend.position="right") +
  geom_density_ridges(aes(x=Perc,y=test, group=interaction(test,carac), fill = carac),
                      rel_min_height = 0.01,alpha=0.5, color="white",scale=1.001)+
  ylab(" ")+
  xlab(" ")+
  scale_x_continuous(labels = scales::percent,limits = c(0.6,1.001))+
  scale_y_discrete(expand = c(0,1,1, 1),
                   labels = c(
                     "se.1." = "Se \n NW",
                     "se.2." = "Se \n NS",
                     "sp.1." = "Sp \n NW",
                     "sp.2." = "Sp \n NS"
                   )
  )+
  annotate(geom = "table",x = 0.6,y = 3.5, 
           label = list(cbind(stable))) 

## separate ##

#dist_res%>%
#gather(key="measu", value="Perc")%>%
#  filter(measu %in%c("se.1.","se.2.","sp.1.","sp.2."))%>%
#  mutate(carac=ifelse(measu=="se.1." | measu=="se.2.","Sensitivity","Specificity"))%>%
#  ggplot(aes(x=Perc,y=measu,fill = carac,))+
#  theme(legend.position="none") +
#  geom_density_ridges(rel_min_height = 0.01,alpha=0.5, color="white")+
#  ylab(" ")+
#  xlab(" ")+
#  scale_x_continuous(labels = scales::percent,limits = c(0.5,1.001))+
#  scale_y_discrete(expand = c(0,1,1, 1),
#                   labels = c(
#                     "se.1." = "Se \n NW",
#                     "se.2." = "Se \n NS",
#                     "sp.1." = "Sp \n NW",
#                     "sp.2." = "Sp \n NS"
#                   )
#  )+
#  annotate(geom = "table",x = 0.5,y = 5.5, 
#           label = list(cbind(stable)))


##Predictive values

prev<-seq(0,1,0.001)

#PPV
PPV1<-(prev*mean(dist_res$se.1.))/(prev*mean(dist_res$se.1.)+((1-prev)*(1-mean(dist_res$sp.1.))))
PPV2<-(prev*mean(dist_res$se.2.))/(prev*mean(dist_res$se.2.)+((1-prev)*(1-mean(dist_res$sp.2.))))

plot(prev,PPV1,type = "l",col="red")
lines(prev,PPV2,col="blue")

#PNV
PNV1<-((1-prev)*mean(dist_res$sp.1.))/((1-prev)*mean(dist_res$sp.1.)+((prev)*(1-mean(dist_res$se.1.))))
PNV2<-((1-prev)*mean(dist_res$sp.2.))/((1-prev)*mean(dist_res$sp.2.)+((prev)*(1-mean(dist_res$se.2.))))

plot(prev,PNV1,type = "l",col="red")
lines(prev,PNV2,type = "l",col="blue")


plot(results)

