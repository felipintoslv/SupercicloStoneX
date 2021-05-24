######################### Código principal ####################################
library(readr)
library(tidyverse)

CRB <- read_csv("CRB.csv", col_types = cols(`Var%` = col_number()), 
                +     locale = locale(decimal_mark = ","))



source("functions.R")

CRB<-trat(CRB)

### onde estáo máximo

max1<-max(CRB$Último)

CRB%>%
  filter(Último == max1)

VP(CRB, "2001-12-31", "2008-07-02")

plot1<-ggplot(CRB, aes(x = Data, y = Último)) +
  geom_line(color = "darkblue") +
  ylim(100,500)+
  ylab("TR/CC CRB" )+
  annotate(geom="text", x=as.Date("2007-06-02"), y=473.52, 
                             label="Valor máximo do último \nsupercilo \ndas commodities") +
  annotate(geom="point", x=as.Date("2008-07-01"), y=473.52, size=10, shape=21, fill="transparent") +
 theme_light()

plot2<-CRB%>%
  filter(Data > "2020-05-21")%>%
  ggplot(aes(x = Data, y = Último)) +
  geom_line(color = "darkblue") +
  ylim(100,210)+
  ylab("TR/CC CRB" ) +
  theme_light()

multiplot(plot1,plot2,cols = 2)



### BCOM Index

BCOM<-trat(BCOM)

### onde estáo máximo

max2<-max(BCOM$Último)

BCOM%>%
  filter(Último == max2)


plot3<-ggplot(BCOM, aes(x = Data, y = Último)) +
  geom_line(color = "darkblue") +
  ylim(50,250)+
  ylab("BCOM" )+
  annotate(geom="text", x=as.Date("2007-07-02"), y=237.953, 
           label="Valor máximo do último \nsupercilo \ndas commodities") +
  annotate(geom="point", x=as.Date("2008-07-02"), y=237.953, size=10, shape=21, fill="transparent") +
  theme_light()

plot4<-CRB%>%
  filter(Data > "2020-05-21")%>%
  ggplot(aes(x = Data, y = Último)) +
  geom_line(color = "darkblue") +
  ylim(100,210)+
  ylab("BCOM" ) +
  theme_light()

multiplot(plot3,plot4,cols = 2)
