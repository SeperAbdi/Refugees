library(ggplot2)
library(readr)
library(leaflet)
library(knitr)
#install.packages("highcharter")
library(highcharter)
library(plyr)
library(dplyr)
library(viridis)
library(plotly)
library(tidyr)
library(ggmap)
library(gridExtra)
library(stringr)
library(tidyverse)
library(geosphere)
library(networkD3)
library(d3Network)
#install.packages("Iso")
library(Iso)
library(treemap)

ASM <- read_csv("asylum_seekers_monthly.csv")

kable((ASM[1:10,]))

# converting to numeric and removing na values
ASM$Value<-as.numeric(as.character(ASM$Value))
ASM<- na.omit(ASM)

#**All plots are interactive**  

# Country of Origin 

ASM %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value, na.rm = T)) -> ASM_byorg

ASM_byorg<- arrange(ASM_byorg, -Total)

hchart(ASM_byorg[1:20,], "column", hcaes(`Origin`, y = Total, color = Total)) %>%
  hc_colorAxis(stops = color_stops(n = 6, colors = c( "#21908C", "#04eadf","#440154"))) %>%
  hc_add_theme(hc_theme_db()) %>%
  hc_title(text = "Most Refugees Originate : ") %>%
  hc_credits(enabled = TRUE, text = "Data Source: UNHCR ", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)
#Although I'm no authority on the world refugee crisis; seeing Syria, Afganistan on the list is understandable. 
#But it surprising to see Russia and China on the list.  
################################################
#*****Done with first plot*****#

###>>>> Afghanistan - Syria 
ASM %>% 
  filter(Origin == "Afghanistan" | Origin == "Syrian Arab Rep.") %>% 
  select(Origin, Year, Month, Value) %>% 
  group_by(Origin,Year) %>% 
  summarise(Total = sum(Value)) ->af_S_tots

hchart(af_S_tots, "line", hcaes(x = Year, y = Total, group = Origin)) %>% 
  hc_add_theme(hc_theme_538()) %>%
  hc_title(text = "Refugees From : AFGHANISTAN - SYRIA : 1999 - 2016 ") %>%
  hc_credits(enabled = TRUE, text = "Data Source: UNHCR ", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = TRUE)

#**I don't know how to explain the sharp drop in values on the charts towards the end of 2016.   **   
#Wondering if I am making some aggregation mistake, Or if data is probably not available here.  
#*****Done with second plot***#

###>>>>>> Afghanistan - Syria 2013 - 2016 Tile plots
ASM %>% 
  filter(Year == 2016 | Year == 2015 | Year == 2014 | Year == 2013) %>% 
  filter(Origin == "Afghanistan" | Origin == "Syrian Arab Rep.") %>% 
  select(Origin, Year, Month, Value) %>% 
  group_by(Origin,Year, Month) %>% 
  summarise(Total = sum(Value)) ->as_m_tots


af<- as_m_tots[as_m_tots$Origin =="Afghanistan", ]
sy<- as_m_tots[as_m_tots$Origin =="Syrian Arab Rep.", ]

afghan <- ggplot(af, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Refugees From Afghanistan")


syria <- ggplot(sy, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Refugees from Syria")

grid.arrange(afghan, syria, ncol = 2)
#*****Done with 3rd plot*****#

#####>>>>>>Mapping Refugee resettlements movements
### Treemap:  Which countries help the most refugees in resettling ?  

resettlement<-read_csv("resettlement.csv")

colnames(resettlement)[1]<-"Country"

resettlement<-na.omit(resettlement)
resettlement$Value<-as.numeric(as.character(resettlement$Value))

resettlement %>% 
  select(Country,Value) %>% 
  group_by(Country) %>% 
  summarise(Total = sum(Value , na.rm = T )) -> acc_by_country

acc_by_country <-arrange(acc_by_country, -Total)

tm <- treemap(acc_by_country, index = c("Country"),
              vSize = "Total", vColor = "Total",
              type = "value", palette = viridis(6))

hctreemap(tm)
#*****Done with 4th plot****#

### Sankey Plot Resettlements of Refugees from Syria Afghanistan Iraq China

resettlement %>% 
  filter(Origin == "Syrian Arab Rep." |Origin == "Afghanistan"|Origin == "Iraq" ) %>% 
  select(Origin , Value , Country) %>% 
  rename(source =  Origin, target = Country) %>% 
  group_by(source, target) %>%
  na.omit() %>% 
  dplyr::summarize(value = sum(Value)) %>% 
  filter(value>4)-> Sank_usa

Nodes <-rbind(data.frame(name = unique(Sank_usa$source)), data.frame(name = unique(Sank_usa$target)))
Links <- Sank_usa
Links$source <- match(Links$source, Nodes$name) - 1
Links$target <- match(Links$target, Nodes$name) - 1

require(d3Network)

sankeyNetwork(
  Links = Links,
  Nodes = Nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  nodeWidth = 60,
  fontSize = 15
)
