library("readr") 
library('tidyr') 
library('dplyr') 
library('ggplot2')
library('ggthemes')
library('corrplot') 
library('lubridate')
library('purrr') 
library('cowplot')
library('maps')
library('viridis')
library('treemap')
library('leaflet')
library('dygraphs')
library('graphics')
library('forecast')
library('xts')
library('IRdisplay')

data = read.csv(file="C:/Users/HP/Downloads/FAO.csv", header=TRUE, sep=",")
head(data)

str(data)
summary(data)

options(repr.plot.width=6, repr.plot.height=6)

missing_data = data %>% summarise_all(funs(sum(is.na(.))/n()))

missing_data = gather(missing_data, key = "variables", value = "percent_missing") 

ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +xlab('variables')+
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+coord_flip()+ theme_economist()

colnames(data) = c("area_abb","area_code","area","item_code", "item", "element_code", "element")

View(data)

#help("options")

options(repr.plot.width=10, repr.plot.height=4)
data = mutate(data, Total=apply(data[11:63], 1, sum, na.rm = T))
data = mutate(data, last5=apply(data[58:63], 1, sum, na.rm = T))

p1 = data %>% group_by(area_abb, element) %>% filter(element == 'Food')%>% 
  summarise(TFO = sum(Total)) %>% ungroup()%>%mutate(pct = prop.table(TFO)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(area_abb, -pct), y = pct)) + 
  geom_bar(stat = 'identity', fill = "green", aes(color = I('black')), size = 0.1) + 
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.5,
            vjust = -0.5, size = 3)+ theme_economist()+ xlab("Country") + ylab("Food production since 1961")

plot(p1)

p2 = data %>% group_by(area_abb, element) %>%filter(element == 'Food')%>% 
  summarise(TFO = sum(last5)) %>% ungroup()%>%mutate(pct = prop.table(TFO)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(area_abb, -pct), y = pct)) + 
  geom_bar(stat = 'identity', fill = "green", aes(color = I('gray')), size = 0.1) + 
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.5,
            vjust = -0.5, size =3)+ theme_economist()+ xlab("Country") + ylab("Food production since 2008-13")

plot(p2)


p3 = data %>% group_by(area_abb, element) %>%filter(element == 'Feed')%>% 
  summarise(TFE = sum(Total)) %>% ungroup()%>%mutate(pct = prop.table(TFE)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(area_abb, -pct), y = pct)) + 
  geom_bar(stat = 'identity', fill = "lightblue", aes(color = I('black')), size = 0.1) + 
  geom_text(aes(label = sprintf("%.2f%%", pct)), 
            vjust = -.5, size = 3)+ theme_dark()+ xlab("Country") + ylab("Feed production since 1961")
plot_grid(p3)

p4 = data %>% group_by(area_abb, element) %>%filter(element == 'Feed')%>% 
  summarise(TFE = sum(last5)) %>% ungroup()%>%mutate(pct = prop.table(TFE)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(area_abb, -pct), y = pct)) + 
  geom_bar(stat = 'identity', fill = "lightblue", aes(color = I('black')), size = 0.1) + 
  geom_text(aes(label = sprintf("%.2f%%", pct)), 
            vjust = -.5, size = 3)+ theme_dark()+ xlab("Country") + ylab("Feed production since 2008-13")

plot(p4)



p5 = data %>% group_by(item) %>%filter(element == 'Food')%>% 
  summarise(TFO = sum(Total)) %>% ungroup()%>%mutate(pct = prop.table(TFO)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(item, pct), y = pct)) + 
  geom_bar(stat = 'identity', fill = "lightyellow", aes(color = I('black')), size = 0.1) + coord_flip()+
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 1.2,
            vjust = -0.5, size = 3)+ theme_economist()+ xlab("Food Item") + ylab("Food Item production since 1961")
plot(p5)

p6 = data %>% group_by(item) %>%filter(element == 'Food')%>% 
  summarise(TFO = sum(last5)) %>% ungroup()%>%mutate(pct = prop.table(TFO)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(item, pct), y = pct)) + ggtitle("Food Item wise Production since 2008-13") +
  geom_bar(stat = 'identity', fill = "#999999", aes(color = I('black')), size = 0.1) + coord_flip()+ 
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 1.2,
            vjust = -0.5, size =3)+ theme_economist() + xlab("Food Item") + ylab("Food Item production since 2008-13")

plot(p6)




p7 = data %>% group_by(item) %>%filter(element == 'Feed')%>% 
  summarise(TFO = sum(Total)) %>% ungroup()%>%mutate(pct = prop.table(TFO)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(item, pct), y = pct)) + 
  geom_bar(stat = 'identity', fill = "#CC6666", aes(color = I('black')), size = 0.1) + coord_flip()+
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.01,
            vjust = -0.5, size = 3)+ theme_economist()+ xlab("Food Item") + ylab("Feed Item production since 1961")

p8 = data %>% group_by(item) %>%filter(element == 'Feed')%>% 
  summarise(TFO = sum(last5)) %>% ungroup()%>%mutate(pct = prop.table(TFO)*100)%>%
  top_n(10, wt = pct)%>%
  ggplot(aes(x = reorder(item, pct), y = pct)) +
  geom_bar(stat = 'identity', fill = "#CC6666", aes(color = I('black')), size = 0.1) + coord_flip()+
  geom_text(aes(label = sprintf("%.2f%%", pct)), hjust = 0.01,
            vjust = -0.5, size =3)+ theme_economist()+ xlab("Food Item") + ylab("Feed Item production since 2008-13")

plot_grid(p7,p8, align = "v")













