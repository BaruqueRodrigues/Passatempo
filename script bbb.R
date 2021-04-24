#####Scritp Graf BBB

  
bbb<-read_excel("teste.xlsx")
bbb$final<-as_date(bbb$final)
class(bbb$final)
deflateBR::
library(deflateBR)
bbb$<-deflateBR::inflation(bbb$premio,bbb$final, "03/2021")
install.packages("priceR")
library(priceR)
dolar<-priceR::historical_exchange_rates("USD", "BRL",
                                  start_date = "2002-04-02", end_date = "2021-04-24")

bbb<-merge(bbb, dolar, by.x = "final", by.y="date")

library(dplyr)

bbb<-bbb%>%mutate(dolar=premio/one_USD_equivalent_to_x_BRL)
bbb$dol_cpi<-priceR::adjust_for_inflation(bbb$dolar, bbb$final,
                             "US", 2019)
usa_inflation<-retrieve_inflation_data("US")

library(ggplot2)

bbb%>%ggplot(aes(x=final))+
  geom_line(aes(y=bbb$ipca), color ="darkred")+
  geom_line(aes(y=bbb$igpm), color ="blue")+
  geom_line(aes(y=bbb$dolar), color ="green")+
  geom_line(aes(y=bbb$dol_cpi), color ="purple")+
  geom_line(aes(y=bbb$premio), color ="orange")+
  theme_minimal()

options(scipen=999)

library(tidyr)
install.packages("jcolors")
bbb2<-bbb%>%gather(indice, valor, premio:dol_cpi, -one_USD_equivalent_to_x_BRL,-final,-edicao)
library(jcolors)
bbb2%>%ggplot(aes(x=final, y=valor, fill =indice))+
  geom_line(aes(color = indice#, linetype = indice
                ), size =1.2)+
  xlab("")+ylab(NULL)+
  theme_minimal()+
  scale_color_manual(values = c("#D36135", "#7FB069", "#ECE4B7", "#E6AA68", "#02020B"))+
  scale_x_date(breaks = "1 year",date_labels = "%Y")+
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, size=rel(1.2), face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        plot.caption = element_text(hjust = 0.5, size = 10))+
  labs(title = "Valor Comparado da Premiação do BBB ao longo dos anos",
       caption = "Baruque Rodrigues e José Arteta 
       ")
    

bbb2%>%ggplot(aes(x=final, y=valor, fill =indice))+
  geom_line(aes(color = indice#, linetype = indice
  ), size =1.2)+
  xlab(NULL)+ylab(NULL)+
  theme_minimal()+
  scale_color_jcolors(palette = "pal3")+
  scale_x_date(breaks = "1 year",date_labels = "%Y")+
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, size=rel(1.2), face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        plot.caption = element_text(hjust = 0.5, size = 10))+
  labs(title = "Valor Comparado da Premiação do BBB ao longo dos anos",
       caption = "Baruque Rodrigues e José Arteta")
