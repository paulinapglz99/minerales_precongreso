#Script para generar un grafico de paletas a partir de datos de minerales
#Alejandra P. Perez-Gonzalez paulinapglz.99@gmail.com

#Recuerda que antes de que empieces a correr cualquier codigo en R, debes de asignar la 
#carpeta de trabajo donde se encuentran nuestros archivos de datos

#El primer paso es instalar las librerias que no tengamos

install.packages("dplyr")   #para el manejo de datos
install.packages("ggplot2") #para los graficos
install.packages("vroom") #para leer datos facilmente
install.packages("stringr") #para filtrar datos 
install.packages("viridis") #colores wonitos

#Llamar a las liberias

library("dplyr")   #para el manejo de datos
library("ggplot2") #para los graficos
library("vroom") #para leer datos facilmente
library("stringr") #para filtrar datos 
library("viridis") #colores wonitos

#Recuerda que debemos tener los datos en nuestro directorio de trabajo
#Ahora tenemos que leeer los datos. Los asignaremos a un objeto.

datos_minerales <- vroom( file = "https://data.biofreelancer.com/minerales" )  #La funcion vroom lee de manera automatica cualquier dataset

#Una vez que hemos leido los datos, podemos visualizarlos en nuestra seccion de Data 

#Como no nos interesa la composicion quimica, vamos a filtrar nuestra columna con los datos que nos interesan
#Y lo vamos a asignar a otro objeto


datos_minerales_filtrados <- datos_minerales %>% 
  select(Name, Molar_Mass, Molar_Volume, Calculated_Density) %>% #filtramos 
  filter(str_detect(Name, "Z"))  %>%  #queremos que solo se analicen 
  filter(Molar_Mass > 150)     #Para simplificar nuestro dataset vamos a usar solo las que en su nombre tengan una A mayuscula

#Quiero asignar desde antes algunos datos para la grafica

vol <- datos_minerales_filtrados$Molar_Volume

names <- datos_minerales_filtrados$Name

dens <- datos_minerales_filtrados$Calculated_Density

#Ahora que tenemos los datos necesarios, vamos a empezar a armar nuestro grafico sobre la densidad calculada
#por cada mineral

burbujas_1 <- ggplot(datos_minerales_filtrados, 
       aes(x=names, y=vol, color = dens))  +
  geom_point(size = 5, 
             alpha=0.7) 

#Visualizamos

burbujas_1

#Ahora vamos a tunearlo

burbujas_2 <-burbujas_1 +
  theme_classic() #Le metemos un tema cute

#Visualizamos

burbujas_2

#Mas tuneado por favor

burbujas_3 <-burbujas_2 +
  scale_color_viridis() +#Esta es una paleta de colores que viene en el paquete viridis
  theme(axis.text.x = element_text(angle = 23, size = 8,
                                   hjust = 1, vjust = 1))  #Modificamos las leyendas de los ejes

#Visualizacion

burbujas_3

#Vamos a ponerle titulo y formato a los nombres de los ejer

burbujas_4 <- burbujas_3 +
  labs(title = "Volumen molar y densidad calculada", #Aqui ponemos el titulo con la funcion labs
       subtitle = "Para minerales") + #Aqui el subtitulo
  labs(x = "Nombres de los minerales", 
       y = "Volumen Molar")

#Visualizamos

burbujas_4

#Para hacer mas facil la visu

burbujas_5 <- burbujas_4 +
    geom_segment( mapping = aes( x = names,
                                 xend = names,
                               y = 0,
                               yend = vol )) 

#Visualizamos

burbujas_5

#Ahora solo vamos a guardar en formato pdf

ggsave( filename = "volumen_densidad.pdf",           # el nombre tiene extension .pdf
        plot = burbujas_5,                        # guardamos el ultimo plot que creamos
        device = "pdf",                             # en formato pdf
        width = 7, height = 5,                      # 7 de ancho por 5 de alto
        units = "in",                               # pulgadas
        dpi = 300 )

