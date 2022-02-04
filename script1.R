#suma de numeros
1+1

#asignacion de variables y operaciones
variable<- 5
variable2 <- 10
result <- variable + variable2
result

# ctrl + l : limpia la consola
# alt + - : pinta <-

#instalar un paquete por consola
#install.packages("mtcars")

#imprimir estructura de mtcars
str(mtcars)

#mostrar de que se trata el dataset
?mtcars

#ver el tipo de una variable
class(mtcars$vs)

#cambiar de tipo de dato
mtcars$vs = as.logical(mtcars$vs)
mtcars$am = as.logical(mtcars$am)
str(mtcars)

#Analizar dataset Orangeec
str(orangeec)

#obtener el resumen de los datos numericos
summary(orangeec)
summary(mtcars)

#transformamos de libras a pesos la columa wt
mtcars.new <- transform( mtcars, wt=(wt*1000)/2 )
mtcars.new

#vemos como quedo ya en pesos
summary(mtcars.new)

#contenedores numericos y suma
tiempo_platzi <- c(25,5,10,15,10)
tiempo_lecturas <- c(30,10,5,10,15)
tiempo_aprendizaje <- tiempo_platzi + tiempo_lecturas
tiempo_aprendizaje

#contenedores de texto
dias_aprendizaje <- c("Lunes","Martes","Miercoles","Jueves","Viernes")
dias_aprendizaje

#contenedores booleanos
dias_mas_20min <- c(TRUE, FALSE, FALSE, TRUE, TRUE)
dias_mas_20min

#operacion sum en contenedores
total_tiempo_platzi <- sum(tiempo_platzi)
total_tiempo_platzi

total_tiempo_lecturas <- sum(tiempo_lecturas)
total_tiempo_lecturas

total_tiempo_adicional <- total_tiempo_platzi + total_tiempo_lecturas
total_tiempo_adicional

#uso de matrices
tiempo_matriz <- matrix(c(tiempo_platzi,tiempo_lecturas), nrow=2, byrow = TRUE)

dias <- c("Lunes","Martes","Miercoles","Jueves","Viernes")
tiempo <- c("tiempo platzi", "tiempo lecturas")

#asignamos nombre de columnas y registros
colnames(tiempo_matriz) <- dias
rownames(tiempo_matriz) <- tiempo
tiempo_matriz

#sumatoria por columnas
colSums(tiempo_matriz)

#agregar una nueva fila
final_matriz <- rbind(tiempo_matriz,c(10,15,30,5,0))
final_matriz
colSums(final_matriz)

#ver un valor en especifico
final_matriz[1,5]

#agregar una nueva columna
Sabado <- c(25,30,10)

Final_matriz2 <- cbind(final_matriz,Sabado)
Final_matriz2

#que carros tienen menos de 6 cilindros
mtcars[mtcars$cyl<6,]

#pir per capita arriba de los 15000
orangeec[orangeec$GDP.PC>=15000,]

#que paises tienen menos del 2% de aporte al pib de economica naranja
orangeec[orangeec$Creat.Ind...GDP<=2,]

#generar un nuevo dataset filtrando otro dataset
new_orangeec <- subset(orangeec, 
                       Internet.penetration...population>80 &
                       Education.invest...GDP>=4.5)  
new_orangeec

#generar un nuevo dataset filtrando otro dataset y columnas
new_orangeec <- subset(orangeec, 
                       Internet.penetration...population>80 &
                         Education.invest...GDP>=4.5,
                       select = c(Country,Creat.Ind...GDP))  
new_orangeec


#cambiar nombre de columna
#instalar paquete: install.packages("plyr")
#importar paquete: library(plyr)
orangeec <- rename(orangeec,c("Creat.Ind...GDP"="AporteEcNja"))
orangeec

#otra forma de renombrar
names(orangeec)[6]<- c("AporteEcNja")
orangeec


nivel_curso <- c("Basico","Intermedio","Avanzado")
nivel_curso

#ver primeros 10 datos de un dataset
head(mtcars)

#ver ultimos datos
tail(mtcars)

#instalar paquete: install.packages("dplyr")
glimpse(orangeec)

#vector del 1 al 8
my_vector <- 1:8
my_vector

#matriz del 1 al 9, dividido cada 3 columnas
my_matriz <- matrix(1:9, ncol=3)
my_matriz

#obtener los primeros 4 lineas de un dataset
my_df <- mtcars[1:4,]
my_df

#hago una lista con todos los objetos anteriores
my_list <- list(my_vector, my_matriz, my_df)
my_list


#graficar scatter plot(x, y)
plot(mtcars$mpg ~ mtcars$cyl,
     xlab="cilindros", ylab="millas por galon",
     main="Relacion cilindros y millas por galon")

plot(mtcars$mpg ~ mtcars$hp,
     xlab="caballos de fuerza", ylab="millas por galon",
     main="Relacion caballos de fuerza y millas por galon")


plot(orangeec$Unemployment ~ orangeec$Education.invest...GDP,
     xlab="Inversion educacion (%PIB)",
     ylab="Desempleo",
     main="Relacion inversion en educacion y desempleo")

plot(orangeec$GDP.PC ~ orangeec$AporteEcNja,
     xlab="Aporte economia naranja al PIB",
     ylab="PID per capita",
     main="Relacion economia naranja y pib")

#EDA con histogramas.
#install.packages("ggplot2")
#library(ggplot2)
qplot(mtcars$hp,
      geom="histogram",
      xlab="caballos de fuerza",
      main="Carros segun caballos de fuerza")

ggplot(mtcars, aes(x=hp))+
  geom_histogram(binwidth = 30)+
  labs(x="Caballos de fuerza",
       y="Cantidad de carros",
       title="Caballos de fuerza en carros seleccionados")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot()+geom_histogram(data=mtcars,
                        aes(x=hp), fill="blue",
                        color="red", binwidth=20)+
  labs(x="Caballos de fuerza",
       y="Cantidad de carros",
       title="Caballos de fuerza en carros seleccionados")+
  xlim(c(80,280))+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  

ggplot()+geom_histogram(data=orangeec,
                        aes(x=GDP.PC), fill="blue",
                        color="red", binwidth=2000)+
  labs(x="Pib per capita",
       y="Cantidad de paises",
       title="Pib per capita en paises latam")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#BOXPLOT para saber max,min,media,25%,75%

boxplot(mtcars$hp, ylab="caballos de fuerza",
        main="caballos de fuerza en carros mtcars")

#3 boxplot para 4,6 y 8 cilidnros contra caballos de fuerza
ggplot(mtcars, aes(x=as.factor(cyl), y=hp, fill=cyl))+
  geom_boxplot(alpha=0.6)+
  labs(x="cilindros",
       y="caballos de fuerza",
       title="caballos de fuerza segun cilindros")


mtcars$am <- factor(mtcars$am, levels=c(TRUE,FALSE),
                    labels=c("manual","automatico"))

ggplot(mtcars, aes(x=am, y=mpg, fill=am))+
  geom_boxplot(alpha=0.6)+
  labs(x="Tipo de caja",
       y="Millas por galon",
       title="Millas por galon segun el tipo de caja")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  

#sacar la media
economy <- mean(orangeec$GDP.PC)
economy

#agregamos una nueva columna con categorizacion
orangeec$Strong_economy <- ifelse(orangeec$GDP.PC<economy,"PorDebajoPromedio",
                                  "Sobre-ArribaPromedio")

ggplot(orangeec, aes(x=Strong_economy, y=AporteEcNja,
                     fill=Strong_economy))+
  geom_boxplot(alpha=0.4)+
  labs(x="Tipo de pais",y="Aporte economia naranja al pic",
       title="Aporte economia naranja en pib paises latam con alto 
       y bajo pib per capita")


ggplot(mtcars, aes(hp,mpg))+
  geom_point()+
  labs(x="caballos fuerza", y="millas por galon",
       title="relacion caballos de fuerza por millas por galon")


ggplot(mtcars, aes(hp,qsec))+
  geom_point(aes(color=am, size=cyl))+
  labs(s="caballos de fuerza",
       y="tiempo en 1/4 milla",
       title="caballos velocidad segun cilindraje y tipo caja")


ggplot(orangeec, aes(Internet.penetration...population,AporteEcNja))+
  geom_point(aes(color=Strong_economy, size=GDP.Growth..))+
  labs(s="Penetracion internet",
       y="Aporte economia naranja PIB",
       title="Internet y aporte economia naranja segun economia y crecimiento pib")


#grafica interactiva
#install.packages("devtools")
#require("devtools")
#install.packages("plotly")
#library(plotly)

my_graph <- ggplot(orangeec, aes(Internet.penetration...population,
                                 AporteEcNja, label=row.names(orangeec)))+
  geom_point()+
  labs(x="Penetracion internet", y="Aporte economia naranja",
       title="penetracion internet y aporte economia naranja")
my_graph

p = ggplotly(my_graph)
p


#La función pairs nos permite cruzar todas las variables 
#del dataset a modo de tabla donde el eje x de una gráfica 
#corresponde a la columna donde se encuentra y el eje y a la fila.

pairs(mtcars[,2:6])

#install.packages("dplyr")

#seleccionar columnas
newdata <- subset(mtcars, select=c(2,7:8,11,12))
pairs(newdata)

#cuales columnas no queremos
pairs(mtcars[,-c(1,3,5,6,9,10)])

Eficientes <- filter(mtcars, mtcars$mpg>=30)
Eficientes

pairs(Eficientes[,2:6])

install.packages("stringr")
library(stringr)

merc <- mtcars %>%
  filter(str_detect(model, "Merc"))

#relacion de variables pero numerico
cor(mtcars[,2:6])


#obtener coeficiente de variacion
#si es menor a 25 quiere decir que los datos estan muy cercanos
#si es mayor quiere decir que hay datos muy alejados de la media
(sd(mtcars$mpg)/mean(mtcars$mpg))*100

#Al momento de sacar el promedio de nuestro dataset orangeec encontramos variables 
#que tienen valores NA, para que estos no afecten nuestro cálculo solamente debemos añadir 
#como argumento na.rm=TRUE

mean(orangeec$AporteEcNja, na.rm=TRUE)
sd(orangeec$AporteEcNja, na.rm=TRUE)

#library(dplyr)
eficientes <- mean(mtcars$mpg)
eficientes
mtcars <- mtcars %>%
  mutate(Mas_eficientes=ifelse(mpg<eficientes,
                               "bajo promedio",
                               "sobre promedio"))


Mas_veloces <- mtcars[mtcars$qsec<16,]
Mas_veloces

mtcars <- mtcars %>%
  mutate(Velocidad_Cuarto_milla=ifelse(qsec<16,
                                       "menos 16 seg",
                                       "mas 16 seg"))

mtcars <- mtcars %>% 
  mutate(Peso_kilo=(wt/2)*1000)

mtcars <- mtcars %>%
  mutate(Peso=ifelse(Peso_kilo <= 1500,
                     "livianos","pesados"))


orangeec <- orangeec %>%
  mutate(Crecimiento_GDP = ifelse(GDP.Growth..>=2.5,
                                  "2.5% o mas",
                                  "Menos 2.5%"))


TopNaranjas <- orangeec %>%
  filter(Country %in% c("Mexico", "Panama", "Argentina", 
                        "Colombia", "Brazil","Paraguay"))
TopNaranjas

TopNaranjas %>%
  arrange(desc(AporteEcNja))

#Grafica con representado 4 variables
#millas por galon eje y
#cilindraje eje x
#pesos, tamaño de las bolitas
#transmision, 2 graficas (manual y automatico)
ggplot(mtcars, aes(x=cyl, y=mpg, size=Peso))+
  geom_point()+
  facet_wrap(~am)

ggplot(TopNaranjas, aes(x=Internet.penetration...population,
                        y=Services...GDP, size=GDP.PC))+
  geom_point()+
  facet_wrap(~Country)


#install.packages(RColorBrewer)
myColors <- brewer.pal(9,"Reds")

ggplot(TopNaranjas, aes(x=Internet.penetration...population,
                        y=GDP.PC, fill=AporteEcNja))+
  geom_tile()+
  facet_wrap(~Country)+
  scale_fill_gradientn(colors=myColors)


#Crear un documento HTML para compartir
#install.packages("rmarkdown")
#library(rmarkdown)
#install.packages("knitr")
#library(knitr)
#File/New File/R Markdown/Documento HTML


#si cruzamos 2 columnas numericas es un scatter plot
plot(orangeec$Services...GDP~orangeec$Education.invest...GDP)

#si cruzamos 2 columnas 1 numerica y otra categorica se hace un boxpplot
plot(mtcars$mpg~mtcars$am)
