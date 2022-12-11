"
Supongamos que nuestro trabajo consiste en aconsejar a un cliente sobre
como mejorar las ventas de un producto particular, y el conjunto de datos
con el que disponemos son datos de publicidad que consisten en las ventas
de aquel producto en 200 diferentes mercados, junto con presupuestos de
publicidad para el producto en cada uno de aquellos mercados para tres
medios de comunicación diferentes: TV, radio, y periódico. No es posible
para nuestro cliente incrementar directamente las ventas del producto. Por
otro lado, ellos pueden controlar el gasto en publicidad para cada uno de
los tres medios de comunicación. Por lo tanto, si determinamos que hay una
asociación entre publicidad y ventas, entonces podemos instruir a nuestro
cliente para que ajuste los presupuestos de publicidad, y así
indirectamente incrementar las ventas.

En otras palabras, nuestro objetivo
es desarrollar un modelo preciso que pueda ser usado para predecir las
ventas sobre la base de los tres presupuestos de medios de comunicación.
Ajuste modelos de regresión lineal múltiple a los datos advertisement.csv y 
elija el modelo más adecuado siguiendo los procedimientos vistos

Considera:

Y: Sales (Ventas de un producto)
X1: TV (Presupuesto de publicidad en TV para el producto)
X2: Radio (Presupuesto de publicidad en Radio para el producto)
X3: Newspaper (Presupuesto de publicidad en Periódico para el producto)
"

# Leer datos
adv <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/advertising.csv")
head(adv)

sum(complete.cases(adv))
# 200: Todos los casos están completos

# Inspección visual de datos
pairs(~ Sales + TV + Radio + Newspaper,
      data = adv)

"
A simple vista, hay una correlación fuerte entre las ventas y la inversión en 
publicidad televisiva. La correlación no parece ser tan fuerte ni tan directa
con las inversiones en los otros canales de publicidad. También, parece que las
inversiones en los diferentes canales publicitarios no están correlacionadas,
lo cual es bueno en el caso de las variables independientes.
"

# Primer modelo
modelo1 <- lm(Sales ~ TV + Radio + Newspaper,
              data = adv)
summary(modelo1)

"
Todos los coeficientes de regresión son significativos, excepto el obtenido para
la inversión en publicidad escrita (periódicos, X3), por lo que se eliminará
esta variable para un modelo posterior.

Este primer modelo explica aproximadamente el 90% de la variabilidad observada
(obtenido del valor ajustado de R cuadrada: 0.9011).
"

# Segundo modelo
"
Debido al alto valor p obtenido para el coeficiente de la variable Newspaper en 
el ANOVA anterior, se elimina esta variable para el siguiente modelo.
"
modelo2 <- lm(Sales ~ TV + Radio,
              data = adv)
summary(modelo2)

"
Ahora sí, para este modelo los coeficientes de regresión para todas las variables
independientes (así como el término constante) son significativos, ya que sus
valores p son muy pequeños.

Este modelo sigue explicando alrededor del 90% de la variabilidad observada, lo
cual es razonablemente bueno.

El valor de R cuadrada (ajustada) es marginalmente mayor que el del modelo anterior
(0.9016 contra 0.9011) pero, aún así, este segundo modelo es mejor porque se ha
dejado de considerar una variable no significativa.

Los coeficientes de regresión son:

TV:     0.054449
Radio:  0.107175

Que representan el incremento marginal en las ventas observado por cada incremento
unitario en el presupuesto del medio correspondiente. Y el modelo se expresa como:

Sales = 0.054449 * TV + 0.107175 * Radio

Se observa que ambos coeficientes son positivos, el incremento en el presupuesto
de cada uno incide positivamente en el incremento de ventas.

El coeficiente del presupuesto de publicidad en radio es casi del doble del de TV,
por lo que la inversión en radio es más redituable que en televisión.

Sin embargo, el error estándar del coeficiente del presupuesto de publicidad en 
radio es casi seis veces mayor que el correspondiente a televisión, por lo que 
se espera que su poder predictivo sea menor. Esto ya se observaba en las gráficas
exploratorias iniciales.

"

# Vamos a analizar el comportamiento de los residuales
residuales <- rstandard(modelo2)

par(mfrow = c(2, 2))
plot(adv$TV, residuales)
plot(adv$Radio, residuales)
qqnorm(residuales)
qqline(residuales)

"
Se observa una ligera tendencia descendente en los extremos alto y bajo del presupuesto
de publicidad en TV para los residuales.

No se observa tendencia para los residuales en relación al presupuesto de publicidad
en radio.

La gráfica QQ muestra desviaciones inferiores y superiores, lo cual deberá observarse
en la prueba de normalidad Shapiro-Wilk.
"
dev.off()

shapiro.test(residuales)

"
Efectivamente, el valor p obtenido es muy pequeño (alrededor de 0.001), por lo que
se rechaza la hipótesis nula de que los residuales observados provienen de una 
distribución normal, como se esperaba de la gráfica QQ.
"

"
Con base en los resultados, se sugiere abandonar la publicidad en periódicos y 
destinar el presupuesto liberado de esta manera a los otros dos medios publicitarios.

Se considera que sería más redituable la inversión en publicidad en radio que en TV,
con la precaución del mayor error estándar observado.

Se sugiere investigar la variabilidad observada para el resultado de la publicidad
en radio. Podría deberse en que los anuncios en diferentes estaciones tienen un
diferente impacto sobre las ventas. Otro factor pudiera ser la ubicación geográfica
de las mismas. No se pueden obtener conclusiones al respecto a partir de los datos
disponibles.
"

