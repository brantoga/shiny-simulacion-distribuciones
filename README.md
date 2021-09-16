# Dashboard en shiny de distribuciones continuas
Un dashboard en shiny para simular las funciones de **densidad**, **distribución** y **sobrevivencia** de las distribuciones:
- Normal
- Beta
- Cauchy
- Chi cuadrada
- Exponencial
- F
- Gamma
- Log normal
- Poisson
- t de Student
- Uniforme
- Weibull

Además de las funciones *empíricas*, se crean las *teóricas*. Por lo que también se ha incluido un **plot de los datos**, un **box plot** y un **Q-Q plot**.
## Recomendación
- El eje X está hecho para una distribución normal con rango de (mu - 4*sd, mu + 4*sd), por lo que al tener una distribución diferente se puede dar a errores de 
tener un eje X demasiado largo. Esto se podría solucionar ajustando el rango para cada distribución de manera individual y acorde a su naturaleza.
## Librerías necesarias:
"stats","shiny","ggplot2","qqplotr"
## Autor
Brando Alberto Toribio García 
- brando.tg24@hotmail.com
