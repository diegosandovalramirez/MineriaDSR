# Proyecto 1 - Análisis de música
Dentro de kmeans.R y de diana.R se encuentran las justificaciones para el codigo.

### Requisitos
- Client ID
- Client Secret
- User ID


Tomando en cuenta ambos algoritmos, el modelo final a usar es la clasificacion jerarquica de Diana. (Divisive Analysis)
La distancia de los valores dentro de los cluster era no solo menor, pero tambien los cluster se encontraban mas separados entre si aun cuando la cantidad era similar. Al trabajar con 4 clusters en kmeans solo conseguiamos crear un cuarto cluster que encapsulaba a los primeros 3. Con el analisis divisivo pudimos crear clusters mas individuales. Probablemente por la naturaleza de "Raiz -> hoja" del algoritmo. El mejor rendimiento tambien se debe al uso de la minima varianza de Ward, la que minimiza la varianza total dentro del cluster. De los aproximadamente 400.000 datos, se seleccionar como muestra 2000. Nos hubiese gustado tener una muestra mas grande pero este llego a ser el limite del hardware en el cual se trabajó. 
Al intentar usar todos los datos se intentaba cargar a memoria un vector de 700+ GB lo que detenia el proceso. 
