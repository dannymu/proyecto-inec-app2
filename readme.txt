Proyecto en APP Shiny de causas de morbilidad en Panamá

Librería necesarias:
shinydashboard, shiny, tidyverse, ggplot2, shinycssloaders, shinyWidgets, leaflet, periscope

Funcionamiento:
1. Descargar todos los archivos y ejecutar "INEC-morbilidad.Rproj", esto cargará el proyecto en R.
2. Abrir el archivo "app.R", en R, dar click a la opción "RUN APP".

Archivo: Candidiasis_BASE_COMPLETA.xlsx
Conjunto de datos de causas de morbilidad en Panamá 
Datos del Instituto Nacional de Estadísticas
Tamaño: [29337 × 9] 

code
Tipo: Cadena de texto (character).
Características: Identifica los códigos criptológicos asociados a las enfermedades relacionadas con la candidiasis. El resumen indica que existen varios códigos (por ejemplo, "B37.2", "B37.3", etc.) y un total de 11 valores únicos.

causas
Tipo: Cadena de texto (character).
Características: Describe la localización o el tipo de candidiasis, con valores como "Candidiasis de la piel y las uñas", "Candidiasis de la vulva y de la vagina", entre otros. Se observan 16 valores únicos en total, representando diferentes presentaciones clínicas.

region
Tipo: Cadena de texto (character).
Características: Indica la región geográfica; en el análisis se muestran 11 regiones distintas de Panamá, como "BOCAS DEL TORO", "CHIRIQUI", "COCLE", entre otros.

ano
Tipo: Numérico (numeric).
Características: Representa el año en el que se registraron los casos. Los datos van desde 2010 hasta 2020, con una tendencia central cercana al 2015 según el resumen.

genero
Tipo: Cadena de texto (character).
Características: Muestra el género de las personas afectadas. Se tienen tres categorías: "Hombres", "Mujeres" y "Total".

edad
Tipo: Cadena de texto (character).
Características: Agrupa las edades en rangos. Los valores incluyen intervalos como "0 a 4 años", "10 a 14 años", "15 a 19 años", etc., así como un valor "TOTAL".

casos
Tipo: Numérico (numeric).
Características: Indica la cantidad de casos reportados. Se observa una amplia distribución de valores, desde 0 hasta cifras considerablemente altas, aunque la mediana es baja (el resumen lo muestra como 1 en la mediana).

poblacion
Tipo: Numérico (numeric).
Características: Representa la población de referencia para la que se calculan los casos de candidiasis. Los valores van desde cifras bajas (mínimo 522) hasta altos (más de 2 millones en el máximo); la mediana es de 12410 y el promedio es de aproximadamente 49781.

prevalencia
Tipo: Numérico (numeric).
Características: Mide la prevalencia de la enfermedad, calculada en alguna unidad que puede ser por cada 100 000 o similar. Se observa una gran variabilidad, con valores mínimos cercanos a 0, una mediana de 4.526, y valores extremos muy altos (hasta 2935.642).