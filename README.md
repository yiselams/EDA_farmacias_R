---
title: "ANÁLISIS GENERAL DE CAF"
subtitle: "Especialización en Analítica y Ciencia de Datos Aplicada"
author: "Yisela Mayorga Salazar"
date: "`r Sys.Date()`"
output:
  html_document: 
    fig_height: 5
    fig_width: 7
    fig_align: 'center'
    highlight: tango
    keep_md: yes
    number_sections: yes
    theme: united
    toc: yes
    toc_depth: 3
    toc_float:
      collapsed: no
      smooth_scroll: yes
  pdf_document:
    toc: yes
    toc_depth: '4'
  word_document:
    toc: yes
    toc_depth: '4'
editor_options:
  
  chunk_output_type: console
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = FALSE,
	message = FALSE,
	warning = FALSE
)
#⚡Recuerde instalar las librerias si no lo ha hecho antes.
# con install.packages
#install.packages("tidyverse")

library(openxlsx)
library(tidyverse)
library(janitor)
library(flextable)
library(kableExtra)
library(knitr)
library(dplyr)
library(officer)
library(scales)
library(showtext)
library(sysfonts)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(gapminder)
library(ggiraph)
library(DT)
library(lubridate)
library(sf)
library(leaflet)
library(stringr)
library(nortest)
library(glue)

names(pdfFonts())
bd <- read.xlsx("BD_TRABAJO_FINAL_INTRO.xlsx")

# 📥 Si quiere leer los datos desde una tabla de Excel. Basta con usar la línea anterior,
# pero esta vez, escribiendo el nombre del archivo con su extensión. Ej:
# encuesta <- read.xlsx("ENCUESTA_FINAL.xlsx")

```

```{r LIMPIAR VARIABLES, include=FALSE}
#Esta función clean_names se usa para limpiar las variables de caracteres especiales
bd <- bd %>% 
  rename(fd= `FO/DIA.SOCIALIZACIÓN`) %>% 
  clean_names() 
```

```{r DUPLICADOS, include=FALSE}
#para mirar duplicados de dos al mismo tiempo
#get_dupes(variable1,variable2)
bd %>% 
  get_dupes(caf)
```
# Introducción

Este informe presenta un análisis descriptivo y estadístico de los datos recolectados en diferentes farmacias de Colombia. Se examinan variables como el número de fórmulas dispensadas diariamente, la infraestructura (comparando el área requerida con el área actual y su variación), y el cliente al que se dispensa. El objetivo es identificar tendencias, evaluar el cumplimiento de estándares y detectar posibles áreas de mejora.

# Metodología

Se utilizó el lenguaje R para procesar y visualizar la información, apoyándose en el conjunto de paquetes del ecosistema tidyverse. Las principales herramientas empleadas fueron:

- `dplyr` para manipulación de datos.
- `ggplot2` para visualización.
- `flextable` para tablas.
- `nortest` para pruebas de normalidad.

## Carga de datos

En esta etapa se verificó la estructura de la base de datos, los tipos de variables y se realizaron los ajustes necesarios para asegurar su correcto procesamiento. El archivo BD_TRABAJO_FINAL_INTRO.xlsx contiene información de 384 farmacias, con un total de 5 variables categóricas y 3 variables cuantitativas.

```{r ELIMINAR DUPLICADOS, include=FALSE}
#Para eliminar duplicados por variable 
#Para eliminar duplicados de varias variables al tiempo distinct(var1,var2,var3)
#distinct(variable, .keep_all = TRUE)
bd <- bd %>% 
  distinct(caf,.keep_all = TRUE)
```

```{r TRANSFORMACION DE CATEGORIAS, include=FALSE}
bd %>% 
   count(regional)

bd <- bd %>% 
  mutate(regional = 
      case_when(
      regional == "ANTIOQUIA 1" ~ "ANTIOQUIA",
      regional == "ANTIOQUIA 2" ~ "ANTIOQUIA",
      regional == "ANTIOQUIA 3" ~ "ANTIOQUIA",
      regional == "BARRANQUILLA ZONA 1" ~ "BARRANQUILLA",
      regional == "BARRANQUILLA ZONA 2" ~ "BARRANQUILLA",
      regional == "BOGOTA ZONA 1" ~ "BOGOTA",
      regional == "BOGOTA ZONA 2" ~ "BOGOTA",
      regional == "BOGOTA ZONA 3" ~ "BOGOTA",
      regional == "BOGOTA ZONA 4" ~ "BOGOTA",
      regional == "BOGOTA ZONA 5" ~ "BOGOTA",
      regional == "BOGOTA ZONA 6" ~ "BOGOTA",
      regional == "BOGOTA ZONA 7" ~ "BOGOTA",
      regional == "BOGOTA ZONA 9" ~ "BOGOTA",
      regional == "BOGOTÁ ZONA 2" ~ "BOGOTA",
      regional == "BOLIVAR CENTRO" ~ "BOLIVAR",
      regional == "BOLIVAR SUR" ~ "BOLIVAR",
      regional == "EJE CAFETERO 1" ~ "EJE CAFETERO",
      regional == "EJE CAFETERO 2" ~ "EJE CAFETERO",
      regional == "EJE CAFETERO 3" ~ "EJE CAFETERO",
      regional == "SUROCCIDENTE 1" ~ "SUROCCIDENTE",
      regional == "SUROCCIDENTE 2" ~ "SUROCCIDENTE",
      regional == "SUROCCIDENTE 3" ~ "SUROCCIDENTE",
      regional == "SUROCCIDENTE 4" ~ "SUROCCIDENTE",
      regional == "SUROCCIDENTE 5" ~ "SUROCCIDENTE",
      
      TRUE  ~ regional
    )
  )

set_flextable_defaults(font.family = "Arial")
```

```{r}
bd <- bd %>% 
            mutate(departamento = case_when(
                      departamento == "ATLANTICO" ~ "ATLÁNTICO",
                      departamento == "BOLIVAR" ~ "BOLÍVAR",
                      departamento == "BOYACA" ~ "BOYACÁ",
                      departamento == "CAQUETA" ~ "CAQUETÁ",
                      departamento == "CORDOBA" ~ "CÓRDOBA",
                      departamento == "GUAJIRA" ~ "LA GUAJIRA",
                      departamento == "CHOCO" ~ "CHOCÓ",
                      departamento == "N. DE SANTANDER" ~ "NORTE DE SANTANDER",
                      departamento == "PATIA" ~ "CAUCA",
                      TRUE ~ departamento))
```


```{r}
bd <- bd %>% 
  mutate(
    area_requerida = as.numeric(area_requerida),
    area_requerida = round(area_requerida,2),
    area_total = as.numeric(area_total),
    dif_area = area_total - area_requerida,
    var_area = -(area_requerida - area_total)/(area_requerida)
  )
```

```{r CATEGORIZAR VARIABLE}
bd<- bd%>%
  mutate(cat_area = 
      case_when(
      dif_area <0 ~ "No cumple con el área requerida",
      TRUE  ~ "Si cumple con el área requerida"
    )
  )

```
# Resultados

En esta sección se presentan los principales hallazgos derivados del análisis de los datos recolectados. 

## Distribución de CAF y Formulación Día por Departamento

Se muestra el total de Centros de Atención Farmacéutica (CAF) y la suma de fórmulas dispensadas diariamente (`fd`) por departamento, permitiendo identificar la concentración de servicios farmacéuticos en el país.



```{r TABLA FORMULACION X CAF Y DPTO}
departamentoxcaf <- bd %>%
  group_by(departamento) %>%
  summarise(
    Total_CAF = n(),  # Contar el número de registros (CAF) por cada departamento
    Total_Formulacion_Dia = sum(fd, na.rm = TRUE)  # Sumar la columna 'fd' (formulación día) por cada departamento
  ) %>%
  ungroup() %>%  # Desagrupar para asegurarnos de que todas las columnas estén disponibles
  arrange(desc(Total_CAF)) %>% # Ordenar por el total de CAF de mayor a menor
  mutate(
    Total_Formulacion_Dia = round(Total_Formulacion_Dia, 0)
  )

# Generar la tabla con flextable
departamentoxcaf %>%
  rename(
    "Departamento" = departamento,
    "Número de CAF" = Total_CAF,
    "Fd" = Total_Formulacion_Dia
  ) %>%
  flextable() %>%
  theme_box() %>%
  color(part = "header", color = "white") %>%
  bg(part = "header", bg = "#0a6ebd") %>%
  fontsize(part = "all", size = 10) %>%
  align(align = "center", part = "all") %>%
  autofit() %>%
  set_caption(caption = as_paragraph(as_chunk("Total de CAF y Formulación Día (Fd) por departamento", props = fp_text(bold = TRUE))))
```

## Formulación Día por Cliente
Antes de analizar los datos por ubicación geográfica, se examina la formulación diaria (fd) total agrupada por cliente. Esta información permite identificar cuáles EPS concentran el mayor volumen de fórmulas, lo que puede estar relacionado con su cobertura, número de afiliados o eficiencia operativa.


```{r TABLA FORMULACION DIA POR CLIENTE}
foxcliente <-  bd %>%
    group_by(cliente) %>%
  summarise(
    'Número de CAF' = n(),
    Fd = sum(fd, na.rm = TRUE),.groups = "drop"  # Sumar la columna 'fd' (formulación día) por cada departamento
  ) %>%
  arrange(desc('Número de CAF')) %>% 
  mutate(Fd = round(Fd, 0))  # Redondear Fd a cero decimales

# Generar la tabla con flextable
foxcliente %>%
  flextable() %>%
   theme_box() %>%
  color(part = "header", color = "white") %>%
  bg(part = "header", bg = "#0a6ebd") %>%
  fontsize(part = "all", size = 10) %>%
  align(align = "center", part = "all") %>%
  autofit() %>%
  set_caption(caption = as_paragraph(as_chunk("Formulación Día por Cliente", props = fp_text(bold = TRUE))))

```

## Formulación Día por Cliente y Departamento
Para profundizar en la distribución geográfica, se analiza la formulación diaria discriminada por cliente y departamento. Esta sección permite identificar, por ejemplo, en qué zonas del país se concentran las operaciones de cada EPS, lo que puede dar indicios sobre cobertura o necesidades regionales.

```{r}
formulacion_cliente_dpto <- bd %>%
  group_by(departamento, cliente) %>%
  summarise(`Fd` = sum(fd, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = cliente, 
    values_from = Fd, 
    values_fill = list(Fd = 0)
    ) %>%
   mutate(across(where(is.numeric), ~round(.x, 0))) %>%
  adorn_totals("row")
  
# Tabla con flextable
formulacion_cliente_dpto %>%
    rename(
    "Departamento" = departamento,
    "Compensar" = COMPENSAR,
    "Capital Salud" = `CAPITAL SALUD`,
    "Ecopetrol" = ECOPETROL,
    "Mutualser"= MUTUALSER,
    "NEPS" = `NUEVA EPS`,
    "Salud Total" = `SALUD TOTAL`
  ) %>%
  flextable() %>%
  theme_box() %>%
  color(part = "header", color = "white") %>%
  bg(part = "header", bg = "#0a6ebd") %>%
  fontsize(part = "all", size = 10) %>%
  align(align = "center", part = "all") %>%
  autofit() %>%
  set_caption(
    caption = as_paragraph(
      as_chunk("Formulación Día por Cliente y Departamento", props = fp_text(bold = TRUE))
    )
  )
```

La siguiente gráfica resume la información presentada en la Tabla 3.3, mostrando de manera visual la distribución total de la formulación diaria por cliente y por departamento. Este análisis permite identificar claramente en qué regiones se concentran los volúmenes más altos de formulación, así como la distribución de las EPS por departamento. 

```{r FORMULACION TOTAL POR CLIENTE Y DEPARTAMENTO}
# 1) Preparamos los datos
fd_cliente_plot <- bd %>%
  rename(
    Departamento = departamento,
    Cliente      = cliente
  ) %>%
  group_by(Departamento, Cliente) %>%
  summarise(
    Fd = sum(fd, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Orden de los factores
departamento_ordenado <- fd_cliente_plot %>%
  group_by(Departamento) %>%
  summarise(Total_Dep = sum(Fd), .groups = "drop") %>%
  arrange(desc(Total_Dep)) %>%
  pull(Departamento)

fd_cliente_plot <- fd_cliente_plot %>%
  mutate(
    Departamento = factor(Departamento, levels = departamento_ordenado)
  )

# 3) El gráfico con tooltip personalizada
p <- ggplot(fd_cliente_plot,
            aes(
              x     = Departamento,
              y     = Fd,
              fill  = Cliente,
              text  = paste0(
                        "<b>Departamento:</b> ", Departamento, "<br>",
                        "<b>Cliente:</b> ", Cliente, "<br>",
                        "<b>Total Fd:</b> ",
                        comma(round(Fd, 0))  # redondear y formatear con separador de miles
                     )
            )) +
  geom_col(position = "stack") +
  labs(
    title = "Formulación total por cliente y departamento",
    x     = "Departamento",
    y     = "Fd",
    fill  = "Cliente"
  ) +
  theme_minimal(base_size = 12, base_family = "Arial") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title       = element_text(face = "bold", size = 12),
    axis.text        = element_text(face = "bold", size = 10),
    axis.text.x      = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.title     = element_text(face = "bold")
  ) +
  scale_fill_brewer(palette = "Set2")

# 4) Interactive con tooltip = text
ggplotly(p, tooltip = "text")
```


## Cumplimiento de Área Mínima Requerida Por el Estandar

En la siguiente gráfica se visualiza la distribución del número de CAF por cliente, diferenciando aquellos que cumplen y no cumplen con el área mínima requerida. Se observa que todos los clientes presentan una proporción significativa de CAF que no cumplen con el estándar, siendo especialmente marcado en clientes como **CAPITAL SALUD, COMPENSAR y ECOPETROL**. Esta visualización refuerza la importancia de implementar estrategias para mejorar la infraestructura en estos puntos.

```{r GRAFICA DE CLIENTE VS CAT AREA}

# 0) Preparar datos
bd_count <- bd %>%
  count(cliente, cat_area, name = "n_caf")

# 1) Construir el ggplot (barras verticales)
p <- ggplot(bd_count,
            aes(
              x    = n_caf,
              y    = cliente,
              fill = cat_area,
              text = paste0(
                       "Cliente: ", cliente, "\n",
                       "N° CAF: ", n_caf, "\n",
                       "Categoría de área: ", cat_area
                     )
            )) +
  geom_col(position = position_dodge()) +        # geom_col() == geom_bar(stat="identity")
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "N° de CAF por Cliente",
    x     = "N° de CAF",        # ahora en horizontal (eje X original es ahora vertical)
    y     = "Cliente",       # eje Y pasa a ser Cliente
    fill  = "Categoría de Área"
  ) +
  theme_bw(base_family = "Arial") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title       = element_text(face = "bold", size = 12),
    axis.text        = element_text(face = "bold", size = 10),
    legend.title     = element_text(face = "bold"),
    legend.text      = element_text()
  )

# 2) Hacerlo interactivo usando sólo tu 'text' en la tooltip
ggplotly(p, tooltip = "text")
```

Se compara el área total disponible de cada CAF frente al área requerida (m²) para cada caso, diferenciando en naranja los que cumplen con lo requerido y en verde azulado los que no. Se aprecia una correlación positiva general, aunque varios puntos quedan por debajo de la línea teórica de cumplimiento, indicando incumplimientos. Esta vista permite identificar rápidamente los casos críticos de los CAF que no cumplen con el estándar de área requerida.

```{r GRAFICA DE AREA TOTAL VS AREA REQ}
q <- ggplot(bd,aes(
  x= area_requerida, 
  y= area_total, 
  color = cat_area,
  text = paste(
    "CAF", caf,"\n%VAR:",round(var_area*100,2),"%",

    "\nÁrea total:",area_total,"m²",
    "\nÁrea requerida:",area_requerida,"m²"
    )
  ))+
  geom_point()+
  scale_color_brewer(palette = "Set2")+
  theme_bw()+
  theme(
    text = element_text(family = "Arial"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) +
    theme(
    panel.grid.major = element_blank(),  # Quitar cuadrícula mayor
    panel.grid.minor = element_blank(),  # Quitar cuadrícula menor
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12,face = "bold"),
    axis.text = element_text(size = 10,face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text()
  )+
  labs(
    title = "Comparación de Área Total vs Área Requerida",
    x = "Área Requerida (m²)",
    y = "Área Total (m²)",
    color = "Categoría de Área"
  )

ggplotly(q, tooltip = c("text"))

```

```{r include=FALSE}
  mapa <- read_sf("MGN_DPTO_POLITICO.shp")
  mapa <- st_transform(mapa, crs = 4326)
```

```{r Funcion mapa, include=FALSE}
plot_map_poly <- function(map_input, show_variable, group_variable, units, palette, legend_title){
  x <- enquo(show_variable)
   ## Filter null polygons
  map <- map_input
    # filter(!!x != 0)
  group <- enquo(group_variable)
  label_names <- map %>% pull(!!group)
  x_values <- pull(map, !!x)
  ## Palette creation
  pal <- colorNumeric(
    palette = palette,
    domain = x_values
  )
 
  ### Label creation
  labels <- sprintf(
    paste0("<strong>%s</strong><br/>%g ", units),
    label_names, x_values
  ) %>% lapply(htmltools::HTML)
  #Esta línea es para ajustar el formato, pero hay que incluirla
  #en la sección de labels (aun no no lo hago por el tiempo que tengo)
  #formatC(100000, format = "d", big.mark = ",")
 
  ### RENDERIZACIÃN DEL MAPA
  leaflet(map) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(weight = 1,
                opacity = 1.0, fillOpacity = 1,
                color = "white",
                fillColor = ~pal(x_values),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                label = labels
                ) %>%
    addLegend(pal = pal, values = x_values, opacity = 0.7, title = legend_title,
              position = "topright")
    #setView(lat=51.51745023576495,lng=-0.12906955580907298,zoom=10)
}

```

```{r include=FALSE}
#Farmacias por departamento
totales <- bd %>% 
  group_by(departamento) %>%
  summarise(n_total = n(), .groups = "drop")
  
#Conteo por cliente y por departamento
cliente_top <- bd %>%
  group_by(departamento, cliente) %>%
  summarise(n_cliente = n(), .groups = "drop") %>%
  group_by(departamento) %>%
  slice_max(n_cliente, n=1, with_ties = FALSE) %>%
  ungroup()

# #Unir ambas y calcular participación
participacion <- cliente_top %>%
  left_join(totales, by = "departamento") %>%
  mutate(porcentaje = round(100 * n_cliente / n_total,2))
  
```

```{r}
mapa <- mapa %>% 
            mutate(DPTO_CNMBR = case_when(DPTO_CNMBR == "Amazonas" ~ "AMAZONAS",
                      DPTO_CNMBR == "Antioquia" ~ "ANTIOQUIA",
                      DPTO_CNMBR == "Atlántico" ~ "ATLÁNTICO",
                      DPTO_CNMBR == "Bolívar" ~ "BOLÍVAR",
                      DPTO_CNMBR == "Boyacá" ~ "BOYACÁ",
                      DPTO_CNMBR == "Caldas" ~ "CALDAS",
                      DPTO_CNMBR == "Cauca" ~ "CAUCA",
                      DPTO_CNMBR == "Cauca" ~ "CAUCA",
                      DPTO_CNMBR == "Cesar" ~ "CESAR",
                      DPTO_CNMBR == "Chocó" ~ "CHOCÓ",
                      DPTO_CNMBR == "Cundinamarca" ~ "CUNDINAMARCA",
                      DPTO_CNMBR == "Guaviare" ~ "GUAVIARE",
                      DPTO_CNMBR == "Huila" ~ "HUILA",
                      DPTO_CNMBR == "La Guajira" ~ "LA GUAJIRA",
                      DPTO_CNMBR == "Magdalena" ~ "MAGDALENA",
                      DPTO_CNMBR == "Meta" ~ "META",
                      DPTO_CNMBR == "Nariño" ~ "NARIÑO",
                      DPTO_CNMBR == "Norte de Santander" ~ "NORTE DE SANTANDER",
                      DPTO_CNMBR == "Putumayo" ~ "PUTUMAYO",
                      DPTO_CNMBR == "Putumayo" ~ "PUTUMAYO",
                      DPTO_CNMBR == "Quindío" ~ "QUINDIO",
                      DPTO_CNMBR == "Risaralda" ~ "RISARALDA",
                      DPTO_CNMBR == "Santander" ~ "SANTANDER",
                      DPTO_CNMBR == "Sucre" ~ "SUCRE",
                      DPTO_CNMBR == "Tolima" ~ "TOLIMA",
                      DPTO_CNMBR == "Valle del Cauca" ~ "VALLE DEL CAUCA",
                      TRUE ~ DPTO_CNMBR))

```

```{r include=FALSE}
mapa$DPTO_CNMBR <- as.character(mapa$DPTO_CNMBR)
participacion$departamento <- as.character(participacion$departamento)

mapa <- left_join(mapa,participacion, by = c("DPTO_CNMBR" = "departamento"))
# Reemplazar los NA por 0 en la variable n (si no hay CAF)
mapa$n_total[is.na(mapa$n_total)] <- 0
mapa$n_cliente[is.na(mapa$n_cliente)] <- 0
mapa$porcentaje[is.na(mapa$porcentaje)] <- 0
mapa$cliente[is.na(mapa$cliente)] <- " "
```

```{r message=FALSE, warning=FALSE, include=FALSE}
# Convertir la geometría a POLYGON o MULTIPOLYGON
mapa <- mapa %>% 
  st_cast("MULTIPOLYGON")  # O usa "MULTIPOLYGON" si es el caso
summary(mapa$n) 
```

```{r}
plot_map_poly <- function(map_input, show_variable, group_variable, extra_label_var = NULL, units = "", palette, legend_title){
  x <- enquo(show_variable)
  group <- enquo(group_variable)

  label_names <- map_input %>% pull(!!group)
  x_values <- pull(map_input, !!x)

  # Si hay variable extra para etiquetas
  if (!is.null(extra_label_var)) {
    extra_labels <- map_input %>% pull({{ extra_label_var }})
    labels <- sprintf(
      "<strong>%s</strong><br/>%s<br/>%g %s",
      label_names, extra_labels, x_values, units
    ) %>% lapply(htmltools::HTML)
  } else {
    labels <- sprintf(
      "<strong>%s</strong><br/>%g %s",
      label_names, x_values, units
    ) %>% lapply(htmltools::HTML)
  }

  pal <- colorNumeric(palette = palette, domain = x_values)

  leaflet(map_input) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(weight = 1,
                opacity = 1, fillOpacity = 1,
                color = "white",
                fillColor = ~pal(x_values),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                label = labels
    ) %>%
    addLegend(pal = pal, values = x_values, opacity = 0.7, title = legend_title,
              position = "topright")
}

```

## Distribución Total De CAF Por Departamento y Cliente Principal

El mapa muestra la cantidad total de CAF por departamento y resalta el cliente con mayor participación. Los colores representan el número de CAF, y al pasar el cursor se despliega un mensaje con el detalle del total y el cliente predominante. Esta visualización facilita la identificación de zonas prioritarias y actores clave para orientar acciones de seguimiento o mejora.
```{r}
mapa <- mapa %>%
  mutate(texto_popup = sprintf(
    "Total CAF: %s<br/>Cliente con mayor participación: %s<br/>Participación: %.1f%%",
    formatC(n_total, format = "d", big.mark = ","),
    cliente,
    porcentaje
  ))

# Llamar a la función que dibuja el mapa
plot_map_poly(
  map_input = mapa,
  show_variable = n_total,
  group_variable = DPTO_CNMBR,
  extra_label_var = "texto_popup",
  units = "CAF",
  palette = brewer.pal(n = 5, name = "Set2"),
  legend_title = "Total de CAF"
)
```
# Análisis estadístico

La siguiente sección presenta un análisis estadístico del área total con formulación, diferenciando entre los casos que cumplen y no cumplen con el área mínima requerida según el estándar del cliente.

## Resumen estadístico por categoría de cumplimiento

La siguiente tabla muestra medidas de tendencia central, dispersión y forma para cada grupo (cumple o no cumple con el área requerida). Se incluyen valores como la mediana, media, cuartiles, desviación estándar, varianza e índice de variabilidad (CV). Esta información permite identificar diferencias significativas entre ambos grupos en cuanto a la magnitud y variabilidad del área formulada. Por ejemplo, los casos que cumplen con el área requerida tienen mayores valores promedio y mayor dispersión.

```{r}
tabla_resumen <- bd %>%
  group_by(cat_area) %>%
  summarise(
    Min = min(area_total, na.rm = TRUE),
    Q1 = quantile(area_total, 0.25, na.rm = TRUE),
    Mediana = median(area_total, na.rm = TRUE),
    Q3 = quantile(area_total, 0.75, na.rm = TRUE),
    Max = max(area_total, na.rm = TRUE),
    Media = mean(area_total, na.rm = TRUE),
    IRQ = IQR(area_total, na.rm = TRUE),
    bigote1 = Q1 - 1.5 * IRQ,
    bigote2 = Q3 + 1.5 * IRQ,
    Desvest = sd(area_total, na.rm = TRUE),
    Varianza = var(area_total, na.rm = TRUE),
    CV = Desvest / Media
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))  # Redondear a 2 decimales

# Tabla con formato flextable estandarizado
flextable(tabla_resumen) %>%
  theme_box() %>%
  color(part = "header", color = "white") %>%
  bg(part = "header", bg = "#0a6ebd") %>%
  fontsize(part = "all", size = 10) %>%
  align(align = "center", part = "all") %>%
  autofit() %>%
  set_caption(
    caption = as_paragraph(
      as_chunk("Resumen estadístico de área total por categoría de cumplimiento", props = fp_text(bold = TRUE))
    )
  )
```
## Distribución porcentual de la variación del área
La siguiente tabla presenta una distribución de frecuencias de la variable _var_area_, expresada en intervalos porcentuales. Se indican las frecuencias absolutas, relativas y acumuladas para cada rango. Esta información permite observar cómo se distribuyen los casos según el grado de sobreformulación o subformulación frente al área mínima requerida. Por ejemplo, más del 64% de los casos presentan una formulación inferior al área mínima (valores negativos), lo que puede señalar un patrón general de subformulación.

```{r}
# Paso 1: Crear la variable var_area en porcentaje
bd <- bd %>%
  mutate(var_area_pct = var_area * 100)

# Paso 2: Crear los intervalos personalizados
bd <- bd %>%
  mutate(intervalo_pct = cut(var_area_pct,
                             breaks = c(-150, -100, -75, -50, -25, 0, 25, 50, 75, 100, 150),
                             labels = c("<-100%", "-100% a -75%", "-75% a -50%", "-50% a -25%",
                                        "-25% a 0%", "0% a 25%", "25% a 50%", "50% a 75%",
                                        "75% a 100%", ">100%"),
                             right = FALSE))

# Paso 3: Generar tabla de frecuencia
tabla_frec <- bd %>%
  count(intervalo_pct) %>%
  rename(`Intervalo (%)` = intervalo_pct,
         `Frecuencia Absoluta` = n) %>%
  mutate(`Frecuencia Relativa (%)` = round(`Frecuencia Absoluta` / sum(`Frecuencia Absoluta`) * 100, 2),
         `Frecuencia Acumulada` = cumsum(`Frecuencia Absoluta`),
         `Frecuencia Acumulada (%)` = round(cumsum(`Frecuencia Relativa (%)`), 2)) %>%
  mutate(across(where(is.numeric), ~round(., 2)))  # Redondear a 2 decimales

# Paso 4: Visualización con flextable (estilo estandarizado)
flextable(tabla_frec) %>%
  theme_box() %>%
  color(part = "header", color = "white") %>%
  bg(part = "header", bg = "#0a6ebd") %>%
  fontsize(part = "all", size = 10) %>%
  align(align = "center", part = "all") %>%
  autofit() %>%
  set_caption(
    caption = as_paragraph(
      as_chunk("Tabla de frecuencia de variación del area expresada en porcentaje", props = fp_text(bold = TRUE))
    )
  )
```

## Prueba de normalidad

La figura presentada a continuación, muestra la distribución empírica acumulada de la variación porcentual del área. Se observa que la mayoría de los valores se concentran entre -50% y 100%, con una clara asimetría hacia valores positivos más extremos. Esta distribución sugiere la presencia de valores atípicos y una posible desviación respecto a la normalidad.

```{r}
bd %>%
  mutate(var_area_pct = var_area * 100) %>%
  count(var_area_pct) %>%
  rename(Frecuencia = n) %>%
  mutate(
    Frec_relativa = Frecuencia / sum(Frecuencia),
    Frec_relativa_acum = cumsum(Frec_relativa)
  ) %>%
   filter(!is.na(var_area_pct), !is.na(Frec_relativa_acum)) %>%
  select(var_area_pct, Frec_relativa_acum) %>%
  ggplot(aes(x = var_area_pct, y = Frec_relativa_acum)) +
    geom_point(color = "#0a6ebd", size = 2) +
    geom_line(color = "#0a6ebd", linewidth = 1) +
    labs(
      title = "Distribución empírica de la variación de área (%)",
      x = "Variación de área (%)",
      y = "Frecuencia relativa acumulada"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "sans"),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 10),
      panel.grid = element_blank(),
    )
```

## Resultado de la prueba de Lilliefors

La prueba de normalidad de Lilliefors mostró que los datos no siguen una distribución normal (se rechaza la hipótesis nula H₀). Esto indica que no se cumple el supuesto de normalidad, por lo que deben considerarse métodos estadísticos no paramétricos o transformaciones de los datos para futuros análisis.
```{r}
resultados_lillie <- bd %>% 
  pull(var_area_pct) %>% 
  na.omit() %>% 
  lillie.test()

pval <- round(resultados_lillie$p.value, 4)

mensaje <- if (pval > 0.05) {
  glue("Según la prueba de Lilliefors, los datos son normales (p = {pval}), por lo que no se rechaza la hipótesis nula (H₀).")
} else {
  glue("Según la prueba de Lilliefors, los datos no son normales (p = {pval}), por lo que se rechaza la hipótesis nula (H₀).")
}
```

# Conclusiones

El presente análisis permitió caracterizar y evaluar la variación del área en diferentes farmacias en el país, aportando evidencia clave para la toma de decisiones institucionales. A partir del tratamiento y exploración de los datos, se concluye lo siguiente:

* **Alta variabilidad en el área formulada:** Se evidenciaron diferencias significativas entre farmacias respecto a la variación del área, con casos que superan ampliamente el 100% y otros con reducciones cercanas al -100%. Esta dispersión sugiere desigualdades en la asignación o gestión de la estructura física, lo cual puede afectar la operación regular de los establecimientos.

* **Distribución no normal de los datos:** A través de la prueba de normalidad de Lilliefors, se concluyó que la variable de variación de área (expresada en porcentaje) no sigue una distribución normal. 

* **Importancia de la automatización y visualización:** El uso de herramientas del ecosistema tidyverse, junto con visualizaciones interactivas y tablas automatizadas, permitió generar un informe dinámico, reproducible y transparente, facilitando la interpretación y toma de decisiones basada en datos.

* En conjunto, este informe evidencia la necesidad de monitorear y revisar los criterios de planificación y asignación de espacio e infraestructura, proponiendo el uso continuo de análisis estadísticos como herramienta de gestión estratégica.

* Al docente Julian Piedrahita, muchas gracias por compartir información y por siempre tener la mejor disposición para enseñar y para responder las inquietudes. 

```{r}

```


