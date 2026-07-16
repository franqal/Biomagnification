####Script trabajo de Biomagnificación de Mercurio####

#####Primero Media y Desviación estandar por tipo de ecosistema acuático###
library(tidyr)
library (tidyverse)
library(ggplot2)
####hago una base de datos solo con la variable de interés##
### inicio con THg total de ríos y todas las categorias de foodweb##
tmr<- Basedatos %>%
  select(tms, Habitat) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Rio"))  # Filtrar filas (ejemplo: mantener filas donde columna2 es 'b' o 'd')


tmr <- na.omit(tmr)
media <- mean(tmr$tms)
desviacion_estandar <- sd(tmr$tms)

tfr <-  Basedatos %>%
  select(tmf, Habitat) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Rio"))

tfr <- na.omit(tfr)
media <- mean(tfr$tmf)
desviacion_estandar <- sd(tfr$tmf)

cat("La media es:", media, "\n")
cat("La desviación estándar es:", desviacion_estandar, "\n")

####Ahora voy a obtener la media y desviación solo de MF de ríos##


tmMF<- Basedatos %>%
  select(TMF15N304, Habitat,Foodweb) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Rio") & Foodweb == "MF")  # Filtrar filas (ejemplo: mantener filas donde columna2 es 'b' o 'd')
tmMF <- na.omit(tmMF)
media <- mean(tmMF$tms)
desviacion_estandar <- sd(tmMF$tms)

tmfMF<- Basedatos %>%
  select(tmf, Habitat,Foodweb) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Rio") & Foodweb == "MF")  # Filtrar filas (ejemplo: mantener filas donde columna2 es 'b' o 'd')
tmfMF <- na.omit(tmfMF)
media <- mean(tmfMF$tmf)
desviacion_estandar <- sd(tmfMF$tmf)


####la media y desviación solo de F de ríos##
tmF<- Basedatos %>%
  select(tms, Habitat,Foodweb) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Rio") & Foodweb == "F")  # Filtrar filas (ejemplo: mantener filas donde columna2 es 'b' o 'd')
tmF <- na.omit(tmF)
media <- mean(tmF$tms)
desviacion_estandar <- sd(tmF$tms)

tmfMF<- Basedatos %>%
  select(tmf, Habitat,Foodweb) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Rio") & Foodweb == "F")  # Filtrar filas (ejemplo: mantener filas donde columna2 es 'b' o 'd')
tmfMF <- na.omit(tmfMF)
media <- mean(tmfMF$tmf)
desviacion_estandar <- sd(tmfMF$tmf)

###HAhora con Lagos media y desviacion estandar total de THg tms y tmf ##

tmlake<- Basedatos %>%
  select(tms, Habitat) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Lago"))

tmlake <- na.omit(tmr)
media <- mean(tmlake$tms)
desviacion_estandar <- sd(tmlake$tms)

tmflake <-  Basedatos %>%
  select(tmf, Habitat) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Lago"))

tmflake <- na.omit(tmflake)
media <- mean(tmflake$tmf)
desviacion_estandar <- sd(tmflake$tmf)

### calculo media y desviacion de tms tmf en Fish##

tmsF<- Basedatos %>%
  select(tms, Habitat,Foodweb) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Lago") & Foodweb == "F")  # Filtrar filas (ejemplo: mantener filas donde columna2 es 'b' o 'd')
tmsF <- na.omit(tmsF)
media <- mean(tmsF$tms)
desviacion_estandar <- sd(tmsF$tms)

tmfF<- Basedatos %>%
  select(tmf, Habitat,Foodweb) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Lago") & Foodweb == "F")  # Filtrar filas (ejemplo: mantener filas donde columna2 es 'b' o 'd')
tmfF <- na.omit(tmfF)
media <- mean(tmfF$tmf)
desviacion_estandar <- sd(tmfF$tmf)

### lagos y MF###

tmsMF<- Basedatos %>%
  select(tms, Habitat,Foodweb) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Lago") & Foodweb == "MF")  # Filtrar filas (ejemplo: mantener filas donde columna2 es 'b' o 'd')
tmsMF <- na.omit(tmsMF)
media <- mean(tmsMF$tms)
desviacion_estandar <- sd(tmsMF$tms)

tmfMF<- Basedatos %>%
  select(tmf, Habitat,Foodweb) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Lago") & Foodweb == "MF")  # Filtrar filas (ejemplo: mantener filas donde columna2 es 'b' o 'd')
tmfMF <- na.omit(tmfMF)
media <- mean(tmfMF$tmf)
desviacion_estandar <- sd(tmfMF$tmf)

##Ahora PF##

tmsPF<- Basedatos %>%
  select(tms, Habitat,Foodweb) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Lago") & Foodweb == "PF")  # Filtrar filas (ejemplo: mantener filas donde columna2 es 'b' o 'd')
tmsPF <- na.omit(tmsPF)
media <- mean(tmsPF$tms)
desviacion_estandar <- sd(tmsPF$tms)

tmfPF<- Basedatos %>%
  select(tmf, Habitat,Foodweb) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Lago") & Foodweb == "PF")  # Filtrar filas (ejemplo: mantener filas donde columna2 es 'b' o 'd')
tmfPF <- na.omit(tmfPF)
media <- mean(tmfPF$tmf)
desviacion_estandar <- sd(tmfPF$tmf)

##Ahora PMF en lagos##


tmsPMF<- Basedatos %>%
  select(tms, Habitat,Foodweb) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Lago") & Foodweb == "PMF")  
tmsPMF <- na.omit(tmsPMF)
media <- mean(tmsPMF$tms)
desviacion_estandar <- sd(tmsPMF$tms)

tmfPMF<- Basedatos %>%
  select(tmf, Habitat,Foodweb) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Lago") & Foodweb == "PMF")  
tmfPMF <- na.omit(tmfPMF)
media <- mean(tmfPMF$tmf)
desviacion_estandar <- sd(tmfPMF$tmf) ###NO HABIAN VALORES###

###Vamos con Costas y Estuarios 
###Primero el total ##
tmsec<- Basedatos %>%
  select(tms, Habitat) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Costa", "Estuario"))  # Filtrar filas (ejemplo: mantener filas donde columna2 es 'b' o 'd')


tmsec <- na.omit(tmsec)
media <- mean(tmsec$tms)
desviacion_estandar <- sd(tmsec$tms)

tmfec <-  Basedatos %>%
  select(tmf, Habitat) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Costa", "Estuario"))

tmfec <- na.omit(tmfec)
media <- mean(tmfec$tmf)
desviacion_estandar <- sd(tmfec$tmf)

##Fish Costas y Estuarios##

tmsecF<- Basedatos %>%
  select(tms, Habitat,Foodweb) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Estuario", "Costa") & Foodweb == "F")  
tmsecF <- na.omit(tmsecF)
media <- mean(tmsecF$tms)
desviacion_estandar <- sd(tmsecF$tms)

tmfecF<- Basedatos %>%
  select(tmf, Habitat,Foodweb) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Lago") & Foodweb == "PMF")  
tmfPMF <- na.omit(tmfPMF)
media <- mean(tmfPMF$tmf)
desviacion_estandar <- sd(tmfPMF$tmf)


#############Voy hacer una regresión con varias pendientes###

library(ggplot2)

###eliimino los N/A de una unica columna sin dañar el resto###
Basedatos$TMF15N304 <- na.omit(Basedatos$TMF15N304)

# Crea la gráfica con ggplot2 por habitat
ggplot(Basedatos, aes(x = Lat, y = tms, color = Habitat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "TMS por hábitat",
       x = "Latitud",
       y = "TMS",
       color = "Habitat") +
  theme_minimal()
ggsave("Habitat.jpg", dpi = 300, width = 6, height = 4, units = "in")

# Crea la gráfica con ggplot2 por Continente

ggplot(Basedatos, aes(x = Lat, y = tms, color = Continente)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "TMS por Continente",
       x = "Latitud",
       y = "TMS",
       color = "Continente") +
  theme_minimal()
ggsave("Continente.jpg", dpi = 300, width = 6, height = 4, units = "in")

# Ajusta un modelo de regresión lineal
modelo <- lm(tms ~ Lat + Habitat, data=tmr)
modelorio <- lm(tms ~ Lat,data=tmr)
# Obtiene los valores de la pendiente para cada categoría
pendientes <- coef(modelorio)["x"]

# Imprime el valor de las pendientes
print(pendientes)
summary(modelorio)

tmr<- Basedatos %>%
  select(tms,Lat, Habitat) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Rio"))

######Paradoja Simpson##################################################
library(tidyverse)      # general use ----
library(broom)          # tidying of stats results ----
library(lme4)           # linear mixed models ----   
library(performance)    # obtain r-squared ----
library(gt)             # create table ----
library(gtExtras)       # table formatting ----
library(palmerpenguins) # simpsons paradox example ----

theme_shannon <- function(){
  # theme minimal creates white background and removes ticks on axes ----
  theme_minimal() +
    theme(
      # removes grid lines from plot ----
      panel.grid = element_blank(),
      # moves legend to top instead of side ----
      legend.position = "top", 
      # removes title from legend, often superfluous ----
      legend.title = element_blank(), 
      # creates the light gray box around the plot ----
      panel.background = element_rect(color = "#F2F2F2"),
      # creates the gray background boxes for faceting labels ----
      strip.background = element_rect(
        color = "#F2F2F2",
        fill = "#F2F2F2"
      ),
      # if using facet grid, this rotates the y text to be more readable ----
      strip.text.y = element_text(angle = 0),
      # this produces a fully left justified title ----
      plot.title.position = "plot"
    )
}

dat_tmsh <- TMF %>% 
  dplyr::select(Habitat, Lat, tmf) %>% 
  # duplicate species variable for coloring & grouping ---
  mutate(habitat_category = Habitat) %>% 
  drop_na()
# species colors used in the palmerpenguins readme ----
colors_habitat <- c(
  "Costa" = "darkorange",
  "Estuario" = "purple",
  "Rio" = "cyan4", "Lago"="blue", "Oceano"="brown"
)

dat_tmsh %>% 
  # add stack for all species to be analyzed together ----
bind_rows(dat_tmsh %>% mutate(habitat_category = "All")) %>% 
  # now examine by 3 species plus all ----
group_by(habitat_category) %>% 
  nest() %>% 
  # within each group, compute base n and correlation ----
mutate(
  base_n = map_int(data, nrow),
  corr = map(data, ~ cor.test(x = .x$Lat, y = .x$tmf) %>% broom::tidy())
) %>% 
  ungroup() %>% 
  # bring results back to raw data ----
unnest(c(data, corr)) %>% 
  mutate(
    # create ordered facet label for plotting ----
    habitat_category = fct_relevel(habitat_category,"Oceano", "Costa", "Estuario", "Rio", "Lago", "All"),
    corr_label =  glue::glue("{habitat_category}\nn = {base_n}\n r = {scales::number(estimate, 0.01)}"),
    corr_label = fct_reorder(as.character(corr_label), as.numeric(habitat_category))
  ) %>% 
  # create scatter plots ----
ggplot(aes(x = Lat, y = tmf)) +
  geom_point(size=3, aes(color = Habitat), alpha = 0.5, show.legend = FALSE) +
  geom_smooth(method = "lm", color = "darkgray", se = FALSE) +
  facet_wrap(. ~ corr_label, ncol = 4) +
  scale_color_manual(values = colors_habitat) + 
  theme_shannon() +theme(
    axis.text = element_text(size = 20),    # Tamaño de la letra en los números de los ejes
    axis.title = element_text(size = 20), plot.title = element_text(size = 20), 
    # Tamaño de la letra de los títulos de los ejes
    
  ) 


ggsave("TMFEcosistema.jpg", dpi = 300, width = 16, height = 14, units = "in")


####Por Continente##
dat_tmsc <- Basedatos %>% 
  dplyr::select(Continente, Lat, tms) %>% 
  # duplicate species variable for coloring & grouping ---
  mutate(Conti_category = Continente) %>% 
  drop_na()
# species colors used in the palmerpenguins readme ----
colors_conti <- c(
  "South America" = "darkorange",
  "Africa" = "purple",
  "Oceania" = "cyan4", "Antarctic"="blue"
)

dat_tmsc %>% 
  # add stack for all species to be analyzed together ----
bind_rows(dat_tmsc %>% mutate(Conti_category = "All")) %>% 
  # now examine by 3 species plus all ----
group_by(Conti_category) %>% 
  nest() %>% 
  # within each group, compute base n and correlation ----
mutate(
  base_n = map_int(data, nrow),
  corr = map(data, ~ cor.test(x = .x$Lat, y = .x$tms) %>% broom::tidy())
) %>% 
  ungroup() %>% 
  # bring results back to raw data ----
unnest(c(data, corr)) %>% 
  mutate(
    # create ordered facet label for plotting ----
    Conti_category = fct_relevel(Conti_category,"Africa", "America", "Antartica", "Oceania", "All"),
    corr_label =  glue::glue("{Conti_category}\nn = {base_n}\n r = {scales::number(estimate, 0.01)}"),
    corr_label = fct_reorder(as.character(corr_label), as.numeric(Conti_category))
  ) %>% 
  # create scatter plots ----
 ggplot(aes(dat_tmsh,x = Lat, y = tms)) +
  geom_point(size=3,aes(color = Continente), alpha = 0.5, show.legend = FALSE) +
  geom_smooth(method = "lm", color = "darkgray", se = FALSE) +
  facet_wrap(. ~ corr_label, ncol = 4) +
  scale_color_manual(values = colors_conti) + 
  theme_shannon()+theme(
    axis.text = element_text(size = 16),    # Tamaño de la letra en los números de los ejes
    axis.title = element_text(size = 16), plot.title = element_text(size = 16), 
    # Tamaño de la letra de los títulos de los ejes
   
  ) 
ggsave("relacionesConti.jpg", dpi = 300, width = 16, height = 14, units = "in")


####Por Clima##
dat_tmscl <- TMF %>% 
  dplyr::select(Clima, Lat, tmf) %>% 
  # duplicate species variable for coloring & grouping ---
  mutate(Clima_category = Clima) %>% 
  drop_na()

dat_tmscl <- TMF %>% 
  dplyr::select(Clima, Lat, tmf) %>% 
  # duplicate species variable for coloring & grouping ---
  mutate(Clima_category = Clima) %>% 
  drop_na()
# species colors used in the palmerpenguins readme ----
colors_clima <- c(
  "Polar" = "darkorange",
  "Temperate" = "purple",
  "Tropical" = "cyan4", "SunTropical"="blue"
)

dat_tmscl %>% 
  # add stack for all species to be analyzed together ----
bind_rows(dat_tmscl %>% mutate(Clima_category = "All")) %>% 
  # now examine by 3 species plus all ----
group_by(Clima_category) %>% 
  nest() %>% 
  # within each group, compute base n and correlation ----
mutate(
  base_n = map_int(data, nrow),
  corr = map(data, ~ cor.test(x = .x$Lat, y = .x$tmf) %>% broom::tidy())
) %>% 
  ungroup() %>% 
  # bring results back to raw data ----
unnest(c(data, corr)) %>% 
  mutate(
    # create ordered facet label for plotting ----
    Clima_category = fct_relevel(Clima_category,"Polar", "Temperate", "SubTropical", "Tropical", "All"),
    corr_label =  glue::glue("{Clima_category}\nn = {base_n}\n r = {scales::number(estimate, 0.01)}"),
    corr_label = fct_reorder(as.character(corr_label), as.numeric(Clima_category))
  ) %>% 
  # create scatter plots ----
ggplot(aes(x = Lat, y = tmf)) +
  geom_point(aes(color = Clima), alpha = 0.5, show.legend = FALSE, size=3) +
  geom_smooth(method = "lm", color = "darkgray", se = FALSE) +
  facet_wrap(. ~ corr_label, ncol = 4) +
  scale_color_manual(values = colors_clima) + 
  theme_shannon()+theme(
    axis.text = element_text(size = 16),    # Tamaño de la letra en los números de los ejes
    axis.title = element_text(size = 16), plot.title = element_text(size = 2), 
    # Tamaño de la letra de los títulos de los ejes
    
  )


ggsave("Clima.jpg", dpi = 300, width = 16, height = 14, units = "in")



######r2 value##

# estimate mixed model ----
mixed_model <- lme4::lmer(tmf ~ Lat + (1 | Habitat), TMF)

# retrieve sign of coefficient ----
coef_sign <- mixed_model %>% 
  broom.mixed::tidy() %>% 
  filter(term == "Lat") %>% 
  pull(estimate) %>% 
  sign()

# retrieve r2 measure ----
r2_by_group <- performance::r2_nakagawa(mixed_model, by_group = TRUE)$R2[1]

# compute adjusted correlation ----
adj_corr <- coef_sign * sqrt(r2_by_group)

# print result ----
adj_corr


##Diferencias entre tms por Continente##
library(ggstatsplot)


ggbetweenstats(
  data  = Basedatos,
  x     = Continente,
  y     = tmf,
  
)+theme(
  text = element_text(size = 16),          # Tamaño de la letra en general
  axis.text.x = element_text(size = 16),   # Tamaño de la letra en el eje X
  axis.text.y = element_text(size = 16),   # Tamaño de la letra en el eje Y
  plot.title = element_text(size = 16),    # Tamaño de la letra del título del gráfico
  legend.text = element_text(size = 22)    # Tamaño de la letra en la leyenda
)
ggsave("diferenciascontinenteTMF.pdf", dpi = 300, width = 10, height = 8, units = "in")

##Diferencias entre tms por habitat##
library(ggstatsplot)


ggbetweenstats(
  data  = Basedatos,
  x     = Habitat,
  y     = tmf,
  
)+theme(
  text = element_text(size = 16),          # Tamaño de la letra en general
  axis.text.x = element_text(size = 16),   # Tamaño de la letra en el eje X
  axis.text.y = element_text(size = 16),   # Tamaño de la letra en el eje Y
  plot.title = element_text(size = 16),    # Tamaño de la letra del título del gráfico
  legend.text = element_text(size = 22)    # Tamaño de la letra en la leyenda
)
ggsave("diferenciashabitat.jpg", dpi = 300, width = 12, height = 10, units = "in")

##Diferencias entre tms por Foodweb##
library(ggstatsplot)


ggbetweenstats(
  data  = Basedatos,
  x     = FW,
  y     = tmf,
  
)+theme(
  text = element_text(size = 16),          # Tamaño de la letra en general
  axis.text.x = element_text(size = 16),   # Tamaño de la letra en el eje X
  axis.text.y = element_text(size = 16),   # Tamaño de la letra en el eje Y
  plot.title = element_text(size = 16),    # Tamaño de la letra del título del gráfico
  legend.text = element_text(size = 22)    # Tamaño de la letra en la leyenda
)
ggsave("diferenciasFWTMF.pdf", dpi = 300, width = 12, height = 10, units = "in")


result = kruskal.test(tms ~ FW,
                      data = Basedatos)
print(result)

z <- Basedatos$tms
hist(z)
shapiro.test(z)

##Diferencias entre tms por Clima##
library(ggstatsplot)

ggbetweenstats(
  data  = Basedatos,
  x     = Clima,
  y     = tmf,
  
)+theme(
  text = element_text(size = 16),          # Tamaño de la letra en general
  axis.text.x = element_text(size = 16),   # Tamaño de la letra en el eje X
  axis.text.y = element_text(size = 16),   # Tamaño de la letra en el eje Y
  plot.title = element_text(size = 16),    # Tamaño de la letra del título del gráfico
  legend.text = element_text(size = 22)    # Tamaño de la letra en la leyenda
)
ggsave("diferenciasclimatmf.pdf", dpi = 300, width = 12, height = 10, units = "in")

############# Comparando diferencias entre las pendientes por habitat#######################

tmr<- TMF %>%
  select(tmf, Habitat, Lat) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Rio"))

tml<- TMF %>%
  select(tmf, Habitat, Lat) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Lago"))

tmo<- TMF %>%
  select(tmf, Habitat, Lat) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Oceano"))

tmc<- TMF %>%
  select(tmf, Habitat, Lat) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Costa"))

tme<- TMF %>%
  select(tmf, Habitat, Lat) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Estuario"))

modelo1 <- lm(tmf ~ Lat, data = tmr)
modelo2 <- lm(tmf ~ Lat, data = tml)
modelo3 <- lm(tmf ~ Lat, data = tmo)
modelo4 <- lm(tmf ~ Lat, data = tmc)
modelo5 <- lm(tmf ~ Lat, data = tme)

# Obtener los coeficientes de las regresiones
coef_modelo1 <- coef(modelo1)
coef_modelo2 <- coef(modelo2)
coef_modelo3 <- coef(modelo3)
coef_modelo4 <- coef(modelo4)
coef_modelo5 <- coef(modelo5)

# Calcular los errores estándar de los coeficientes
se_modelo1 <- summary(modelo1)$coefficients[, "Std. Error"]
se_modelo2 <- summary(modelo2)$coefficients[, "Std. Error"]
se_modelo3 <- summary(modelo3)$coefficients[, "Std. Error"]
se_modelo4 <- summary(modelo4)$coefficients[, "Std. Error"]
se_modelo5 <- summary(modelo5)$coefficients[, "Std. Error"]

# Calcular la diferencia de pendientes y su error estándar
diferencia_pendientes <- coef_modelo4[2] - coef_modelo5[2]
se_diferencia <- sqrt(se_modelo4[2]^2 + se_modelo5[2]^2)
# Calcular el estadístico de prueba (z-value)
z_value <- diferencia_pendientes / se_diferencia

# Calcular el p-valor
p_valor <- 2 * (1 - pnorm(abs(z_value)))

# Imprimir los resultados
cat("Diferencia de pendientes:", diferencia_pendientes, "\n")
cat("Error estándar de la diferencia:", se_diferencia, "\n")
cat("Estadístico de prueba (z-value):", z_value, "\n")
cat("P-valor:", p_valor, "\n")

########JUntos########
modelo <- lm(tms ~ Lat * Habitat, data = Basedatos)
coeficientes <- coef(modelo)
se <- summary(modelo)$coefficients[, "Std. Error"]
t_values <- coeficientes / se
p_valores <- 2 * (1 - pt(abs(t_values), df = 130 - length(coeficientes)))
cat("\nP-valores:\n")
print(p_valores)


###Comparando diferencias entre las pendientes por Continente###

tmaf<- TMF %>%
  select(tmf, Continente, Lat) %>%  # Seleccionar columnas
  filter(Continente %in% c("Africa"))

tmam<- Basedatos %>%
  select(tms, Continente, Lat) %>%  # Seleccionar columnas
  filter(Continente %in% c("America"))

tmo<- Basedatos %>%
  select(tms, Habitat, Lat) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Oceano"))

tmc<- Basedatos %>%
  select(tms, Habitat, Lat) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Costa"))

tme<- Basedatos %>%
  select(tms, Habitat, Lat) %>%  # Seleccionar columnas
  filter(Habitat %in% c("Estuario"))

modelo1 <- lm(tmf ~ Lat, data = tmaf)
modelo2 <- lm(tms ~ Lat, data = tmam)
modelo3 <- lm(tms ~ Lat, data = tmo)
modelo4 <- lm(tms ~ Lat, data = tmc)
modelo5 <- lm(tms ~ Lat, data = tme)

# Obtener los coeficientes de las regresiones
coef_modelo1 <- coef(modelo1)
coef_modelo2 <- coef(modelo2)
coef_modelo3 <- coef(modelo3)
coef_modelo4 <- coef(modelo4)
coef_modelo5 <- coef(modelo5)

# Calcular los errores estándar de los coeficientes
se_modelo1 <- summary(modelo1)$coefficients[, "Std. Error"]
se_modelo2 <- summary(modelo2)$coefficients[, "Std. Error"]
se_modelo3 <- summary(modelo3)$coefficients[, "Std. Error"]
se_modelo4 <- summary(modelo4)$coefficients[, "Std. Error"]
se_modelo5 <- summary(modelo5)$coefficients[, "Std. Error"]

# Calcular la diferencia de pendientes y su error estándar
diferencia_pendientes <- coef_modelo1[2] - coef_modelo2[2]
se_diferencia <- sqrt(se_modelo1[2]^2 + se_modelo2[2]^2)
# Calcular el estadístico de prueba (z-value)
z_value <- diferencia_pendientes / se_diferencia

# Calcular el p-valor
p_valor <- 2 * (1 - pnorm(abs(z_value)))

# Imprimir los resultados
cat("Diferencia de pendientes:", diferencia_pendientes, "\n")
cat("Error estándar de la diferencia:", se_diferencia, "\n")
cat("Estadístico de prueba (z-value):", z_value, "\n")
cat("P-valor:", p_valor, "\n")



##############Modelos con las variables####

library(ggplot2)
library(gamlss)
library(dplyr)
library(plyr)
library(ggpubr)


Basedatos$tmf <- na.omit(Basedatos$tmf)

histDist(tms,family="NO", data=prueba, nbins=40)
histDist(tms,family="GA", data=prueba, nbins=40)

m1_gl<-lm(data=prueba, tms~NO3+scale(chl)+scale(Temp)+scale(OD)+scale(pH))
summary(m1_gl)
plot(m1_gl)

m2_gl<-lm(data=prueba, tmf~pH+scale(NO3)+scale(Temp)+scale(chl))
summary(m2_gl)
plot(m2_gl)
library(writexl)

# Ejemplo de exportación a Excel
write_xlsx(weightable(mods), path = "resumen.xlsx")


library(lme4)
library(MuMIn)
install.packages("glmulti")
library(glmulti)
mm_glm<-lmer(data=prueba, tms~OD+(1|Clima)+scale(pH)+scale(chl)+scale(NO3)+scale(Temp))
summary(mm_glm)
plot(mm_glm)
log_likelihood <- logLik(mm_glm)
mm_glm<-lmer(data = prueba, tms ~ OD + (1|Clima) + scale(pH) + scale(chl) + scale(NO3) + scale(Temp))

mf8 <- lm(tms ~ pH + NO3 + Temp + chl + OD, data = prueba)

mf8 <- lm(tmf ~ pH + NO3 + Temp + chl + OD, data = prueba, na.action = na.fail)

ms <- dredge(mf8)
mods <- glmulti(mf8, crit = "aic", level = 1)
weightable(mods)
nobs(mf8)
plot(mods, type = "s")

######RANDOMFOREST####
library(randomForest)

# Generar datos de ejemplo (puedes reemplazar esto con tus propios datos)
set.seed(123)
datos <- data.frame(
  Var1 = rnorm(100),
  Var2 = rnorm(100),
  Var3 = rnorm(100),
  Var4 = rnorm(100),
  Var5 = rnorm(100),
  Objetivo = rbinom(100, 1, 0.5)  # Variable dependiente binaria (0 o 1)
)

# Divide los datos en conjuntos de entrenamiento y prueba
indices_entrenamiento <- sample(1:nrow(prueba), 0.7*nrow(prueba))  # 70% para entrenamiento
datos_entrenamiento <- prueba[indices_entrenamiento, ]
datos_prueba <- prueba[-indices_entrenamiento, ]

# Entrenamiento del modelo
modelo_rf <- randomForest(tms ~ Temp + OD + NO3 + chl + pH, data = datos_entrenamiento, ntree = 500, importance = TRUE)

# Predicciones en el conjunto de prueba
predicciones <- predict(modelo_rf, datos_prueba)

# Evaluación del modelo (por ejemplo, usando la matriz de confusión)
matriz_confusion <- table(predicciones, datos_prueba$tms)
cat("Matriz de Confusión:\n")
print(matriz_confusion)

# Opcional: Visualización de la importancia de las características
importancia_caracteristicas <- importance(modelo_rf)
print(importancia_caracteristicas)

importancia <- randomForest::importance(modelo_rf)
barplot(importancia[ ,1], names.arg=rownames(importancia), 
        main="Importancia de Variables", 
        col="lightblue", las=2, cex.names=0.7)


###########TFMF###

library(ggstatsplot)


ggbetweenstats(
  data  = TMF,
  x     = Habitat,
  y     = tmf,
  
)+theme(
  text = element_text(size = 16),          # Tamaño de la letra en general
  axis.text.x = element_text(size = 16),   # Tamaño de la letra en el eje X
  axis.text.y = element_text(size = 16),   # Tamaño de la letra en el eje Y
  plot.title = element_text(size = 16),    # Tamaño de la letra del título del gráfico
  legend.text = element_text(size = 22)    # Tamaño de la letra en la leyenda
)
ggsave("tmfdiferenciasecosistema.jpg", dpi = 300, width = 12, height = 10, units = "in")

###FW TMF##

library(ggstatsplot)


ggbetweenstats(
  data  = TMF,
  x     = FW,
  y     = tmf,
  
)+theme(
  text = element_text(size = 16),          # Tamaño de la letra en general
  axis.text.x = element_text(size = 16),   # Tamaño de la letra en el eje X
  axis.text.y = element_text(size = 16),   # Tamaño de la letra en el eje Y
  plot.title = element_text(size = 16),    # Tamaño de la letra del título del gráfico
  legend.text = element_text(size = 22)    # Tamaño de la letra en la leyenda
)
ggsave("tmfdiferenciasFW.jpg", dpi = 300, width = 12, height = 10, units = "in")

#####Continente#
ggbetweenstats(
  data  = Basedatos,
  x     = Continente,
  y     = tmf,
  
)+theme(
  text = element_text(size = 16),          # Tamaño de la letra en general
  axis.text.x = element_text(size = 16),   # Tamaño de la letra en el eje X
  axis.text.y = element_text(size = 16),   # Tamaño de la letra en el eje Y
  plot.title = element_text(size = 16),    # Tamaño de la letra del título del gráfico
  legend.text = element_text(size = 22)    # Tamaño de la letra en la leyenda
)
ggsave("tmfcontinente.jpg", dpi = 300, width = 12, height = 10, units = "in")

####Clima##

ggbetweenstats(
  data  = Basedatos,
  x     = Clima,
  y     = tmf,
  
)+theme(
  text = element_text(size = 16),          # Tamaño de la letra en general
  axis.text.x = element_text(size = 16),   # Tamaño de la letra en el eje X
  axis.text.y = element_text(size = 16),   # Tamaño de la letra en el eje Y
  plot.title = element_text(size = 16),    # Tamaño de la letra del título del gráfico
  legend.text = element_text(size = 22)    # Tamaño de la letra en la leyenda
)
ggsave("tmfdifclima.jpg", dpi = 300, width = 12, height = 10, units = "in")


####Por Continente##
regtmf <-Basedatos %>% 
  dplyr::select(Continente, Lat, tmf) %>% 
  # duplicate species variable for coloring & grouping ---
  mutate(Conti_category = Continente) %>% 
  drop_na()
# species colors used in the palmerpenguins readme ----
colors_conti <- c(
  "America" = "darkorange",
  "Africa" = "purple",
   "Oceania"="blue", "Antartica"="red"
)

regtmf %>% 
  # add stack for all species to be analyzed together ----
bind_rows(regtmf %>% mutate(Conti_category = "All")) %>% 
  # now examine by 3 species plus all ----
group_by(Conti_category) %>% 
  nest() %>% 
  # within each group, compute base n and correlation ----
mutate(
  base_n = map_int(data, nrow),
  corr = map(data, ~ cor.test(x = .x$Lat, y = .x$tmf) %>% broom::tidy())
) %>% 
  ungroup() %>% 
  # bring results back to raw data ----
unnest(c(data, corr)) %>% 
  mutate(
    # create ordered facet label for plotting ----
    Conti_category = fct_relevel(Conti_category,"Africa", "America", "Antartica", "Oceania", "All"),
    corr_label =  glue::glue("{Conti_category}\nn = {base_n}\n r = {scales::number(estimate, 0.01)}"),
    corr_label = fct_reorder(as.character(corr_label), as.numeric(Conti_category))
  ) %>% 
  # create scatter plots ----
ggplot(aes(regtmf,x = Lat, y = tmf)) +
  geom_point(size=3,aes(color = Continente), alpha = 0.5, show.legend = FALSE) +
  geom_smooth(method = "lm", color = "darkgray", se = FALSE) +
  facet_wrap(. ~ corr_label, ncol = 4) +
  scale_color_manual(values = colors_conti) + 
  theme_shannon()+theme(
    axis.text = element_text(size = 16),    # Tamaño de la letra en los números de los ejes
    axis.title = element_text(size = 16), plot.title = element_text(size = 16), 
    # Tamaño de la letra de los títulos de los ejes
    
  ) 
ggsave("tmfConti.jpg", dpi = 300, width = 16, height = 14, units = "in")


#Clima#

clitmf <- Basedatos %>% 
  dplyr::select(Clima, Lat, tmf) %>% 
  # duplicate species variable for coloring & grouping ---
  mutate(Clima_category = Clima) %>% 
  drop_na()
# species colors used in the palmerpenguins readme ----
colors_clima <- c(
  "Tropical" = "darkgreen",
  "SubTropical" = "purple",
  "Temperate"="blue", 
  "Polar"="red"
)

clitmf %>% 
  # add stack for all species to be analyzed together ----
bind_rows(clitmf %>% mutate(Clima_category = "All")) %>% 
  # now examine by 3 species plus all ----
group_by(Clima_category) %>% 
  nest() %>% 
  # within each group, compute base n and correlation ----
mutate(
  base_n = map_int(data, nrow),
  corr = map(data, ~ cor.test(x = .x$Lat, y = .x$tmf) %>% broom::tidy())
) %>% 
  ungroup() %>% 
  # bring results back to raw data ----
unnest(c(data, corr)) %>% 
  mutate(
    # create ordered facet label for plotting ----
    Clima_category = fct_relevel(Clima_category,"Tropical", "SubTropical",  "Temperate","Polar", "All"),
    corr_label =  glue::glue("{Clima_category}\nn = {base_n}\n r = {scales::number(estimate, 0.01)}"),
    corr_label = fct_reorder(as.character(corr_label), as.numeric(Clima_category))
  ) %>% 
  # create scatter plots ----
ggplot(aes(regtmf,x = Lat, y = tmf)) +
  geom_point(size=3,aes(color = Clima), alpha = 0.5, show.legend = FALSE) +
  geom_smooth(method = "lm", color = "darkgray", se = FALSE) +
  facet_wrap(. ~ corr_label, ncol = 4) +
  scale_color_manual(values = colors_clima) + 
  theme_shannon()+theme(
    axis.text = element_text(size = 16),    # Tamaño de la letra en los números de los ejes
    axis.title = element_text(size = 16), plot.title = element_text(size = 16), 
    # Tamaño de la letra de los títulos de los ejes
    
  ) 
ggsave("tmfClima.jpg", dpi = 300, width = 16, height = 14, units = "in")


###Por Habitat TMF###



habtmf <- Basedatos %>% 
  dplyr::select(Habitat, Lat, tmf) %>% 
  # duplicate species variable for coloring & grouping ---
  mutate(habitat_category = Habitat) %>% 
  drop_na()
# species colors used in the palmerpenguins readme ----
colors_habitat <- c(
  "Rio" = "darkgreen",
  "Lago" = "purple",
  "Estuario"="blue", 
  "Costa"="red", "Oceano"="brown"
)

habtmf %>% 
  # add stack for all species to be analyzed together ----
bind_rows(habtmf %>% mutate(habitat_category = "All")) %>% 
  # now examine by 3 species plus all ----
group_by(habitat_category) %>% 
  nest() %>% 
  # within each group, compute base n and correlation ----
mutate(
  base_n = map_int(data, nrow),
  corr = map(data, ~ cor.test(x = .x$Lat, y = .x$tmf) %>% broom::tidy())
) %>% 
  ungroup() %>% 
  # bring results back to raw data ----
unnest(c(data, corr)) %>% 
  mutate(
    # create ordered facet label for plotting ----
    habitat_category = fct_relevel(habitat_category,"Lago", "Rio",  "Costa","Estuario","Oceano", "All"),
    corr_label =  glue::glue("{habitat_category}\nn = {base_n}\n r = {scales::number(estimate, 0.01)}"),
    corr_label = fct_reorder(as.character(corr_label), as.numeric(habitat_category))
  ) %>% 
  # create scatter plots ----
ggplot(aes(regtmf,x = Lat, y = tmf)) +
  geom_point(size=3,aes(color = Habitat), alpha = 0.5, show.legend = FALSE) +
  geom_smooth(method = "lm", color = "darkgray", se = FALSE) +
  facet_wrap(. ~ corr_label, ncol = 4) +
  scale_color_manual(values = colors_habitat) + 
  theme_shannon()+theme(
    axis.text = element_text(size = 16),    # Tamaño de la letra en los números de los ejes
    axis.title = element_text(size = 16), plot.title = element_text(size = 16), 
    # Tamaño de la letra de los títulos de los ejes
    
  ) 
ggsave("tmfhabitat.jpg", dpi = 300, width = 16, height = 14, units = "in")


#######PARA obtener las p-value por cada regresion#####
pv <- TMF %>%
  filter(Continente %in% c("Oceania"))
modelo_regresion <- lm(tmf ~ Lat, data =pv)
summary(modelo_regresion)


######Relación año biomagnificación##

# Modelo de regresión lineal
model <- lm(estudio ~ tmf, data = Basedatos)
model_summary <- summary(model)

# Extraer R^2 y p-value
r_squared <- model_summary$r.squared
p_value <- model_summary$coefficients[2, 4]  # P-value para la pendiente

# Crear texto para anotar en el gráfico
text_label <- paste("R^2 =", round(r_squared, 2), "\np-value =", round(p_value, 3))

# Crear una etiqueta con R^2 y p-value
label <- paste0("R² = ", round(r_squared, 2), "\np-value = ", format.pval(p_value, digits = 2))

# Crear el gráfico
ggplot(Basedatos, aes(x = estudio, y = tmf)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  annotate("text", x = Inf, y = Inf, label = label, hjust = 6, vjust = 1, size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.margin = margin(1, 1, 1, 1, "cm"), axis.line = element_line(colour = "black")) +
  labs(       
         x = "Year", 
       y = "TMF")

ggsave("tmfaño.jpg", dpi = 300, width = 16, height = 14, units = "in")


df_filtered <- Basedatos %>%
  select(estudio, tms) %>%
  filter(tms >= 0)


####Promedio y desviaciones##
library(tidyverse)
BS <- TMF %>%
  select(Clima, FW, tmf)
data_promedio <- BS %>%
  group_by(Clima,FW) %>%
  summarise(promedio_numerico = mean(tmf, na.rm = TRUE),
            desviacion_estandar = sd(tmf, na.rm = TRUE))

modelo_regresion <- lm(tms ~ Lat, data = Basedatos)
summary(modelo_regresion)





# Cargar librerías necesarias
library(readxl)
library(dplyr)

# Cargar el archivo Excel
file_path <- "ruta/del/archivo/TMF.xlsx" # Cambiar a la ruta correcta
data <- read_excel(file_path)

# Calcular el promedio y desviación estándar por Habitat y FW
resultados <- TMF %>%
  group_by(Clima, FW) %>%
  summarise(
    promedio_tmf = mean(tmf, na.rm = TRUE),
    sd_tmf = sd(tmf, na.rm = TRUE)
  )

resultados <- TMF %>%
  group_by(Clima) %>%
  summarise(
    promedio_tmf = mean(tmf, na.rm = TRUE),
    sd_tmf = sd(tmf, na.rm = TRUE)
  )

# Mostrar los resultados
print(resultados)





library(ggplot2)
library(dplyr)
library(readxl)
library(writexl)
library(broom)

# Leer datos desde objeto existente o Excel
datos <- Basedatos  # Si ya lo leíste
# datos <- read_excel("Basedatos.xlsx", sheet = "Basedatos")  # si no lo has leído

# Filtrar columnas necesarias
df <- datos %>%
  select(FW, tmf, Lat) %>%
  filter(!is.na(FW), !is.na(tmf), !is.na(Lat)) %>%
  mutate(FW = as.factor(FW))

# --------- 1. Gráfico combinado con facet_wrap y guardar PDF ----------
p_facet <- ggplot(df, aes(x = Lat, y = tmf)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~FW, scales = "free", ncol = 1) +  # vertical
  labs(title = "",
       x = "Latitud", y = "TMS") +
  theme_minimal() 

# Guardar en PDF (más alto)
ggsave("regresiones_FW TMF.pdf", p_facet, width = 8, height = 12)

# --------- 2. Tabla de estadísticos de regresión ----------
resultados <- df %>%
  group_by(FW) %>%
  group_modify(~{
    modelo <- lm(tmf ~ Lat, data = .x)
    resumen <- summary(modelo)
    conf <- tryCatch(confint(modelo), error = function(e) NULL)
    
    if (!is.null(conf) && "Lat" %in% rownames(conf)) {
      tibble(
        R2 = resumen$r.squared,
        p_value = coef(resumen)[2, 4],
        Pendiente = coef(modelo)[2],
        Intercepto = coef(modelo)[1],
        Std_Error = coef(resumen)[2, 2],
        IC_95_inf = conf["Lat", 1],
        IC_95_sup = conf["Lat", 2],
        n = nrow(.x)
      )
    } else {
      tibble(
        R2 = NA,
        p_value = NA,
        Pendiente = NA,
        Intercepto = coef(modelo)[1],
        Std_Error = NA,
        IC_95_inf = NA,
        IC_95_sup = NA,
        n = nrow(.x)
      )
    }
  })

# Mostrar tabla en consola
print(resultados)

# --------- 3. Guardar tabla como Excel ----------
write_xlsx(resultados, "resultados_regresion_FWTMF.xlsx")



####Voy a hacer los gradicos de violin separados por ecosistema y por FW

# Leer la base de datos
df <- Basedatos

# Asegurar consistencia de nombres
names(df) <- tolower(trimws(names(df)))

# Crear los dos grupos según el tipo de hábitat
grupo1 <- df[df$habitat %in% c("Costa", "Estuario", "Oceano"), ]
grupo2 <- df[df$habitat %in% c("Rio", "Lago"), ]

# Verificar cuántos registros hay en cada grupo
cat("Grupo 1 (Costa, Estuario, Oceano):", nrow(grupo1), "registros\n")
cat("Grupo 2 (Rio, Lago):", nrow(grupo2), "registros\n")

# Gráfico para Grupo 1
ggstatsplot::ggbetweenstats(
  data = grupo1,
  x = fw,
  y = tms,
  title = "Grup: Coast Estuary & Ocean",
  xlab = "Food Web",
  ylab = "TMS",
  type = "parametric",  # Cambia a "nonparametric" si los datos no son normales
  messages = FALSE
)
ggsave("Grupo1costaestuariooceanotms.jpg", dpi = 300, width = 12, height = 10, units = "in")
# Gráfico para Grupo 2
ggstatsplot::ggbetweenstats(
  data = grupo2,
  x = fw,
  y = tms,
  title = "Grup: Rivers & Lakes",
  xlab = "Food Web",
  ylab = "TMS",
  type = "parametric",
  messages = FALSE
)
ggsave("Grupo2rioslagostms.jpg", dpi = 300, width = 12, height = 10, units = "in")
#####CON TMF

# Gráfico para Grupo 1 con TMF
ggstatsplot::ggbetweenstats(
  data = grupo1,
  x = fw,
  y = tmf,
  title = "Grup: Coast Estuary & Ocean",
  xlab = "Food Web",
  ylab = "TMF",
  type = "parametric",  # Cambiar a "nonparametric" si lo necesitas
  messages = FALSE
)
ggsave("Grupo1AguasaladaTMF.jpg", dpi = 300, width = 12, height = 10, units = "in")
# Gráfico para Grupo 2 con TMF
ggstatsplot::ggbetweenstats(
  data = grupo2,
  x = fw,
  y = tmf,
  title = "Grup: Rivers & Lakes",
  xlab = "Food Web",
  ylab = "TMF",
  type = "parametric",
  messages = FALSE
)
ggsave("Grupo2rios lago tmf.jpg", dpi = 300, width = 12, height = 10, units = "in")



# Paquetes
library(tidyverse)
library(lme4)
library(lmerTest)   # para p-values
library(performance)

# Asegurar que continent sea factor
Basedatos$Continente <- as.factor(Basedatos$Continente)

# =========================
# MODELO CON INTERACCIÓN
# =========================

modelo1 <- lm(tms ~ Lat * Continente, data = Basedatos)

summary(modelo1)

# ANOVA del modelo
anova(modelo1)

# R2
performance::r2(modelo1)

# =========================
# VISUALIZACIÓN
# =========================

ggplot(Basedatos, aes(x = Lat, y = tms, color = Continente)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw()

# =========================
# MODELO MIXTO
# =========================
# intercepto aleatorio por continente

modelo2 <- lmer(TMS ~ Latitude + (1|Continent), data = datos)

summary(modelo2)

performance::r2(modelo2)

# =========================
# MODELO MIXTO CON PENDIENTES
# =========================
# permite que la pendiente de latitude
# cambie entre continentes

modelo3 <- lmer(TMS ~ Latitude + (Latitude|Continent), data = datos)

summary(modelo3)

performance::r2(modelo3)

# =========================
# COMPARAR MODELOS
# =========================

anova(modelo2, modelo3)

# =========================
# EXTRAER RESULTADOS LIMPIOS
# =========================

library(broom)

tidy(modelo1)

# para mixed models
library(broom.mixed)

tidy(modelo2)
tidy(modelo3)


# =====================================================
# WORLD MAP WITH EQUATOR LINE
# AND SAMPLING LOCATIONS
# =====================================================

# =========================
# INSTALL PACKAGES IF NEEDED
# =========================

# install.packages(c(
#   "ggplot2",
#   "sf",
#   "rnaturalearth",
#   "rnaturalearthdata",
#   "readxl",
#   "dplyr"
# ))

# =========================
# LOAD LIBRARIES
# =========================

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readxl)
library(dplyr)

# =========================
# LOAD COORDINATES
# =========================

datos <- read_excel("Coord.xlsx")

# =========================
# LOAD WORLD MAP
# =========================

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# =========================
# CREATE EQUATOR LINE
# =========================

equator <- data.frame(
  x = c(-180, 180),
  y = c(0, 0)
)

# =========================
# CREATE MAP
# =========================

mapa <- ggplot() +
  
  # Continents
  geom_sf(
    data = world,
    fill = "gray85",
    color = "white",
    linewidth = 0.2
  ) +
  
  # Equator line
  geom_path(
    data = equator,
    aes(x = x, y = y),
    color = "red",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  
  # Sampling locations
  geom_point(
    data = datos,
    aes(
      x = Long,
      y = Lat
    ),
    color = "black",
    size = 2.5,
    alpha = 0.9
  ) +
  
  # Full world map
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-90, 90),
    expand = FALSE
  ) +
  
  # Labels
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  # Theme
  theme_minimal(base_size = 14) +
  
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray85"),
    legend.position = "none",
    axis.title = element_text(face = "bold")
  )

# =========================
# SHOW MAP
# =========================

print(mapa)

# =========================
# SAVE JPG (HIGH RESOLUTION)
# =========================

ggsave(
  filename = "World_Map_Equator.jpg",
  plot = mapa,
  width = 14,
  height = 8,
  dpi = 600
)

# =========================
# SAVE PDF (VECTOR QUALITY)
# =========================

ggsave(
  filename = "World_Map_Equator.pdf",
  plot = mapa,
  width = 14,
  height = 8,
  device = cairo_pdf
)




###################### Plates separados por continente y All para TMS #########################

library(tidyverse)
library(broom)
library(glue)
library(forcats)

#========================

# THEME

#========================

theme_shannon <- function(){
  
  theme_minimal() +
    
   
  theme(
    
    panel.grid = element_blank(),
    
    legend.position = "top",
    
    legend.title = element_blank(),
    
    panel.background = element_rect(
      color = "#F2F2F2"
    ),
    
    strip.background = element_rect(
      color = "#F2F2F2",
      fill = "#F2F2F2"
    ),
    
    strip.text.y = element_text(angle = 0),
    
    plot.title.position = "plot"
  )
  
  
}

#========================

# DATA

#========================

dat_tmsc <- Basedatos %>%
  
  dplyr::select(
    Continente,
    Lat,
    TMF15N304
  ) %>%
  
  mutate(
    Conti_category = Continente
  ) %>%
  
  drop_na()

#========================

# COLORS

#========================

colors_conti <- c(
  "South America" = "darkorange",
  "Africa" = "purple",
  "Oceania" = "cyan4",
  "Antarctic" = "red",
  "All" = "black"
)

#========================

# PANEL 1:

# CONTINENTS SEPARATED

#========================

plot_continents <- dat_tmsc %>%
  
  group_by(Conti_category) %>%
  
  nest() %>%
  
  mutate(
  
    base_n = map_int(data, nrow),
    
    corr = map(
      data,
      ~ cor.test(
        x = .x$Lat,
        y = .x$TMF15N304
      ) %>% broom::tidy()
    )
   
    
  ) %>%
  
  ungroup() %>%
  
  unnest(c(data, corr)) %>%
  
  mutate(
    
    Conti_category = fct_relevel(
      Conti_category,
      "Africa",
      "South America",
      "Antarctic",
      "Oceania"
    ),
    
    corr_label = glue(
      "{Conti_category}\n",
      "n = {base_n}\n",
      "r = {scales::number(estimate, 0.01)}"
    ),
    
    corr_label = fct_reorder(
      corr_label,
      as.numeric(Conti_category)
    )
    
    
  ) %>%
  
  ggplot(
    aes(
      x = Lat,
      y = TMF15N304
    )
  ) +
  
  geom_point(
    aes(color = Conti_category),
    size = 3,
    alpha = 0.5,
    show.legend = FALSE
  ) +
  
  geom_smooth(
    method = "lm",
    color = "darkgray",
    se = FALSE
  ) +
  
  facet_wrap(
    ~ corr_label,
    ncol = 2
  ) +
  
  scale_color_manual(
    values = colors_conti
  ) +
  
  theme_shannon() +
  
  theme(
    
    
    axis.text = element_text(size = 16),
    
    axis.title = element_text(size = 16),
    
    plot.title = element_text(size = 16)
   
    
  )

plot_continents

ggsave(
  "relaciones_continentesTMF28 5 26.pdf",
  plot = plot_continents,
  dpi = 300,
  width = 14,
  height = 10,
  units = "in"
)

#========================

# PANEL 2:##

##ALL CONTINENTS COMBINED
#WITH COLORS BY CONTINENT

#========================

cor_all <- cor.test(
  dat_tmsc$Lat,
  dat_tmsc$TMF15N304
)

plot_all <- dat_tmsc %>%
  
  ggplot(
    aes(
      x = Lat,
      y = TMF15N304,
      color = Continente
    )
  ) +
  
  geom_point(
    size = 3,
    alpha = 0.6
  ) +
  
  geom_smooth(
    method = "lm",
    color = "darkgray",
    se = FALSE
  ) +
  
  scale_color_manual(
    values = colors_conti
  ) +
  
  labs(
    
    title = glue(
      "All continents combined\n",
      "n = {nrow(dat_tmsc)} | ",
      "r = {round(cor_all$estimate, 2)} | ",
      "p = {round(cor_all$p.value, 3)}"
    ),
    
    x = "Latitude",
    
    y = "TMF",
    
    color = "Continent"
    
  ) +
  
  theme_shannon() +
  
  theme(
    
    axis.text = element_text(size = 16),
    
    axis.title = element_text(size = 16),
    
    plot.title = element_text(size = 16),
    
    legend.text = element_text(size = 14)
    
  )

plot_all

ggsave(
  "relaciones_allTMF 28 5 2026.pdf",
  plot = plot_all,
  dpi = 300,
  width = 8,
  height = 6,
  units = "in"
)
