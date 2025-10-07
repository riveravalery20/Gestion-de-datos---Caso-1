library(readr)
library(tidyverse)
library(stringr)

# Cargamos microdatos (Módulo de ventas o ingresos- módulo de costos, gastos y activos)
costos <- read_csv("Módulo de costos, gastos y activos.csv")
ventas <- read_csv("Módulo de ventas o ingresos.csv")

# Unimos bases con varaiableS Y(VENTAS_MES_ANTERIOR),X1(GASTOS_MES),X2(CONSUMO_INTERMEDIO),X3(VALOR_AGREGADO),X4(INV_ACTIVOS),X5(REGION)
BaseFinal <- costos %>% 
  inner_join(ventas, by = c("DIRECTORIO", "SECUENCIA_P",
                            "SECUENCIA_ENCUESTA", "CLASE_TE", "COD_DEPTO", "AREA")) %>%
  mutate(
    AREA = str_squish(as.character(AREA)), #cambiamos AREA para volver en variable categorica
    REGION = case_when(
      AREA %in% c("08","13","20","44","47","70","23") ~ "Caribe",
      AREA %in% c("05","11","15","17","25","68","73","76") ~ "Andina",
      AREA %in% c("27","52","19") ~ "Pacifica",
      AREA %in% c("85","81","50","99") ~ "Orinoquia",
      AREA %in% c("91","97","95","98","86") ~ "Amazonia",
      AREA %in% c("88") ~ "Insular",
      TRUE ~ "No hay registro"
    ),
    REGION = as.factor(REGION),
    # Inversión en activos (suma de activos fijos) sumamos  (P3017_B a P3017_G) los cuales agrupan los gastos operativos básicos de un negocio
    INV_ACTIVOS = rowSums(select(., starts_with("P3017_")), na.rm = TRUE),
    
  ) %>% 
  select(VENTAS_MES_ANTERIOR, GASTOS_MES, CONSUMO_INTERMEDIO,
         VALOR_AGREGADO, INV_ACTIVOS, REGION, COD_DEPTO) %>% 
  mutate(VENTAS_MES_ANTERIOR=as.numeric(VENTAS_MES_ANTERIOR),
         GASTOS_MES=as.numeric(GASTOS_MES), CONSUMO_INTERMEDIO= as.numeric(CONSUMO_INTERMEDIO),
         VALOR_AGREGADO=as.numeric(VALOR_AGREGADO), INV_ACTIVOS=as.numeric(INV_ACTIVOS))%>%
  filter(VENTAS_MES_ANTERIOR>0,VALOR_AGREGADO>0, INV_ACTIVOS>0, GASTOS_MES>0)
                                                                                                              )))
View(BaseFinal)
# Análisis y modelo
summary(BaseFinal$INV_ACTIVOS)
summary(BaseFinal)

Modelo <- lm(
  VENTAS_MES_ANTERIOR ~ GASTOS_MES +
    VALOR_AGREGADO + INV_ACTIVOS
  + factor(REGION),
  data = BaseFinal
)
summary(Modelo)

# Gráficos exploratorios
ggplot(BaseFinal, aes(x = VENTAS_MES_ANTERIOR, y = GASTOS_MES)) + geom_point()
ggplot(BaseFinal, aes(x = INV_ACTIVOS, y = VENTAS_MES_ANTERIOR)) + geom_point()

plot(Modelo, 1)

