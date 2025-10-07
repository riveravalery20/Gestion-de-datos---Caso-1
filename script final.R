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
    AREA = str_squish(as.character(AREA)), 
    COD_DEPTO = str_squish(as.character(COD_DEPTO)), # Asegura que COD_DEPTO esté como carácter
    
    REGION = case_when(
      COD_DEPTO %in% c("08","13","20","44","47","70","23") ~ "Caribe",
      COD_DEPTO %in% c("05","11","15","17","18","25","41","54","63","66","68","73","76") ~ "Andina",
      COD_DEPTO %in% c("19","27","52") ~ "Pacifica",
      COD_DEPTO %in% c("50","81","85","97","99") ~ "Orinoquia-Amazonia",
      COD_DEPTO %in% c("91","95","98") ~ "Amazonia",
      COD_DEPTO %in% c("88") ~ "Insular",
      TRUE ~ "No hay registro"
    ),
    REGION = as.factor(REGION),
    INV_ACTIVOS = rowSums(select(., starts_with("P3017_")), na.rm = TRUE)
  ) %>% 
  select(VENTAS_MES_ANTERIOR, GASTOS_MES, P3017_A,
         VALOR_AGREGADO, INV_ACTIVOS, REGION, COD_DEPTO) %>% 
  mutate(VENTAS_MES_ANTERIOR=as.numeric(VENTAS_MES_ANTERIOR),
         GASTOS_MES=as.numeric(GASTOS_MES), P3017_A=as.numeric(P3017_A),
         VALOR_AGREGADO=as.numeric(VALOR_AGREGADO), INV_ACTIVOS=as.numeric(INV_ACTIVOS)) %>%
  filter(VENTAS_MES_ANTERIOR>0,VALOR_AGREGADO>0, INV_ACTIVOS>0, GASTOS_MES>0, P3017_A>0)
                                                                                                              
View(BaseFinal)
# Análisis y modelo
summary(BaseFinal$INV_ACTIVOS)
summary(BaseFinal)

Modelo <- lm(
  VENTAS_MES_ANTERIOR ~ GASTOS_MES +
    VALOR_AGREGADO + INV_ACTIVOS
  + factor(REGION)+ P3017_A,
  data = BaseFinal
)
summary(Modelo)

# Gráficos exploratorios
ggplot(BaseFinal, aes(x = VENTAS_MES_ANTERIOR, y = GASTOS_MES)) + geom_point()
ggplot(BaseFinal, aes(x = INV_ACTIVOS, y = VENTAS_MES_ANTERIOR)) + geom_point()

plot(Modelo, 1)

