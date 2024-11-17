library(dplyr)
library(tidyr)

data(mtcars)
df <- as.data.frame(mtcars)

df_selected <- df %>%
  select(mpg, cyl, hp, gear) %>%
  filter(cyl > 4)

print(df_selected)

df_sorted <- df_selected %>%
  arrange(desc(hp)) %>%
  rename(consumo = mpg, potencia = hp)

print(df_sorted)

df_efficiency <- df_sorted %>%
  mutate(eficiencia = consumo / potencia) %>%
  group_by(cyl) %>%
  summarise(consumo_medio = mean(consumo), potencia_max = max(potencia))

print(df_efficiency)

df_gear <- data.frame(
  gear = c(3, 4, 5),
  tipo_transmision = c("Manual", "Automática", "Semiautomática")
)

df_joined <- left_join(df_sorted, df_gear, by = "gear")

print(df_joined)

df_long <- df_joined %>%
  pivot_longer(cols = c(consumo, potencia, eficiencia), 
               names_to = "medida", values_to = "valor")

df_long_grouped <- df_long %>%
  group_by(cyl, gear, tipo_transmision, medida) %>%
  summarise(valor = mean(valor), .groups = "drop")

df_wide <- df_long_grouped %>%
  pivot_wider(names_from = medida, values_from = valor)

print(df_wide)
