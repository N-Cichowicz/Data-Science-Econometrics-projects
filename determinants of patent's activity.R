#Projekt ekonomiczny MP

# ============================================================
# 1. Pakiety
# ============================================================

library(tidyverse)
library(readr)
library(dplyr)
library(purrr)

# ============================================================
# 2. Folder z danymi
# ============================================================

data_dir <- "C:/Users/Norbert/Desktop/studia/modele panelowe/dane"

# Szybki test, czy R widzi folder
list.files(data_dir)

# ============================================================
# 3. Funkcja do wczytywania pojedynczego pliku WDI
# ============================================================

read_wdi_file <- function(file_name, variable_name) {
  
  file_path <- file.path(data_dir, file_name)
  
  read_csv(
    file_path,
    skip = 4,
    na = c("", "NA", ".."),
    show_col_types = FALSE
  ) %>%
    select(
      country = `Country Name`,
      iso3c = `Country Code`,
      indicator = `Indicator Name`,
      indicator_code = `Indicator Code`,
      matches("^[0-9]{4}$")
    ) %>%
    pivot_longer(
      cols = matches("^[0-9]{4}$"),
      names_to = "year",
      values_to = variable_name
    ) %>%
    mutate(
      year = as.integer(year),
      across(all_of(variable_name), as.numeric)
    ) %>%
    select(country, iso3c, year, all_of(variable_name))
}

# ============================================================
# 4. Wczytanie zmiennych
# ============================================================

patents <- read_wdi_file("patent .csv", "patents")

rd_gdp <- read_wdi_file(
  "research and development expenditure.csv",
  "rd_gdp"
)

tertiary_enrollment <- read_wdi_file(
  "schl.enrollment.csv",
  "tertiary_enrollment"
)

gdp_pc <- read_wdi_file(
  "gdp per capita.csv",
  "gdp_pc"
)

hightech_export <- read_wdi_file(
  "high-tech export.csv",
  "hightech_export"
)

population <- read_wdi_file(
  "population total.csv",
  "population"
)

# ============================================================
# 5. Lista krajów OECD
# ============================================================

oecd_iso3 <- c(
  "AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE",
  "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL",
  "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX",
  "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN",
  "ESP", "SWE", "CHE", "TUR", "GBR", "USA"
)

# ============================================================
# 6. Łączenie w jedną bazę
# ============================================================

panel_patents <- list(
  patents,
  rd_gdp,
  tertiary_enrollment,
  gdp_pc,
  hightech_export,
  population
) %>%
  reduce(full_join, by = c("country", "iso3c", "year")) %>%
  filter(iso3c %in% oecd_iso3) %>%
  arrange(iso3c, year)

# ============================================================
# 7. Zmienne do modeli
# ============================================================

panel_patents <- panel_patents %>%
  group_by(iso3c) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    rd_gdp_lag1 = lag(rd_gdp, 1),
    rd_gdp_lag2 = lag(rd_gdp, 2),
    tertiary_enrollment_lag1 = lag(tertiary_enrollment, 1),
    gdp_pc_lag1 = lag(gdp_pc, 1),
    hightech_export_lag1 = lag(hightech_export, 1),
    log_population = log(population),
    patents_per_million = patents / population * 1000000
  ) %>%
  ungroup()

panel_patents_2007_2021 <- panel_patents %>%
  filter(year >= 2007, year <= 2021)

# ============================================================
# 8. Kontrola braków
# ============================================================

missing_summary <- panel_patents_2007_2021 %>%
  summarise(
    n_obs = n(),
    countries = n_distinct(iso3c),
    patents_missing = sum(is.na(patents)),
    rd_gdp_missing = sum(is.na(rd_gdp)),
    tertiary_missing = sum(is.na(tertiary_enrollment)),
    gdp_pc_missing = sum(is.na(gdp_pc)),
    hightech_missing = sum(is.na(hightech_export)),
    population_missing = sum(is.na(population)),
    rd_gdp_lag1_missing = sum(is.na(rd_gdp_lag1)),
    tertiary_lag1_missing = sum(is.na(tertiary_enrollment_lag1)),
    gdp_pc_lag1_missing = sum(is.na(gdp_pc_lag1)),
    hightech_lag1_missing = sum(is.na(hightech_export_lag1))
  )

missing_by_country <- panel_patents_2007_2021 %>%
  group_by(iso3c, country) %>%
  summarise(
    n_obs = n(),
    patents_missing = sum(is.na(patents)),
    rd_gdp_missing = sum(is.na(rd_gdp)),
    tertiary_missing = sum(is.na(tertiary_enrollment)),
    gdp_pc_missing = sum(is.na(gdp_pc)),
    hightech_missing = sum(is.na(hightech_export)),
    population_missing = sum(is.na(population)),
    .groups = "drop"
  ) %>%
  arrange(desc(rd_gdp_missing + tertiary_missing + hightech_missing))

print(missing_summary)
print(missing_by_country)

write_csv(panel_patents, file.path(data_dir, "panel_patents_oecd_full.csv"))
write_csv(panel_patents_2007_2021, file.path(data_dir, "panel_patents_oecd_2007_2021.csv"))
write_csv(missing_summary, file.path(data_dir, "missing_summary_2007_2021.csv"))
write_csv(missing_by_country, file.path(data_dir, "missing_by_country_2007_2021.csv"))

#REALNY KOD TERAZ
install.packages(c(
  "tidyverse",
  "fixest",
  "modelsummary",
  "performance"
))

library(tidyverse)
library(fixest)
library(modelsummary)
library(performance)

# ============================================================
# 2. WCZYTANIE DANYCH
# ============================================================

data_dir <- "C:/Users/Norbert/Desktop/studia/modele panelowe/dane"

panel <- read_csv(
  file.path(data_dir, "panel_patents_oecd_2007_2021_CLEAN.csv"),
  show_col_types = FALSE
)

glimpse(panel)

# ============================================================
# 3. PRZYGOTOWANIE DANYCH
# ============================================================

panel <- panel %>%
  mutate(
    iso3c = as.factor(iso3c),
    year = as.factor(year),
    log_population = log(population),
    log_gdp_pc_lag1 = log(gdp_pc_lag1)
  )

# Próba do modelu głównego
model_data_main <- panel %>%
  filter(
    !is.na(patents),
    !is.na(rd_gdp_lag1),
    !is.na(tertiary_enrollment_lag1),
    !is.na(gdp_pc_lag1),
    !is.na(population),
    patents >= 0,
    population > 0
  )

# Próba do modelu rozszerzonego z high-tech export
model_data_hightech <- panel %>%
  filter(
    !is.na(patents),
    !is.na(rd_gdp_lag1),
    !is.na(tertiary_enrollment_lag1),
    !is.na(gdp_pc_lag1),
    !is.na(hightech_export_lag1),
    !is.na(population),
    patents >= 0,
    population > 0
  )

# Kontrola liczebności prób
model_data_main %>%
  summarise(
    n_obs = n(),
    countries = n_distinct(iso3c),
    min_year = min(as.numeric(as.character(year))),
    max_year = max(as.numeric(as.character(year)))
  )

model_data_hightech %>%
  summarise(
    n_obs = n(),
    countries = n_distinct(iso3c),
    min_year = min(as.numeric(as.character(year))),
    max_year = max(as.numeric(as.character(year)))
  )

# ============================================================
# 4. STATYSTYKI OPISOWE
# ============================================================

desc_stats <- model_data_main %>%
  select(
    patents,
    patents_per_million,
    rd_gdp_lag1,
    tertiary_enrollment_lag1,
    log_gdp_pc_lag1,
    gdp_pc_lag1,
    population
  ) %>%
  summarise(
    across(
      everything(),
      list(
        n = ~sum(!is.na(.)),
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
      )
    )
  )

desc_stats

write_csv(desc_stats, file.path(data_dir, "desc_stats_patents.csv"))

# ============================================================
# 5. WYKRES: PATENTY NA MILION MIESZKAŃCÓW
# ============================================================

plot_patents_pm <- model_data_main %>%
  group_by(year) %>%
  summarise(
    patents_per_million_avg = mean(patents_per_million, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = as.numeric(as.character(year)), y = patents_per_million_avg)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Średnia liczba zgłoszeń patentowych na milion mieszkańców w krajach OECD",
    x = "Rok",
    y = "Patenty na milion mieszkańców"
  ) +
  theme_minimal()

plot_patents_pm

ggsave(
  filename = file.path(data_dir, "wykres_patenty_na_milion.png"),
  plot = plot_patents_pm,
  width = 8,
  height = 5,
  dpi = 300
)

# ============================================================
# 6. MODELE POISSONA
# ============================================================

# Model 1: pooled Poisson
model_1_pool_poisson <- fepois(
  patents ~ rd_gdp_lag1 + tertiary_enrollment_lag1 + log_gdp_pc_lag1 +
    offset(log_population),
  data = model_data_main,
  vcov = ~ iso3c
)

# Model 2: FE Poisson z efektami krajów
model_2_fe_country <- fepois(
  patents ~ rd_gdp_lag1 + tertiary_enrollment_lag1 + log_gdp_pc_lag1 +
    offset(log_population) | iso3c,
  data = model_data_main,
  vcov = ~ iso3c
)

# Model 3: FE Poisson z efektami krajów i lat — MODEL GŁÓWNY
model_3_fe_country_year <- fepois(
  patents ~ rd_gdp_lag1 + tertiary_enrollment_lag1 + log_gdp_pc_lag1 +
    offset(log_population) | iso3c + year,
  data = model_data_main,
  vcov = ~ iso3c
)

# Model 4: FE Poisson z high-tech export — model rozszerzony
model_4_hightech <- fepois(
  patents ~ rd_gdp_lag1 + tertiary_enrollment_lag1 + log_gdp_pc_lag1 +
    hightech_export_lag1 +
    offset(log_population) | iso3c + year,
  data = model_data_hightech,
  vcov = ~ iso3c
)

# ============================================================
# 7. WYNIKI W KONSOLI
# ============================================================

etable(
  model_1_pool_poisson,
  model_2_fe_country,
  model_3_fe_country_year,
  model_4_hightech,
  se.below = TRUE,
  fitstat = ~ n + ll + aic + bic
)

# ============================================================
# 8. TABELA MODELI DO HTML
# ============================================================

models_list <- list(
  "Pooled Poisson" = model_1_pool_poisson,
  "FE: kraje" = model_2_fe_country,
  "FE: kraje i lata" = model_3_fe_country_year,
  "FE + high-tech" = model_4_hightech
)

modelsummary(
  models_list,
  output = file.path(data_dir, "model_results_patents.html"),
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "IC|Log|Adj|Pseudo|Within|FE",
  title = "Tabela. Wyniki estymacji modeli Poissona dla liczby zgłoszeń patentowych"
)

# ============================================================
# 9. INTERPRETACJA PROCENTOWA — MODEL GŁÓWNY
# ============================================================

main_effects <- broom::tidy(model_3_fe_country_year) %>%
  filter(term %in% c(
    "rd_gdp_lag1",
    "tertiary_enrollment_lag1",
    "log_gdp_pc_lag1"
  )) %>%
  mutate(
    percent_effect = 100 * (exp(estimate) - 1)
  )

main_effects

write_csv(
  main_effects,
  file.path(data_dir, "main_model_percent_effects.csv")
)

# ============================================================
# 10. NADMIERNA DYSPERSJA — PROSTA DIAGNOSTYKA
# ============================================================

# Dla modelu pooled można użyć check_overdispersion
check_overdispersion(model_1_pool_poisson)

# Dla modelu FE zrobimy prosty wskaźnik:
pearson_resid <- residuals(model_3_fe_country_year, type = "pearson")
df_resid <- nobs(model_3_fe_country_year) - length(coef(model_3_fe_country_year))

dispersion_ratio <- sum(pearson_resid^2, na.rm = TRUE) / df_resid

dispersion_ratio

# ============================================================
# 11. ZAPIS PRÓBEK MODELOWYCH
# ============================================================

write_csv(
  model_data_main,
  file.path(data_dir, "model_data_main_patents.csv")
)

write_csv(
  model_data_hightech,
  file.path(data_dir, "model_data_hightech_patents.csv")
)

#Negative binomial
install.packages(c("MASS", "sandwich", "lmtest", "modelsummary", "broom"))


library(MASS)
library(sandwich)
library(lmtest)
library(modelsummary)
library(broom)

# ============================================================
# 12. MODELE NEGATIVE BINOMIAL
# ============================================================

# Model NB 1: pooled negative binomial
model_nb_1_pool <- glm.nb(
  patents ~ rd_gdp_lag1 + tertiary_enrollment_lag1 + log_gdp_pc_lag1 +
    offset(log_population),
  data = model_data_main,
  control = glm.control(maxit = 100)
)

# Model NB 2: negative binomial z efektami krajów
model_nb_2_country <- glm.nb(
  patents ~ rd_gdp_lag1 + tertiary_enrollment_lag1 + log_gdp_pc_lag1 +
    factor(iso3c) +
    offset(log_population),
  data = model_data_main,
  control = glm.control(maxit = 100)
)

# Model NB 3: negative binomial z efektami krajów i lat
model_nb_3_country_year <- glm.nb(
  patents ~ rd_gdp_lag1 + tertiary_enrollment_lag1 + log_gdp_pc_lag1 +
    factor(iso3c) + factor(year) +
    offset(log_population),
  data = model_data_main,
  control = glm.control(maxit = 100)
)

# ============================================================
# 13. NB: WYNIKI Z KLASTROWANYMI BŁĘDAMI
# ============================================================

nb_1_results <- coeftest(
  model_nb_1_pool,
  vcov = vcovCL(model_nb_1_pool, cluster = ~ iso3c, data = model_data_main, type = "HC1")
)

nb_2_results <- coeftest(
  model_nb_2_country,
  vcov = vcovCL(model_nb_2_country, cluster = ~ iso3c, data = model_data_main, type = "HC1")
)

nb_3_results <- coeftest(
  model_nb_3_country_year,
  vcov = vcovCL(model_nb_3_country_year, cluster = ~ iso3c, data = model_data_main, type = "HC1")
)

nb_1_results
nb_2_results
nb_3_results

# ============================================================
# 14. TABELA MODELI NEGATIVE BINOMIAL
# ============================================================

nb_models <- list(
  "NB pooled" = model_nb_1_pool,
  "NB: kraje" = model_nb_2_country,
  "NB: kraje i lata" = model_nb_3_country_year
)

nb_vcov <- list(
  vcovCL(model_nb_1_pool, cluster = model_data_main$iso3c, type = "HC1"),
  vcovCL(model_nb_2_country, cluster = model_data_main$iso3c, type = "HC1"),
  vcovCL(model_nb_3_country_year, cluster = model_data_main$iso3c, type = "HC1")
)

modelsummary(
  nb_models,
  vcov = nb_vcov,
  output = file.path(data_dir, "model_results_negative_binomial.html"),
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "Log|Adj|Pseudo|FE",
  title = "Tabela. Wyniki estymacji modeli ujemnego dwumianowego dla liczby zgłoszeń patentowych"
)

# ============================================================
# 15. PORÓWNANIE EFEKTÓW: POISSON FE VS NB FE
# ============================================================

# Funkcja do wyciągania efektów procentowych
extract_percent_effects_nb <- function(model, model_name) {
  
  tidy(model) %>%
    filter(term %in% c(
      "rd_gdp_lag1",
      "tertiary_enrollment_lag1",
      "log_gdp_pc_lag1"
    )) %>%
    mutate(
      model = model_name,
      percent_effect = 100 * (exp(estimate) - 1)
    ) %>%
    select(model, term, estimate, percent_effect, std.error, statistic, p.value)
}

nb_percent_effects <- bind_rows(
  extract_percent_effects_nb(model_nb_1_pool, "NB pooled"),
  extract_percent_effects_nb(model_nb_2_country, "NB country FE"),
  extract_percent_effects_nb(model_nb_3_country_year, "NB country + year FE")
)

nb_percent_effects

write_csv(
  nb_percent_effects,
  file.path(data_dir, "negative_binomial_percent_effects.csv")
)

# ============================================================
# 16. PARAMETR THETA W MODELU NB
# ============================================================

theta_results <- tibble(
  model = c("NB pooled", "NB country FE", "NB country + year FE"),
  theta = c(
    model_nb_1_pool$theta,
    model_nb_2_country$theta,
    model_nb_3_country_year$theta
  ),
  se_theta = c(
    model_nb_1_pool$SE.theta,
    model_nb_2_country$SE.theta,
    model_nb_3_country_year$SE.theta
  )
)

theta_results

write_csv(
  theta_results,
  file.path(data_dir, "negative_binomial_theta.csv")
)

# ============================================================
# TESTY ISTOTNOŚCI EFEKTÓW STAŁYCH W MODELU POISSONA
# ============================================================

library(lmtest)

# Model pooled Poisson
glm_poisson_pool <- glm(
  patents ~ rd_gdp_lag1 + tertiary_enrollment_lag1 + log_gdp_pc_lag1 +
    offset(log_population),
  family = poisson(link = "log"),
  data = model_data_main
)

# Poisson z efektami krajów
glm_poisson_country <- glm(
  patents ~ rd_gdp_lag1 + tertiary_enrollment_lag1 + log_gdp_pc_lag1 +
    factor(iso3c) +
    offset(log_population),
  family = poisson(link = "log"),
  data = model_data_main
)

# Poisson z efektami krajów i lat
glm_poisson_country_year <- glm(
  patents ~ rd_gdp_lag1 + tertiary_enrollment_lag1 + log_gdp_pc_lag1 +
    factor(iso3c) + factor(year) +
    offset(log_population),
  family = poisson(link = "log"),
  data = model_data_main
)

# 1. Czy efekty krajów są istotne?
lrtest(glm_poisson_pool, glm_poisson_country)

# 2. Czy efekty lat są istotne po uwzględnieniu efektów krajów?
lrtest(glm_poisson_country, glm_poisson_country_year)

# 3. Czy model z efektami krajów i lat jest lepszy od pooled?
lrtest(glm_poisson_pool, glm_poisson_country_year)