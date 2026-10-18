# ============================================================
# PROJEKT: ZAAWANSOWANE MODELE PANELOWE
# Determinanty wartości rynkowej zawodników Premier League
# ============================================================


# ============================================================
# 1. PAKIETY
# ============================================================

library(readxl)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(car)
library(modelsummary)
library(broom)
library(pandoc)


# ============================================================
# 2. WCZYTANIE DANYCH
# ============================================================

data_raw <- read_excel(
  "C:\\Users\\Norbert\\Downloads\\premier_league_panel_450_FINAL_complete.xlsx",
  sheet = "Panel_450_FINAL"
)


# ============================================================
# 3. PRZYGOTOWANIE DANYCH
# ============================================================

data <- data_raw %>%
  mutate(
    player_id = as.factor(player_id),
    season_label = as.factor(season_label),
    squad_main = as.factor(squad_main),
    pos_clean = as.factor(pos_clean),
    
    UCL = as.numeric(UCL),
    UEL = as.numeric(UEL),
    UECL = as.numeric(UECL),
    offensive_player = as.numeric(offensive_player),
    
    log_player_market_value = as.numeric(log_player_market_value),
    log_club_market_value_excl_player = as.numeric(log_club_market_value_excl_player),
    
    Min_clean = as.numeric(Min_clean),
    Min_1000 = as.numeric(Min_1000),
    Gls = as.numeric(Gls),
    Ast = as.numeric(Ast),
    CrdY = as.numeric(CrdY),
    CrdR = as.numeric(CrdR),
    age = as.numeric(age),
    age_sq = as.numeric(age_sq),
    fifa_points = as.numeric(fifa_points),
    fifa_ranking = as.numeric(fifa_ranking)
  ) %>%
  filter(
    !is.na(log_player_market_value),
    !is.na(log_club_market_value_excl_player),
    !is.na(Min_1000),
    !is.na(Gls),
    !is.na(Ast),
    !is.na(age),
    !is.na(age_sq),
    !is.na(fifa_points)
  )


# ============================================================
# 4. OPIS STRUKTURY DANYCH
# ============================================================

# Liczba obserwacji
nrow(data)

# Liczba zawodników
length(unique(data$player_id))

# Liczba obserwacji w sezonach
table(data$season_label)

# Struktura panelu: w ilu sezonach występuje dany zawodnik
panel_structure <- data %>%
  group_by(player_id) %>%
  summarise(
    liczba_sezonow = n_distinct(season_label),
    .groups = "drop"
  ) %>%
  count(liczba_sezonow, name = "liczba_zawodnikow") %>%
  mutate(
    liczba_obserwacji = liczba_sezonow * liczba_zawodnikow,
    odsetek_zawodnikow = round(100 * liczba_zawodnikow / sum(liczba_zawodnikow), 1)
  )

panel_structure

# Suma kontrolna
sum(panel_structure$liczba_zawodnikow)
sum(panel_structure$liczba_obserwacji)


# ============================================================
# 5. STRUKTURA PANELOWA
# ============================================================

pdata <- pdata.frame(
  data,
  index = c("player_id", "season_label")
)

pdim(pdata)


# ============================================================
# 6. GŁÓWNA SPECYFIKACJA MODELU
# ============================================================

formula_main <- log_player_market_value ~
  Min_1000 +
  Gls +
  Ast +
  CrdY +
  CrdR +
  age +
  age_sq +
  log_club_market_value_excl_player +
  fifa_points +
  UCL +
  UEL +
  UECL +
  offensive_player +
  season_label


formula_no_time <- log_player_market_value ~
  Min_1000 +
  Gls +
  Ast +
  CrdY +
  CrdR +
  age +
  age_sq +
  log_club_market_value_excl_player +
  fifa_points +
  UCL +
  UEL +
  UECL +
  offensive_player


# ============================================================
# 7. POOLED OLS
# ============================================================

model_pool <- plm(
  formula_main,
  data = pdata,
  model = "pooling"
)

summary(model_pool)

pool_cluster <- coeftest(
  model_pool,
  vcov = vcovHC(model_pool, type = "HC1", cluster = "group")
)

pool_cluster


# ============================================================
# 8. RANDOM EFFECTS
# ============================================================

model_re <- plm(
  formula_main,
  data = pdata,
  model = "random",
  random.method = "swar"
)

summary(model_re)

re_cluster <- coeftest(
  model_re,
  vcov = vcovHC(model_re, type = "HC1", cluster = "group")
)

re_cluster


# ============================================================
# 9. FIXED EFFECTS
# ============================================================

model_fe <- plm(
  formula_main,
  data = pdata,
  model = "within",
  effect = "individual"
)

summary(model_fe)

fe_cluster <- coeftest(
  model_fe,
  vcov = vcovHC(model_fe, type = "HC1", cluster = "group")
)

fe_cluster


# ============================================================
# 10. TESTY WYBORU MODELU
# ============================================================

# FE vs Pooled OLS
# H0: brak efektów indywidualnych, pooled OLS wystarczy.
pFtest(model_fe, model_pool)

# RE vs Pooled OLS
# H0: brak efektów losowych, pooled OLS wystarczy.
plmtest(model_pool, type = "bp")

# Hausman: FE vs RE
# H0: RE jest zgodny.
phtest(model_fe, model_re)


# ============================================================
# 11. TEST EFEKTÓW SEZONOWYCH
# ============================================================

model_fe_no_time <- plm(
  formula_no_time,
  data = pdata,
  model = "within",
  effect = "individual"
)

# H0: efekty sezonowe nie są potrzebne.
pFtest(model_fe, model_fe_no_time)


# ============================================================
# 12. MUNDLAK / CRE
# ============================================================

data_cre <- data %>%
  group_by(player_id) %>%
  mutate(
    mean_Min_1000 = mean(Min_1000, na.rm = TRUE),
    mean_Gls = mean(Gls, na.rm = TRUE),
    mean_Ast = mean(Ast, na.rm = TRUE),
    mean_CrdY = mean(CrdY, na.rm = TRUE),
    mean_CrdR = mean(CrdR, na.rm = TRUE),
    mean_age = mean(age, na.rm = TRUE),
    mean_age_sq = mean(age_sq, na.rm = TRUE),
    mean_log_club_value = mean(log_club_market_value_excl_player, na.rm = TRUE),
    mean_fifa_points = mean(fifa_points, na.rm = TRUE),
    mean_UCL = mean(UCL, na.rm = TRUE),
    mean_UEL = mean(UEL, na.rm = TRUE),
    mean_UECL = mean(UECL, na.rm = TRUE)
  ) %>%
  ungroup()

pdata_cre <- pdata.frame(
  data_cre,
  index = c("player_id", "season_label")
)

formula_cre <- log_player_market_value ~
  Min_1000 +
  Gls +
  Ast +
  CrdY +
  CrdR +
  age +
  age_sq +
  log_club_market_value_excl_player +
  fifa_points +
  UCL +
  UEL +
  UECL +
  offensive_player +
  season_label +
  mean_Min_1000 +
  mean_Gls +
  mean_Ast +
  mean_CrdY +
  mean_CrdR +
  mean_age +
  mean_age_sq +
  mean_log_club_value +
  mean_fifa_points +
  mean_UCL +
  mean_UEL +
  mean_UECL


# Używamy Wallace-Hussain, bo Swamy-Arora robił problem osobliwości.
model_cre <- plm(
  formula_cre,
  data = pdata_cre,
  model = "random",
  random.method = "walhus"
)

summary(model_cre)

cre_cluster <- coeftest(
  model_cre,
  vcov = vcovHC(model_cre, type = "HC1", cluster = "group")
)

cre_cluster


# Test Mundlaka: łączna istotność średnich indywidualnych
# H0: średnie indywidualne są łącznie nieistotne.
linearHypothesis(
  model_cre,
  c(
    "mean_Min_1000 = 0",
    "mean_Gls = 0",
    "mean_Ast = 0",
    "mean_CrdY = 0",
    "mean_CrdR = 0",
    "mean_age = 0",
    "mean_age_sq = 0",
    "mean_log_club_value = 0",
    "mean_fifa_points = 0",
    "mean_UCL = 0",
    "mean_UEL = 0",
    "mean_UECL = 0"
  ),
  vcov. = vcovHC(model_cre, type = "HC1", cluster = "group")
)


# ============================================================
# 13. DIAGNOSTYKA
# ============================================================

# Heteroskedastyczność
bptest(model_pool)
bptest(model_fe)
bptest(model_re)

# Autokorelacja
pbgtest(model_fe)
pwartest(model_fe)

# Zależność przekrojowa
pcdtest(model_fe, test = "cd")


# ============================================================
# 14. VIF
# ============================================================

# VIF na zwykłym lm, bo car::vif nie działa dobrze bezpośrednio na plm.
# Przy age i age_sq VIF może być wysoki, bo kwadrat wieku jest naturalnie
# silnie powiązany z wiekiem. Zostawiamy jednak age i age_sq, żeby zachować
# prostą i zgodną z literaturą specyfikację wieku oraz wieku do kwadratu.
model_lm_vif <- lm(
  formula_main,
  data = data
)

vif(model_lm_vif)


# ============================================================
# 15. TABELA GŁÓWNYCH WYNIKÓW
# ============================================================

models <- list(
  "Pooled OLS" = model_pool,
  "RE" = model_re,
  "FE" = model_fe,
  "CRE/Mundlak" = model_cre
)

vcovs <- list(
  vcovHC(model_pool, type = "HC1", cluster = "group"),
  vcovHC(model_re, type = "HC1", cluster = "group"),
  vcovHC(model_fe, type = "HC1", cluster = "group"),
  vcovHC(model_cre, type = "HC1", cluster = "group")
)

modelsummary(
  models,
  vcov = vcovs,
  stars = TRUE,
  output = "model_results_age.html"
)


# ============================================================
# 16. ROBUSTNESS: PRÓG 900 MINUT
# ============================================================

data_900 <- data %>%
  filter(Min_clean >= 900)

nrow(data_900)
length(unique(data_900$player_id))
table(data_900$season_label)

pdata_900 <- pdata.frame(
  data_900,
  index = c("player_id", "season_label")
)

model_fe_900 <- plm(
  formula_main,
  data = pdata_900,
  model = "within",
  effect = "individual"
)

summary(model_fe_900)

fe_900_cluster <- coeftest(
  model_fe_900,
  vcov = vcovHC(model_fe_900, type = "HC1", cluster = "group")
)

fe_900_cluster


# Tabela robustness
modelsummary(
  list(
    "FE >= 450 min" = model_fe,
    "FE >= 900 min" = model_fe_900
  ),
  vcov = list(
    vcovHC(model_fe, type = "HC1", cluster = "group"),
    vcovHC(model_fe_900, type = "HC1", cluster = "group")
  ),
  stars = TRUE,
  output = "robustness_900_age.html"
)


# ============================================================
# 17. OPCJONALNIE: MODEL BEZ BRAMKARZY
# ============================================================

data_outfield <- data %>%
  filter(pos_clean != "GK")

pdata_outfield <- pdata.frame(
  data_outfield,
  index = c("player_id", "season_label")
)

model_fe_outfield <- plm(
  formula_main,
  data = pdata_outfield,
  model = "within",
  effect = "individual"
)

summary(model_fe_outfield)

fe_outfield_cluster <- coeftest(
  model_fe_outfield,
  vcov = vcovHC(model_fe_outfield, type = "HC1", cluster = "group")
)

fe_outfield_cluster

modelsummary(
  list(
    "FE all players" = model_fe,
    "FE outfield only" = model_fe_outfield
  ),
  vcov = list(
    vcovHC(model_fe, type = "HC1", cluster = "group"),
    vcovHC(model_fe_outfield, type = "HC1", cluster = "group")
  ),
  stars = TRUE,
  output = "robustness_outfield_age.html"
)


# ============================================================
# 18. GDZIE SĄ PLIKI?
# ============================================================

getwd()

summary(model_pool)
summary(re_cluster)
re_cluster


mundlak_terms <- c(
  "mean_Min_1000 = 0",
  "mean_Gls = 0",
  "mean_Ast = 0",
  "mean_CrdY = 0",
  "mean_CrdR = 0",
  "mean_age = 0",
  "mean_age_sq = 0",
  "mean_log_club_market_value_excl_player = 0",
  "mean_fifa_points = 0",
  "mean_UCL = 0",
  "mean_UEL = 0",
  "mean_UECL = 0"
)

# ============================================================
# TEST MUNDLAKA
# ============================================================
library(car)

# Sprawdzenie nazw współczynników w modelu Mundlaka
names(coef(model_cre))

# Automatyczne wybranie wszystkich średnich indywidualnych z modelu Mundlaka
mundlak_coef_names <- names(coef(model_cre))[grepl("^mean_", names(coef(model_cre)))]

mundlak_coef_names

# Macierz hipotez: wszystkie średnie indywidualne = 0
L_mundlak <- matrix(
  0,
  nrow = length(mundlak_coef_names),
  ncol = length(coef(model_cre))
)

colnames(L_mundlak) <- names(coef(model_cre))
rownames(L_mundlak) <- mundlak_coef_names

for (i in seq_along(mundlak_coef_names)) {
  L_mundlak[i, mundlak_coef_names[i]] <- 1
}

# Test łącznej istotności średnich indywidualnych
linearHypothesis(
  model_cre,
  hypothesis.matrix = L_mundlak,
  rhs = rep(0, length(mundlak_coef_names)),
  vcov = vcovHC(model_cre, type = "HC1", cluster = "group")
)