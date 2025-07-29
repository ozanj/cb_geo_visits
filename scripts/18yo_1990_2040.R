# -------------------------------------------------------------
# 18-year-old population by race / Hispanic origin, 1990-2040
# -------------------------------------------------------------
library(tidyverse)      # dplyr, tidyr, ggplot2 …
library(readxl)         # read_xlsx()
library(scales)         # comma()

# ---------- 1.  1990-2024 POPULATION ESTIMATES ---------------
# ---- 1a. helper that downloads one ASR6H workbook and returns a tidy tibble
read_asr6h <- function(url, years_expected) {
  tmp <- tempfile(fileext = ".xlsx")
  download.file(url, tmp, mode = "wb")
  
  read_xlsx(tmp, skip = 3, col_types = "text") |>   # skip title rows
    filter(AGE == "18", SEX == "0") |>              # both sexes, age 18
    transmute(
      year = as.integer(YEAR),
      hisp = HISP,                                  # 1 = Hisp, 0 = Not-Hisp
      race = RACE,
      pop  = as.integer(POPESTIMATE)
    ) |>
    filter(year %in% years_expected)
}

est_10_19 <- read_asr6h(
  "https://www2.census.gov/programs-surveys/popest/tables/2010-2019/national/asrh/nc-est2019-asr6h.xlsx",
  2010:2019)   # :contentReference[oaicite:0]{index=0}

est_20_24 <- read_asr6h(
  "https://www2.census.gov/programs-surveys/popest/tables/2020-2024/national/asrh/nc-est2024-asr6h.xlsx",
  2020:2024)   # released with Vintage 2024 :contentReference[oaicite:1]{index=1}

# ---- 1b.  1990-2009 intercensal estimates via the API --------
# (The intercensal ASR6H files aren’t posted as xlsx, so we call the API.)
library(censusapi)
# ->  get a key at https://api.census.gov/data/key_signup.html  then:
Sys.setenv(CENSUS_KEY = "aff360d1fe8a919619776f48e975f03b8bb1379e")
#census_api_key('aff360d1fe8a919619776f48e975f03b8bb1379e', install = TRUE)
Sys.getenv("CENSUS_API_KEY") # retreive API key

years_90_09 <- 1990:2009
est_90_09 <- map_dfr(
  years_90_09,
  ~ getCensus(
    name   = "timeseries/pep/charagegroups",
    vintage = 2024,                    # latest vintage; returns 1980-2024
    vars   = c("POP", "ORIGIN", "RACE"),
    region = "us:*",
    AGE    = "18", SEX = "0", time = .x
  ) |>
    mutate(year = as.integer(time)) |>
    select(year, hisp = ORIGIN, race = RACE, pop = POP)
)

# ---------- 2.  2025-2040 POPULATION PROJECTIONS --------------
proj <- read_csv(
  "https://www2.census.gov/programs-surveys/popproj/datasets/2023/2023-popproj/np2023_d1_mid.csv",
  col_types = cols(.default = "c")
) |>
  filter(AGE == "18", SEX == "0", YEAR %in% as.character(2025:2040)) |>
  transmute(
    year = as.integer(YEAR),
    hisp = ORIGIN,
    race = RACE,
    pop  = as.integer(POP)
  )

# ---------- 3.  COMBINE + RECODE INTO 7 RACE/ETHNIC GROUPS ----
cats <- tribble(
  ~hisp, ~race, ~group,
  "1",   "0",   "Hispanic (any race)",
  "0",   "1",   "White, NH",
  "0",   "2",   "Black, NH",
  "0",   "3",   "AI/AN, NH",
  "0",   "4",   "Asian, NH",
  "0",   "5",   "NHPI, NH",
  "0",   "6",   "Two+ races, NH"
)

pop_long <- bind_rows(est_90_09, est_10_19, est_20_24, proj) |>
  inner_join(cats, by = c("hisp", "race")) |>
  group_by(year, group) |>
  summarise(pop = sum(pop), .groups = "drop")

# ---------- 4.  PLOT ------------------------------------------
ggplot(pop_long, aes(year, pop, fill = group)) +
  geom_col(width = 0.9) +
  scale_x_continuous(breaks = seq(1990, 2040, 5)) +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(type = "qual", palette = "Paired", name = NULL) +
  labs(title = "U.S. 18-year-olds by race/ethnicity, 1990 – 2040",
       subtitle = "Blue = estimates, Orange = projections (2025+)",
       x = NULL, y = "Population") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
