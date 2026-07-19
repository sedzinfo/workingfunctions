library(ggplot2)
library(dplyr)
library(maps)
library(jsonlite)

# ── download ──────────────────────────────────────────────────────────────────
url <- paste0(
  "https://data.cdc.gov/resource/bi63-dtpu.json",
  "?cause_name=Unintentional%20injuries",
  "&$where=year%3C%3D2020",
  "&$limit=50000"
)
df_raw <- jsonlite::fromJSON(url)

# ── clean ─────────────────────────────────────────────────────────────────────
df <- df_raw %>%
  mutate(
    year        = as.integer(year),
    AADR        = as.numeric(aadr),
    state_lower = tolower(state)
  ) %>%
  filter(!is.na(AADR), state != "United States")

# ── map ───────────────────────────────────────────────────────────────────────
states_map <- map_data("state")

# ── plot ──────────────────────────────────────────────────────────────────────
ggplot(df, aes(map_id = state_lower)) +
  geom_map(aes(fill = AADR), map = states_map,
           color = "white", linewidth = 0.15) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  facet_wrap(~ year, ncol = 4) +
  scale_fill_gradient(low = "#cfe2f3", high = "#1a3a5c", name = "AADR") +
  coord_fixed(1.3) +
  labs(title = "AADR Accidents (unintentional injuries) (V01-X59,Y85-Y86)") +
  theme_bw(base_size = 11) +
  theme(
    axis.text       = element_text(size = 7),
    strip.text      = element_text(size = 8),
    plot.title      = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )