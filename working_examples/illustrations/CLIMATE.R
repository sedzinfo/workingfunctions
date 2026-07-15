##########################################################################################
# SEA ICE
##########################################################################################
library(sf)
library(ggplot2)
library(rnaturalearth)
library(gifski)

sf::sf_use_s2(FALSE)

# ── helpers ────────────────────────────────────────────────────────────────────
month_dirs <- c("01_Jan","02_Feb","03_Mar","04_Apr","05_May","06_Jun",
                "07_Jul","08_Aug","09_Sep","10_Oct","11_Nov","12_Dec")

fetch_ice_extent <- function(year, month = "09", pole = "N",
                             tmpdir = file.path(tempdir(), "nsidc_ice")) {
  dir.create(tmpdir, showWarnings = FALSE)
  mm       <- sprintf("%02d", as.integer(month))
  yymm     <- paste0(year, mm)
  pole_dir <- ifelse(pole == "N", "north", "south")
  mon_dir  <- month_dirs[as.integer(mm)]
  url <- paste0(
    "https://noaadata.apps.nsidc.org/NOAA/G02135/",
    pole_dir, "/monthly/shapefiles/shp_extent/", mon_dir, "/",
    "extent_", pole, "_", yymm, "_polygon_v4.0.zip"
  )
  zip_path <- file.path(tmpdir, paste0("ice_", pole, "_", yymm, ".zip"))
  if (!file.exists(zip_path) || file.size(zip_path) < 1000)
    tryCatch(download.file(url, zip_path, quiet = TRUE, mode = "wb"),
             error = function(e) NULL)
  if (!file.exists(zip_path) || file.size(zip_path) < 1000) return(NULL)
  exdir <- file.path(tmpdir, paste0("ice_", pole, "_", yymm))
  suppressWarnings(unzip(zip_path, exdir = exdir, overwrite = FALSE))
  shp <- list.files(exdir, "\\.shp$", full.names = TRUE)[1]
  if (is.na(shp)) return(NULL)
  tryCatch(sf::st_read(shp, quiet = TRUE), error = function(e) NULL)
}

# ── base layers (built once) ───────────────────────────────────────────────────
crs_polar <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=0 +datum=WGS84 +units=m"

world_proj <- sf::st_transform(
  ne_countries(scale = "medium", returnclass = "sf"), crs_polar
)
clip_circle <- sf::st_buffer(
  sf::st_sfc(sf::st_point(c(0, 90)), crs = 4326) |> sf::st_transform(crs_polar),
  dist = 5.5e6
)
world_clip <- sf::st_intersection(sf::st_make_valid(world_proj), clip_circle)

# helper: project + clip a single raw shapefile
clip_year <- function(shp) {
  proj <- sf::st_transform(shp, crs_polar) |> sf::st_make_valid()
  tryCatch(sf::st_intersection(proj, clip_circle), error = function(e) NULL)
}

# ── 1979 reference (red outline on every frame) ────────────────────────────────
raw_1979  <- fetch_ice_extent(1979)
ice_1979  <- if (!is.null(raw_1979)) clip_year(raw_1979) else NULL

# ── real extent values from NSIDC CSV ─────────────────────────────────────────
areas_csv <- read.csv(
  "https://noaadata.apps.nsidc.org/NOAA/G02135/north/monthly/data/N_09_extent_v4.0.csv",
  strip.white = TRUE
)

# ── frame loop ─────────────────────────────────────────────────────────────────
years_all  <- 1979:2024
frames_dir <- file.path(tempdir(), "arctic_frames")
dir.create(frames_dir, showWarnings = FALSE)

plots <- list()

for (yr in years_all) {
  raw <- fetch_ice_extent(yr)
  if (is.null(raw)) { message("skip ", yr); next }
  
  ice_yr <- clip_year(raw)
  if (is.null(ice_yr) || nrow(ice_yr) == 0) { message("skip ", yr, " (clip failed)"); next }
  
  area <- areas_csv$extent[trimws(as.character(areas_csv$year)) == as.character(yr)]
  area_lbl <- if (length(area) && !is.na(area[1])) sprintf("%.2f M km²", area[1]) else ""
  
  p <- ggplot() +
    geom_sf(data = clip_circle, fill = "#1a3a5c", color = NA) +
    { if (!is.null(ice_1979))
      geom_sf(data = ice_1979, fill = NA, color = "#FF5252",
              linewidth = 0.9, alpha = 0.75) } +
    geom_sf(data = ice_yr,     fill = "#E3F4FF", color = "#90CAF9", linewidth = 0.1) +
    geom_sf(data = world_clip, fill = "#5D4037", color = "#3E2723", linewidth = 0.2) +
    labs(
      title    = paste("Arctic Sea Ice — September", yr),
      subtitle = paste0(area_lbl, "   |   red outline = 1979 extent"),
      caption  = "Source: NSIDC Sea Ice Index v4.0"
    ) +
    theme_void(base_size = 13) +
    theme(
      plot.background = element_rect(fill = "#0D1B2A", color = NA),
      plot.title      = element_text(color = "white",  face = "bold", size = 18,
                                     hjust = 0.5, margin = margin(14, 0, 4, 0)),
      plot.subtitle   = element_text(color = "#90CAF9", size = 10,
                                     hjust = 0.5, margin = margin(0, 0, 8, 0)),
      plot.caption    = element_text(color = "#546E7A", size = 7.5,
                                     hjust = 1,  margin = margin(6, 10, 8, 0)),
      plot.margin     = margin(10, 10, 10, 10)
    )
  
  plots[[as.character(yr)]] <- p
  message("frame ", yr, " ready (", length(plots), " so far)")
}

# ── save each frame as PNG ─────────────────────────────────────────────────────
message("Saving PNGs...")
frame_paths <- character(length(plots))
for (i in seq_along(plots)) {
  path <- file.path(frames_dir, sprintf("frame_%04d.png", i))
  ggsave(path, plots[[i]], width = 2000/120, height = 2000/120,
         dpi = 120, bg = "#0D1B2A")
  frame_paths[i] <- path
}

# ── stitch into GIF ────────────────────────────────────────────────────────────
message("Stitching GIF...")
gifski::gifski(frame_paths, gif_file = "arctic_ice_animated.gif",
               width = 2000, height = 2000, delay = 1)
message("Done: arctic_ice_animated.gif")
##########################################################################################
# 
##########################################################################################
# https://github.com/jbkunst/jbkunst.github.io/blob/master/_posts/2016-06-23-case-study-animation-and-others-vizs.md
library(highcharter)
library(rwf)
df_nasa<-read.csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",header=TRUE,stringsAsFactors=FALSE,na.strings="***",skip=1)
dfm<-df_nasa<-df_nasa[,c("Year","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")]
dfma<-data.frame(Year=df_nasa$Year,
                 lower=apply(dfm[,2:length(dfm)],1,min,na.rm=TRUE),
                 upper=apply(dfm[,2:length(dfm)],1,max,na.rm=TRUE),
                 decade=paste0(substr(df_nasa$Year,1,3),0))
dfr<-reshape::melt(df_nasa,id.vars="Year",variable_name="month")
df_nasa<-merge(dfr,dfma,sort=FALSE)
df_nasa$month<-as.character(df_nasa$month)
df_nasa$year_mon<-paste0(df_nasa$Year,"-",stringr::str_pad(sapply(df_nasa$month,function(x) grep(paste("(?i)",x,sep=""),month.abb)),2, pad = "0"),"-01")
names(df_nasa)<-tolower(names(df_nasa))
df_nasa<-df_nasa[,c("year_mon","value","lower","upper","year","decade","month")]
df_nasa$decade<-as.numeric(df_nasa$decade)
df<-df_nasa[order(df_nasa$year_mon),]
# df<-read.csv("https://raw.githubusercontent.com/hrbrmstr/hadcrut/master/data/temps.csv")
##########################################################################################
# 
##########################################################################################
df<-dplyr::mutate(df,date=lubridate::ymd(year_mon),
                  tmpstmp=datetime_to_timestamp(date),
                  year=lubridate::year(date),
                  month=lubridate::month(date,label=TRUE),
                  unite=colorize(value,viridis::viridis(10,option="B")),
                  unite=hex_to_rgba(unite,0.65))
dfcolyrs<-df %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(value=median(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(color_y=colorize(value,viridis::viridis(10,option="B")),color_y=hex_to_rgba(color_y,0.65)) %>%
  dplyr::select(-value)
df<-dplyr::left_join(df,dfcolyrs,by="year")
lsseries<-df %>%
  dplyr::group_by(year) %>%
  dplyr::do(data=.$value,color=dplyr::first(.$color_y)) %>%
  dplyr::mutate(name=year) %>%
  list_parse()
hc1<-highchart() %>%
  hc_chart(polar=TRUE) %>%
  hc_plotOptions(series=list(marker=list(enabled=FALSE),animation=TRUE,pointIntervalUnit="month")) %>%
  hc_legend(enabled=FALSE) %>%
  hc_xAxis(type="datetime",labels=list(format="{value:%B}")) %>%
  hc_tooltip(headerFormat="{point.key}",xDateFormat="%B",pointFormat="{series.name}: {point.y}") %>%
  hc_add_series_list(lsseries)
hc1
##########################################################################################
# 
##########################################################################################
lsseries2<-df %>%
  dplyr::group_by(year) %>%
  dplyr::do(data=.$value,color="transparent",enableMouseTracking=FALSE,color2=dplyr::first(.$color_y)) %>%
  dplyr::mutate(name=year) %>%
  list_parse()
hc11<-highchart() %>%
  hc_chart(polar=TRUE) %>%
  hc_plotOptions(series=list(marker=list(enabled=FALSE),animation=TRUE,pointIntervalUnit="month")) %>%
  hc_legend(enabled=FALSE) %>%
  hc_title(text="Animated Spiral") %>%
  hc_xAxis(type="datetime",min=0,max=365*24*36e5,labels=list(format="{value:%B}")) %>%
  hc_tooltip(headerFormat="{point.key}",xDateFormat="%B",pointFormat=" {series.name}: {point.y}") %>%
  hc_add_series_list(lsseries2) %>%
  hc_chart(events=list(load=JS("function() {
  console.log('ready');
  var duration=16*1000;
  var delta=duration/this.series.length;
  var delay=500;
  this.series.map(function(e){
    setTimeout(function() {e.update({color:e.options.color2,enableMouseTracking:true});e.chart.setTitle({text:e.name})
    },delay)
    delay=delay + delta;
  });
}")))
hc11
##########################################################################################
# 
##########################################################################################
hc2<-hc1%>%
  hc_chart(polar=FALSE,type="spline") %>%
  hc_xAxis(max=(365-1)*24*36e5) %>%
  hc_yAxis(tickPositions=c(-1.5,0,1.5))
hc2
##########################################################################################
# 
##########################################################################################
m<-df%>%
  dplyr::select(year,month,value) %>%
  tidyr::spread(year,value) %>%
  dplyr::select(-month) %>%
  as.matrix()
rownames(m)<-month.abb
m<-remove_nc(m,value=-1)
hc3<-hchart(m) %>%
  hc_colorAxis(stops=color_stops(10,viridis::viridis(10,option="B")),min=-1,max=1) %>%
  hc_yAxis(title=list(text=NULL),tickPositions=FALSE,labels=list(format="{value}",useHTML=TRUE))
hc3
##########################################################################################
# 
##########################################################################################
dsts<-df%>%
  dplyr::mutate(name=paste(decade,month)) %>%
  dplyr::select(x=tmpstmp,y=value,name)
hc4<-highchart() %>%
  hc_xAxis(type="datetime") %>%
  hc_yAxis(tickPositions=c(-1.5,0,1.5,2)) %>%
  hc_add_series(dsts,name="Global Temperature",type="line",color=hex_to_rgba(viridis::viridis(10,option="B")[7]),lineWidth=1,states=list(hover=list(lineWidth=1)),shadow=FALSE) 
hc4
##########################################################################################
# 
##########################################################################################
dscr<-df %>%
  dplyr::mutate(name=paste(decade,month)) %>%
  dplyr::select(x=tmpstmp,low=lower,high=upper,name,color=color_y)
hc5<-highchart() %>%
  hc_yAxis(tickPositions=c(-2,0,1.5,2)) %>%
  hc_xAxis(type="datetime") %>%
  hc_add_series(dscr,name="Global Temperature",type="columnrange")
hc5
##########################################################################################
# 
##########################################################################################
library(ggplot2)
library(dplyr)
library(tidyr)
library(gifski)

# ── data ───────────────────────────────────────────────────────────────────────
df_nasa <- read.csv(
  "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",
  header = TRUE, stringsAsFactors = FALSE, na.strings = "***", skip = 1
)

df_long <- df_nasa[, c("Year", month.abb)] |>
  pivot_longer(cols = all_of(month.abb), names_to = "month", values_to = "temp") |>
  mutate(
    temp      = as.numeric(temp),
    month_num = match(month, month.abb)
  ) |>
  filter(!is.na(temp), Year >= 1880)

years   <- sort(unique(df_long$Year))
n_years <- length(years)

# ── close the loop per year (Jan repeated as month 13) ────────────────────────
closed <- df_long |>
  group_by(Year) |>
  arrange(month_num) |>
  group_modify(~ bind_rows(.x, filter(.x, month_num == 1) |> mutate(month_num = 13))) |>
  ungroup()

# ── reference circles (Paris targets) ─────────────────────────────────────────
circle_df <- expand.grid(
  month_num = seq(1, 13, length.out = 200),
  r         = c(0, 1.5, 2.0)
)

# ── month labels positioned outside the plot ──────────────────────────────────
month_labels <- data.frame(
  month_num = 1:12,
  label     = month.abb
)

# ── frame loop ─────────────────────────────────────────────────────────────────
frames_dir <- file.path(tempdir(), "spiral_frames")
dir.create(frames_dir, showWarnings = FALSE)
frame_paths <- c()

temp_lim  <- c(-1.0, 1.6)
label_r   <- 1.85   # month labels just outside the outer ring

# spokes: one radial line per month
spokes_df <- data.frame(
  month_num = rep(1:12, each = 2),
  temp      = rep(c(temp_lim[1], label_r - 0.05), 12),
  grp       = rep(1:12, each = 2)
)

for (i in seq_along(years)) {
  yr           <- years[i]
  years_so_far <- years[seq_len(i)]
  
  past <- closed |> filter(Year %in% years_so_far, Year != yr)
  curr <- closed |> filter(Year == yr)
  
  past <- past |>
    mutate(yr_idx = match(Year, years) / n_years)
  
  p <- ggplot() +
    
    # ── spokes ──────────────────────────────────────────────────────────────
  geom_path(data = spokes_df,
            aes(x = month_num, y = temp, group = grp),
            color = "gray25", linewidth = 0.25) +
    
    # ── reference circles ───────────────────────────────────────────────────
  geom_path(data = circle_df,
            aes(x = month_num, y = r, group = r),
            color = "gray30", linewidth = 0.35, linetype = "dashed") +
    annotate("text", x = 0.5, y = c(0, 1.5, 2.0) + 0.04,
             label = c("0°C", "1.5°C", "2°C"),
             color = "gray50", size = 2.8, hjust = 1) +
    
    # ── all past years ──────────────────────────────────────────────────────
  geom_path(data = past,
            aes(x = month_num, y = temp, group = Year, color = yr_idx),
            linewidth = 0.4, alpha = 0.7) +
    scale_color_viridis_c(option = "B", limits = c(0, 1), guide = "none") +
    
    # ── current year (white + thick) ────────────────────────────────────────
  geom_path(data = curr,
            aes(x = month_num, y = temp),
            color = "white", linewidth = 1.6, alpha = 0.95) +
    
    # ── month labels ────────────────────────────────────────────────────────
  geom_text(data = month_labels,
            aes(x = month_num, y = label_r, label = label),
            inherit.aes = FALSE,
            color = "gray70", size = 3.5, fontface = "bold") +
    
    # ── polar setup ─────────────────────────────────────────────────────────
  coord_polar(theta = "x", start = -pi / 6, clip = "off") +
    scale_x_continuous(limits = c(1, 13), breaks = 1:12, labels = NULL) +
    scale_y_continuous(limits = c(temp_lim[1], label_r + 0.1)) +
    
    labs(
      title   = as.character(yr),
      caption = "NASA GISS Surface Temperature Analysis v4  |  Anomaly vs 1951-1980 baseline"
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "#050510", color = NA),
      panel.background = element_rect(fill = "#050510", color = NA),
      plot.title   = element_text(color = "white", face = "bold", size = 38,
                                  hjust = 0.5, margin = margin(16, 0, 0, 0)),
      plot.caption = element_text(color = "gray40", size = 7.5, hjust = 0.5,
                                  margin = margin(0, 0, 12, 0)),
      plot.margin  = margin(10, 10, 10, 10)
    )
  
  path <- file.path(frames_dir, sprintf("frame_%04d.png", i))
  ggsave(path, p, width = 1800/150, height = 1900/150,
         dpi = 150, bg = "#050510")
  frame_paths <- c(frame_paths, path)
  message("frame ", i, "/", n_years, "  (", yr, ")")
}

# ── stitch ─────────────────────────────────────────────────────────────────────
gifski::gifski(frame_paths,
               gif_file = "climate_spiral.gif",
               width = 1800, height = 1900,
               delay = 0.1)        # 10 fps — slow down to 0.2 if too fast
message("Done: climate_spiral.gif")
##########################################################################################
# 
##########################################################################################
# remotes::install_github("ropensci/rnoaa")
library(rnoaa)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(sf)
library(rnaturalearth)

# ── 1. find the LGAV station ───────────────────────────────────────────────────
stations <- isd_stations(refresh = FALSE)
lgav     <- stations[stations$icao %in% "LGAV", ]
message("Station: ", lgav$station_name, "  USAF=", lgav$usaf, "  WBAN=", lgav$wban)

usaf <- lgav$usaf[1]
wban <- lgav$wban[1]

# ── 2. download hourly ISD data year by year (cached in tempdir) ───────────────
years     <- 2004:2026
data_list <- list()

for (yr in years) {
  tryCatch({
    d <- rnoaa::isd(usaf = usaf, wban = wban, year = yr, progress = FALSE)
    data_list[[as.character(yr)]] <- d
    message("  downloaded ", yr, " (", nrow(d), " rows)")
  }, error = function(e) message("  skip ", yr, ": ", conditionMessage(e)))
}

df_raw <- bind_rows(data_list)

# ── 3. clean & parse ───────────────────────────────────────────────────────────
# rnoaa::isd() returns temperature in tenths of °C
df <- df_raw |>
  mutate(
    date      = as.Date(date),
    temp      = as.numeric(temperature) / 10,
    dew       = as.numeric(temperature_dewpoint) / 10,
    wind_spd  = as.numeric(wind_speed) / 10,
    slp       = as.numeric(air_pressure) / 10
  ) |>
  # replace NOAA sentinel missing values
  mutate(across(c(temp, dew, wind_spd, slp), ~ ifelse(. > 900 | . < -200, NA, .))) |>
  filter(!is.na(date))

# ── 4. daily aggregates ────────────────────────────────────────────────────────
daily_max <- df |> group_by(date) |>
  summarise(temp_max = max(temp, na.rm = TRUE),
            wind_max = max(wind_spd, na.rm = TRUE), .groups = "drop")

daily_min <- df |> group_by(date) |>
  summarise(temp_min = min(temp, na.rm = TRUE), .groups = "drop")

daily <- left_join(daily_max, daily_min, by = "date") |>
  filter(is.finite(temp_max), is.finite(temp_min)) |>
  mutate(year = year(date))

# ── 5. temperature time series plot ───────────────────────────────────────────
ggplot(daily, aes(x = date)) +
  geom_ribbon(aes(ymin = temp_min, ymax = temp_max, fill = temp_max),
              alpha = 0.7) +
  scale_fill_gradientn(
    colours = c("#2166ac", "#74add1", "#fee090", "#f46d43", "#a50026"),
    name = "Max °C"
  ) +
  geom_hline(yintercept = 0, color = "white", linewidth = 0.4, linetype = "dashed") +
  labs(
    title    = "Athens — El. Venizelos Airport (LGAV)",
    subtitle = "Daily temperature range 2004–2022",
    x = NULL, y = "Temperature (°C)",
    caption = "Source: NOAA ISD via rnoaa"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    legend.position  = "right"
  )

ggsave("lgav_temperature.png", width = 12, height = 5, dpi = 150)

# ── 6. monthly average temperature heatmap ────────────────────────────────────
monthly <- df |>
  mutate(year = year(date), month = month(date, label = TRUE)) |>
  group_by(year, month) |>
  summarise(avg_temp = mean(temp, na.rm = TRUE), .groups = "drop")

ggplot(monthly, aes(x = year, y = month, fill = avg_temp)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_gradientn(
    colours = c("#2166ac", "#abd9e9", "#ffffbf", "#fdae61", "#d73027"),
    name = "°C"
  ) +
  labs(
    title    = "Monthly Mean Temperature — LGAV",
    x = NULL, y = NULL,
    caption = "Source: NOAA ISD via rnoaa"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank())

ggsave("lgav_heatmap.png", width = 10, height = 5, dpi = 150)

# ── 7. simple map (no Google key needed) ──────────────────────────────────────
greece <- ne_countries(country = "Greece", scale = "medium", returnclass = "sf")
station_pt <- st_as_sf(lgav[1, ], coords = c("lon", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = greece, fill = "#e8e0d5", color = "gray60") +
  geom_sf(data = station_pt, color = "#c0392b", size = 4, shape = 21,
          fill = "#e74c3c", stroke = 1.2) +
  annotate("text", x = lgav$lon[1] + 0.4, y = lgav$lat[1],
           label = "LGAV", color = "#c0392b", fontface = "bold", hjust = 0) +
  coord_sf(xlim = c(19, 29), ylim = c(34, 42)) +
  labs(title = "Athens Eleftherios Venizelos Airport",
       caption = "Source: NOAA ISD station list") +
  theme_void(base_size = 11) +
  theme(plot.background = element_rect(fill = "#d6eaf8", color = NA))

ggsave("lgav_map.png", width = 7, height = 6, dpi = 150)

# ── 8. time series decomposition ──────────────────────────────────────────────
ts_temp <- ts(daily$temp_max, frequency = 365,
              start = c(year(min(daily$date)), yday(min(daily$date))))
fit <- decompose(ts_temp)
autoplot(fit) +
  labs(title = "Seasonal decomposition — LGAV daily max temperature") +
  theme_minimal()

ggsave("lgav_decomposition.png", width = 10, height = 7, dpi = 150)

message("Done. Four PNGs saved.")



