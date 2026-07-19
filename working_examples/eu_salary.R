##########################################################################################
# 
##########################################################################################
library(rwf)
library(dplyr)
library(tidyr)
library(countrycode)
library(plotly)
library(eurostat)
library(gganimate)
library(gifski)
library(scales)

directory<-paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/")

df_eusalary <- get_eurostat("nama_10_fte", filters = list(unit = "EUR"))
df_eusalary <- df_eusalary[!df_eusalary$geo %in% c("EU27_2020","EA20"), ]
df_eusalary$year <- lubridate::year(df_eusalary$time)
df_eusalary$country <- countrycode(df_eusalary$geo, origin="eurostat", destination="country.name")
df_eusalary$iso <- countrycode(df_eusalary$geo, origin="eurostat", destination="iso2c")
df_eusalary$flag_url<-paste0("https://flagcdn.com/w40/",tolower(df_eusalary$iso),".png")
df_eusalary$rank<-df_eusalary$color<-NA
for(i in unique(df_eusalary$year)) {
  df_eusalary[df_eusalary$year%in%i,"rank"]<-rank(-df_eusalary[df_eusalary$year%in%i,"values"],ties.method="min")
}
df_eusalary[df_eusalary$country%in%"Greece","color"]<-"#E74C3C"
df_eusalary[!df_eusalary$country%in%"Greece","color"]<-"gray70"
font_style<-list(size=20,color="gray25",weight="bold")
title   = "Average full time adjusted salary [nama_10_fte]"
##########################################################################################
# TIMESERIES
##########################################################################################
colors=c("#e6194b","#3cb44b","#ffe119","#0082c8","#f58231","#911eb4","#46f0f0",
         "#f032e6","#d2f53c","#fabebe","#008080","#e6beff","#aa6e28","#fffac8",
         "#800000","#aaffc3","#808000","#ffd8b1","#000080","#808080","#ffffff",
         "#000000")
plot_ly(df_eusalary[df_eusalary$values>0,],
        x=~year,
        y=~values,
        color=~country,
        mode='lines+markers',
        type='scatter',
        text=~paste("Year:",year,
                    "<br>Value:",values,
                    "<br>Country Name:",country),
        hoverinfo='text',
        colors=colors,
        showlegend=TRUE,
        marker=list(size=8)) %>%
  plotly::layout(title=title,
                 autosize=TRUE,
                 margin=list(l=50,r=50,b=50,t=50,pad=0),
                 legend=list(orientation="vertical",xanchor="right",x=30,y=1),
                 title="",
                 xaxis=list(title="Year",tickangle=-90),
                 yaxis=list(title=""),
                 showlegend=TRUE,
                 font=font_style)
##########################################################################################
# 
##########################################################################################
plot_ly(df_eusalary,
           x=~country,
           y=~values,
           frame=~year,
           ids=~country,
           type='bar',
           text=~paste("Year:",year,
                       "<br>Value:",values,
                       "<br>Country:",country),
           hoverinfo='text',
           colors=colors,
           showlegend=FALSE) %>%
  layout(
    title=title,
    xaxis=list(title="",tickangle=-90),
    yaxis=list(title="Euro"),
    font=font_style,
    margin=list(l=50,r=50,b=200,t=100)
  ) %>%
  animation_opts(
    frame=500,
    transition=500,
    easing="linear",
    redraw=FALSE,
    mode="immediate"
  )
##########################################################################################
# 
##########################################################################################
plot_ly() %>%
  add_bars(
    data         = df_eusalary[df_eusalary$color == "gray70", ],
    x            = ~rank,
    y            = ~values,
    frame        = ~year,
    ids          = ~rank,
    marker       = list(color = "gray70"),
    text         = ~country,
    hoverinfo    = "text",
    textposition = "outside",
    textangle    = 90,
    showlegend   = FALSE
  ) %>%
  add_bars(
    data         = df_eusalary[df_eusalary$color %in% "#E74C3C", ],
    x            = ~rank,
    y            = ~values,
    frame        = ~year,
    ids          = ~rank,
    marker       = list(color = "#E74C3C"),
    text         = ~country,
    hoverinfo    = "text",
    textposition = "outside",
    textangle    = 90,
    showlegend   = FALSE
  ) %>%
  layout(
    xaxis  = list(title = "Rank", tickmode = "array"),
    yaxis  = list(title = "Euro"),
    title  = title,
    margin = list(l = 50, r = 50, b = 200, t = 100),
    font   = font_style
  ) %>%
  animation_opts(frame = 1000, transition = 0, easing = "linear", redraw = TRUE)
##########################################################################################
# 
##########################################################################################
p <- ggplot(df_eusalary, aes(x = rank, y = values, fill = color, group = country)) +
  geom_col(width = 0.85, show.legend = FALSE) +
  geom_text(aes(y = 0, label = country, color = color),
            hjust = 1, nudge_y = -400,
            size = 3.8, fontface = "bold", show.legend = FALSE) +
  geom_text(aes(label = comma(values, accuracy = 1)),
            hjust = 0, nudge_y = 400,
            color = "gray80", size = 3.2) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_reverse(breaks = NULL) +
  # scale_y_continuous(labels = comma,expand = expansion(mult = c(0.35, 0.08))) +
  scale_y_continuous(
    labels  = comma,
    breaks  = seq(0, 100000, by = 20000),
    limits  = c(-8000, NA),
    expand  = expansion(mult = c(0, 0.08))
  ) +
  geom_text(
    data = df_eusalary %>% dplyr::distinct(year),
    aes(x = 24, y = 85000, label = year),
    inherit.aes = FALSE,
    hjust = 1, vjust = 1,
    size = 30, color = "white", alpha = 0.15,
    fontface = "bold"
  ) +
  coord_flip(clip = "off") +
  labs(title   = title,
       # subtitle = "{closest_state}",
       x       = NULL,
       y       = "Euros",
       caption = "Source: Eurostat  |  Greece highlighted in red") +
  theme_minimal(base_size = 20) +
  theme(
    plot.background    = element_rect(fill = "#0F1923", color = NA),
    panel.background   = element_rect(fill = "#0F1923", color = NA),
    panel.grid.major.x = element_line(color = "#1E2D3D", linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.minor.x = element_line(color = "#1E2D3D", linewidth = 0.4),
    axis.text.x  = element_text(color = "#90A4AE", size = 10),
    axis.text.y  = element_blank(),
    plot.title   = element_text(color = "white", face = "bold", size = 20,
                                margin = margin(12, 0, 2, 0)),
    plot.subtitle = element_text(color = "gray80", face = "bold", size = 18,
                                 hjust = 0.98,
                                 margin = margin(0, 0, 6, 0)),
    plot.caption = element_text(color = "#546E7A", size = 8, hjust = 1,
                                margin = margin(8, 0, 8, 0)),
    plot.margin  = margin(10, 30, 10, 120)) +
  transition_states(year, transition_length = 0, state_length = 5) +
  ease_aes("cubic-in-out")

p

animate(p,
        nframes  = 100,
        fps      = 10,
        duration = 20,
        width    = 900,
        height   = 800,
        renderer = gifski_renderer("eu_salary_race.gif"))














