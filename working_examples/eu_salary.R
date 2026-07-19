##########################################################################################
# 
##########################################################################################
library(rwf)
library(dplyr)
library(tidyr)
library(countrycode)
library(plotly)

directory<-paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/")

df<-read.csv(paste0(directory,"estat_nama_10_fte_en.csv"))
df<-df[df$unit%in%"EUR",]
df<-df[!df$geo%in%c("EU27_2020","EA20"),]
df<-df[,c("geo","TIME_PERIOD","OBS_VALUE")]
# df$TIME_PERIOD<-factor(as.numeric(df$TIME_PERIOD),levels=sort(unique(as.numeric(df$TIME_PERIOD))))
# df$geo<-factor(df$geo,levels=sort(unique(df$geo)))
df$country_name<-countrycode(df$geo,origin="eurostat",destination="country.name")
df$iso<-countrycode(df$geo,origin="eurostat",destination="iso2c")

years<-1995:2024
df_expanded<-df %>%
  complete(iso,TIME_PERIOD=years,fill=list(OBS_VALUE=0)) %>%
  group_by(iso) %>%
  fill(country_name,.direction="downup") %>%
  ungroup()

df_expanded$flag_url<-paste0("https://flagcdn.com/w40/",tolower(df_expanded$iso),".png")
df_expanded$geo<-NULL
names(df_expanded)<-c("iso","year","value","country","flag")
title<-"Average full-time adjusted salaries per employee in Greece -in red-\ncompared with EU countries (source: Eurostat)"
##########################################################################################
# 
##########################################################################################
font_style<-list(size=20,color="gray25",weight="bold")
colors=c("#e6194b","#3cb44b","#ffe119","#0082c8","#f58231","#911eb4","#46f0f0",
         "#f032e6","#d2f53c","#fabebe","#008080","#e6beff","#aa6e28","#fffac8",
         "#800000","#aaffc3","#808000","#ffd8b1","#000080","#808080","#ffffff",
         "#000000")
last_points<-df_expanded %>%
  group_by(iso) %>%
  filter(year==max(year)) %>%
  ungroup() %>%
  mutate(flag=paste0("https://flagcdn.com/w40/",tolower(iso),".png"))
flag_images<-lapply(1:nrow(last_points),function(i) {
  list(
    source=last_points$flag[i],
    xref="x",
    yref="y",
    x=last_points$year[i],
    y=last_points$value[i],
    xanchor="center",
    yanchor="middle",
    sizex=100,
    sizey=300,
    sizing="contain",
    opacity=1,
    layer="above"
  )
})
##########################################################################################
# 
##########################################################################################
plot_ly(df_expanded[df_expanded$value>0,],
        x=~year,
        y=~value,
        color=~country,
        mode='lines+markers',
        type='scatter',
        text=~paste("Year:",year,
                    "<br>Value:",value,
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
dff<-data.frame(df_expanded[df_expanded$year%in%2023,])
df_expanded$country<-factor(df_expanded$country,levels=as.character(dff[order(-dff$value),"country"]))

p<-plot_ly(df_expanded,
           x=~country,
           y=~value,
           frame=~year,
           ids=~country,
           type='bar',
           # text=~paste("Year:",year,
           #             "<br>Value:",value,
           #             "<br>Country:",country),
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
pb<-plotly::plotly_build(p)
pb

for (i in seq_along(pb$x$frames)) {
  frame_data<-pb$x$frames[[i]]$data[[1]]
  df_frame<-df_expanded[df_expanded$year == pb$x$frames[[i]]$name,]
  images<-lapply(seq_len(nrow(df_frame)),function(j) {
    list(
      source=df_frame$flag_url[j],
      xref="x",
      yref="paper",
      x=df_frame$country[j],
      y=0,
      xanchor="center",
      yanchor="bottom",
      sizex=0.8,
      sizey=0.05,
      sizing="stretch",
      opacity=1,
      layer="above"
    )
  })
  pb$x$frames[[i]]$layout<-list(images=images)
}

pb
##########################################################################################
# 
##########################################################################################
for(i in unique(df_expanded$year)) {
  df_expanded[df_expanded$year%in%i,"rank"]<-rank(df_expanded[df_expanded$year%in%i,"value"],ties.method="max")
}

df_expanded<-df_expanded[!duplicated(df_expanded),]

# Colors
unique_countries<-unique(df_expanded$country)
colors<-setNames(rep("lightgray", length(unique_countries)), unique_countries)
colors["Greece"]<-"red"

# Plot
p<-plot_ly(df_expanded,
           x=~rank,
           y=~value,
           frame=~year,
           ids=~rank,
           text=~paste("Year:",year,
                       "<br>Value:",value,
                       "<br>Country:",country),
           type="bar",
           marker=list(color=~colors),
           hoverinfo="text",
           showlegend=FALSE
) %>%
  layout(
    xaxis=list(
      title="Rank",
      tickmode="array"
      # tickvals=~rank,
      # tickangle=-90
    ),
    yaxis=list(title="Euro"),
    title=title,
    margin=list(l=50, r=50, b=200, t=100),
    font=font_style
  ) %>%
  animation_opts(
    frame=1000,
    transition=1000,
    easing="linear",
    redraw=TRUE
  )
p
##########################################################################################
# 
##########################################################################################
df_income<-read.csv(paste0(directory,"nama_10_fte__custom_19762430_linear.csv"))
df_income<-df_income[,c("geo","TIME_PERIOD","OBS_VALUE")]
df_income$iso<-countrycode(df_income$geo,origin="country.name.en",destination="iso2c")
df_income$flag_url<-paste0("https://flagcdn.com/w40/",tolower(df_income$iso),".png")
df_income<-df_income[complete.cases(df_income),]
df_income$year_rank<-NA
df_income$colors<-"lightgray"
df_income[df_income$geo%in%"Greece","colors"]<-"red"

for(i in unique(df_income$TIME_PERIOD)) {
  df_income[df_income$TIME_PERIOD%in%i,"year_rank"]<-rank(-df_income[df_income$TIME_PERIOD%in%i,"OBS_VALUE"],ties.method="min")
}

head(df_income)

plot_ly() %>%
  add_bars(
    data         = df_income[df_income$colors == "lightgray", ],
    x            = ~year_rank,
    y            = ~OBS_VALUE,
    frame        = ~TIME_PERIOD,
    ids          = ~year_rank,
    marker       = list(color = "lightgray"),
    text         = ~geo,
    hoverinfo    = "text",
    textposition = "outside",
    textangle    = 90,
    showlegend   = FALSE
  ) %>%
  add_bars(
    data         = df_income[df_income$colors == "red", ],
    x            = ~year_rank,
    y            = ~OBS_VALUE,
    frame        = ~TIME_PERIOD,
    ids          = ~year_rank,
    marker       = list(color = "red"),
    text         = ~geo,
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
df_eusalary<-openxlsx::read.xlsx(paste0(directory,"nama_10_fte__custom_22122988_spreadsheet.xlsx"),sheet="Data",startRow = 8)
df_eusalary[df_eusalary==":"]<-NA
df_eusalary[df_eusalary=="not available"]<-NA
df_eusalary[df_eusalary=="Special value"]<-NA
df_eusalary[df_eusalary=="GEO (Labels)"]<-NA
df_eusalary<-remove_nc(df_eusalary,remove_rows = TRUE)
df_eusalary[2:length(df_eusalary)]<-change_data_type(df_eusalary[2:length(df_eusalary)],type="numeric")
df_eusalary<-df_eusalary[!df_eusalary$TIME%in%"European Union - 27 countries (from 2020)",]
df_eusalary<-reshape2::melt(df_eusalary)

df_eusalary$rank<-df_eusalary$color<-NA

for(i in unique(df_eusalary$variable)) {
  df_eusalary[df_eusalary$variable%in%i,"rank"]<-rank(-df_eusalary[df_eusalary$variable%in%i,"value"],ties.method="min")
}

df_eusalary[df_eusalary$TIME%in%"Greece","color"]<-"#E74C3C"
df_eusalary[!df_eusalary$TIME%in%"Greece","color"]<-"gray70"

head(df_eusalary)

names(df_eusalary)<-c("country","year","value","color","rank")

library(gganimate)
library(gifski)
library(scales)
p <- ggplot(df_eusalary, aes(x = rank, y = value, fill = color, group = country)) +
  geom_col(width = 0.85, show.legend = FALSE) +
  geom_text(aes(y = 0, label = country, color = color),
            hjust = 1, nudge_y = -400,
            size = 3.8, fontface = "bold", show.legend = FALSE) +
  # geom_text(aes(label = comma(value, accuracy = 1)),
  #           hjust = 0, nudge_y = 400,
  #           color = "gray80", size = 3.2) +
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
  labs(title   = "Average full time adjusted salary [nama_10_fte__custom_22122988]",
       # subtitle = "{closest_state}",
       x       = NULL,
       y       = "Euros per employee per annum",
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
  transition_states(year, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out")

p

animate(p,
        nframes  = 300,
        fps      = 10,
        width    = 900,
        height   = 800,
        renderer = gifski_renderer("eu_salary_race.gif"))














