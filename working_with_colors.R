################################################################################
#                            Workshops for Ukraine                             #
#          "Color Palette Choice and Customization in R and ggplot2"           #
#                       Cédric Scherer | January 26, 2023                      #
################################################################################



## SETUP #######################################################################


## to run all codes, install the following typeface and restart RStudio:
## Asap Condensed (available via Google Fonts, contained in the "fonts" folder)

## also, make sure to install the following packages 
## (to 'uncomment' the code below, highlight lines 13-20 and press CMD+SHIFT+C)
# packages <- c(
#   "ggplot2", "dplyr", "readr", "forcats", "colorspace", "prismatic", 
#   "colorblindr", "RColorBrewer", "rcartocolor", "scico",
#   "MetBrewer", "viridis", "ggthemes" ## <- only needed for the last part
# )
# 
# install.packages(setdiff(packages, rownames(installed.packages())))



## WORKING WITH COLORS IN R ####################################################


## -----------------------------------------------------------------------------
pal_a <- c("green", "red", "blue")


## -----------------------------------------------------------------------------
pal_b <- c("olivedrab", "salmon3", "slateblue")


## -----------------------------------------------------------------------------
pal_c <- c("#28A87D", "#A83D28", "#2853a8")


## -----------------------------------------------------------------------------
pal_d <- c(rgb(40, 168, 125, maxColorValue = 255), 
           rgb(168, 61, 40, maxColorValue = 255), 
           rgb(40, 83, 168, maxColorValue = 255))


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_d)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_d, type = "scatter")


## -----------------------------------------------------------------------------
colorRampPalette(pal_d)(35)


## -----------------------------------------------------------------------------
pal_seq_multi <- colorRampPalette(pal_d)(35)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq_multi, type = "pie")


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq_multi, type = "bar")


## -----------------------------------------------------------------------------
pal_seq_multi <- colorRampPalette(pal_d)(35)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq_multi, type = "map")


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq_multi, type = "heatmap")


## -----------------------------------------------------------------------------
colors <- c("#28A87D", "grey98")
pal_seq <- colorRampPalette(colors)(35)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq, type = "heatmap")


## -----------------------------------------------------------------------------
colors <- c("#7754BF", "grey98", "#208462")
pal_div <- colorRampPalette(colors)(35)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_div)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_div, type = "heatmap")


## -----------------------------------------------------------------------------
colors <- c("#AA89F6", "grey5", "#49BD92")
pal_div_dark <- colorRampPalette(colors)(35)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_div_dark)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_div_dark, type = "heatmap")


## -----------------------------------------------------------------------------
colors <- colorspace::darken(c("#28A87D", "grey98"), .35)
pal_seq_dark <- colorRampPalette(colors)(35)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq_dark, type = "map")


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq_dark, type = "heatmap")


## -----------------------------------------------------------------------------
colors <- c(colorspace::darken("#28A87D", .35), "grey98")
pal_seq_dark <- colorRampPalette(colors)(35)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq_dark, type = "map")


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq_dark, type = "heatmap")


## -----------------------------------------------------------------------------
colors <- c(colorspace::darken("#28A87D", .35, space = "HLS"), "grey98")
pal_seq_dark_hls <- colorRampPalette(colors)(35)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq_dark_hls, type = "map")


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq_dark_hls, type = "heatmap")


## -----------------------------------------------------------------------------
colors <- c("grey5", colorspace::lighten("#28A87D", .85))
pal_seq_rev <- colorRampPalette(colors)(35)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq_rev, type = "map")


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq_rev, type = "heatmap")


## -----------------------------------------------------------------------------
colors <- c("red", "grey98", "blue")
pal_div_raw <- colorRampPalette(colors)(35)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_div_raw, type = "map")


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_div_raw, type = "heatmap")


## -----------------------------------------------------------------------------
colors_des <- colorspace::desaturate(colors, .7)
pal_div_des <- colorRampPalette(colors_des)(35)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_div_des, type = "map")


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_div_des, type = "heatmap")


## -----------------------------------------------------------------------------
colors_des <- prismatic::clr_desaturate(colors, .7)
pal_div_des <- colorRampPalette(colors_des)(35)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_div_des, type = "map")


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_div_des, type = "heatmap")


## -----------------------------------------------------------------------------
colors <- c(prismatic::clr_darken("#28A87D", .35), "grey98")
pal_seq_dark <- colorRampPalette(colors)(35)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq_dark, type = "map")


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq_dark, type = "heatmap")


## -----------------------------------------------------------------------------
colors <- c(prismatic::clr_mix("blue", "green"), "grey98")
pal_seq_mix <- colorRampPalette(colors)(35)


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq_mix, type = "map")


## -----------------------------------------------------------------------------
colorspace::demoplot(pal_seq_mix, type = "heatmap")


## -----------------------------------------------------------------------------
colorspace::hcl_palettes(plot = TRUE)


## -----------------------------------------------------------------------------
colorspace::hcl_palettes(palette = "PuBuGn", n = 10, plot = TRUE)


## -----------------------------------------------------------------------------
pal <- colorspace::sequential_hcl(palette = "PuBuGn", n = 10)
plot(prismatic::color(pal))


## -----------------------------------------------------------------------------
RColorBrewer::display.brewer.all()


## -----------------------------------------------------------------------------
RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)


## -----------------------------------------------------------------------------
pal <- RColorBrewer::brewer.pal(name = "RdYlBu", n = 10)
plot(prismatic::color(pal))


## -----------------------------------------------------------------------------
scico::scico_palette_show()


## -----------------------------------------------------------------------------
pal <- scico::scico(palette = "tokyo", n = 10)
plot(prismatic::color(pal))


## -----------------------------------------------------------------------------
rcartocolor::display_carto_all()


## -----------------------------------------------------------------------------
rcartocolor::display_carto_all(colorblind_friendly = TRUE)


## -----------------------------------------------------------------------------
pal <- rcartocolor::carto_pal(name = "ag_Sunset", n = 10)
plot(prismatic::color(pal))


## -----------------------------------------------------------------------------
pal_extd <- colorRampPalette(pal)(20)
plot(prismatic::color(pal_extd))



## WORKING WITH COLORS IN GGPLOT2 ##############################################


## -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(prismatic)

theme_set(theme_minimal(
  base_family = "Asap Condensed", base_size = 15, base_line_size = .5)
)

theme_update(
  panel.grid.minor = element_blank(),
  plot.title = element_text(
    face = "bold", size = rel(1.5), margin = margin(b = 15)
  ),
  plot.title.position = "plot",
  plot.caption = element_text(
    color = "grey45", size = rel(.7), hjust = 0, margin = margin(t = 15)
  ),
  plot.caption.position = "plot"
)


## -----------------------------------------------------------------------------
sf_world <- readr::read_rds("./data/urban-gdp-pop-sf.rds")
df_world <- readr::read_csv("./data/urban-gdp-pop.csv")

dplyr::glimpse(df_world)


## -----------------------------------------------------------------------------
p <- 
  ggplot(
    data = df_world, 
    mapping = aes(x = gdp_per_capita, y = urban_pop, size = pop_est)
  ) + 
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_x_log10(labels = scales::label_dollar()) + 
  scale_y_continuous(labels = function(y) paste0(y, "%"), limits = c(0, 100)) + 
  scale_size_area(
    max_size = 15, breaks = c(10, 100, 500, 1000) * 10^6,
    labels = scales::label_number(scale = 1/10^6, suffix = "M")
  ) +
  labs(
    x = NULL, y = NULL,
    fill = "Continent:", size = "Population:",
    title = "Urban population vs. GDP per capita, 2016",
    caption = "Sources: ourworldindata.org/urbanization; NaturalEarth | x-axis on log scale"
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 3)),
    size = guide_legend(override.aes = list(color = "black", fill = "grey85"))
  )

p + geom_point(aes(fill = region_un), shape = 21, alpha = .5, stroke = .7)


## -----------------------------------------------------------------------------
p +
  geom_point(
    aes(fill = region_un), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  scale_fill_viridis_d()


## -----------------------------------------------------------------------------
p +
  geom_point(
    aes(fill = urban_pop), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  scale_fill_viridis_c()


## -----------------------------------------------------------------------------
p +
  geom_point(
    aes(fill = urban_pop), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  scale_fill_viridis_c(
    name = "Urban share:", 
    labels = function(x) paste0(x, "%")
  ) +
  guides(fill = guide_colorbar())


## -----------------------------------------------------------------------------
p +
  geom_point(
    aes(fill = region_un), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  rcartocolor::scale_fill_carto_d()


## -----------------------------------------------------------------------------
p +
  geom_point(
    aes(fill = region_un), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  rcartocolor::scale_fill_carto_d(
    palette = "Bold"
  )


## -----------------------------------------------------------------------------
pal <- rcartocolor::carto_pal(
  name = "Bold", n = 5
)

p +
  geom_point(
    aes(fill = region_un), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  scale_fill_manual(
    values = pal
  )


## -----------------------------------------------------------------------------
pal <- rcartocolor::carto_pal(
  name = "Bold", n = 6
)

p +
  geom_point(
    aes(fill = region_un), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  scale_fill_manual(
    values = pal
  )


## -----------------------------------------------------------------------------
pal <- rcartocolor::carto_pal(
  name = "Bold", n = 8
)[c(1:3,5,7)]

p +
  geom_point(
    aes(fill = region_un), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  scale_fill_manual(
    values = pal
  )


## -----------------------------------------------------------------------------
pal_dark <- clr_darken(pal, .4)

p +
  geom_point(
    aes(fill = region_un), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  scale_fill_manual(
    values = pal_dark
  )


## -----------------------------------------------------------------------------
p +
  geom_point(
    aes(fill = region_un,
        color = after_scale(fill)), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  scale_fill_manual(
    values = pal
  )


## -----------------------------------------------------------------------------
p +
  geom_point(
    aes(fill = region_un,
        color = after_scale(
          clr_darken(fill, .6)
        )), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  scale_fill_manual(
    values = pal
  )


## -----------------------------------------------------------------------------
b <- ggplot(
    data = df_world, 
    mapping = aes(
      x = gdp_per_capita, 
      y = forcats::fct_reorder(
        region_un, -gdp_per_capita
      )
    )
  ) +
  scale_x_continuous(
    labels = scales::dollar_format()
  ) +
  guides(color = "none") +
  labs(x = "GDP per capita", y = NULL) +
  theme(panel.grid.major.y = element_blank())

b +
  geom_boxplot(
    aes(color = region_un), 
    width = .7
  ) +
  scale_color_manual(values = pal) 


## -----------------------------------------------------------------------------
b +
  geom_boxplot(
    aes(color = region_un, 
        fill = after_scale(
          clr_lighten(color, .65)
        )), 
    width = .7
  ) +
  scale_color_manual(values = pal)


## -----------------------------------------------------------------------------
p +
  geom_point(
    aes(fill = region_un != "Africa"), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  scale_fill_manual(
    values = c("#D00000", "grey80")
  )


## -----------------------------------------------------------------------------
p +
  geom_point(
    aes(fill = region_un != "Africa"), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  scale_fill_manual(
    values = c("#D00000", "grey80"),
    labels = c("African countries", "Other countries"),
    name = NULL
  )


## -----------------------------------------------------------------------------
p +
  geom_point(
    aes(fill = region_un != "Africa"), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  scale_fill_manual(
    values = c("#D00000", "grey80")
  ) +
  guides(fill = "none") +
  labs(subtitle = "of <b style='color:#D00000;'>African</b> and <b style='color:grey70;'>non-African</b> countries") +
  theme(
    plot.subtitle = ggtext::element_markdown(
      margin = margin(t = -15, b = 20),
      size = rel(1.3)
    )
  )


## -----------------------------------------------------------------------------
p +
  geom_point(
    aes(fill = region_un != "Africa",
        color = after_scale(fill)), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  scale_fill_manual(
    values = c("#D00000", "grey80")
  ) +
  labs(subtitle = "of <b style='color:#D00000;'>African</b> and <b style='color:grey70;'>non-African</b> countries") +
  guides(fill = "none") +
  theme(
    plot.subtitle = ggtext::element_markdown(
      margin = margin(t = -15, b = 20),
      size = rel(1.3)
    )
  )


## -----------------------------------------------------------------------------
df_world <- df_world %>% 
  mutate(
    cont_lumped = ifelse(
      continent %in% c("Asia", "Europe"), continent, "Other"
    ),
    cont_lumped = forcats::fct_relevel(
      cont_lumped, "Other", after = Inf
    )
  )


## -----------------------------------------------------------------------------
pal_hl <- c("#7754BF", "#28A74D", "grey80")

p +
  geom_point(
    data = df_world,
    aes(fill = cont_lumped,
        color = after_scale(clr_darken(fill, .3))), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  scale_fill_manual(values = pal_hl) +
  labs(subtitle = "of <b style='color:#7754BF;'>Asian</b> and <b style='color:#28A74D;'>European</b> countries compared to all <span style='color:grey60;'>other countries</span>") +
  guides(fill = "none") +
  theme(
    plot.subtitle = ggtext::element_markdown(
      margin = margin(t = -15, b = 20),
      size = rel(1.3)
    )
  )


## -----------------------------------------------------------------------------
m <- 
  ggplot(data = sf_world) +
  geom_sf(aes(fill = urban_pop), color = "white", lwd = .3) +
  coord_sf(crs = "+proj=eqearth") +
  labs(
    title = "Share of people living in urban areas, 2016",
    caption = "Sources: ourworldindata.org/urbanization; NaturalEarth",
    fill = NULL
  ) +
  theme(
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    legend.key.width = unit(2.5, "lines"),
    legend.key.height = unit(.6, "lines"),
    legend.position = "bottom"
  )

m


## -----------------------------------------------------------------------------
m + scale_fill_distiller(palette = "YlGnBu")


## -----------------------------------------------------------------------------
m + scale_fill_distiller(palette = "YlGnBu", direction = 1)


## -----------------------------------------------------------------------------
m + scale_fill_distiller(palette = "RdYlBu", direction = 1)


## -----------------------------------------------------------------------------
m + scale_fill_distiller(palette = "RdYlBu", direction = 1)


## -----------------------------------------------------------------------------
m + scale_fill_distiller(palette = "RdYlBu", direction = 1, limits = c(0, 100))


## -----------------------------------------------------------------------------
m + scale_fill_distiller(
  palette = "RdYlBu", direction = 1, limits = c(0, 100),
  na.value = "grey85", labels = function(x) paste0(x, "%")
)


## -----------------------------------------------------------------------------
m + scale_fill_gradient2(
  low = "#7754BF", mid = "grey90", high = "#208462", midpoint = 50, 
  na.value = "grey85", labels = function(x) paste0(x, "%")
)


## -----------------------------------------------------------------------------
pt <- p +
  geom_point(
    data = df_world,
    aes(fill = cont_lumped,
        color = after_scale(clr_darken(fill, .3))), 
    shape = 21, alpha = .5, stroke = .7
  ) +
  scale_fill_manual(values = pal_hl)

mt <- m + scale_fill_gradient2(
  low = "#7754BF", mid = "grey90", high = "#208462", midpoint = 50, 
  na.value = "grey85", labels = function(x) paste0(x, "%")
)


## -----------------------------------------------------------------------------
colorblindr::cvd_grid(pt)


## -----------------------------------------------------------------------------
colorblindr::cvd_grid(mt)


## -----------------------------------------------------------------------------
pal_seq_1 <- viridis::cividis(n = 7)
pal_seq_2 <- scico::scico(palette = "bamako", n = 7)

pal_div_1 <- RColorBrewer::brewer.pal(name = "RdYlBu", n = 7)
pal_div_2 <- MetBrewer::met.brewer(name = "Hiroshige", n = 7)

pal_cat_1 <- rcartocolor::carto_pal(name = "Prism", n = 7)
pal_cat_2 <- ggthemes::excel_pal()(7)


## -----------------------------------------------------------------------------
colorspace::specplot(pal_seq_1)

## -----------------------------------------------------------------------------
colorspace::specplot(pal_seq_2)


## -----------------------------------------------------------------------------
colorspace::specplot(pal_div_1)

## -----------------------------------------------------------------------------
colorspace::specplot(pal_div_2)


## -----------------------------------------------------------------------------
colorspace::specplot(pal_cat_1)

## -----------------------------------------------------------------------------
colorspace::specplot(pal_cat_2)


## -----------------------------------------------------------------------------
colorspace::hcl_color_picker()


## -----------------------------------------------------------------------------
colorspace::choose_color()


## -----------------------------------------------------------------------------
colorspace::hcl_wizard()


## -----------------------------------------------------------------------------
colorspace::choose_palette()


## -----------------------------------------------------------------------------
colorspace::cvd_emulator()



## APPENDIX: DATA PREPARATION ##################################################


## -----------------------------------------------------------------------------
library(tidyverse)

sf_world_raw <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")
owid_data <- readr::read_csv(here::here("data", "owid-urbanization-vs-gdp.csv"))

sf_world <- sf_world_raw %>% 
  left_join(
    owid_data %>% 
      janitor::clean_names() %>% 
      rename(urban_pop = "urban_population_percent_long_run_to_2016_owid") %>% 
      filter(year == 2016), 
    by = c("iso_a3" = "code")
  ) %>% 
  dplyr::select(
    sovereignt, iso_a3, type, continent = continent.x, region_un, subregion, 
    gdp_per_capita, urban_pop, pop_est, economy, income_grp
  ) %>% 
  mutate(pop_est = as.numeric(pop_est)) %>% 
  filter(!sovereignt == "Antarctica")

df_world <- sf_world %>% 
  sf::st_drop_geometry() %>% 
  filter(!is.na(gdp_per_capita), !is.na(urban_pop))

readr::write_rds(sf_world, here::here("data", "urban-gdp-pop-sf.rds"))
readr::write_csv(df_world, here::here("data", "urban-gdp-pop.csv"))


## THE END :) ##################################################################
