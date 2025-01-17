###########################################################
#                                                         #
#         Create animated maps with Economic data         #
#       https://www.youtube.com/watch?v=8kunLjakT0c       #
#                                                         #
###########################################################

# We can download data from databank.worldbank.org

# Let's load some libraries
libs <- c(
  "tidyverse", "sf", "rnaturalearth",
  "wbstats", "gganimate", "classInt"
)

installedLibs <- libs %in% rownames(installed.packages())
if(any(installedLibs == FALSE)){
  install.packages(libs[!installedLibs])
}

invisible(lapply(libs, library, character.only=TRUE))

# Now we can use the Worldbank package to find the data that we are interested in
kpiDF <- wbstats::wb_search(pattern="internet users")

internetDF <- wbstats::wb_data(
  indicator = "IT.NET.USER.ZS",
  country = "countries_only",
  start_date = 2001, end_date = 2022
)

internetWorldDF <- internetDF |>
  dplyr::select(, c(1, 4:5))

names(internetWorldDF) <- c(
  "iso2", "Year", "users"
)

internetWorldDF

# We download the world shape file
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

worldSf <- rnaturalearth::ne_countries(
  type = "countries", scale = "small"
) |>
  sf::st_as_sf() |>
  sf::st_transform(crsLONGLAT)

head(worldSf)
names(worldSf)
plot(sf::st_geometry(worldSf))

# Get rid of Antarctica to make map bigger (no data there anyway)
worldSfNoAntartica <- worldSf |>
  dplyr::filter(region_un != "Antarctica") |>
  dplyr::select(iso_a2, name)

plot(sf::st_geometry(worldSfNoAntartica))

# 3. JOIN DATA & SHAPEFILE
internetWorldDF <- dplyr::left_join(
  worldSfNoAntartica, internetWorldDF,
  by = c("iso_a2" = "iso2")
)

internetWorldDF

# 4. PROJECTION
# Robinson. Make it a bit rounder
robinson_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

internetWorldDFRobinson <- internetWorldDF |>
  sf::st_transform(robinson_crs)
plot(sf::st_geometry(internetWorldDFRobinson))

# 5. BREAKS. We do some steps before animating, before animating, to make things a bit nicer
vmin <- min(internetWorldDF$users, na.rm = T)
vmax <- max(internetWorldDF$users, na.rm = T)
brk <- round(classIntervals(
  internetWorldDF$users,
  n = 6,
  style = "fisher"
)
$brks, 1) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)
breaks <- c(vmin, brk)

cols <- rev(c(
  "#001f3a", "#30314e", "#5c435f",
  "#8d5369", "#c0636c", "#dc8177",
  "#e6a988"
))

# 6. ANIMATE. We write a function to make things interesting

get_animated_world_map <- function() {
  world_map <- ggplot(
    data = internetWorldDF,
    aes(fill = users)
  ) +
    geom_sf(color = "white", size = 0.05) +
    scale_fill_gradientn(
      name = "",
      colours = cols,
      breaks = breaks,
      labels = round(breaks, 1),
      limits = c(vmin, vmax),
      na.value = "grey70"
    ) +
    coord_sf(crs = robinson_crs) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = c(.5, -.015),
      legend.text = element_text(size = 11, color = "grey10"),
      panel.grid.major = element_line(color = "white", size = .2),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        face = "bold", size = 20,
        color = "grey10", hjust = .5, vjust = -3
      ),
      plot.subtitle = element_text(
        size = 40, color = "#c43c4e",
        hjust = .5, vjust = -1
      ),
      plot.caption = element_text(
        size = 10, color = "grey10",
        hjust = .5, vjust = -10
      ),
      plot.margin = unit(c(t = -4, r = -4, b = -4, l = -4), "lines"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank()
    ) +
    labs(
      x = "",
      y = "",
      title = "Internet users (% of population)",
      subtitle = "Year: {as.integer(closest_state)}",
      caption = "©2023 Milos Popovic (https://milospopovic.net)
        World Development Indicators, The World Bank"
    )
  
  return(world_map)
}

# Try it out
worldMap <- get_animated_world_map()
print(worldMap)

timelapseWorldMap <- worldMap +
  transition_states(Year) +
  enter_fade() +
  exit_fade() +
  ease_aes("quadratic-in-out", interval = .2)

animatedWorld <- gganimate::animate(
  timelapseWorldMap,
  nframes = 120,
  duration = 20,
  start_pause = 3,
  end_pause = 30,
  height = 6,
  width = 7.15,
  res = 300,
  units = "in",
  fps = 15,
  renderer = gifski_renderer(loop = T)
)

gganimate::anim_save(
  "internet_users_world.gif", animatedWorld
)

# 2B. FILTER ASIA
#-------------------------
# Asia
unique(worldSf$continent)

internetAsiaDf <- internetDF |>
  dplyr::select(, c(1, 4:5))

names(internetAsiaDf) <- c(
  "iso2", "Year", "users"
)

internet_asia_df

asiaSf <- worldSf |>
  dplyr::filter(continent == "Asia")
plot(sf::st_geometry(asiaSf))
head(asiaSf)

# 3. JOIN DATA & SHAPEFILE
#-------------------------
internetAsiaSf <- dplyr::inner_join(
  asiaSf, internetAsiaDf,
  by = c("iso_a2" = "iso2")
)

# 4. BREAKS
#----------
vmin <- min(internetAsiaDf$users, na.rm = T)
vmax <- max(internetAsiaDf$users, na.rm = T)
brk <- round(classIntervals(
  internetAsiaDf$users,
  n = 6,
  style = "fisher"
)
$brks, 1) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)
breaks <- c(vmin, brk)

cols <- rev(c(
  "#001f3a", "#30314e", "#5c435f",
  "#8d5369", "#c0636c", "#dc8177",
  "#e6a988"
))

# 5. ANIMATE
#-----------
get_animated_asia_map <- function() {
  asiaMap <- ggplot(
    data = internetAsiaSf,
    aes(fill = users)
  ) +
    geom_sf(color = "white", size = 0.05) +
    scale_fill_gradientn(
      name = "",
      colours = cols,
      breaks = breaks,
      labels = round(breaks, 1),
      limits = c(vmin, vmax),
      na.value = "grey70"
    ) +
    coord_sf() +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = c(.5, -.015),
      legend.text = element_text(size = 11, color = "grey10"),
      panel.grid.major = element_line(color = "white", size = .2),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        face = "bold", size = 20,
        color = "grey10", hjust = .5, vjust = -3
      ),
      plot.subtitle = element_text(
        size = 40, color = "#c43c4e",
        hjust = .5, vjust = -1
      ),
      plot.caption = element_text(
        size = 10, color = "grey10",
        hjust = .5, vjust = -10
      ),
      plot.margin = unit(c(t = -4, r = -2, b = -4, l = -2), "lines"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank()
    ) +
    labs(
      x = "",
      y = "",
      title = "Internet users (% of population)",
      subtitle = "Year: {as.integer(closest_state)}",
      caption = "©2023 Milos Popovic (https://milospopovic.net)
        World Development Indicators, The World Bank"
    )
  
  return(asiaMap)
}

asiaMap <- get_animated_asia_map()
print(asiaMap)

timelapseAsiaMap <- asiaMap +
  transition_states(Year) +
  enter_fade() +
  exit_fade() +
  ease_aes("quadratic-in-out", interval = .2)

animatedAsia <- gganimate::animate(
  timelapseAsiaMap,
  nframes = 120,
  duration = 20,
  start_pause = 3,
  end_pause = 30,
  height = 6,
  width = 7.15,
  res = 300,
  units = "in",
  fps = 15,
  renderer = gifski_renderer(loop = T)
)

gganimate::anim_save(
  "internet_users_asia.gif", animatedAsia
)
