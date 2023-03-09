library(tidyverse)
library(hrbrthemes)
library(grid)
library(ggtext)
library(gghighlight)

hrbrthemes::update_geom_font_defaults()

theme_set(theme_ipsum_rc(plot_margin = margin(l=10, t=10, b=10, r=10)) +
          theme(plot.subtitle = element_markdown(color = "gray40", lineheight = 1.5, size = rel(0.8)),
                plot.caption = element_markdown(color = "gray40")))

read_data <- function()
{
  read_csv("data/rsv_denmark.csv",
          col_types = "cdccc")%>%
  mutate(`Age Group` = fct_recode(`Age Group`,
                                  "0-5 months" = "01. 0-5 måneder",
                                  "6-11 months" = "02. 6-11 måneder",
                                  "1 year olds" = "03. 1 år",
                                  "2 year olds" = "04. 2 år",
                                  "3-5 years" = "05. 3-5 år",
                                  "6-14 years" = "06. 6-14 år",
                                  "15-44 years" = "07. 15-44 år",
                                  "45-64 years" = "08. 45-64 år",
                                  "65-74 years" = "09. 65-74 år",
                                  "75-84 years" = "10. 75-84 år",
                                  "85+ years" = "11. 85+ år"))%>%
  separate(week_sort, into = c("start_year", "Week"), sep = " uge ", remove = F)%>%
  separate(Season, into = c("season_start_year", "season_end_year"), sep = "/", remove = F)%>%
  mutate(Season = gsub("/20", "/", Season))%>%
  mutate(Year = ifelse(start_year =="År 1", as.integer(season_start_year), as.integer(season_start_year)+1))%>%
  mutate(Week = as.integer(Week))%>%
  select(-week_sort, -start_year, -season_start_year, -season_end_year)%>%
  complete(Year=seq(2010, 2025), Week,  `Age Group`, Season, type, fill = list(value = 0))%>%
  filter(Year %in% years_with_53_weeks | Week != 53)%>%
  mutate(Date = ISOweek::ISOweek2date(paste0(Year, "-W", str_pad(Week, 2, pad = "0"), "-7")))
}

years_with_53_weeks = c(2015, 2020, 2026)

line_colors <- append(colorspace::lighten(scales::hue_pal()(6), amount = 0.5), c("red", "black"))

fill_colors <- function(n_age_groups){
  colors = list()
  for (color in scales::hue_pal()(8)){
    for (i in 0:(n_age_groups-1)){
      colors = append(colors, colorspace::lighten(color, amount = i*0.15))
    } 
  } 
  colors
}

fill_colors_inverse <- function(n_age_groups){
  colors = list()
  for (color in scales::hue_pal()(8)){
    for (i in 0:(n_age_groups-1)){
      colors = append(colors, colorspace::lighten(color, amount = ((n_age_groups-1)-i)*0.15))
    } 
  } 
  colors
}


plot_inset <- function(n_age_groups){
  df%>%
    filter(type=="admissions")%>%
    group_by(Season, `Age Group`)%>%
    summarise(value = sum(value), .groups = "drop")%>%
    mutate(value = as.integer(as.factor(Season))*n_age_groups + as.integer(as.factor(`Age Group`)) -(n_age_groups+1))%>%
    ggplot(aes(x = as.factor(Season), y = (as.factor(`Age Group`)), fill = fct_rev(as.factor(value)))) +
    scale_fill_manual(values = fill_colors_inverse(n_age_groups)) +
    scale_y_discrete(position = "right") +
    geom_tile(color = "black", size = .5) + theme_minimal() + theme(legend.position = "none") +
    labs(x = "",y = "") +
    theme(axis.text.x = element_text(size = 15, angle = -45, vjust = 0.5, hjust=0),
          axis.text.y = element_text(size = 15))
}

plot_inset_small <- function(n_age_groups){
  range = (n_age_groups*7+1):(n_age_groups*8)
    
  df%>%
    filter(type=="admissions")%>%
    filter(Season=="2015/16")%>%
    group_by(Season, `Age Group`)%>%
    summarise(value = sum(value), .groups = "drop")%>%
    mutate(value = as.integer(as.factor(Season))*n_age_groups + as.integer(as.factor(`Age Group`)) -(n_age_groups-1))%>%
    ggplot(aes(x = as.factor(Season), y = (as.factor(`Age Group`)), fill = fct_rev(as.factor(value)))) +
    scale_fill_manual(values = fill_colors_inverse(n_age_groups)[(n_age_groups*7+1):(n_age_groups*8)]) +
    scale_y_discrete(position = "right") +
    geom_tile(color = "black", size = .5) + theme_minimal() + theme(legend.position = "none") +
    labs(x = "",y = "") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 15))
}

savePlot_ <- function(plot, name) {
  w <- getOption("repr.plot.width")/s
  h <- getOption("repr.plot.height")/s
  dpi <- getOption("repr.plot.res")
    
  ggsave(paste0("img/RSV_Denmark_", name, ".png"), plot = plot,
         width = w, height = h, bg = "white", dpi = dpi, scale = s)
  ggsave(paste0("img/RSV_Denmark_", name, "@2x", ".png"), plot = plot,
         width = w, height = h, bg = "white", dpi = dpi*2, scale = s)
  return(plot)
}

savePlot <- function(name) {
  structure(
    "A save function maskerading as a layer.", 
    class = "save_plot",
    fn = "savePlot_",
    name = name
  )
}
ggplot_add.save_plot <- function(object, plot, object_name) {
  # a method for the `+` operator for save_plot objects.
  # - "object to add" (arguments to the RHS of the `+`)
  # - plot is the existing plot (on the LHS of the `+`)
  # - object_name is the unevaluated call on the RHS of the `+`
  
  # extract the `fn` attribute from `save_plot` output
  fn <- attr(object, "fn")
  
  # extract arguments `arg1` and `arg2` from `save_plot` output
  save_plot_args <- attributes(object)[!names(attributes(object)) %in% 
                                   c("class", "fn")]
  
  # call `fn` with the arguments `plot`, `arg1`, and `arg2`
  new_plot <- do.call(
    fn,
    c(list(plot), save_plot_args)
  )
  
  # return the new plot
  new_plot
}
