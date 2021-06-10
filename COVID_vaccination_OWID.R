## animated plot of COVID deaths and vaccinations
## data pulled from https://github.com/owid/covid-19-data/ (Our World in Data)
## written by Kristen Panthagani, PhD (@kmpanthagani)


## import libraries
library(readr)
library(knitr)
library(reshape2)
library(dplyr)
library(ggplot2)
library(gganimate)
library(ggrepel)
library(scales)
library(ggthemr)

options(scipen=10000)

## import data

cases_raw <- readr::read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
cases <- as.data.frame(cases_raw)
US <- dplyr::filter(cases, location == "United States")
US$Date <- as.Date(US$date, format="%m / %d / %y")

# remove dates with zero deaths/vaccinations
US <- dplyr::filter(US, Date > as.Date("2020-02-28"))

## determine first and last date in data
last_date <- max(US$Date)
first_date <- min(US$Date)
first_vax <- as.Date("2021-01-14")


# set conversion coefficient for second axis
coeff <- summary(US$people_fully_vaccinated)[6]/summary(US$new_deaths_smoothed)[6]

# convert vaccinated numbers for axis scaling
US$vacc_converted <- as.numeric(US$people_fully_vaccinated)/coeff 

# filter to include data to plot
columns_to_plot <- c("Date", "new_deaths_smoothed","vacc_converted")
US_filtered <- subset(US, select = columns_to_plot)

# change absence of vaccination from NA to 0
US_filtered$vacc_converted <- as.character(US_filtered$vacc_converted)
US_filtered$vacc_converted[US_filtered$Date < first_vax] <- "0"
US_filtered$vacc_converted <- as.numeric(US_filtered$vacc_converted)

## reformat for plotting
melted <- melt(US_filtered, id.vars = c("Date"))

## add labels for plotting
melted$label <- as.character(melted$variable)
melted$label[melted$label == "new_deaths_smoothed"] <- "New Deaths"
melted$label[melted$label == "vacc_converted"] <- "Total\nVaccinated"
melted$label <- factor(melted$label)

## get rid of NAs
melted <- dplyr::filter(melted, !is.na(value))


####################### plot aesthetics #######################################

ggthemr('fresh')

## select custom colors, if you want to
colorz <- c("#31CE04","#A9A3A2","#31CE07")
set_swatch(colorz)

## assign text to caption
caption_date <- as.character(format(last_date, "%b %d, %Y"))
caption <- paste("updated ", caption_date, sep = "")
caption <- paste(caption, "data from ourworldindata.org", sep = "\n")
caption <- paste(caption, "made by @kmpanthagani", sep = "\n")

## assign title
title <- paste("COVID-19 in the US")


## plot data
p <- ggplot(melted, aes(x = Date, y = value, color = label)) + 
  geom_point(size = 1.5, alpha = 0.5) + 
  scale_y_continuous( ## add second y-axis
    name = "Daily COVID-19 Deaths (smoothed)",
    sec.axis = sec_axis(~.*coeff, name="Total Fully Vaccinated", labels = scales::comma),
    labels = scales::comma) + 
  geom_text_repel(aes(label=label, color = label), size = 4, fontface = "bold", show.legend = FALSE, direction = "y", na.rm = TRUE, hjust = 0, vjust = 0,  check_overlap = TRUE, xlim = c(first_date,last_date + 5000)) +
  labs(caption = caption) +
  coord_cartesian(clip = 'off') 

## set plot theme
theme <- theme(axis.ticks = element_blank(),
               plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family="Arial"),
               legend.position = "none",
               axis.title.x = element_blank(),
               axis.title.y = element_text(margin = margin(r = 20)),
               axis.title.y.right = element_text(margin = margin(l = 30)),
               plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, "cm"))

## make animation with gganimate 
anim <- p + theme +transition_time(Date) + shadow_mark(exclude_layer = 2)  + ggtitle(title, subtitle = 'Date: {frame_time}') + view_follow()
gif <- animate(anim, nframes = 800, duration = 40, fps = 20, end_pause = 200)

## write gif using magick
magick::image_write(gif, path="/path/to/file/filename.gif", quality = 40)
