## animated plot of daily new COVID cases and deaths for US counties
## data pulled from https://github.com/CSSEGISandData/ (Johns Hopkins)
## written by Kristen Panthagani, PhD
## would be cool if you tag/cite me if you post plots made with this code (twitter @kmpanthagani)


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

## select county name to plot (most in format "County, State, US" but a few have formatting errors)
counties <- c("Harris, Texas, US")

## select number of days to average daily deaths and cases
num_days_average <- 7

## import data
cases_raw <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
deaths_raw <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"

cases <- read_csv(cases_raw)
cases <- as.data.frame(cases)

deaths <- read_csv(deaths_raw)
deaths <- as.data.frame(deaths)


######## filter deaths data to include county of interest ########

colnames_to_filter <- c("UID","iso2","iso3","code3","FIPS","Admin2","Province_State","Country_Region","Lat","Long_","Population")

indeces <- c()

for (i in 1:length(counties)) {
  index <- grep(counties[i], deaths$Combined_Key)
  indeces <- append(indeces, index)
}

deaths_filt <- deaths[indeces,]
deaths_filt2 <- dplyr::select(deaths_filt, select = -colnames_to_filter)
deaths_filt2$Combined_Key <- paste(deaths_filt2$Combined_Key,"deaths",sep = " ")


######## filter cases data to include county of interest ########

colnames_to_filter2 <- c("UID","iso2","iso3","code3","FIPS","Admin2","Province_State","Country_Region","Lat","Long_")

indeces <- c()

for (i in 1:length(counties)) {
  index <- grep(counties[i], cases$Combined_Key)
  indeces <- append(indeces, index)
}

cases_filt <- cases[indeces,]
cases_filt2 <- dplyr::select(cases_filt, select = -colnames_to_filter2)
cases_filt2$Combined_Key <- paste(cases_filt2$Combined_Key,"cases",sep = " ")


## merge deaths and cases data

merge <- rbind(deaths_filt2, cases_filt2)

merge_t <- t(merge)
colnames(merge_t) <- merge_t[1,]
merge_t <- as.data.frame(merge_t)
merge_t2 <- merge_t[-c(1),]

colnames(merge_t2) <- c("deaths", "cases")

data <- merge_t2
data[,1] <- as.numeric(as.character(data[,1]))
data[,2] <- as.numeric(as.character(data[,2]))

## calculate daily deaths and cases (imported data is cumulative deaths and cases)
data$deaths_daily <- 0
data$cases_daily <- 0

for (i in 2:length(row.names(data))) {
  data$deaths_daily[i] <- data$deaths[i] - data$deaths[i-1]
  data$cases_daily[i] <- data$cases[i] - data$cases[i-1]
}


## calulate rolling average of daily deaths and cases
days <- num_days_average
days1 <- days+1

data$deathsav <- 0
data$casesav <- 0

for (i in days1:length(row.names(data))) {
  j <- i-days
  data$deathsav[i] <- mean(data$deaths_daily[j:i])
  data$casesav[i] <- mean(data$cases_daily[j:i])
}

data_final <- dplyr::select(data, select = -c("deaths_daily","cases_daily"))

## reformat date data
data_final$Date <- as.Date(row.names(data_final), format="%m / %d / %y")

## I don't know why I did this, but didn't feel like changing the variable name
data <- data_final

## determine first and last date in data
last_date <- max(data$Date)
first_date <- min(data$Date)

## reformat for plotting
melted <- melt(data, id.vars = c("Date"))

## add labels for plotting
melted$label <- as.character(melted$variable)
melted$label[melted$label == "deathsav"] <- "New Deaths"
melted$label[melted$label == "casesav"] <- "New Cases"
melted$label[melted$label == "cases"] <- "Cumulative\nCases"
melted$label[melted$label == "deaths"] <- "Cumulative\nDeaths"

melted$label <- factor(melted$label)

## get rid of zero and negative values
melted$value[melted$value == 0] <- NA
melted$value[melted$value < 0] <- NA
melted <- dplyr::filter(melted, !is.na(value))

####################### SELECT HERE WHAT DATA YOU WANT TO PLOT ##################################

## no filtering plots cumulative deaths, cumulative cases, daily new deaths, and daily new cases
#melted <- melted

##plots daily new deaths and cases only
#melted <- dplyr::filter(melted, (label == "New Cases" | label == "New Deaths")) 

##plots daily new cases only
melted <- dplyr::filter(melted, (label == "New Cases")) 


####################### plot aesthetics #######################################

ggthemr('fresh')

## select custom colors, if you want to
colorz <- c("#A9A3A2","#D3150C","#A9A3A1")
set_swatch(colorz)

## assign text to caption
caption_date <- as.character(format(last_date, "%b %d, %Y"))
caption <- paste("updated ", caption_date, sep = "")
caption <- paste(caption, "data from coronavirus.jhu.edu", sep = "\n")
caption <- paste(caption, "made by @kmpanthagani", sep = "\n")

## assign title
title <- paste("COVID-19 in",counties[1])

## plot data
p <- ggplot(melted, aes(x = Date, y = value, color = label)) + 
  geom_point(size = 3, alpha = 0.5) + 
  geom_text_repel(aes(label=label, color = label), size = 6, fontface = "bold", show.legend = FALSE, direction = "y", na.rm = TRUE, hjust = 0, vjust = 0,  check_overlap = TRUE, xlim = c(first_date,last_date + 45)) +
  labs(caption = caption) +
  ylab("New COVID-19 Cases (7 day average)") + ######## adjust y-axis label to whatever data you plotted
  #scale_y_log10() + #log scale optional
  coord_cartesian(clip = 'off') 

## set plot theme
theme <- theme(axis.ticks = element_blank(),
               plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family="Arial"),
               legend.position = "none",
               axis.title.x = element_blank(),
               plot.margin = margin(t = 0.5, r = 4, b = 0.5, l = 0.5, "cm"))

## make animation with gganimate 
anim <- p + theme +transition_time(Date) + shadow_mark(exclude_layer = 2)  + ggtitle(title, subtitle = 'Date: {frame_time}') + view_follow()
gif <- animate(anim, nframes = 400, duration = 20, fps = 20, end_pause = 200)

## write gif using magick
magick::image_write(gif, path="/path/filename.gif", quality = 100)
