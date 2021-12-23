################################################################
################################################################
#####                       R-script                       #####
#####                  Tobias Ruttenauer                   #####
#####                    Covid passport                    ##### 
#####                      14.09.2021				               #####
################################################################
################################################################



rm(list = ls())

library(ggplot2)
library(viridis)
library(cowplot)
library(texreg)
library(grid)
library(gridtext)
library(gridExtra)
library(colorspace)

library(parallel)
library(doParallel)

library(Synth)
library(SCtools)

library(ISOweek)
library(wpp2019)



gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}




setwd("DIRECTORY/02_Data")


ncores <- parallel::detectCores(logical = FALSE)




###################
#### Load data ####
###################


load("Corona_all_daily.RData")




#### -------------------------------------------------- ####
#### Descirptives general with introduction of measures ####
#### -------------------------------------------------- ####

countries <- c("ITA", "ISR", "FRA", "DEU", "DNK", "CHE")
corona_all.df$date <- as.Date(corona_all.df$date, format = "%Y%m%d")
corona_subset.df <- corona_all.df[which(corona_all.df $iso_code %in% countries & corona_all.df$date >= "2021-03-01"), ]

### Intervention
intervention <- data.frame(iso_code = countries, 
                           countryname = c("Italy", "Israel", "France", "Germany", "Denmark", "Switzerland"),
                           date = c("2021-08-06", "2021-07-29", "2021-08-09", "2021-08-23", "2021-04-21", "2021-09-13"))
intervention$date <- as.Date(intervention$date)


### Plot cases and intervnetions
zp_desc <- ggplot(corona_subset.df[corona_subset.df$date <= "2021-11-01",], 
                 aes(date, new_cases_smoothed_per_million))+
  geom_line(aes(colour = countryname, group = countryname),  lwd = 1) +
  geom_vline(aes(xintercept = date, colour = countryname),
             data = intervention, lty = 2, lwd = 1, show.legend = FALSE) + 
  scale_color_viridis_d(option = "turbo", direction = -1, begin = 0.2, end = 0.9) +
  scale_x_date(expand = c(0,0)) +
  coord_cartesian(clip = "off", xlim = as.Date(c("2021-03-01", "2021-11-01"))) +
  labs(y = paste0("COVID-19 cases per million population"),
       x = "Date",
       title = "Daily COVID-19 cases per capita and dates of COVID certificates") + 
  theme_bw() + 
  theme(text = element_text( size = 18),
        legend.position = c(0.01,0.99), legend.justification = c("left", "top"),
        #legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size = 16),
        axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black",)) 
  # ggrepel::geom_text_repel(data = corona_subset.df[corona_subset.df$date == "2021-10-20",] , 
  #                          aes(label = countryname, x = date, y = new_cases_smoothed_per_million, color = countryname, size = 20), 
  #                                         direction = "y", hjust = 0.5, nudge_x = 10, box.padding = 1, fontface = 'bold', show.legend = FALSE,
  #                          xlim = as.Date(c("2021-10-20", "2021-11-10"))) +
  # ggrepel::geom_text_repel(data = intervention, 
  #                          aes(label = countryname, x = date, y = 1200, color = countryname, size = 20), 
  #                          direction = "y", hjust = 0.5, nudge_x = -6, nudge_y = 100, box.padding = 1, fontface = 'bold', show.legend = FALSE)

zp_desc


png(file = paste0("../03_Output/", "Cases_Descriptives_", "all", ".png"), width = 16, height = 10, units = "in", res = 300)
zp_desc
dev.off()

pdf(file = paste0("../03_Output/", "Cases_Descriptives_", "all", ".pdf"), width = 16, height = 10)
zp_desc
dev.off()

cairo_ps(file = paste0("../03_Output/", "Cases_Descriptives_", "all", ".eps"), width = 16, height = 10)
zp_desc
dev.off()




### Plot Deaths and intervnetions
zp_desc2 <- ggplot(corona_subset.df[corona_subset.df$date <= "2021-10-30",], 
                  aes(date, new_deaths_smoothed_per_million))+
  geom_line(aes(colour = countryname, group = countryname),  lwd = 1) +
  geom_vline(aes(xintercept = date, colour = countryname),
             data = intervention, lty = 2, lwd = 1, show.legend = FALSE) + 
  scale_color_viridis_d(option = "turbo", direction = -1, begin = 0.2, end = 0.9) +
  scale_x_date(expand = c(0,0)) +
  coord_cartesian(clip = "off", xlim = as.Date(c("2021-03-01", "2021-10-30"))) +
  labs(y = paste0("COVID-19 deaths per mio capita"),
       x = "Date",
       title = "Daily COVID-19 deaths / capita and dates of COVID certificates") + 
  theme_bw() + 
  theme(text = element_text( size = 18),
        legend.position = "bottom",
        #legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size = 16),
        axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black",),
        plot.margin = unit(c(0.1, 3, 0.1, 0.1), "cm")
  ) +
  ggrepel::geom_text_repel(data = corona_subset.df[corona_subset.df$date == "2021-10-20",] , 
                           aes(label = countryname, x = date, y = new_deaths_smoothed_per_million, color = countryname, size = 20), 
                           direction = "y", hjust = 0.5, nudge_x = 10, box.padding = 1, fontface = 'bold', show.legend = FALSE,
                           xlim = as.Date(c("2021-10-20", "2021-11-10"))) +
  ggrepel::geom_text_repel(data = intervention, 
                           aes(label = countryname, x = date, y = 7.906, color = countryname, size = 20), 
                           direction = "y", hjust = 0.5, nudge_x = -6, nudge_y = 100, box.padding = 1, fontface = 'bold', show.legend = FALSE)

zp_desc2


png(file = paste0("../03_Output/", "Deaths_Descriptives_", "all", ".png"), width = 16, height = 10, units = "in", res = 300)
zp_desc2
dev.off()

pdf(file = paste0("../03_Output/", "Deaths_Descriptives_", "all", ".pdf"), width = 16, height = 10)
zp_desc2
dev.off()

cairo_ps(file = paste0("../03_Output/", "Deaths_Descriptives_", "all", ".eps"), width = 16, height = 10)
zp_desc2
dev.off()




### Plot Vaccinations and intervnetions
zp_desc3 <- ggplot(corona_subset.df[corona_subset.df$date <= "2021-10-20",], 
                   aes(date, new_vaccinations_smoothed_per_million))+
  geom_line(aes(colour = countryname, group = countryname),  lwd = 1) +
  geom_vline(aes(xintercept = date, colour = countryname),
             data = intervention, lty = 2, lwd = 1, show.legend = FALSE) + 
  scale_color_viridis_d(option = "turbo", direction = -1, begin = 0.2, end = 0.9) +
  scale_x_date(expand = c(0,0)) +
  coord_cartesian(clip = "off", xlim = as.Date(c("2021-03-01", "2021-10-20"))) +
  labs(y = paste0("Daily vaccine doses per mio capita"),
       x = "Date",
       title = "Daily vaccine doses / capita and dates of COVID certificates") + 
  theme_bw() + 
  theme(text = element_text( size = 18),
        legend.position = "bottom",
        #legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size = 16),
        axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black",),
        plot.margin = unit(c(0.1, 3, 0.1, 0.1), "cm")
  ) +
  ggrepel::geom_text_repel(data = corona_subset.df[corona_subset.df$date == "2021-10-20",] , 
                           aes(label = countryname, x = date, y = new_vaccinations_smoothed_per_million, color = countryname, size = 20), 
                           direction = "y", hjust = 0.5, nudge_x = 10, box.padding = 1, fontface = 'bold', show.legend = FALSE,
                           xlim = as.Date(c("2021-10-20", "2021-11-10"))) +
  ggrepel::geom_text_repel(data = intervention, 
                           aes(label = countryname, x = date, y = 15334, color = countryname, size = 20), 
                           direction = "y", hjust = 0.5, nudge_x = -6, nudge_y = 100, box.padding = 1, fontface = 'bold', show.legend = FALSE)

zp_desc3


png(file = paste0("../03_Output/", "Vaccinations_Descriptives_", "all", ".png"), width = 16, height = 10, units = "in", res = 300)
zp_desc3
dev.off()

pdf(file = paste0("../03_Output/", "Vaccinations_Descriptives_", "all", ".pdf"), width = 16, height = 10)
zp_desc3
dev.off()

cairo_ps(file = paste0("../03_Output/", "Vaccinations_Descriptives_", "all", ".eps"), width = 16, height = 10)
zp_desc3
dev.off()




#### ----------------------------------------------- ####
#### Age-specific Descriptives for treated countries ####
#### ----------------------------------------------- ####


##############################
#### Load population data ####
##############################


data(pop)

popF <-  popF[, c("name", "age", "2020")]
names(popF)[3] <- "pop_f"
popM <-  popM[, c("name", "age", "2020")]
names(popM)[3] <- "pop_m"

population.df <- merge(popF, popM, by = c("name", "age"))
population.df$pop_f <- population.df$pop_f * 1000
population.df$pop_m <- population.df$pop_m * 1000
population.df$pop <- population.df$pop_f + population.df$pop_m




###################################################
#### Add age specific vaccination rates Israel ####
###################################################
# https://data.gov.il/dataset/covid-19/resource/57410611-936c-49a6-ac3c-838171055b1f

vaccine_isr.df <- read.csv("vaccinated-per-day-2021-11-06.csv",
                       header = TRUE, stringsAsFactors = FALSE)
names(vaccine_isr.df) <- tolower(names(vaccine_isr.df))
names(vaccine_isr.df)[1] <- "date"

vaccine_isr.df$date <- as.Date(vaccine_isr.df$date)

### Replace < 15 by random number
set.seed(12313)
for(v in c("first_dose", "second_dose", "third_dose")){
  oo <- which(vaccine_isr.df[, v] == "<15")
  l <- length(oo)
  vaccine_isr.df[oo, v] <- sample(0:15, l, replace = TRUE)
  vaccine_isr.df[, v] <- as.numeric(vaccine_isr.df[, v])
}

### Vaccines total (without boosters)
vaccine_isr.df$vaccines_total <- vaccine_isr.df$first_dose + vaccine_isr.df$second_dose


### Israel population
pop.df <- population.df[population.df$name == "Israel", ]

# Aggregate to relevant groups
pop.df$age_group <- NA
age_list <-  c("0-4" = "0-19", 
               "5-9" = "0-19", 
               "10-14" = "0-19", 
               "15-19" = "0-19", 
               "20-24" = "20-29", 
               "25-29" = "20-29", 
               "30-34" = "30-39", 
               "35-39" = "30-39", 
               "40-44" = "40-49", 
               "45-49" = "40-49", 
               "50-54" = "50-59", 
               "55-59" = "50-59", 
               "60-64" = "60-69", 
               "65-69" = "60-69", 
               "70-74" = "70-79", 
               "75-79" = "70-79", 
               "80-84" = "80+", 
               "85-89" = "80+", 
               "90-94" = "80+", 
               "95-99" = "80+",
               "100+" = "80+")
for(i in 1:length(age_list)){
  pop.df$age_group[which(pop.df$age == names(age_list)[i])] <- age_list[[i]]
}

pop.df <- aggregate(pop.df[, c("pop", "pop_f", "pop_m")],
                            by = list(age_group = pop.df$age_group),
                            FUN = function(x) sum (x))

### Aggregate vaccine data
vaccine_isr.df$age_group[vaccine_isr.df$age_group %in% c("80-89", "90+")] <- "80+"
vaccine_isr.df <- aggregate(vaccine_isr.df[, -c(1:2)],
                            by = list(date = vaccine_isr.df$date,
                                      age_group = vaccine_isr.df$age_group),
                            FUN = function(x) sum (x))
  
# Merge
vaccine_isr.df <- merge(vaccine_isr.df, pop.df[, c("age_group", "pop")], 
                        by = "age_group", all.x = TRUE)

# Per mio inhabs
vaccine_isr.df$vaccines_total_permil <- vaccine_isr.df$vaccines_total / (vaccine_isr.df$pop/1000000)
vaccine_isr.df$vaccines_first_permil <- vaccine_isr.df$first_dose / (vaccine_isr.df$pop/1000000)


### Plot descriptives
treat <- as.Date("2021-07-29")
start <- treat - 60
end <- treat + 60

zp_isr <- ggplot(vaccine_isr.df[vaccine_isr.df$date > start & vaccine_isr.df$date < end, ], 
                 aes(date, vaccines_total_permil))+
  geom_smooth(aes(colour = age_group, group = age_group), span = 0.15, se = FALSE,lwd = 1.0) +
  geom_vline(xintercept = treat, colour = gray(1/2), lty = 1, lwd = 1) + 
  #scale_x_continuous(limits = c(-60, 30), breaks = seq(-60, 30, 20)) + 
  scale_color_viridis_d(option = "turbo", direction = -1) +
  labs(y = paste0("Daily vaccine doses per mio capita"),
       x = "Date",
       title = "Israel: Age-specific vaccinations") + 
  theme_bw() + 
  theme(text = element_text( size = 16),
        legend.position = "bottom",
        #legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size = 14),
        axis.title.x = element_text(colour = "black", margin = margin(t = 10, r = 0 , b = 0, l = 0)),
        axis.title.y = element_text(colour = "black", margin = margin(t = 0, r = 10 , b = 0, l = 0)),
        axis.text.x = element_text(colour = "black"),
  )
# c1_p2 <- c1_p2 + guides(fill = guide_legend(title = "N treated", reverse = T))
zp_isr


png(file = paste0("../03_Output/", "Descriptives_", "Israel", ".png"), width = 10, height = 8, units = "in", res = 300)
zp_isr
dev.off()



### Only first
zp_isr1 <- ggplot(vaccine_isr.df[vaccine_isr.df$date > start & vaccine_isr.df$date < end, ], 
                 aes(date, vaccines_first_permil))+
  geom_smooth(aes(colour = age_group, group = age_group), span = 0.15, se = FALSE,lwd = 1.0) +
  geom_vline(xintercept = treat, colour = gray(1/2), lty = 1, lwd = 1) + 
  #scale_x_continuous(limits = c(-60, 30), breaks = seq(-60, 30, 20)) + 
  scale_color_viridis_d(option = "turbo", direction = -1) +
  labs(y = paste0("Daily vaccine doses per mio capita"),
       x = "Date",
       title = "Israel: Age-specific vaccinations") + 
  theme_bw() + 
  theme(text = element_text( size = 16),
        legend.position = "bottom",
        #legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size = 14),
        axis.title.x = element_text(colour = "black", margin = margin(t = 10, r = 0 , b = 0, l = 0)),
        axis.title.y = element_text(colour = "black", margin = margin(t = 0, r = 10 , b = 0, l = 0)),
        axis.text.x = element_text(colour = "black"),
  )
# c1_p2 <- c1_p2 + guides(fill = guide_legend(title = "N treated", reverse = T))
zp_isr1



###################################################
#### Add age specific vaccination rates France ####
###################################################
# https://www.data.gouv.fr/en/datasets/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/



vaccine_fra.df <- read.csv("vacsi-a-fra-2021-11-08-19h08.csv",
                           header = TRUE, stringsAsFactors = FALSE, sep = ";")
names(vaccine_fra.df) <- tolower(names(vaccine_fra.df))
names(vaccine_fra.df)[3] <- "date"

vaccine_fra.df$date <- as.Date(vaccine_fra.df$date)

### Vaccines total (without boosters)
vaccine_fra.df$vaccines_total <- vaccine_fra.df$n_dose1 + vaccine_fra.df$n_complet


### France popultaion
pop.df <- read.csv("vacsi-tot-a-fra-2021-11-08-19h09.csv",
                           header = TRUE, stringsAsFactors = FALSE, sep = ";")
names(pop.df) <- tolower(names(vaccine_fra.df))

# Merge
vaccine_fra.df <- merge(vaccine_fra.df, pop.df[, c("clage_vacsi", "pop")], 
                        by = "clage_vacsi", all.x = TRUE)


### Aggregate vaccination age groups
vaccine_fra.df$age_group <- NA
age_list <- list("04" = "0-17",
                 "09" = "0-17",
                 "11" = "0-17",
                 "17" = "0-17",
                 "24" = "18-29",
                 "29" = "18-29",
                 "39" = "30-39",
                 "49" = "40-49",
                 "59" = "50-59",
                 "64" = "60-69",
                 "69" = "60-69",
                 "74" = "70-79",
                 "79" = "70-79",
                 "80" = "80+")
for(i in 1:length(age_list)){
  vaccine_fra.df$age_group[which(vaccine_fra.df$clage_vacsi == as.numeric(names(age_list))[i])] <- age_list[[i]]
}
vaccine_fra.df <- aggregate(vaccine_fra.df[, c("n_dose1", "n_complet", "vaccines_total", "pop")],
                    by = list(date = vaccine_fra.df$date,
                              age_group = vaccine_fra.df$age_group),
                    FUN = function(x) sum (x))

# Merge
vaccine_fra.df <- merge(vaccine_fra.df, pop.df, by = "age_group", all.x = TRUE)

# Per mio inhabs

vaccine_fra.df$vaccines_total_permil <- vaccine_fra.df$vaccines_total / (vaccine_fra.df$pop/1000000)
vaccine_fra.df$vaccines_first_permil <- vaccine_fra.df$n_dose1 / (vaccine_fra.df$pop/1000000)


### Plot descriptives
treat <- as.Date("2021-08-09")
start <- treat - 60
end <- treat + 60

zp_fra <- ggplot(vaccine_fra.df[vaccine_fra.df$date > start & vaccine_fra.df$date < end, ], 
                 aes(date, vaccines_total_permil))+
  geom_smooth(aes(colour = age_group, group = age_group), span = 0.15, se = FALSE,lwd = 1.0) +
  geom_vline(xintercept = treat, colour = gray(1/2), lty = 1, lwd = 1) + 
  #scale_x_continuous(limits = c(-60, 30), breaks = seq(-60, 30, 20)) + 
  scale_color_viridis_d(option = "turbo", direction = -1) +
  labs(y = paste0("Daily vaccine doses per mio capita"),
       x = "Date",
       title = "France: Age-specific vaccinations") + 
  theme_bw() + 
  theme(text = element_text( size = 16),
        legend.position = "bottom",
        #legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size = 14),
        axis.title.x = element_text(colour = "black", margin = margin(t = 10, r = 0 , b = 0, l = 0)),
        axis.title.y = element_text(colour = "black", margin = margin(t = 0, r = 10 , b = 0, l = 0)),
        axis.text.x = element_text(colour = "black"),
  )
# c1_p2 <- c1_p2 + guides(fill = guide_legend(title = "N treated", reverse = T))
zp_fra


png(file = paste0("../03_Output/", "Descriptives_", "France", ".png"), width = 10, height = 8, units = "in", res = 300)
zp_fra
dev.off()



### only first
zp_fra1 <- ggplot(vaccine_fra.df[vaccine_fra.df$date > start & vaccine_fra.df$date < end, ], 
                 aes(date, vaccines_first_permil))+
  geom_smooth(aes(colour = age_group, group = age_group), span = 0.15, se = FALSE,lwd = 1.0) +
  geom_vline(xintercept = treat, colour = gray(1/2), lty = 1, lwd = 1) + 
  #scale_x_continuous(limits = c(-60, 30), breaks = seq(-60, 30, 20)) + 
  scale_color_viridis_d(option = "turbo", direction = -1) +
  labs(y = paste0("Daily vaccine doses per mio capita"),
       x = "Date",
       title = "France: Age-specific vaccinations") + 
  theme_bw() + 
  theme(text = element_text( size = 16),
        legend.position = "bottom",
        #legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size = 14),
        axis.title.x = element_text(colour = "black", margin = margin(t = 10, r = 0 , b = 0, l = 0)),
        axis.title.y = element_text(colour = "black", margin = margin(t = 0, r = 10 , b = 0, l = 0)),
        axis.text.x = element_text(colour = "black"),
  )
# c1_p2 <- c1_p2 + guides(fill = guide_legend(title = "N treated", reverse = T))
zp_fra1





########################################################
#### Add age specific vaccination rates Switzerland ####
########################################################
# https://www.covid19.admin.ch/en/vaccination/persons



### Weekly vaccines per age group

vaccine_che.df <- read.csv("COVID19VaccDosesAdministered_AKL10_w.csv",
                           header = TRUE, stringsAsFactors = FALSE)

# Aggregate regions
vaccine_che.df <- aggregate(vaccine_che.df[, c("entries", "sumTotal", "pop")],
                            by = list(date = vaccine_che.df$date,
                                      age_group = vaccine_che.df$altersklasse_covid19),
                            FUN = function(x) sum(x))

### Daily total vaccines
vaccine_tot.df <- read.csv("COVID19VaccDosesAdministered.csv",
                           header = TRUE, stringsAsFactors = FALSE)

# Aggregate regions
vaccine_tot.df <- aggregate(vaccine_tot.df[, c("mean7d"), drop = FALSE],
                            by = list(date = vaccine_tot.df$date),
                            FUN = function(x) sum(x))

# Share on each week
vaccine_tot.df$week <- ISOweek(vaccine_tot.df$date)
vaccine_tot.df$doses_sum <- ave(vaccine_tot.df$mean7d,
                                vaccine_tot.df$week,
                               FUN = function(x) sum(x))
vaccine_tot.df$doses_share <- vaccine_tot.df$mean7d / vaccine_tot.df$doses_sum

### Merge weekly data and distribute
names(vaccine_che.df)[1] <- "week"
vaccine_che.df$week <- paste0(substr(vaccine_che.df$week, 1, 4), "-W", substr(vaccine_che.df$week, 5, 6))
vaccine_che.df <- merge(vaccine_tot.df[,which(names(vaccine_tot.df) != "entries")], 
                        vaccine_che.df, by = "week", all = TRUE)
vaccine_che.df$vaccines_total <- vaccine_che.df$entries * vaccine_che.df$doses_share


# Per mio inhabs
vaccine_che.df$vaccines_total_permil <- vaccine_che.df$vaccines_total / (vaccine_che.df$pop/1000000)


### Drop overlapping groups (and 0-9 without any info)
drop <- c("12 - 15", "16 - 64", "65+", "0 - 9")
vaccine_che.df <- vaccine_che.df[which(! vaccine_che.df$age_group %in% drop), ]
vaccine_che.df$age_group <- gsub(" ", "", vaccine_che.df$age_group)


### Plot descriptives
treat <- as.Date("2021-09-13")
start <- treat - 60
end <- treat + 60

vaccine_che.df$date <- as.Date(vaccine_che.df$date)

zp_che <- ggplot(vaccine_che.df[vaccine_che.df$date > start & vaccine_che.df$date < end, ], 
                 aes(date, vaccines_total_permil))+
  geom_smooth(aes(colour = age_group, group = age_group), span = 0.15, se = FALSE,lwd = 1.0) +
  geom_vline(xintercept = treat, colour = gray(1/2), lty = 1, lwd = 1) + 
  #scale_x_continuous(limits = c(-60, 30), breaks = seq(-60, 30, 20)) + 
  scale_color_viridis_d(option = "turbo", direction = -1) +
  labs(y = paste0("Daily vaccine doses per mio capita"),
       x = "Date",
       title = "Switzerland: Age-specific vaccinations") + 
  theme_bw() + 
  theme(text = element_text( size = 16),
        legend.position = "bottom",
        #legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size = 14),
        axis.title.x = element_text(colour = "black", margin = margin(t = 10, r = 0 , b = 0, l = 0)),
        axis.title.y = element_text(colour = "black", margin = margin(t = 0, r = 10 , b = 0, l = 0)),
        axis.text.x = element_text(colour = "black"),
  )
# c1_p2 <- c1_p2 + guides(fill = guide_legend(title = "N treated", reverse = T))
zp_che


png(file = paste0("../03_Output/", "Descriptives_", "Switzerland", ".png"), width = 10, height = 8, units = "in", res = 300)
zp_che
dev.off()



### Switzerland first (larger event and disco introduction)
treat1 <- as.Date("2021-07-01")
treat2 <- as.Date("2021-09-13")
start <- treat1 - 30
end <- treat2 + 30

vaccine_che.df$date <- as.Date(vaccine_che.df$date)

zp_che1 <- ggplot(vaccine_che.df[vaccine_che.df$date > start & vaccine_che.df$date < end, ], 
                 aes(date, vaccines_total_permil))+
  geom_smooth(aes(colour = age_group, group = age_group), span = 0.15, se = FALSE,lwd = 1.0) +
  geom_vline(xintercept = treat1, colour = gray(1/2), lty = 2, lwd = 1) + 
  geom_vline(xintercept = treat2, colour = gray(1/2), lty = 1, lwd = 1) + 
  #scale_x_continuous(limits = c(-60, 30), breaks = seq(-60, 30, 20)) + 
  scale_color_viridis_d(option = "turbo", direction = -1) +
  labs(y = paste0("Daily vaccine doses per mio capita"),
       x = "Date",
       title = "Switzerland: Age-specific vaccinations") + 
  theme_bw() + 
  theme(text = element_text( size = 16),
        legend.position = "bottom",
        #legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size = 14),
        axis.title.x = element_text(colour = "black", margin = margin(t = 10, r = 0 , b = 0, l = 0)),
        axis.title.y = element_text(colour = "black", margin = margin(t = 0, r = 10 , b = 0, l = 0)),
        axis.text.x = element_text(colour = "black"),
  )
# c1_p2 <- c1_p2 + guides(fill = guide_legend(title = "N treated", reverse = T))
zp_che1


png(file = paste0("../03_Output/", "Descriptives_", "Switzerland_1", ".png"), width = 9, height = 6, units = "in", res = 300)
zp_che1
dev.off()

pdf(file = paste0("../03_Output/", "Descriptives_", "Switzerland_1", ".pdf"), width = 9, height = 6)
zp_che1
dev.off()

cairo_ps(file = paste0("../03_Output/", "Descriptives_", "Switzerland_1", ".eps"), width = 9, height = 6)
zp_che1
dev.off()




##################################################
#### Add age specific vaccination rates Italy ####
##################################################
# https://github.com/italia/covid19-opendata-vaccini



### Daily vaccines per age group
vaccine_ita.df <- read.csv("https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/somministrazioni-vaccini-latest.csv",
                           header = TRUE, stringsAsFactors = FALSE)
names(vaccine_ita.df)[which(names(vaccine_ita.df) == "fascia_anagrafica")] <- "age_group"


### Population data
pop.df <- read.csv("https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/platea.csv",
                   header = TRUE, stringsAsFactors = FALSE)
names(pop.df)[which(names(pop.df) == "fascia_anagrafica")] <- "age_group"
names(pop.df)[which(names(pop.df) == "totale_popolazione")] <- "pop"

# Aggregate
pop.df <- aggregate(pop.df[, c("pop"), drop = FALSE],
                            by = list(age_group = pop.df$age_group),
                            FUN = function(x) sum(x))


### Aggregate vaccinations regions

# Combine 80+ group
vaccine_ita.df$age_group[vaccine_ita.df$age_group %in% c("80-89", "90+")] <- "80+"

# Aggregate
vaccine_ita.df <- aggregate(vaccine_ita.df[, c("prima_dose", "seconda_dose", "pregressa_infezione")],
                            by = list(date = vaccine_ita.df$data_somministrazione,
                                      age_group = vaccine_ita.df$age_group),
                            FUN = function(x) sum(x))

### Merge vaccines and population
vaccine_ita.df <- merge(vaccine_ita.df, pop.df, all.x = TRUE,
                        by = c("age_group"))



# Total vaccines (add pregressa_infezione: people who are fully immunized after 1 dose )
vaccine_ita.df$vaccines_total <- vaccine_ita.df$prima_dose + vaccine_ita.df$seconda_dose + vaccine_ita.df$pregressa_infezione


# Per mio inhabs
vaccine_ita.df$vaccines_total_permil <- vaccine_ita.df$vaccines_total / (vaccine_ita.df$pop/1000000)
vaccine_ita.df$vaccines_first_permil <- (vaccine_ita.df$prima_dose + vaccine_ita.df$pregressa_infezione) / (vaccine_ita.df$pop/1000000)



### Plot descriptives
treat <- as.Date("2021-08-06")
start <- treat - 60
end <- treat + 60

vaccine_ita.df$date <- as.Date(vaccine_ita.df$date)

zp_ita <- ggplot(vaccine_ita.df[vaccine_ita.df$date > start & vaccine_ita.df$date < end, ], 
                 aes(date, vaccines_total_permil))+
  geom_smooth(aes(colour = age_group, group = age_group), span = 0.15, se = FALSE,lwd = 1.0) +
  geom_vline(xintercept = treat, colour = gray(1/2), lty = 1, lwd = 1) + 
  #scale_x_continuous(limits = c(-60, 30), breaks = seq(-60, 30, 20)) + 
  scale_color_viridis_d(option = "turbo", direction = -1) +
  labs(y = paste0("Daily vaccine doses per mio capita"),
       x = "Date",
       title = "Italy: Age-specific vaccinations") + 
  theme_bw() + 
  theme(text = element_text( size = 16),
        legend.position = "bottom",
        #legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        strip.background = element_rect(fill = NA, colour = NA),
        axis.title.x = element_text(colour = "black", margin = margin(t = 10, r = 0 , b = 0, l = 0)),
        axis.title.y = element_text(colour = "black", margin = margin(t = 0, r = 10 , b = 0, l = 0)),
        axis.text.x = element_text(colour = "black"),
  )
# c1_p2 <- c1_p2 + guides(fill = guide_legend(title = "N treated", reverse = T))
zp_ita


png(file = paste0("../03_Output/", "Descriptives_", "Italy", ".png"), width = 10, height = 8, units = "in", res = 300)
zp_ita
dev.off()



### only first

zp_ita1 <- ggplot(vaccine_ita.df[vaccine_ita.df$date > start & vaccine_ita.df$date < end, ], 
                 aes(date, vaccines_first_permil))+
  geom_smooth(aes(colour = age_group, group = age_group), span = 0.15, se = FALSE,lwd = 1.0) +
  geom_vline(xintercept = treat, colour = gray(1/2), lty = 1, lwd = 1) + 
  #scale_x_continuous(limits = c(-60, 30), breaks = seq(-60, 30, 20)) + 
  scale_color_viridis_d(option = "turbo", direction = -1) +
  labs(y = paste0("Daily vaccine doses per mio capita"),
       x = "Date",
       title = "Italy: Age-specific vaccinations") + 
  theme_bw() + 
  theme(text = element_text( size = 16),
        legend.position = "bottom",
        #legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        strip.background = element_rect(fill = NA, colour = NA),
        axis.title.x = element_text(colour = "black", margin = margin(t = 10, r = 0 , b = 0, l = 0)),
        axis.title.y = element_text(colour = "black", margin = margin(t = 0, r = 10 , b = 0, l = 0)),
        axis.text.x = element_text(colour = "black"),
  )
# c1_p2 <- c1_p2 + guides(fill = guide_legend(title = "N treated", reverse = T))
zp_ita1




#############################
#### Add to combined plot####
#############################


png(file = paste0("../03_Output/", "Descriptives_", "comb", ".png"), width = 16, height = 12, units = "in", res = 300)
grid.arrange(grobs = list(zp_fra + ggtitle("France"),
                          zp_isr + ggtitle("Israel"), 
                          zp_ita + ggtitle("Italy"),
                          zp_che + ggtitle("Switzerland")),
             ncol = 2, 
             top = textGrob("Age-specific vaccinations rates / capita", gp = gpar(fontsize = 22))
)
dev.off()

pdf(file = paste0("../03_Output/", "Descriptives_", "comb", ".pdf"), width = 16, height = 12)
grid.arrange(grobs = list(zp_fra + ggtitle("France"),
                          zp_isr + ggtitle("Israel"), 
                          zp_ita + ggtitle("Italy"),
                          zp_che + ggtitle("Switzerland")),
             ncol = 2, 
             top = textGrob("Age-specific vaccinations rates / capita", gp = gpar(fontsize = 22))
)
dev.off()

cairo_ps(file = paste0("../03_Output/", "Descriptives_", "comb", ".eps"), width = 16, height = 12)
grid.arrange(grobs = list(zp_fra + ggtitle("France"),
                          zp_isr + ggtitle("Israel"), 
                          zp_ita + ggtitle("Italy"),
                          zp_che + ggtitle("Switzerland")),
             ncol = 2, 
             top = textGrob("Age-specific vaccinations rates / capita", gp = gpar(fontsize = 22))
)
dev.off()



png(file = paste0("../03_Output/", "Descriptives_", "comb_first", ".png"), width = 16, height = 12, units = "in", res = 300)
grid.arrange(grobs = list(zp_fra1 + ggtitle("France"),
                          zp_isr1 + ggtitle("Israel"), 
                          zp_ita1 + ggtitle("Italy")),
             ncol = 2, 
             top = textGrob("Age-specific vaccinations rates / capita (1st dose only)", gp = gpar(fontsize = 22))
)
dev.off()



pdf(file = paste0("../03_Output/", "Descriptives_", "comb_first", ".pdf"), width = 16, height = 12)
grid.arrange(grobs = list(zp_fra1 + ggtitle("France"),
                          zp_isr1 + ggtitle("Israel"), 
                          zp_ita1 + ggtitle("Italy")),
             ncol = 2, 
             top = textGrob("Age-specific vaccinations rates / capita (1st dose only)", gp = gpar(fontsize = 22))
)
dev.off()


cairo_ps(file = paste0("../03_Output/", "Descriptives_", "comb_first", ".eps"), width = 16, height = 12)
grid.arrange(grobs = list(zp_fra1 + ggtitle("France"),
                          zp_isr1 + ggtitle("Israel"), 
                          zp_ita1 + ggtitle("Italy")),
             ncol = 2, 
             top = textGrob("Age-specific vaccinations rates / capita (1st dose only)", gp = gpar(fontsize = 22))
)
dev.off()

