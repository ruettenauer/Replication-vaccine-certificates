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




##############################################
#### Construct new variables for analysis ####
##############################################

### Numerical date
corona_all.df$date <- as.Date(corona_all.df$date, format = "%Y%m%d")
corona_all.df$date_num <- as.numeric(corona_all.df$date)

### date group (3 dates intervals)
corona_all.df$date_gr <- cut(corona_all.df$date_num, breaks = seq(min(corona_all.df$date_num), max(corona_all.df$date_num)+3, 3),
                             include.lowest = TRUE, right = FALSE)



### NPI index
# This is similar to stringency index but uses only a subset of measures
# See codebook: https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/index_methodology.md

# Subindizes
vars <- c("c1_schoolclosing"                 ,
          "c2_workplaceclosing"              ,
          "c3_cancelpublicevents"            ,
          "c4_restrictionsongatherings"      ,
          "c5_closepublictransport"          ,
          "c6_stayathomerequirements"        ,
          "c7_restrictionsoninternalmovement",
          "c8_internationaltravelcontrols"   ,
          "h8_protectionofelderlypeople"    ,
          "h2_testingpolicy"                 ,
          "h3_contacttracing"                ,
          "h6_facialcoverings")

for(v in vars){
  v1 <- paste0(substr(v, 1, 2), "_flag")
  x <- corona_all.df[, v]
  max <- max(corona_all.df[, v], na.rm = TRUE)
  if(v %in% c("c8_internationaltravelcontrols", "h2_testingpolicy", "h3_contacttracing")){ # No flag for international travel
    f <- rep(0, length(x))
  }else{
    f <- ifelse(corona_all.df[, v1] > 0.5, 1, 0)
  }
  
  F <- ifelse(max(f, na.rm = TRUE) > 0, 1, 0)
  F_f <- F - f
  F_f <- ifelse(x <= 0.5, x, F_f) # original F - f treated as zero if x is zero, <= take 0.5 here for rounding
  ind <- 100 * ((x - 0.5 * F_f) / max)
  
  # Subindex variable
  n1 <- paste0(substr(v, 1, 2), "_subindex")
  corona_all.df[, n1] <- ind
}

# Mean of subindizes
vars <- paste0(substr(vars, 1, 2), "_subindex")
corona_all.df$npi_index <- rowMeans(corona_all.df[, vars], na.rm = TRUE)

summary(corona_all.df$npi_index)
# hist(corona_all.df$npi_index)


### Dichotomize instances (use subindex > 50)

vars <- c("c1_schoolclosing"                 ,
          "c2_workplaceclosing"              ,
          "c3_cancelpublicevents"            ,
          "c4_restrictionsongatherings"      ,
          "c5_closepublictransport"          ,
          "c6_stayathomerequirements"        ,
          "c7_restrictionsoninternalmovement",
          "c8_internationaltravelcontrols"   ,
          "h8_protectionofelderlypeople"    ,
          "h2_testingpolicy"                 ,
          "h3_contacttracing"                ,
          "h6_facialcoverings")

for(v in vars){
  v2 <- paste0(substr(v, 1, 2), "_subindex")
  n1 <- sub("_.*", "", v)
  n1 <- paste0(n1, "_impl")
  
  corona_all.df[, n1] <- NA
  corona_all.df[which(round(corona_all.df[, v2], 0) <= 50), n1] <- 0
  corona_all.df[which(round(corona_all.df[, v2], 0) > 50), n1] <- 1 # set one if rounded value == max
  
  print(n1)
  print(table(corona_all.df[, n1]))
}


#################################################
#### Add age specific vaccination rates ECDC ####
#################################################
#https://covid19-vaccine-report.ecdc.europa.eu/


vaccine.df <- read.csv("dataset_2021-W37.csv",
                        header = TRUE, stringsAsFactors = FALSE)
names(vaccine.df) <- tolower(names(vaccine.df))


# Reduce to country, group, week, aggreagte
vaccine2.df <- aggregate(vaccine.df[, c("numberdosesadministered", "first.dose", "second.dose")],
                                   by = list(countryname = vaccine.df$reportingcountry,
                                             group = vaccine.df$group,
                                             week = vaccine.df$week,
                                             group.population = vaccine.df$group.population),
                        FUN = function(x) sum (x))
vaccine2.df <- vaccine2.df[order(vaccine2.df$countryname, vaccine2.df$week, vaccine2.df$group), ]
names(vaccine2.df)[which(names(vaccine2.df) == "numberdosesadministered")] <- "vaccine_total"
names(vaccine2.df)[which(names(vaccine2.df) == "first.dose")] <- "vaccine_first"
names(vaccine2.df)[which(names(vaccine2.df) == "second.dose")] <- "vaccine_second"


# Make per mio inhabitants
vaccine2.df$vaccine_total_pc <- vaccine2.df$vaccine_total / (vaccine2.df$group.population/1000000)
vaccine2.df$vaccine_first_pc <- vaccine2.df$vaccine_first / (vaccine2.df$group.population/1000000)
vaccine2.df$vaccine_second_pc <- vaccine2.df$vaccine_second / (vaccine2.df$group.population/1000000)


# Make wide 
vaccine2.df$group <- tolower(vaccine2.df$group)
vaccine2.df$group <- gsub("1_", "", vaccine2.df$group)
vaccine2.df$group <- gsub("<", "below", vaccine2.df$group, fixed = TRUE)
vaccine2.df$group <- gsub("+", "andabove", vaccine2.df$group, fixed = TRUE)
table(vaccine2.df$group)

vaccine2.df <- vaccine2.df[vaccine2.df$group != "-", ]
vaccine2.df <- tidyr::pivot_wider(vaccine2.df, id_cols = c("countryname", "week"),
                                  names_from = "group",
                                  values_from = c("vaccine_total_pc", "vaccine_first_pc", "vaccine_second_pc"))



### Add to other data

corona_all.df$week <- format(corona_all.df$date, "%G-%V")

# all matched?
all(unique(vaccine2.df$countryname) %in% unique(corona_all.df$countryname)) 

corona_all.df <- merge(corona_all.df, vaccine2.df,
                       by = c("countryname", "week"), all.x = TRUE)



### Distribute the weekly vaccine doses as the daily vaccinations
corona_all.df$doses_sum <- ave(corona_all.df$new_vaccinations_smoothed_per_million,
                               corona_all.df$iso_code, corona_all.df$week,
                               FUN = function(x) sum(x))
corona_all.df$doses_share <- corona_all.df$new_vaccinations_smoothed_per_million / corona_all.df$doses_sum
  
vars <- c("vaccine_total_pc_agebelow60",                                                                         
          "vaccine_total_pc_age60andabove",                                                                      
          "vaccine_total_pc_age10_14",                                                                            
          "vaccine_total_pc_age15_17",                                                                            
          "vaccine_total_pc_age18_24",                                                                            
          "vaccine_total_pc_age25_49",                                                                            
          "vaccine_total_pc_age50_59",                                                                            
          "vaccine_total_pc_age60_69",                                                                            
          "vaccine_total_pc_age70_79",                                                                            
          "vaccine_total_pc_age80andabove")
for(v in vars){
  corona_all.df[, paste0("daily_", v)] <- corona_all.df[, v] * corona_all.df$doses_share
}








##############################
#### Lag fully vaccinated ####
##############################

corona_all.df <- corona_all.df[order(corona_all.df$iso_code, corona_all.df$date), ]

corona_all.df$lag_fully_vaccinated <- ave(corona_all.df$fully_vaccinationed_c_imp,
                                          corona_all.df$iso_code,
                                          FUN = function(x) dplyr::lag(x, 14))


#######################
#### Reduce sample ####
#######################


### Exclude countries with zero cases
corona_all.df$meandeaths <- ave(corona_all.df$deaths_c,
                                corona_all.df$countryname,
                                FUN = function(x) mean(x, na.rm = TRUE))
summary(corona_all.df$meandeaths)
table(corona_all.df$iso_code[which(corona_all.df$meandeaths == 0 | is.na(corona_all.df$meandeaths))])

corona_sample.df <- corona_all.df[which(corona_all.df$meandeaths != 0), ]




### ----- ATTENTION: Set NA all dates with negative cases / deaths ----- ###

corona_sample.df$cases_c[which(corona_sample.df$cases_c < 0)] <- NA
corona_sample.df$deaths_c[which(corona_sample.df$deaths_c < 0)] <- NA



############################
#### Select time period ####
############################

corona_subsample.df <- corona_sample.df[corona_sample.df$date >= "2021-01-01", ]

corona_subsample.df <- corona_subsample.df[order(corona_subsample.df$iso_code, corona_subsample.df$date), ]



####-----------------------------------------####
#### Contruct indicator of vaccine passports ####
####-----------------------------------------####


### Temporally shifted passport passport
shift <- 21
corona_subsample.df <- corona_subsample.df[order(corona_subsample.df$iso_code, corona_subsample.df$date),]


### Passport indicator
corona_subsample.df$passport <- 0
corona_subsample.df$passport_shift <- 0

# Germany 2021-08-23 | counties below a specific incidence rate can be exempted depending on federl state rules (7 day rate < 35 new infections per 100,000) (https://www.bundesregierung.de/breg-de/aktuelles/bund-laender-beratung-corona-1949606)
oo <- which(corona_subsample.df$iso_code == "DEU" & corona_subsample.df$date >= "2021-08-23")
corona_subsample.df$passport[oo] <- 1
oo <- which(corona_subsample.df$iso_code == "DEU" & corona_subsample.df$date >= (as.Date("2021-08-23") - shift))
corona_subsample.df$passport_shift[oo] <- 1

# France 2021-08-09 (https://www.gouvernement.fr/en/coronavirus-covid-19, https://uk.ambafrance.org/Health-pass-and-vaccination-in-the-UK) | Mandatory for workers with contact from 2021-08-30 [and public sector from 2021-09-15?] on (https://www.connexionfrance.com/French-news/French-health-pass-pass-sanitaire-to-become-obligatory-for-public-facing-employees-of-public-venues-from-August-30)
oo <- which(corona_subsample.df$iso_code == "FRA" & corona_subsample.df$date >= "2021-08-09")
corona_subsample.df$passport[oo] <- 1
oo <- which(corona_subsample.df$iso_code == "FRA" & corona_subsample.df$date >= (as.Date("2021-08-09") - shift))
corona_subsample.df$passport_shift[oo] <- 1

# Israel 2021-07-29 (https://www.bbc.co.uk/news/world-europe-56522408, https://www.timesofisrael.com/ministers-okay-return-of-green-pass-plan-to-ban-travel-to-uk-cyprus-turkey/)
oo <- which(corona_subsample.df$iso_code == "ISR" & corona_subsample.df$date >= "2021-07-29")
corona_subsample.df$passport[oo] <- 1
oo <- which(corona_subsample.df$iso_code == "ISR" & corona_subsample.df$date >= (as.Date("2021-07-29") - shift))
corona_subsample.df$passport_shift[oo] <- 1

# Denmark 2021-04-21
oo <- which(corona_subsample.df$iso_code == "DNK" & corona_subsample.df$date >= "2021-04-21")
corona_subsample.df$passport[oo] <- 1
oo <- which(corona_subsample.df$iso_code == "DNK" & corona_subsample.df$date >= (as.Date("2021-04-21") - shift))
corona_subsample.df$passport_shift[oo] <- 1

# Italy 2021-08-06 | ITALY due to implement mandatory for workers from 2021-10-15 (https://www.bbc.co.uk/news/world-europe-58590187)
oo <- which(corona_subsample.df$iso_code == "ITA" & corona_subsample.df$date >= "2021-08-06")
corona_subsample.df$passport[oo] <- 1
oo <- which(corona_subsample.df$iso_code == "ITA" & corona_subsample.df$date >= (as.Date("2021-08-06") - shift))
corona_subsample.df$passport_shift[oo] <- 1

# Switzerland 2021-09-13 | since 2021-07-01 mandatory for nightclubs, discos + events >1,000 participants | mandatory 3G rules for hospitality and cultural venues (https://www.newsd.admin.ch/newsd/message/attachments/68144.pdf, https://www.swr.de/swraktuell/baden-wuerttemberg/suedbaden/3g-zertifikatspflicht-in-der-schweiz-100.html)
oo <- which(corona_subsample.df$iso_code == "CHE" & corona_subsample.df$date >= "2021-09-13")
corona_subsample.df$passport[oo] <- 1
oo <- which(corona_subsample.df$iso_code == "CHE" & corona_subsample.df$date >= (as.Date("2021-09-13") - shift))
corona_subsample.df$passport_shift[oo] <- 1









####---------------------------------------------####
#### Synthetic control function single countries ####
####---------------------------------------------####

synth_fun <- function(data = NULL, treat.date = NULL, treat.id = NULL,
                      control.ids = NULL,
                      dependent = NULL,
                      predictors = NULL,
                      predictors.op = "mean",
                      time.predictors.prior = -1,
                      special.predictors,
                      time = "date",
                      unit.variable = "iso_code",
                      unit.names.variable = "countryname", 
                      bwdating,
                      before = 60, after = 60,
                      yaxis = NULL, name = NULL,
                      legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99)){
  
  if(is.null(yaxis)){
    yaxis <- dependent
  }
  if(is.null(name)){
    name <- dependent
  }
  
  treat <- treat.date
  start <- treat - before
  
  corona_subsample2.df <- data[data[, time] >= (start - bwdating), ]
  
  
  corona_subsample2.df$iso_code_num <- as.numeric(as.factor(corona_subsample2.df[, unit.variable]))
  corona_subsample2.df$date_num <- as.numeric(corona_subsample2.df[, time])
  
  
  ### France
  oo <- which(corona_subsample2.df$iso_code == treat.id)
  id_treat <- unique(corona_subsample2.df$iso_code_num[oo])
  
  # Potential controls
  oo <- which(corona_subsample2.df$iso_code %in% control.ids)
  id_control <- unique(corona_subsample2.df$iso_code_num[oo])
  
  
  # Treatment period
  t_treat <- unique(corona_subsample2.df$date_num[corona_subsample2.df$date == treat])
  t_pre <- unique(corona_subsample2.df$date_num[corona_subsample2.df$date == start])
  
  # Realign around zero
  corona_subsample2.df$date_num <- corona_subsample2.df$date_num - t_treat
  t_pre <- t_pre - t_treat
  
  
  # End date
  maxd <- corona_subsample2.df[corona_subsample2.df$iso_code_num == id_treat, ]
  maxd <- max(maxd$date[complete.cases(maxd[, c(dependent)])])
  
  end <- min(after, maxd - treat )
  
  corona_subsample2.df$date_num <- corona_subsample2.df$date_num + bwdating
  
  ### Run Synth, cases
  dataprep_out1 <- dataprep(
    foo = corona_subsample2.df[,],
    predictors = predictors,
    predictors.op = predictors.op,
    time.predictors.prior = time.predictors.prior,
    special.predictors = special.predictors,
    dependent = dependent,
    unit.variable = "iso_code_num",
    unit.names.variable = "countryname",
    time.variable = "date_num",
    treatment.identifier = id_treat,
    controls.identifier = id_control,
    time.optimize.ssr = t_pre:-1,
    time.plot = (t_pre + bwdating):(end + bwdating)
  )
  
  synth_out1 <- synth(data.prep.obj = dataprep_out1)
  
  # path.plot(synth_out1, dataprep_out1)
  # gaps.plot(synth_out1, dataprep_out1)
  
  
  plac.res <- generate.placebos(
    dataprep_out1,
    synth_out1,
    Sigf.ipop = 5,
    strategy = "multiprocess"
  )
  p1 <- plot_placebos(plac.res)
  placebo.df <- p1$data
  placebo.df$diff <- placebo.df$tr - placebo.df$cont
  placebo.df$date <- as.Date(as.Date(treat) + as.numeric(placebo.df$year)  - bwdating)
  placebo.df$year <- placebo.df$year  - bwdating
  placebo.df <- placebo.df[which(!is.na(placebo.df$diff)), ]
  
  ###### Plot
  # Overall development
  data <- data.frame(dataprep_out1$Y1plot)
  names(data) <- "ATT_1"
  data$ATT_2 <- dataprep_out1$Y0plot %*% synth_out1$solution.w
  data$date <- as.Date(as.Date(treat) + as.numeric(rownames(data)) - bwdating)
  data <- tidyr::pivot_longer(data, cols = c("ATT_1", "ATT_2"),
                              names_to = "group", values_to = "ATT")
  data$group <- factor(data$group, levels = c("ATT_1", "ATT_2"), labels = c("Treated groups", "Synthetic Control"))
  
  c1_p1 <- ggplot(data, aes(date, ATT))+
    geom_line(mapping = aes(color = group, linetype = group), lwd = 1.0) +
    geom_vline(xintercept = as.Date(treat), colour = gray(1/2), lty = 1, lwd = 1) + 
    geom_vline(xintercept = as.Date(treat - bwdating), colour = gray(1/2), lty = 2, lwd = 1) + 
    #scale_x_continuous(limits = c(-60, 30), breaks = seq(-60, 30, 20)) + 
    labs(y = "Daily vaccine doses per mio capita",
         x = "Date",
         title = paste0("Development: ", name)) + 
    theme_bw() + 
    theme(text = element_text( size = 12),
          legend.position = legend.position, legend.justification = legend.justification,
          #legend.background = element_blank(),
          legend.title = element_blank(),
          axis.text.y = element_text(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title.x = element_text(colour = "black", margin = margin(t = 10, r = 0 , b = 0, l = 0)),
          axis.title.y = element_text(colour = "black", margin = margin(t = 0, r = 10 , b = 0, l = 0)),
          axis.text.x = element_text(colour = "black"),
    )
  # c1_p1 <- c1_p1 + guides(fill = guide_legend(title = "N treated", reverse = T))
  c1_p1
  
  
  
  # Vaccines 
  data <- data.frame(dataprep_out1$Y1plot - (dataprep_out1$Y0plot %*% synth_out1$solution.w))
  names(data) <- "ATT"
  data$date <- as.numeric(rownames(data)) - bwdating
  
  c1_p2 <- ggplot(data, aes(date, ATT))+
    geom_line(data = data, lwd = 1.0) +
    geom_hline(yintercept = 0, colour = "black", lty = 3, lwd = 1, alpha = 0.7)+
    geom_vline(xintercept = 0, colour = gray(1/2), lty = 1, lwd = 1) + 
    geom_line(data = placebo.df, mapping = aes(x = year, y = diff, group = id), alpha = 0.2, lwd = 0.5) +
    geom_vline(xintercept = 0 - bwdating, colour = gray(1/2), lty = 2, lwd = 1) + 
    #scale_x_continuous(limits = c(-60, 30), breaks = seq(-60, 30, 20)) + 
    labs(y = paste0("Difference in ", yaxis),
         x = "Days since intervention",
         title = paste0("Difference: ", name)) + 
    theme_bw() + 
    theme(text = element_text( size = 12),
          legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99),
          legend.background = element_blank(),
          legend.key = element_blank(),
          axis.text.y = element_text(colour = "black"),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title.x = element_text(colour = "black", margin = margin(t = 10, r = 0 , b = 0, l = 0)),
          axis.title.y = element_text(colour = "black", margin = margin(t = 0, r = 10 , b = 0, l = 0)),
          axis.text.x = element_text(colour = "black"),
    )
  # c1_p2 <- c1_p2 + guides(fill = guide_legend(title = "N treated", reverse = T))
  c1_p2
  
  return(list(c1_p1, c1_p2))
  
}




####-----------------------####
#### Define macros for all ####
####-----------------------####

predictors = c("median_age", "life_expectancy", "gdp_per_capita", "aged_70_older", "population_density")
predictors.op = "mean"
time.predictors.prior = -1

special.predictors_cases = list(
  list("new_cases_smoothed_per_million", c(-7:-1), "mean"),
  list("new_cases_smoothed_per_million", c(-14:-8), "mean"),
  list("new_cases_smoothed_per_million", c(-21:-15), "mean"),
  list("new_cases_smoothed_per_million", c(-28:-22), "mean"),
  #list("new_cases_smoothed_per_million", c(-35:-29), "mean"),
  list("total_vaccinations_per_hundred", c(-7:-1), "mean"),
  list("total_vaccinations_per_hundred", c(-14:-8), "mean"),
  list("total_vaccinations_per_hundred", c(-21:-15), "mean"),
  list("total_vaccinations_per_hundred", c(-28:-22), "mean"),
  list("new_tests_smoothed_per_thousand", c(-21:-8), "mean"),
  list("c1_subindex", c(-21:-1), "mean"),
  list("c2_subindex", c(-21:-1), "mean"),
  list("c3_subindex", c(-21:-1), "mean"),
  list("c4_subindex", c(-21:-1), "mean"),
  list("c5_subindex", c(-21:-1), "mean"),
  list("c6_subindex", c(-21:-1), "mean"),
  list("c7_subindex", c(-21:-1), "mean"),
  list("c8_subindex", c(-21:-1), "mean"),
  list("h8_subindex", c(-21:-1), "mean"),
  list("h2_subindex", c(-21:-1), "mean"),
  list("h3_subindex", c(-21:-1), "mean"),
  list("h6_subindex", c(-21:-1), "mean"))

special.predictors_vaccine = list(
  list("new_cases_smoothed_per_million", c(-7:-1), "mean"),
  list("new_cases_smoothed_per_million", c(-14:-8), "mean"),
  list("new_cases_smoothed_per_million", c(-21:-15), "mean"),
  list("new_cases_smoothed_per_million", c(-28:-22), "mean"),
  list("new_vaccinations_smoothed_per_million", c(-7:-1), "mean"),
  list("new_vaccinations_smoothed_per_million", c(-14:-8), "mean"),
  list("new_vaccinations_smoothed_per_million", c(-21:-15), "mean"),
  list("new_vaccinations_smoothed_per_million", c(-28:-22), "mean"),
  #list("new_vaccinations_smoothed_per_million", c(-35:-29), "mean"),
  list("fully_vaccinationed_c_imp", c(-21:-7), "mean"),
  list("c1_subindex", c(-21:-1), "mean"),
  list("c2_subindex", c(-21:-1), "mean"),
  list("c3_subindex", c(-21:-1), "mean"),
  list("c4_subindex", c(-21:-1), "mean"),
  list("c5_subindex", c(-21:-1), "mean"),
  list("c6_subindex", c(-21:-1), "mean"),
  list("c7_subindex", c(-21:-1), "mean"),
  list("c8_subindex", c(-21:-1), "mean"),
  list("h8_subindex", c(-21:-1), "mean"),
  list("h2_subindex", c(-21:-1), "mean"),
  list("h3_subindex", c(-21:-1), "mean"),
  list("h6_subindex", c(-21:-1), "mean"))

### Control group (Extended for Denmark)
isos <- c("AUT", "BEL", "CAN", "CZE", "ESP", "FIN", "GBR", "GRC", "HRV", "IRL", "LTU", "LUX", "NLD", "POL", "PRT", "SVK", "SVN", "SWE", "USA")







####--------------------------####
#### Synthetic control France ####
####--------------------------####

treat <- as.Date("2021-08-09")
id <- "FRA"
name <- "France"

### Synthetic control VACCINES

plot.list1_1 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                        control.ids = isos,
                        dependent = "new_vaccinations_smoothed_per_million",
                        predictors = predictors,
                        predictors.op = predictors.op,
                        time.predictors.prior = time.predictors.prior,
                        special.predictors = special.predictors_vaccine,
                        time = "date",
                        unit.variable = "iso_code",
                        unit.names.variable = "countryname",
                        bwdating = 20,
                        before = 60, after = 60,
                        yaxis = "daily vaccine doses per mio capita", 
                        name = "vaccine doses / capita",
                        legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/placebo_", "Vaccinations_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list1_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
             )
dev.off()

png(file = paste0("../03_Output/placebo_", "Vaccinations_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list1_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
             )
dev.off()





### Synthetic control CASES

plot.list1_2 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "new_cases_smoothed_per_million",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_cases,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "COVID-19 cases per mio capita", 
                         name = "Daily COVID-19 cases / capita",
                         legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99))


### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/placebo_", "Cases_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list1_2, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()

png(file = paste0("../03_Output/placebo_", "Cases_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list1_2, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()








####--------------------------####
#### Synthetic control Germany ####
####--------------------------####

treat <- as.Date("2021-08-23")
id <- "DEU"
name <- "Germany"

### Synthetic control VACCINES

plot.list2_1 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "new_vaccinations_smoothed_per_million",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_vaccine,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "daily vaccine doses per mio capita", 
                         name = "vaccine doses / capita",
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/placebo_", "Vaccinations_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list2_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()

png(file = paste0("../03_Output/placebo_", "Vaccinations_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list2_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()





### Synthetic control CASES

plot.list2_2 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "new_cases_smoothed_per_million",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_cases,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "COVID-19 cases per mio capita", 
                         name = "Daily COVID-19 cases / capita",
                         legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99))


### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/placebo_", "Cases_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list2_2, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()

png(file = paste0("../03_Output/placebo_", "Cases_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list2_2, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()





####--------------------------####
#### Synthetic control Israel ####
####--------------------------####

treat <- as.Date("2021-07-29")
id <- "ISR"
name <- "Israel"

### Synthetic control VACCINES

plot.list3_1 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "new_vaccinations_smoothed_per_million",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_vaccine,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "daily vaccine doses per mio capita", 
                         name = "vaccine doses / capita",
                         legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99))


### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/placebo_", "Vaccinations_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list3_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()

png(file = paste0("../03_Output/placebo_", "Vaccinations_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list3_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()





### Synthetic control CASES

plot.list3_2 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "new_cases_smoothed_per_million",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_cases,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "COVID-19 cases per mio capita", 
                         name = "Daily COVID-19 cases / capita",
                         legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99))


### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/placebo_", "Cases_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list3_2, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()

png(file = paste0("../03_Output/placebo_", "Cases_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list3_2, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()







####--------------------------####
#### Synthetic control Denmark ####
####--------------------------####

treat <- as.Date("2021-04-21")
id <- "DNK"
name <- "Denmark"

### Synthetic control VACCINES

plot.list4_1 <- synth_fun(data = corona_subsample.df[corona_subsample.df$date < "2021-08-01", ], 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "new_vaccinations_smoothed_per_million",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_vaccine,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "daily vaccine doses per mio capita", 
                         name = "vaccine doses / capita",
                         legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99))


### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/placebo_", "Vaccinations_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list4_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()

png(file = paste0("../03_Output/placebo_", "Vaccinations_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list4_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()





### Synthetic control CASES

plot.list4_2 <- synth_fun(data = corona_subsample.df[corona_subsample.df$date < "2021-08-01", ], 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "new_cases_smoothed_per_million",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_cases,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "COVID-19 cases per mio capita", 
                         name = "Daily COVID-19 cases / capita",
                         legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99))


### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/placebo_", "Cases_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list4_2, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()

png(file = paste0("../03_Output/placebo_", "Cases_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list4_2, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()









####--------------------------####
#### Synthetic control Italy ####
####--------------------------####

treat <- as.Date("2021-08-06")
id <- "ITA"
name <- "Italy"

### Synthetic control VACCINES

plot.list5_1 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "new_vaccinations_smoothed_per_million",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_vaccine,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "daily vaccine doses per mio capita", 
                         name = "vaccine doses / capita",
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/placebo_", "Vaccinations_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list5_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()

png(file = paste0("../03_Output/placebo_", "Vaccinations_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list5_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()





### Synthetic control CASES

plot.list5_2 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "new_cases_smoothed_per_million",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_cases,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "COVID-19 cases per mio capita", 
                         name = "Daily COVID-19 cases / capita",
                         legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99))


### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/placebo_", "Cases_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list5_2, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()

png(file = paste0("../03_Output/placebo_", "Cases_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list5_2, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()










####--------------------------####
#### Synthetic control Switzerland ####
####--------------------------####

treat <- as.Date("2021-09-13")
id <- "CHE"
name <- "Switzerland"

### Synthetic control VACCINES

plot.list6_1 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "new_vaccinations_smoothed_per_million",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_vaccine,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "daily vaccine doses per mio capita", 
                         name = "vaccine doses / capita",
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/placebo_", "Vaccinations_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list6_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()

png(file = paste0("../03_Output/placebo_", "Vaccinations_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list6_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()





### Synthetic control CASES

plot.list6_2 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "new_cases_smoothed_per_million",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_cases,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "COVID-19 cases per mio capita", 
                         name = "Daily COVID-19 cases / capita",
                         legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99))


### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/placebo_", "Cases_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list6_2, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()

png(file = paste0("../03_Output/placebo_", "Cases_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list6_2, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()





