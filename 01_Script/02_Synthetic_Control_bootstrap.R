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
library(future.apply)

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
shift <- 20
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

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Bootstrap function for parallel
bs_synth <- function(id_control_new, Y,
                     id_treat, corona_subsample2.df, 
                     predictors, predictors.op, time.predictors.prior,
                     special.predictors, dependent, t_pre, end, bwdating){
  ### new sample
  X <- data.frame(iso_code_num = c(id_treat, id_control_new))
  X$iso_code_num_new <- c(id_treat, (1000 + 1:length(id_control_new)))
  X <- merge(X, corona_subsample2.df[,], by = "iso_code_num")
  id_control_new <- 1000 + 1:length(id_control_new)
  X$countryname_new <- paste0(X$countryname, X$iso_code_num_new)
  

  tryCatch({ # Avoid stopping because of singularities
    ### Run Synth, cases
    dataprep_outbs <- dataprep(
      foo = X,
      predictors = predictors,
      predictors.op = predictors.op,
      time.predictors.prior = time.predictors.prior,
      special.predictors = special.predictors,
      dependent = dependent,
      unit.variable = "iso_code_num_new",
      unit.names.variable = "countryname_new",
      time.variable = "date_num",
      treatment.identifier = id_treat,
      controls.identifier = id_control_new,
      time.optimize.ssr = t_pre:-1,
      time.plot = (t_pre + bwdating):(end + bwdating)
    )
    synth_outbs <- synth(data.prep.obj = dataprep_outbs)
    
    Y0 <- dataprep_outbs$Y0plot 
    Y1 <- dataprep_outbs$Y1plot 
    
    w <- synth_outbs$solution.w
    names <- unique(X[, c("iso_code_num_new", "countryname")])
    names <- names$countryname[match(as.numeric(row.names(w)), names$iso_code_num_new)]
    rownames(w) <- names
    
    YC <- Y0 %*% w
    Ydiff <- Y1 - Y0 %*% w
    
    # YC.mat[, i] <- YC
    # Ydiff.mat[, i] <- Ydiff
    # Y0list[[i]] <- Y0
    # wlist[[i]]<- w
    return(list(YC, Ydiff))
    
  }, error = function(e){return(list(rep(NA, length(Y)), rep(NA, length(Y))))})
}

# Synth control function
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
                      bwdating = 0,
                      before = 60, after = 60,
                      yaxis = NULL, name = NULL,
                      legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99),
                      Rbs = 1000, alpha = 0.05, seed = 24, workers = availableCores()){
  
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
  
  ### Bootstrap
  set.seed(seed)
  
  NC <- length(id_control)
  
  Y <- dataprep_out1$Y1plot 
  
  YC.mat <- matrix(NA, nrow = nrow(dataprep_out1$Y0plot), ncol = Rbs)
  Ydiff.mat <- matrix(NA, nrow = nrow(dataprep_out1$Y0plot), ncol = Rbs)
  
  wlist <- vector(mode = "list", length = Rbs)
  Y0list <- vector(mode = "list", length = Rbs)
  idlist  <- vector(mode = "list", length = Rbs)
  
  for(i in 1:Rbs){
    idlist[[i]] <- sample(id_control, NC, replace = TRUE)
  }  
  

  # Bootstrap parallel
  plan(multisession, workers = workers)
  
  bs.res <- future_lapply(idlist, function(x) bs_synth(x, Y,
                                                       id_treat, corona_subsample2.df, 
                                                       predictors, predictors.op, time.predictors.prior,
                                                       special.predictors, dependent, t_pre, end, bwdating))
  plan("sequential")
  
  YC.mat <- lapply(bs.res, `[[`, 1)
  Ydiff.mat <- lapply(bs.res, `[[`, 2)
  
  YC.mat <- data.frame(YC.mat)
  Ydiff.mat <- data.frame(Ydiff.mat)
  
  
  
  # Percentile CIs
  pc <- c( (1 * alpha/2),  (1 * (1 - alpha/2)))
  nal <- apply(YC.mat, 1, FUN = function(x) any(is.na(x)))
  percentiles <- t(apply(YC.mat, 1, FUN = function(x) quantile(x, probs = pc, na.rm = TRUE)))
  percentiles.diff <- t(apply(Ydiff.mat, 1, FUN = function(x) quantile(x, probs = pc, na.rm = TRUE)))
  
  #percentiles[which(nal), ] <- NA
  
  
  ###### Plot
  # Overall development
  data <- data.frame(dataprep_out1$Y1plot)
  names(data) <- "ATT_1"
  data$ATT_2 <- dataprep_out1$Y0plot %*% synth_out1$solution.w
  
  data$ll2 <- percentiles[,1]
  data$ul2 <- percentiles[,2]
  data$ll2[which(is.na(data$ATT_2))] <- NA
  data$ul2[which(is.na(data$ATT_2))] <- NA
  
  
  data$date <- as.Date(as.Date(treat) + as.numeric(rownames(data)) - bwdating)
  data <- tidyr::pivot_longer(data, cols = c("ATT_1", "ATT_2"),
                              names_to = "group", values_to = "ATT")
  data$group <- factor(data$group, levels = c("ATT_1", "ATT_2"), labels = c("Treated groups", "Synthetic Control"))
  
  c1_p1 <- ggplot(data, aes(date, ATT))+
    geom_line(mapping = aes(color = group, linetype = group), lwd = 1.0) +
    geom_ribbon(mapping = aes(ymin = ll2, ymax = ul2), alpha = 0.3, fill = gg_color_hue(2)[2]) +
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
  
  data$ll <- percentiles.diff[,1]
  data$ul <- percentiles.diff[,2]
  data$ll[which(is.na(data$ATT))] <- NA
  data$ul[which(is.na(data$ATT))] <- NA
  
  data$date <- as.numeric(rownames(data)) - bwdating
  
  c1_p2 <- ggplot(data, aes(date, ATT))+
    geom_line(data = data, lwd = 1.0) +
    geom_ribbon(mapping = aes(ymin = ll, ymax = ul), alpha = 0.3) +
    geom_hline(yintercept = 0, colour = "black", lty = 3, lwd = 1, alpha = 0.7)+
    geom_vline(xintercept = 0, colour = gray(1/2), lty = 1, lwd = 1) + 
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


### Descriptive table

desc.df <- data.frame(matrix(NA, nrow = 6*2, ncol = 9))
names(desc.df)[1] <- "country"
desc.df$country <- rep(c("France", "Israel", "Italy", "Switzerland", "Denmark", "Germany"), each = 2)

intervals <- list(c(-40, -21), 
                  c(-20, 1), 
                  c(1, 20), 
                  c(21, 40))




####--------------------------####
#### Synthetic control France ####
####--------------------------####

treat <- as.Date("2021-08-09")
id <- "FRA"
name <- "France"


### Desc
c <- 2
for(i in intervals){
  s <- treat + i[1]
  e <- treat + i[2]
  oo <- which(corona_subsample.df$iso_code == id & corona_subsample.df$date >= s & corona_subsample.df$date <= e)
  mt <- round(mean(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdt <- round(sd(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdt <- paste0("(", sdt, ")")
  oo <- which(corona_subsample.df$iso_code %in% isos & corona_subsample.df$date >= s & corona_subsample.df$date <= e)
  mc <- round(mean(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdc <- round(sd(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdc <- paste0("(", sdc, ")")
  oo <- which(desc.df$country == name)
  desc.df[oo, c] <- c(mt, sdt)
  desc.df[oo, c+1] <- c(mc, sdc)
  c <- c + 2
}


### Synthetic control VACCINES
t <- Sys.time()
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
                        legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99),
                        Rbs = 1000, seed = 23)
Sys.time() - t
save(plot.list1_1, file = "plot_list1_1.RData")

### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/", "Vaccinations_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list1_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
             )
dev.off()

png(file = paste0("../03_Output/", "Vaccinations_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list1_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
             )
dev.off()





# ### Synthetic control CASES
# 
# plot.list1_2 <- synth_fun(data = corona_subsample.df,
#                          treat.date = treat,
#                          treat.id = id,
#                          control.ids = isos,
#                          dependent = "new_cases_smoothed_per_million",
#                          predictors = predictors,
#                          predictors.op = predictors.op,
#                          time.predictors.prior = time.predictors.prior,
#                          special.predictors = special.predictors_cases,
#                          time = "date",
#                          unit.variable = "iso_code",
#                          unit.names.variable = "countryname",
#                          before = 60, after = 60,
#                          yaxis = "COVID-19 cases per mio capita",
#                          name = "Daily COVID-19 cases / capita",
#                          legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99))
# 
# 
# 
# ### Export
# g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
#                      "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
#                      paste0("Control pool: ", paste(isos, collapse = ", "))),
#               gp = gpar(fontsize = 10), just = "right", x = 1)
# 
# pdf(file = paste0("../03_Output/", "Cases_Synthetic_", name, ".pdf"), width = 12, height = 6)
# grid.arrange(grobs = plot.list1_2, ncol = 2,
#              # bottom = g,
#              top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
# )
# dev.off()
# 
# png(file = paste0("../03_Output/", "Cases_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
# grid.arrange(grobs = plot.list1_2, ncol = 2,
#              # bottom = g,
#              top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
# )
# dev.off()








####--------------------------####
#### Synthetic control Germany ####
####--------------------------####

treat <- as.Date("2021-08-23")
id <- "DEU"
name <- "Germany"

### Desc
c <- 2
for(i in intervals){
  s <- treat + i[1]
  e <- treat + i[2]
  oo <- which(corona_subsample.df$iso_code == id & corona_subsample.df$date >= s & corona_subsample.df$date <= e)
  mt <- round(mean(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdt <- round(sd(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdt <- paste0("(", sdt, ")")
  oo <- which(corona_subsample.df$iso_code %in% isos & corona_subsample.df$date >= s & corona_subsample.df$date <= e)
  mc <- round(mean(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdc <- round(sd(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdc <- paste0("(", sdc, ")")
  oo <- which(desc.df$country == name)
  desc.df[oo, c] <- c(mt, sdt)
  desc.df[oo, c+1] <- c(mc, sdc)
  c <- c + 2
}

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
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99),
                         Rbs = 1000, seed = 23)

save(plot.list2_1, file = "plot_list2_1.RData")


### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/", "Vaccinations_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list2_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()

png(file = paste0("../03_Output/", "Vaccinations_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list2_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()





# ### Synthetic control CASES
# 
# plot.list2_2 <- synth_fun(data = corona_subsample.df,
#                          treat.date = treat,
#                          treat.id = id,
#                          control.ids = isos,
#                          dependent = "new_cases_smoothed_per_million",
#                          predictors = predictors,
#                          predictors.op = predictors.op,
#                          time.predictors.prior = time.predictors.prior,
#                          special.predictors = special.predictors_cases,
#                          time = "date",
#                          unit.variable = "iso_code",
#                          unit.names.variable = "countryname",
#                          bwdating = 20,
#                          before = 60, after = 60,
#                          yaxis = "COVID-19 cases per mio capita",
#                          name = "Daily COVID-19 cases / capita",
#                          legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99),
#                          Rbs = 1000, seed = 23)
# 
# 
# ### Export
# g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
#                      "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
#                      paste0("Control pool: ", paste(isos, collapse = ", "))),
#               gp = gpar(fontsize = 10), just = "right", x = 1)
# 
# pdf(file = paste0("../03_Output/", "Cases_Synthetic_", name, ".pdf"), width = 12, height = 6)
# grid.arrange(grobs = plot.list2_2, ncol = 2,
#              # bottom = g,
#              top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
# )
# dev.off()
# 
# png(file = paste0("../03_Output/", "Cases_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
# grid.arrange(grobs = plot.list2_2, ncol = 2,
#              # bottom = g,
#              top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
# )
# dev.off()





####--------------------------####
#### Synthetic control Israel ####
####--------------------------####

treat <- as.Date("2021-07-29")
id <- "ISR"
name <- "Israel"

### Desc
c <- 2
for(i in intervals){
  s <- treat + i[1]
  e <- treat + i[2]
  oo <- which(corona_subsample.df$iso_code == id & corona_subsample.df$date >= s & corona_subsample.df$date <= e)
  mt <- round(mean(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdt <- round(sd(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdt <- paste0("(", sdt, ")")
  oo <- which(corona_subsample.df$iso_code %in% isos & corona_subsample.df$date >= s & corona_subsample.df$date <= e)
  mc <- round(mean(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdc <- round(sd(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdc <- paste0("(", sdc, ")")
  oo <- which(desc.df$country == name)
  desc.df[oo, c] <- c(mt, sdt)
  desc.df[oo, c+1] <- c(mc, sdc)
  c <- c + 2
}


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
                         legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99),
                         Rbs = 1000, seed = 23)

save(plot.list3_1, file = "plot_list3_1.RData")


### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/", "Vaccinations_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list3_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()

png(file = paste0("../03_Output/", "Vaccinations_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list3_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()





# ### Synthetic control CASES
# 
# plot.list3_2 <- synth_fun(data = corona_subsample.df,
#                          treat.date = treat,
#                          treat.id = id,
#                          control.ids = isos,
#                          dependent = "new_cases_smoothed_per_million",
#                          predictors = predictors,
#                          predictors.op = predictors.op,
#                          time.predictors.prior = time.predictors.prior,
#                          special.predictors = special.predictors_cases,
#                          time = "date",
#                          unit.variable = "iso_code",
#                          unit.names.variable = "countryname",
#                          bwdating = 20,
#                          before = 60, after = 60,
#                          yaxis = "COVID-19 cases per mio capita",
#                          name = "Daily COVID-19 cases / capita",
#                          legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99),
#                          Rbs = 1000, seed = 23)
# 
# 
# ### Export
# g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
#                      "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
#                      paste0("Control pool: ", paste(isos, collapse = ", "))),
#               gp = gpar(fontsize = 10), just = "right", x = 1)
# 
# pdf(file = paste0("../03_Output/", "Cases_Synthetic_", name, ".pdf"), width = 12, height = 6)
# grid.arrange(grobs = plot.list3_2, ncol = 2,
#              # bottom = g,
#              top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
# )
# dev.off()
# 
# png(file = paste0("../03_Output/", "Cases_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
# grid.arrange(grobs = plot.list3_2, ncol = 2,
#              # bottom = g,
#              top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
# )
# dev.off()







####--------------------------####
#### Synthetic control Denmark ####
####--------------------------####

treat <- as.Date("2021-04-21")
id <- "DNK"
name <- "Denmark"


### Desc
c <- 2
for(i in intervals){
  s <- treat + i[1]
  e <- treat + i[2]
  oo <- which(corona_subsample.df$iso_code == id & corona_subsample.df$date >= s & corona_subsample.df$date <= e)
  mt <- round(mean(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdt <- round(sd(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdt <- paste0("(", sdt, ")")
  oo <- which(corona_subsample.df$iso_code %in% isos & corona_subsample.df$date >= s & corona_subsample.df$date <= e)
  mc <- round(mean(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdc <- round(sd(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdc <- paste0("(", sdc, ")")
  oo <- which(desc.df$country == name)
  desc.df[oo, c] <- c(mt, sdt)
  desc.df[oo, c+1] <- c(mc, sdc)
  c <- c + 2
}

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
                         legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99),
                         Rbs = 1000, seed = 23)

save(plot.list4_1, file = "plot_list4_1.RData")


### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/", "Vaccinations_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list4_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()

png(file = paste0("../03_Output/", "Vaccinations_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list4_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()





# ### Synthetic control CASES
# 
# plot.list4_2 <- synth_fun(data = corona_subsample.df[corona_subsample.df$date < "2021-08-01", ],
#                          treat.date = treat,
#                          treat.id = id,
#                          control.ids = isos,
#                          dependent = "new_cases_smoothed_per_million",
#                          predictors = predictors,
#                          predictors.op = predictors.op,
#                          time.predictors.prior = time.predictors.prior,
#                          special.predictors = special.predictors_cases,
#                          time = "date",
#                          unit.variable = "iso_code",
#                          unit.names.variable = "countryname",
#                          bwdating = 20,
#                          before = 60, after = 60,
#                          yaxis = "COVID-19 cases per mio capita",
#                          name = "Daily COVID-19 cases / capita",
#                          legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99),
#                          Rbs = 1000, seed = 23)
# 
# 
# ### Export
# g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
#                      "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
#                      paste0("Control pool: ", paste(isos, collapse = ", "))),
#               gp = gpar(fontsize = 10), just = "right", x = 1)
# 
# pdf(file = paste0("../03_Output/", "Cases_Synthetic_", name, ".pdf"), width = 12, height = 6)
# grid.arrange(grobs = plot.list4_2, ncol = 2,
#              # bottom = g,
#              top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
# )
# dev.off()
# 
# png(file = paste0("../03_Output/", "Cases_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
# grid.arrange(grobs = plot.list4_2, ncol = 2,
#              # bottom = g,
#              top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
# )
# dev.off()









####--------------------------####
#### Synthetic control Italy ####
####--------------------------####

treat <- as.Date("2021-08-06")
id <- "ITA"
name <- "Italy"


### Desc
c <- 2
for(i in intervals){
  s <- treat + i[1]
  e <- treat + i[2]
  oo <- which(corona_subsample.df$iso_code == id & corona_subsample.df$date >= s & corona_subsample.df$date <= e)
  mt <- round(mean(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdt <- round(sd(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdt <- paste0("(", sdt, ")")
  oo <- which(corona_subsample.df$iso_code %in% isos & corona_subsample.df$date >= s & corona_subsample.df$date <= e)
  mc <- round(mean(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdc <- round(sd(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdc <- paste0("(", sdc, ")")
  oo <- which(desc.df$country == name)
  desc.df[oo, c] <- c(mt, sdt)
  desc.df[oo, c+1] <- c(mc, sdc)
  c <- c + 2
}

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
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99),
                         Rbs = 1000, seed = 23)

save(plot.list5_1, file = "plot_list5_1.RData")


### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/", "Vaccinations_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list5_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()

png(file = paste0("../03_Output/", "Vaccinations_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list5_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()





# ### Synthetic control CASES
# 
# plot.list5_2 <- synth_fun(data = corona_subsample.df,
#                          treat.date = treat,
#                          treat.id = id,
#                          control.ids = isos,
#                          dependent = "new_cases_smoothed_per_million",
#                          predictors = predictors,
#                          predictors.op = predictors.op,
#                          time.predictors.prior = time.predictors.prior,
#                          special.predictors = special.predictors_cases,
#                          time = "date",
#                          unit.variable = "iso_code",
#                          unit.names.variable = "countryname",
#                          bwdating = 20,
#                          before = 60, after = 60,
#                          yaxis = "COVID-19 cases per mio capita",
#                          name = "Daily COVID-19 cases / capita",
#                          legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99),
#                          Rbs = 1000, seed = 23)
# 
# 
# ### Export
# g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
#                      "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
#                      paste0("Control pool: ", paste(isos, collapse = ", "))),
#               gp = gpar(fontsize = 10), just = "right", x = 1)
# 
# pdf(file = paste0("../03_Output/", "Cases_Synthetic_", name, ".pdf"), width = 12, height = 6)
# grid.arrange(grobs = plot.list5_2, ncol = 2,
#              # bottom = g,
#              top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
# )
# dev.off()
# 
# png(file = paste0("../03_Output/", "Cases_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
# grid.arrange(grobs = plot.list5_2, ncol = 2,
#              # bottom = g,
#              top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
# )
# dev.off()










####--------------------------####
#### Synthetic control Switzerland ####
####--------------------------####

treat <- as.Date("2021-09-13")
id <- "CHE"
name <- "Switzerland"


### Desc
c <- 2
for(i in intervals){
  s <- treat + i[1]
  e <- treat + i[2]
  oo <- which(corona_subsample.df$iso_code == id & corona_subsample.df$date >= s & corona_subsample.df$date <= e)
  mt <- round(mean(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdt <- round(sd(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdt <- paste0("(", sdt, ")")
  oo <- which(corona_subsample.df$iso_code %in% isos & corona_subsample.df$date >= s & corona_subsample.df$date <= e)
  mc <- round(mean(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdc <- round(sd(corona_subsample.df[oo, "new_vaccinations_smoothed_per_million"]), 0)
  sdc <- paste0("(", sdc, ")")
  oo <- which(desc.df$country == name)
  desc.df[oo, c] <- c(mt, sdt)
  desc.df[oo, c+1] <- c(mc, sdc)
  c <- c + 2
}


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
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99),
                         Rbs = 1000, seed = 23)

save(plot.list6_1, file = "plot_list6_1.RData")


### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/", "Vaccinations_Synthetic_", name, ".pdf"), width = 12, height = 6)
grid.arrange(grobs = plot.list6_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()

png(file = paste0("../03_Output/", "Vaccinations_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot.list6_1, ncol = 2,
             # bottom = g,
             top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()





# ### Synthetic control CASES
# 
# plot.list6_2 <- synth_fun(data = corona_subsample.df,
#                          treat.date = treat,
#                          treat.id = id,
#                          control.ids = isos,
#                          dependent = "new_cases_smoothed_per_million",
#                          predictors = predictors,
#                          predictors.op = predictors.op,
#                          time.predictors.prior = time.predictors.prior,
#                          special.predictors = special.predictors_cases,
#                          time = "date",
#                          unit.variable = "iso_code",
#                          unit.names.variable = "countryname",
#                          bwdating = 20,
#                          before = 60, after = 60,
#                          yaxis = "COVID-19 cases per mio capita",
#                          name = "Daily COVID-19 cases / capita",
#                          legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99),
#                          Rbs = 1000, seed = 23)
# 
# 
# ### Export
# g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
#                      "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
#                      paste0("Control pool: ", paste(isos, collapse = ", "))),
#               gp = gpar(fontsize = 10), just = "right", x = 1)
# 
# pdf(file = paste0("../03_Output/", "Cases_Synthetic_", name, ".pdf"), width = 12, height = 6)
# grid.arrange(grobs = plot.list6_2, ncol = 2,
#              # bottom = g,
#              top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
# )
# dev.off()
# 
# png(file = paste0("../03_Output/", "Cases_Synthetic_", name, ".png"), width = 12, height = 6, units = "in", res = 300)
# grid.arrange(grobs = plot.list6_2, ncol = 2,
#              # bottom = g,
#              top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
# )
# dev.off()





####-------------------------####
#### Export combined figures ####
####-------------------------####

# P1
p1 <- grid.arrange(grobs = plot.list1_1, ncol = 2,
                   # bottom = g,
                   top = textGrob(paste0("a) COVID health certificates: ", "France"), gp = gpar(fontsize = 16))
)
p2 <- grid.arrange(grobs = plot.list3_1, ncol = 2,
                   # bottom = g,
                   top = textGrob(paste0("b) COVID health certificates: ", "Israel"), gp = gpar(fontsize = 16))
)
p3 <- grid.arrange(grobs = plot.list5_1, ncol = 2,
                   # bottom = g,
                   top = textGrob(paste0("c) COVID health certificates: ", "Italy"), gp = gpar(fontsize = 16))
)



### Export
g <- textGrob(paste0("Data sources: Our World in Data (https://github.com/owid/covid-19-data)\n",
                     "Oxford COVID-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)\n",
                     paste0("Control pool: ", paste(isos, collapse = ", "))),
              gp = gpar(fontsize = 10), just = "right", x = 1)

pdf(file = paste0("../03_Output/", "Vaccinations_Synthetic_", "panel", ".pdf"), width = 12, height = 16)
grid.arrange(grobs = list(p1, p2, p3), ncol = 1,  nrow = 3,
             # bottom = g,
             # top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()


cairo_ps(file = paste0("../03_Output/", "Vaccinations_Synthetic_", "panel", ".eps"), width = 12, height = 16)
grid.arrange(grobs = list(p1, p2, p3), ncol = 1,  nrow = 3,
             # bottom = g,
             # top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()


png(file = paste0("../03_Output/", "Vaccinations_Synthetic_", "panel", ".png"), width = 12, height = 16, units = "in", res = 300)
grid.arrange(grobs = list(p1, p2, p3), ncol = 1, nrow = 3,
             # bottom = g,
             # top = textGrob(paste0("COVID health certificates: ", name), gp = gpar(fontsize = 16))
)
dev.off()


####---------------------####
#### Export descriptives ####
####---------------------####

# Export
write.csv(desc.df, file = "../03_Output/Descriptives.csv", 
          row.names = FALSE)


####-----------------------------####
#### Sum over the total vaccines ####
####-----------------------------####


### Extract -20 to +40 days
c1.df <- plot.list1_1[[2]]$data
c1.df$country <- "France"
c1.df <- c1.df[c1.df$date >= -20 & c1.df$date <= 39,  ]

c2.df <- plot.list2_1[[2]]$data
c2.df$country <- "Germany"
c2.df <- c2.df[c2.df$date >= -20 & c2.df$date <= 39,  ]

c3.df <- plot.list3_1[[2]]$data
c3.df$country <- "Israel"
c3.df <- c3.df[c3.df$date >= -20 & c3.df$date <= 39,  ]

c4.df <- plot.list4_1[[2]]$data
c4.df$country <- "Denmark"
c4.df <- c4.df[c4.df$date >= -20 & c4.df$date <= 39,  ]

c5.df <- plot.list5_1[[2]]$data
c5.df$country <- "Italy"
c5.df <- c5.df[c5.df$date >= -20 & c5.df$date <= 39,  ]

c6.df <- plot.list6_1[[2]]$data
c6.df$country <- "Switzerland"
c6.df <- c6.df[c6.df$date >= -20 & c6.df$date <= 39,  ]


### Get summary statistics by group
c_all.df <- rbind(c1.df, c2.df, c3.df, c4.df, c5.df, c6.df)
c_all.df$after <- ifelse(c_all.df$date >= 0, "after", "before")
c_all.df <- c_all.df[which(!is.na(c_all.df$ATT)), ]

c_all.df$sum <- ave(c_all.df$ATT, 
                    c_all.df$country, c_all.df$after,
                    FUN = function(x) sum(x))
c_all.df$days <- ave(c_all.df$ATT, 
                     c_all.df$country, c_all.df$after,
                     FUN = function(x) length(x))

### Reduce
c_all.df <- c_all.df[, c("country", "days", "sum", "after")]
c_all.df <- unique(c_all.df)
summary.df <- tidyr::pivot_wider(c_all.df, id_cols = "country",
                                 names_from = after, values_from = c("days", "sum"))



### Add back total population
pop.df <- unique(corona_all.df[, c("countryname", "population")])
names(pop.df)[1] <- "country"
summary.df <- merge(summary.df, pop.df, by = "country", all.x = TRUE, all.y = FALSE)

# Total numbers
summary.df$sum_before_total <- summary.df$sum_before * (summary.df$population/1000000)
summary.df$sum_after_total <- summary.df$sum_after * (summary.df$population/1000000)


### Order
summary.df <- summary.df[, c("country", "days_before", "sum_before_total", "sum_before",
                             "days_after", "sum_after_total", "sum_after")]

countries <- c("France", "Italy", "Israel", "Denmark", "Germany", "Switzerland")
summary.df <- summary.df[match(countries, summary.df$country),]

### Round
oo <- which(names(summary.df) %in% c("sum_before_total", "sum_before", "sum_after_total", "sum_after"))
summary.df[, oo] <- sapply(summary.df[, oo], FUN = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))


# Export
write.csv(summary.df, file = "../03_Output/Summary_metric.csv", 
          row.names = FALSE)





### Lower bound
c1.df <- plot.list1_1[[2]]$data
c1.df$country <- "France"
c1.df <- c1.df[c1.df$date >= -20 & c1.df$date <= 39,  ]

c2.df <- plot.list2_1[[2]]$data
c2.df$country <- "Germany"
c2.df <- c2.df[c2.df$date >= -20 & c2.df$date <= 39,  ]

c3.df <- plot.list3_1[[2]]$data
c3.df$country <- "Israel"
c3.df <- c3.df[c3.df$date >= -20 & c3.df$date <= 39,  ]

c4.df <- plot.list4_1[[2]]$data
c4.df$country <- "Denmark"
c4.df <- c4.df[c4.df$date >= -20 & c4.df$date <= 39,  ]

c5.df <- plot.list5_1[[2]]$data
c5.df$country <- "Italy"
c5.df <- c5.df[c5.df$date >= -20 & c5.df$date <= 39,  ]

c6.df <- plot.list6_1[[2]]$data
c6.df$country <- "Switzerland"
c6.df <- c6.df[c6.df$date >= -20 & c6.df$date <= 39,  ]


### Get summary statistics by group
c_all.df <- rbind(c1.df, c2.df, c3.df, c4.df, c5.df, c6.df)
c_all.df$after <- ifelse(c_all.df$date >= 0, "after", "before")
c_all.df <- c_all.df[which(!is.na(c_all.df$ATT)), ]

c_all.df$sum <- ave(c_all.df$ll, 
                    c_all.df$country, c_all.df$after,
                    FUN = function(x) sum(x))
c_all.df$days <- ave(c_all.df$ll, 
                     c_all.df$country, c_all.df$after,
                     FUN = function(x) length(x))

### Reduce
c_all.df <- c_all.df[, c("country", "days", "sum", "after")]
c_all.df <- unique(c_all.df)
lower.df <- tidyr::pivot_wider(c_all.df, id_cols = "country",
                               names_from = after, values_from = c("days", "sum"))



### Add back total population
pop.df <- unique(corona_all.df[, c("countryname", "population")])
names(pop.df)[1] <- "country"
lower.df <- merge(lower.df, pop.df, by = "country", all.x = TRUE, all.y = FALSE)

# Total numbers
lower.df$sum_before_total <- lower.df$sum_before * (lower.df$population/1000000)
lower.df$sum_after_total <- lower.df$sum_after * (lower.df$population/1000000)


### Order
lower.df <- lower.df[, c("country", "days_before", "sum_before_total", "sum_before",
                         "days_after", "sum_after_total", "sum_after")]

countries <- c("France", "Italy", "Israel", "Denmark", "Germany", "Switzerland")
lower.df <- lower.df[match(countries, lower.df$country),]

### Round
oo <- which(names(lower.df) %in% c("sum_before_total", "sum_before", "sum_after_total", "sum_after"))
lower.df[, oo] <- sapply(lower.df[, oo], FUN = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))





### upper bound
c1.df <- plot.list1_1[[2]]$data
c1.df$country <- "France"
c1.df <- c1.df[c1.df$date >= -20 & c1.df$date <= 39,  ]

c2.df <- plot.list2_1[[2]]$data
c2.df$country <- "Germany"
c2.df <- c2.df[c2.df$date >= -20 & c2.df$date <= 39,  ]

c3.df <- plot.list3_1[[2]]$data
c3.df$country <- "Israel"
c3.df <- c3.df[c3.df$date >= -20 & c3.df$date <= 39,  ]

c4.df <- plot.list4_1[[2]]$data
c4.df$country <- "Denmark"
c4.df <- c4.df[c4.df$date >= -20 & c4.df$date <= 39,  ]

c5.df <- plot.list5_1[[2]]$data
c5.df$country <- "Italy"
c5.df <- c5.df[c5.df$date >= -20 & c5.df$date <= 39,  ]

c6.df <- plot.list6_1[[2]]$data
c6.df$country <- "Switzerland"
c6.df <- c6.df[c6.df$date >= -20 & c6.df$date <= 39,  ]


### Get summary statistics by group
c_all.df <- rbind(c1.df, c2.df, c3.df, c4.df, c5.df, c6.df)
c_all.df$after <- ifelse(c_all.df$date >= 0, "after", "before")
c_all.df <- c_all.df[which(!is.na(c_all.df$ATT)), ]

c_all.df$sum <- ave(c_all.df$ul, 
                    c_all.df$country, c_all.df$after,
                    FUN = function(x) sum(x))
c_all.df$days <- ave(c_all.df$ul, 
                     c_all.df$country, c_all.df$after,
                     FUN = function(x) length(x))

### Reduce
c_all.df <- c_all.df[, c("country", "days", "sum", "after")]
c_all.df <- unique(c_all.df)
upper.df <- tidyr::pivot_wider(c_all.df, id_cols = "country",
                               names_from = after, values_from = c("days", "sum"))



### Add back total population
pop.df <- unique(corona_all.df[, c("countryname", "population")])
names(pop.df)[1] <- "country"
upper.df <- merge(upper.df, pop.df, by = "country", all.x = TRUE, all.y = FALSE)

# Total numbers
upper.df$sum_before_total <- upper.df$sum_before * (upper.df$population/1000000)
upper.df$sum_after_total <- upper.df$sum_after * (upper.df$population/1000000)


### Order
upper.df <- upper.df[, c("country", "days_before", "sum_before_total", "sum_before",
                         "days_after", "sum_after_total", "sum_after")]

countries <- c("France", "Italy", "Israel", "Denmark", "Germany", "Switzerland")
upper.df <- upper.df[match(countries, upper.df$country),]

### Round
oo <- which(names(upper.df) %in% c("sum_before_total", "sum_before", "sum_after_total", "sum_after"))
upper.df[, oo] <- sapply(upper.df[, oo], FUN = function(x) format(round(as.numeric(x), 0), nsmall=0, big.mark=","))



### Combine 
bounds.df <- lower.df
for(i in c("sum_before_total", "sum_before", "sum_after_total", "sum_after")){
  bounds.df[, i] <- paste0("[", gsub(" ", "", bounds.df[, i]), " ; ", gsub(" ", "", upper.df[, i]), "]") 
}

write.csv(bounds.df, file = "../03_Output/Summary_metric_ci.csv", 
          row.names = FALSE)








### ------------------------- ------------ ------------------------- ###
### ------------------------- Age specific ------------------------- ###
### ------------------------- ------------ ------------------------- ###

# Synth control function
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
                      bwdating = 0,
                      before = 60, after = 60,
                      yaxis = NULL, name = NULL,
                      legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99),
                      Rbs = 1000, alpha = 0.05, seed = 24, workers = availableCores()){
  
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
    time.optimize.ssr = (t_pre + bwdating):-1,
    time.plot = (t_pre + bwdating):(end + bwdating)
  )
  
  synth_out1 <- synth(data.prep.obj = dataprep_out1)
  
  # ### Bootstrap
  # set.seed(seed)
  # 
  # NC <- length(id_control)
  # 
  # Y <- dataprep_out1$Y1plot 
  # 
  # YC.mat <- matrix(NA, nrow = nrow(dataprep_out1$Y0plot), ncol = Rbs)
  # Ydiff.mat <- matrix(NA, nrow = nrow(dataprep_out1$Y0plot), ncol = Rbs)
  # 
  # wlist <- vector(mode = "list", length = Rbs)
  # Y0list <- vector(mode = "list", length = Rbs)
  # idlist  <- vector(mode = "list", length = Rbs)
  # 
  # for(i in 1:Rbs){
  #   idlist[[i]] <- sample(id_control, NC, replace = TRUE)
  # }  
  # 
  # 
  # # Bootstrap parallel
  # plan(multisession, workers = workers)
  # 
  # bs.res <- future_lapply(idlist, function(x) bs_synth(x, Y,
  #                                                      id_treat, corona_subsample2.df, 
  #                                                      predictors, predictors.op, time.predictors.prior,
  #                                                      special.predictors, dependent, t_pre, end, bwdating))
  # plan("sequential")
  # 
  # YC.mat <- lapply(bs.res, `[[`, 1)
  # Ydiff.mat <- lapply(bs.res, `[[`, 2)
  # 
  # YC.mat <- data.frame(YC.mat)
  # Ydiff.mat <- data.frame(Ydiff.mat)
  # 
  # 
  # 
  # # Percentile CIs
  # pc <- c( (1 * alpha/2),  (1 * (1 - alpha/2)))
  # nal <- apply(YC.mat, 1, FUN = function(x) any(is.na(x)))
  # percentiles <- t(apply(YC.mat, 1, FUN = function(x) quantile(x, probs = pc, na.rm = TRUE)))
  # percentiles.diff <- t(apply(Ydiff.mat, 1, FUN = function(x) quantile(x, probs = pc, na.rm = TRUE)))
  
  #percentiles[which(nal), ] <- NA
  
  
  ###### Plot
  # Overall development
  data <- data.frame(dataprep_out1$Y1plot)
  names(data) <- "ATT_1"
  data$ATT_2 <- dataprep_out1$Y0plot %*% synth_out1$solution.w
  
  # data$ll2 <- percentiles[,1]
  # data$ul2 <- percentiles[,2]
  # data$ll2[which(is.na(data$ATT_2))] <- NA
  # data$ul2[which(is.na(data$ATT_2))] <- NA
  
  
  data$date <- as.Date(as.Date(treat) + as.numeric(rownames(data)) - bwdating)
  data <- tidyr::pivot_longer(data, cols = c("ATT_1", "ATT_2"),
                              names_to = "group", values_to = "ATT")
  data$group <- factor(data$group, levels = c("ATT_1", "ATT_2"), labels = c("Treated groups", "Synthetic Control"))
  
  c1_p1 <- ggplot(data, aes(date, ATT))+
    geom_line(mapping = aes(color = group, linetype = group), lwd = 1.0) +
    #geom_ribbon(mapping = aes(ymin = ll2, ymax = ul2), alpha = 0.3, fill = gg_color_hue(2)[2]) +
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
  
  # data$ll <- percentiles.diff[,1]
  # data$ul <- percentiles.diff[,2]
  # data$ll[which(is.na(data$ATT))] <- NA
  # data$ul[which(is.na(data$ATT))] <- NA
  
  data$date <- as.numeric(rownames(data)) - bwdating
  
  c1_p2 <- ggplot(data, aes(date, ATT))+
    geom_line(data = data, lwd = 1.0) +
    #geom_ribbon(mapping = aes(ymin = ll, ymax = ul), alpha = 0.3) +
    geom_hline(yintercept = 0, colour = "black", lty = 3, lwd = 1, alpha = 0.7)+
    geom_vline(xintercept = 0, colour = gray(1/2), lty = 1, lwd = 1) + 
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


special.predictors_vaccine_1 = list(
  list("new_cases_smoothed_per_million", c(-7:-1), "mean"),
  list("new_cases_smoothed_per_million", c(-14:-8), "mean"),
  list("new_cases_smoothed_per_million", c(-21:-15), "mean"),
  list("new_cases_smoothed_per_million", c(-28:-22), "mean"),
  list("daily_vaccine_total_pc_age15_17", c(-7:-1), "mean"),
  list("daily_vaccine_total_pc_age15_17", c(-14:-8), "mean"),
  list("daily_vaccine_total_pc_age15_17", c(-21:-15), "mean"),
  list("daily_vaccine_total_pc_age15_17", c(-28:-22), "mean"),
  #list("daily_vaccine_total_pc_age15_17", c(-35:-29), "mean"),
  list("fully_vaccinationed_c_imp", c(-28:-15), "mean"),
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

special.predictors_vaccine_2 = list(
  list("new_cases_smoothed_per_million", c(-7:-1), "mean"),
  list("new_cases_smoothed_per_million", c(-14:-8), "mean"),
  list("new_cases_smoothed_per_million", c(-21:-15), "mean"),
  list("new_cases_smoothed_per_million", c(-28:-22), "mean"),
  list("daily_vaccine_total_pc_age18_24", c(-7:-1), "mean"),
  list("daily_vaccine_total_pc_age18_24", c(-14:-8), "mean"),
  list("daily_vaccine_total_pc_age18_24", c(-21:-15), "mean"),
  list("daily_vaccine_total_pc_age18_24", c(-28:-22), "mean"),
  #list("daily_vaccine_total_pc_age18_24", c(-35:-29), "mean"),
  list("fully_vaccinationed_c_imp", c(-28:-15), "mean"),
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

special.predictors_vaccine_3 = list(
  list("new_cases_smoothed_per_million", c(-7:-1), "mean"),
  list("new_cases_smoothed_per_million", c(-14:-8), "mean"),
  list("new_cases_smoothed_per_million", c(-21:-15), "mean"),
  list("new_cases_smoothed_per_million", c(-28:-22), "mean"),
  list("daily_vaccine_total_pc_age25_49", c(-7:-1), "mean"),
  list("daily_vaccine_total_pc_age25_49", c(-14:-8), "mean"),
  list("daily_vaccine_total_pc_age25_49", c(-21:-15), "mean"),
  list("daily_vaccine_total_pc_age25_49", c(-28:-22), "mean"),
  #list("daily_vaccine_total_pc_age25_49", c(-35:-29), "mean"),
  list("fully_vaccinationed_c_imp", c(-28:-15), "mean"),
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


special.predictors_vaccine_4 = list(
  list("new_cases_smoothed_per_million", c(-7:-1), "mean"),
  list("new_cases_smoothed_per_million", c(-14:-8), "mean"),
  list("new_cases_smoothed_per_million", c(-21:-15), "mean"),
  list("new_cases_smoothed_per_million", c(-28:-22), "mean"),
  list("daily_vaccine_total_pc_age50_59", c(-7:-1), "mean"),
  list("daily_vaccine_total_pc_age50_59", c(-14:-8), "mean"),
  list("daily_vaccine_total_pc_age50_59", c(-21:-15), "mean"),
  list("daily_vaccine_total_pc_age50_59", c(-28:-22), "mean"),
  # list("daily_vaccine_total_pc_age50_59", c(-35:-29), "mean"),
  list("fully_vaccinationed_c_imp", c(-28:-15), "mean"),
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

special.predictors_vaccine_5 = list(
  list("new_cases_smoothed_per_million", c(-7:-1), "mean"),
  list("new_cases_smoothed_per_million", c(-14:-8), "mean"),
  list("new_cases_smoothed_per_million", c(-21:-15), "mean"),
  list("new_cases_smoothed_per_million", c(-28:-22), "mean"),
  list("daily_vaccine_total_pc_age60andabove", c(-7:-1), "mean"),
  list("daily_vaccine_total_pc_age60andabove", c(-14:-8), "mean"),
  list("daily_vaccine_total_pc_age60andabove", c(-21:-15), "mean"),
  list("daily_vaccine_total_pc_age60andabove", c(-28:-22), "mean"),
  # list("daily_vaccine_total_pc_age60andabove", c(-35:-29), "mean"),
  list("fully_vaccinationed_c_imp", c(-28:-15), "mean"),
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

### Control group 

nonnaisos <- unique(corona_subsample.df$iso_code[!is.na(corona_subsample.df$daily_vaccine_total_pc_age18_24)])
sort(nonnaisos)

isos <- c("AUT", "BEL", "CYP", "CZE", "ESP", "EST", "FIN", "GRC", "HRV", "IRL", "ISL", "LTU", "LUX", "LVA", "MLT", "POL", "PRT", "ROU", "SVK", "SVN", "SWE")

isos_dnk <- c("AUT", "BEL", "CYP", "CZE", "ESP", "EST", "FIN", "GRC", "HRV", "IRL", "ISL", "LTU", "LUX", "LVA", "MLT", "POL", "PRT", "SVK", "SVN", "SWE")

bwdating = 20

####--------------------------####
#### Synthetic control France ####
####--------------------------####

treat <- as.Date("2021-08-09")
id <- "FRA"
name <- "France"


# ### Synthetic control 15-17
# 
# plot.list_1 <- synth_fun(data = corona_subsample.df, 
#                          treat.date = treat, 
#                          treat.id = id,
#                          control.ids = isos,
#                          dependent = "daily_vaccine_total_pc_age18_24",
#                          predictors = predictors,
#                          predictors.op = predictors.op,
#                          time.predictors.prior = time.predictors.prior,
#                          special.predictors = special.predictors_vaccine_1,
#                          time = "date",
#                          unit.variable = "iso_code",
#                          unit.names.variable = "countryname",
#                          bwdating = 20,
#                          before = 60, after = 60,
#                          yaxis = "daily vaccine doses per mio capita", 
#                          name = "vaccine doses / capita",
#                          legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


### Synthetic control 18-24

plot.list_2 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "daily_vaccine_total_pc_age18_24",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_vaccine_2,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "daily vaccine doses per mio capita", 
                         name = "vaccine doses / capita",
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


### Synthetic control 25-49

plot.list_3 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "daily_vaccine_total_pc_age25_49",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_vaccine_3,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "daily vaccine doses per mio capita", 
                         name = "vaccine doses / capita",
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


plot.list_4 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "daily_vaccine_total_pc_age50_59",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_vaccine_4,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "daily vaccine doses per mio capita", 
                         name = "vaccine doses / capita",
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


plot.list_5 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "daily_vaccine_total_pc_age60andabove",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_vaccine_5,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "daily vaccine doses per mio capita", 
                         name = "vaccine doses / capita",
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


### Export


pdf(file = paste0("../03_Output/", "Vaccinations_age_Synthetic_", name, ".pdf"), width = 18, height = 10)
grid.arrange(grobs = list(arrangeGrob(grobs = plot.list_2, ncol = 2, top = textGrob(paste0("18 - 24 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_3, ncol = 2, top = textGrob(paste0("25 - 49 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_4, ncol = 2, top = textGrob(paste0("50 - 59 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_5, ncol = 2, top = textGrob(paste0("60+ years old"), gp = gpar(fontsize = 16)))),
             ncol = 2
)
dev.off()

png(file = paste0("../03_Output/", "Vaccinations_age_Synthetic_", name, ".png"), width = 18, height = 10, units = "in", res = 300)
grid.arrange(grobs = list(arrangeGrob(grobs = plot.list_2, ncol = 2, top = textGrob(paste0("18 - 24 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_3, ncol = 2, top = textGrob(paste0("25 - 49 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_4, ncol = 2, top = textGrob(paste0("50 - 59 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_5, ncol = 2, top = textGrob(paste0("60+ years old"), gp = gpar(fontsize = 16)))),
             ncol = 2
)
dev.off()




### Export one plot
data.df <- rbind(cbind(plot.list_2[[2]]$data, group = 1), 
                 cbind(plot.list_3[[2]]$data, group = 2),
                 cbind(plot.list_4[[2]]$data, group = 3),
                 cbind(plot.list_5[[2]]$data, group = 4))

data.df$group <- factor(data.df$group, levels = c(1:4), labels = c("18 - 24 years old",
                                                                   "25 - 49 years old",
                                                                   "50 - 59 years old",
                                                                   "60+ years old"))
cols <- cols <- viridis(4, begin = 0, end = 1, direction = -1)


zp1 <- ggplot(data.df, aes(date, ATT))+
  geom_line(aes(colour = group, linetype = group), lwd = 1.0) +
  geom_hline(yintercept = 0, colour = "black", lty = 3, lwd = 1, alpha = 0.7)+
  geom_vline(xintercept = 0, colour = gray(1/2), lty = 1, lwd = 1) + 
  geom_vline(xintercept = -20, colour = gray(1/2), lty = 2, lwd = 1) + 
  #scale_x_continuous(limits = c(-60, 30), breaks = seq(-60, 30, 20)) + 
  scale_color_manual(values = cols) +
  labs(y = paste0("Difference in ", "daily vaccine doses per mio capita"),
       x = "Days since intervention",
       title = paste0("France")) + 
  theme_bw() + 
  theme(text = element_text( size = 12),
        legend.position = c(0.01,0.99), legend.justification = c(0.01,0.99),
        #legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.x = element_text(colour = "black", margin = margin(t = 10, r = 0 , b = 0, l = 0)),
        axis.title.y = element_text(colour = "black", margin = margin(t = 0, r = 10 , b = 0, l = 0)),
        axis.text.x = element_text(colour = "black"),
  )
# c1_p2 <- c1_p2 + guides(fill = guide_legend(title = "N treated", reverse = T))
zp1






####--------------------------####
#### Synthetic control Italy ####
####--------------------------####

treat <- as.Date("2021-08-06")
id <- "ITA"
name <- "Italy"


# ### Synthetic control 15-17
# 
# plot.list_1 <- synth_fun(data = corona_subsample.df, 
#                          treat.date = treat, 
#                          treat.id = id,
#                          control.ids = isos,
#                          dependent = "daily_vaccine_total_pc_age18_24",
#                          predictors = predictors,
#                          predictors.op = predictors.op,
#                          time.predictors.prior = time.predictors.prior,
#                          special.predictors = special.predictors_vaccine_1,
#                          time = "date",
#                          unit.variable = "iso_code",
#                          unit.names.variable = "countryname",
#                          bwdating = 20,
#                          before = 60, after = 60,
#                          yaxis = "daily vaccine doses per mio capita", 
#                          name = "vaccine doses / capita",
#                          legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


### Synthetic control 18-24

plot.list_2 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "daily_vaccine_total_pc_age18_24",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_vaccine_2,
                         bwdating = 20,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         before = 60, after = 60,
                         yaxis = "daily vaccine doses per mio capita", 
                         name = "vaccine doses / capita",
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


### Synthetic control 25-49

plot.list_3 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "daily_vaccine_total_pc_age25_49",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_vaccine_3,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "daily vaccine doses per mio capita", 
                         name = "vaccine doses / capita",
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


plot.list_4 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "daily_vaccine_total_pc_age50_59",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_vaccine_4,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "daily vaccine doses per mio capita", 
                         name = "vaccine doses / capita",
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


plot.list_5 <- synth_fun(data = corona_subsample.df, 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos,
                         dependent = "daily_vaccine_total_pc_age60andabove",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_vaccine_5,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "daily vaccine doses per mio capita", 
                         name = "vaccine doses / capita",
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


### Export


pdf(file = paste0("../03_Output/", "Vaccinations_age_Synthetic_", name, ".pdf"), width = 18, height = 10)
grid.arrange(grobs = list(arrangeGrob(grobs = plot.list_2, ncol = 2, top = textGrob(paste0("18 - 24 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_3, ncol = 2, top = textGrob(paste0("25 - 49 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_4, ncol = 2, top = textGrob(paste0("50 - 59 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_5, ncol = 2, top = textGrob(paste0("60+ years old"), gp = gpar(fontsize = 16)))),
             ncol = 2
)
dev.off()

png(file = paste0("../03_Output/", "Vaccinations_age_Synthetic_", name, ".png"), width = 18, height = 10, units = "in", res = 300)
grid.arrange(grobs = list(arrangeGrob(grobs = plot.list_2, ncol = 2, top = textGrob(paste0("18 - 24 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_3, ncol = 2, top = textGrob(paste0("25 - 49 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_4, ncol = 2, top = textGrob(paste0("50 - 59 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_5, ncol = 2, top = textGrob(paste0("60+ years old"), gp = gpar(fontsize = 16)))),
             ncol = 2
)
dev.off()




### Export one plot
data2.df <- rbind(cbind(plot.list_2[[2]]$data, group = 1), 
                  cbind(plot.list_3[[2]]$data, group = 2),
                  cbind(plot.list_4[[2]]$data, group = 3),
                  cbind(plot.list_5[[2]]$data, group = 4))

data2.df$group <- factor(data2.df$group, levels = c(1:4), labels = c("18 - 24 years old",
                                                                     "25 - 49 years old",
                                                                     "50 - 59 years old",
                                                                     "60+ years old"))






####--------------------------####
#### Synthetic control Denmark ####
####--------------------------####

treat <- as.Date("2021-04-21")
id <- "DNK"
name <- "Denmark"


# ### Synthetic control 15-17
# 
# plot.list_1 <- synth_fun(data = corona_subsample.df[corona_subsample.df$date < "2021-08-01", ], 
#                          treat.date = treat, 
#                          treat.id = id,
#                          control.ids = isos_dnk,
#                          dependent = "daily_vaccine_total_pc_age18_24",
#                          predictors = predictors,
#                          predictors.op = predictors.op,
#                          time.predictors.prior = time.predictors.prior,
#                          special.predictors = special.predictors_vaccine_1,
#                          time = "date",
#                          unit.variable = "iso_code",
#                          unit.names.variable = "countryname",
#                          bwdating = 20,
#                          before = 60, after = 60,
#                          yaxis = "daily vaccine doses per mio capita", 
#                          name = "vaccine doses / capita",
#                          legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


### Synthetic control 18-24

plot.list_2 <- synth_fun(data = corona_subsample.df[corona_subsample.df$date < "2021-08-01", ], 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos_dnk,
                         dependent = "daily_vaccine_total_pc_age18_24",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_vaccine_2,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "daily vaccine doses per mio capita", 
                         name = "vaccine doses / capita",
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


### Synthetic control 25-49

plot.list_3 <- synth_fun(data = corona_subsample.df[corona_subsample.df$date < "2021-08-01", ], 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos_dnk,
                         dependent = "daily_vaccine_total_pc_age25_49",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_vaccine_3,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "daily vaccine doses per mio capita", 
                         name = "vaccine doses / capita",
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


plot.list_4 <- synth_fun(data = corona_subsample.df[corona_subsample.df$date < "2021-08-01", ], 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos_dnk,
                         dependent = "daily_vaccine_total_pc_age50_59",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_vaccine_4,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "daily vaccine doses per mio capita", 
                         name = "vaccine doses / capita",
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


plot.list_5 <- synth_fun(data = corona_subsample.df[corona_subsample.df$date < "2021-08-01", ], 
                         treat.date = treat, 
                         treat.id = id,
                         control.ids = isos_dnk,
                         dependent = "daily_vaccine_total_pc_age60andabove",
                         predictors = predictors,
                         predictors.op = predictors.op,
                         time.predictors.prior = time.predictors.prior,
                         special.predictors = special.predictors_vaccine_5,
                         time = "date",
                         unit.variable = "iso_code",
                         unit.names.variable = "countryname",
                         bwdating = 20,
                         before = 60, after = 60,
                         yaxis = "daily vaccine doses per mio capita", 
                         name = "vaccine doses / capita",
                         legend.position = c(0.99,0.99), legend.justification = c(0.99,0.99))


### Export


pdf(file = paste0("../03_Output/", "Vaccinations_age_Synthetic_", name, ".pdf"), width = 18, height = 10)
grid.arrange(grobs = list(arrangeGrob(grobs = plot.list_2, ncol = 2, top = textGrob(paste0("18 - 24 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_3, ncol = 2, top = textGrob(paste0("25 - 49 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_4, ncol = 2, top = textGrob(paste0("50 - 59 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_5, ncol = 2, top = textGrob(paste0("60+ years old"), gp = gpar(fontsize = 16)))),
             ncol = 2
)
dev.off()

png(file = paste0("../03_Output/", "Vaccinations_age_Synthetic_", name, ".png"), width = 18, height = 10, units = "in", res = 300)
grid.arrange(grobs = list(arrangeGrob(grobs = plot.list_2, ncol = 2, top = textGrob(paste0("18 - 24 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_3, ncol = 2, top = textGrob(paste0("25 - 49 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_4, ncol = 2, top = textGrob(paste0("50 - 59 years old"), gp = gpar(fontsize = 16))),
                          arrangeGrob(grobs = plot.list_5, ncol = 2, top = textGrob(paste0("60+ years old"), gp = gpar(fontsize = 16)))),
             ncol = 2
)
dev.off()



### Export one plot
data3.df <- rbind(cbind(plot.list_2[[2]]$data, group = 1), 
                  cbind(plot.list_3[[2]]$data, group = 2),
                  cbind(plot.list_4[[2]]$data, group = 3),
                  cbind(plot.list_5[[2]]$data, group = 4))

data3.df$group <- factor(data3.df$group, levels = c(1:4), labels = c("18 - 24 years old",
                                                                     "25 - 49 years old",
                                                                     "50 - 59 years old",
                                                                     "60+ years old"))



#### Plot all three countries ####

data.df$country <- "France"
data2.df$country <- "Italy"
data3.df$country <- "Denmark"

data_all.df <- rbind(data.df, data2.df)

cols <- inferno (4, begin = 0.2, end = 0.9, direction = -1)

zp_all <- ggplot(data_all.df, aes(date, ATT))+
  facet_wrap(vars(country), ncol = 3, scales = "free")+
  geom_line(aes(colour = group, ), lwd = 1.0) +
  geom_hline(yintercept = 0, colour = "black", lty = 3, lwd = 1, alpha = 0.7)+
  geom_vline(xintercept = 0, colour = gray(1/2), lty = 1, lwd = 1) + 
  geom_vline(xintercept = -20, colour = gray(1/2), lty = 2, lwd = 1) + 
  #scale_x_continuous(limits = c(-60, 30), breaks = seq(-60, 30, 20)) + 
  scale_color_manual(values = cols) +
  labs(y = paste0("Difference in ", "daily vaccine doses per mio capita"),
       x = "Days since intervention",
       title = "COVID health certificates: Age-specific vaccinations") + 
  theme_bw() + 
  theme(text = element_text( size = 14),
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
zp_all



png(file = paste0("../03_Output/", "Vaccinations_age_Synthetic_", "comb", ".png"), width = 12, height = 6, units = "in", res = 300)
zp_all
dev.off()