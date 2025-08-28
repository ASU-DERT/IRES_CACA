# This script reads in a single L2 sumdat file from the LiCor 6400-09 and summarizes the key output variables.
# This was created for 2023 Gobabeb IRES projects where soil respiration values are quite low.
# Due to issues with predicting dc/dt at a target value with very low respiration rates (see Heaather's 
# notes on this), this script calculates efflux for single drawdown cycle as the mean of all efflux values
# for this cycle.
# Created July 6, 2023 Heather Throop
# Updated July 24, 2023 for Calicorena group (HLT)

library(tidyverse)

#df_caca_sr_sumdat=read.csv("2023_CaCa_SoilRespiration_sumdata_L2.csv",header=TRUE) # reads in the soil respiration file
df_caca_sr_sumdat=read.csv("https://www.dropbox.com/scl/fi/0vfcrvg0ls4sp870eayf1/2023_CaCa_SoilRespiration_sumdata_L2.csv?rlkey=ceve6x8zhprjjj5vs591hhvl9&st=6htubk1d&dl=1")
df_caca_sr_sumdat$Treatment.type <- as.factor(df_caca_sr_sumdat$Treatment.type) # convert plant from continuous variable to factor
df_caca_sr_sumdat$MsmtCycle <- as.factor(as.character(df_caca_sr_sumdat$MsmtCycle)) # convert plant from continuous variable to factor
df_caca_sr_sumdat$Date <- strptime(df_caca_sr_sumdat$Date, format = "%m/%d/%y") # parses date into a date that R understands - step1
df_caca_sr_sumdat$Date <- as.POSIXct(df_caca_sr_sumdat$Date) # parses date into a date that R understands - step2
df_caca_sr_sumdat$EFFLUX <- as.numeric(df_caca_sr_sumdat$EFFLUX) # parses date into a date that R understands - step2

### note - need to change HHMMSS using package hms only since R adds in today's date 
df_caca_sr_sumdat$HHMMSS <- as.POSIXct((df_caca_sr_sumdat$HHMMSS), format="%H:%M:%S") #format time 
df_caca_sr_sumdat$Plant <- as.factor(df_caca_sr_sumdat$Plant) # convert plant from continuous variable to factor

# Reduce to one efflux value per drawdown cycle -----------------------------------------------------
# Also remove extra lines that are not being used for our mean values

msmtcycle_means <- df_caca_sr_sumdat |> 
  filter(Discard == "N") |> # keep only the logs not flagged to be discarded
  filter(Mode == "3") |> # remove the summary efflux values (Mode == 4) and extra logs (Mode ==2) since we are not using them
  group_by(Date, Plant, Treatment.type, MsmtCycle) |> 
  summarize(
    efflux = mean(EFFLUX),
    logs = n(),
    Tsoil_C = mean(Tsoil_C),
    Tair = mean(Tair),
    Time = mean(HHMMSS)
  ) 

# Reduce to one mean efflux value per plant per day -----------------------------------------------------

daily_plant_sr_means <- msmtcycle_means |> 
  filter(efflux > 0) |> # keep only the efflux values that are positive (a few negative values from mode = 4 data, when not all points were logged)
  group_by(Date, Plant, Treatment.type) |> 
  summarize(
    mean_efflux = mean(efflux), # grand mean of efflux values for each collar per day
    n_cycles = n(), # number of measurement cycles
    sd_efflux = sd(efflux, na.rm = TRUE),
    se_efflux = sd_efflux/sqrt(n_cycles),
    Tsoil_C = mean(Tsoil_C),
    Tair = mean(Tair),
    Msmt_Time = mean(Time)
  ) 

# plot each plant for each day
daily_plant_sr_means |>
  ggplot(aes(Date, mean_efflux)) +
  geom_point(aes(color = Plant, shape = Plant)) +
  geom_line(aes(color = Plant, linetype = Plant)) +
  geom_errorbar(aes(ymin = mean_efflux - se_efflux, ymax = mean_efflux + se_efflux), 
                linewidth = 0.4, 
                position = position_dodge(0.3), # not working... error bars should be offset slightly
                width = 0.2) + 
  facet_grid(Treatment.type ~ .,)

# box plot of Treatment.type differences in soil respiration
# currently we have one measurement per plant (no repeats among days) -- we will need to consider 
# repeated sampling when adding more data

daily_plant_sr_means |>
  ggplot(aes(Treatment.type, mean_efflux)) +
  geom_boxplot()

# ANOVA of Treatment.type differences (note that data have not been normalized)

Treatment.type_sr_aov <- aov(mean_efflux ~ Treatment.type, data = daily_plant_sr_means)
summary(Treatment.type_sr_aov)

# Explore how soil respiration varies with soil temperature

daily_plant_sr_means |>
  filter(Tsoil_C > 0) |> # keep only the logs where Tsoil is greater than 0 (negative values are due to non-functioning soil T probe)
  ggplot(aes(Tsoil_C, mean_efflux)) +
  geom_point(aes(color=Treatment.type, shape = Treatment.type))

# Daily mean differences between Treatment.types  -----------------------------------------------------

daily_Treatment.type_sr_means <- daily_plant_sr_means |> 
  group_by(Date, Treatment.type) |> 
  summarize(
    efflux_by_Treatment.type = mean(mean_efflux), # grand mean of efflux values for each collar per day
    n_plants = n(), # number of measurement cycles
    sd_efflux = sd(mean_efflux, na.rm = TRUE),
    se_efflux = sd_efflux/sqrt(n_plants)
  ) 

# plot Treatment.type means for each day
daily_Treatment.type_sr_means |>
  ggplot(aes(Date, efflux_by_Treatment.type)) +
  geom_point(aes(color = Treatment.type, shape = Treatment.type)) +
  geom_line(aes(color = Treatment.type, linetype = Treatment.type)) +
  geom_errorbar(aes(ymin = efflux_by_Treatment.type - se_efflux, ymax = efflux_by_Treatment.type + se_efflux), 
                linewidth = 0.4, 
                position = position_dodge(0.3), # not working... error bars should be offset slightly
                width = 0.2) 


