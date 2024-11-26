# This script takes the L2 sumdat file from the LiCor 6400 and summarizes the key output variables.
# This was created for 2023 Gobabeb IRES projects.
# Created July 7, 2023 Heather Throop
# Updated July 24, 2023 for Calicorema group
# Updated July 29, 2023 HT

library(tidyverse) # note that you need the tidyverse package installed in R to run this script

df_CaCa_ps_sumdat=read.csv("2023_cacarires_sumdat_updated.csv",header=TRUE) # reads in the photosynthesis file
df_CaCa_ps_sumdat$Date <- strptime(df_CaCa_ps_sumdat$Date, format = "%m/%d/%y") # parses date into a date that R understands - step1
df_CaCa_ps_sumdat$Date <- as.POSIXct(df_CaCa_ps_sumdat$Date) # parses date into a date that R understands - step2
### note - need to change this to hms using package hms only since R adds in today's date 
df_CaCa_ps_sumdat$HHMMSS <- as.POSIXct((df_CaCa_ps_sumdat$HHMMSS), format="%H:%M:%S") #format time 
df_CaCa_ps_sumdat$Plant <- as.factor(df_CaCa_ps_sumdat$Plant) # convert plant from continuous variable to factor
df_CaCa_ps_sumdat$Stem <- as.factor(df_CaCa_ps_sumdat$Stem) # convert stem from continuous variable to factor

# look at all the Photo data first to check for strange values, etc
df_CaCa_ps_sumdat |>
  ggplot(aes(Date, Photo)) +
  geom_point(aes(color = Plant, shape = Stem)) +
  facet_grid(Treatment ~ .,)

df_CaCa_ps_cleaned <- df_CaCa_ps_sumdat |>
  filter(Discard == "N") |> # keep only the logs not flagged to be discarded
  filter(Ci < 420) |> # remove Ci values greater than 420
  filter(Ci > 100) # remove Ci values less then 100

df_CaCa_ps_sumdat |>
  ggplot(aes(Date, Photo)) +
  geom_point(aes(color = Plant, shape = Stem)) +
  facet_grid(Treatment ~ .,)

# Reduce to one mean value for photosynthesis (and other key variables) per stem -----------------------------------------------------
# Also remove extra lines that are not being used for our mean values

stem_means <- df_CaCa_ps_cleaned |> 
  filter(Discard == "N") |> # keep only the logs not flagged to be discarded
  filter(Photo > -1) |> # remove net respiration data if <-1 (WE SHOULD DISCUSS...)
  group_by(Date, Plant, Treatment, Stem) |> 
  summarize(
    Photo = mean(Photo),
    Cond = mean(Cond),
    Ci = mean(Ci),
    Trmmol = mean(Trmmol),
    TBlk = mean(TBlk),
    Tleaf = mean(Tleaf),
    PARi = mean(PARi),
    CO2S = mean(CO2S),
    Time = mean(HHMMSS),
    logs = n(),
    Area = mean(Area)
  ) 

# Reduce to one mean value for photosynthesis (and other key variables) per plant per day -----------------------------------------------------

daily_plant_ps_means <- stem_means |> 
  group_by(Date, Plant, Treatment) |> 
  summarize(
    mean_Photo = mean(Photo), # grand mean of Photo values for each plant for each day
    n = n(), # number stems per plant
    sd_Photo = sd(Photo, na.rm = TRUE),
    se_Photo = sd_Photo/sqrt(n),
    Cond = mean(Cond),
    Ci = mean(Ci),
    Trmmol = mean(Trmmol),
    TBlk = mean(TBlk),
    Tleaf = mean(Tleaf),
    PARi = mean(PARi),
    CO2S = mean(CO2S),
    Time = mean(Time),
    Area = mean(Area)
  ) 


# box plot of treatment differences in photosynthesis
# does not account for repeated sampling of the same plants

daily_plant_ps_means |>
  ggplot(aes(Treatment, mean_Photo)) +
  geom_boxplot()

daily_plant_ps_means |>
  ggplot(aes(Treatment, Trmmol)) +
  geom_boxplot()

daily_plant_ps_means |>
  ggplot(aes(Treatment, Cond)) +
  geom_boxplot()

# ANOVA of treatment differences (note that data have not been normalized & does not take repeated measures into account)
treatment_ps_aov <- aov(mean_Photo ~ Treatment, data = daily_plant_ps_means)
summary(treatment_ps_aov)

# Explore how photosynthesis varies with stem area

daily_plant_ps_means|>
  ggplot(aes(Area, Photo)) +
  geom_point(aes(color=Treatment, shape = Treatment))

# Daily mean differences between treatments  -----------------------------------------------------

daily_treatment_ps_means <- daily_plant_ps_means |> 
  group_by(Date, Treatment) |> 
  summarize(
    photo_by_trt = mean(mean_Photo), # grand mean of efflux values for each collar per day
    n_plants = n(), # number of plants measured
    sd_photo = sd(mean_Photo, na.rm = TRUE),
    se_photo = sd_photo/sqrt(n_plants)
  ) 

# plot treatment means for each day
daily_treatment_ps_means |>
  ggplot(aes(x = Date, y = photo_by_trt, group = Treatment, color = Treatment)) +
  geom_point(aes(color = Treatment, shape = Treatment), size = 4) +
  geom_line(aes(color = Treatment, linetype = Treatment), linewidth = 0.85) +
  geom_errorbar(aes(ymin = photo_by_trt - se_photo, ymax = photo_by_trt + se_photo), 
                linewidth = 0.4, 
                position = position_dodge(0.3), # not working... error bars should be offset slightly
                width = 0.6) +
  labs(x= "", y= "Photosynthesis (µmol m-2 sec-1)") +
  labs(title = "") +
  theme_classic() +
  theme(panel.grid.major = element_blank(), #no grid lines
        panel.grid.minor = element_blank())  +
  theme(legend.title=element_blank()) +
 # scale_color_discrete(breaks=c("Control","SmallPulse","BigPulse")) 
  scale_color_manual(values=c("#d7191c", "#008837", "#2c7bb6"), 
                     breaks=c("Control", "SmallPulse", "BigPulse"),
                     labels=c("Control", "Small Pulse", "Big Pulse")) +
  scale_linetype_manual(values=c("dotdash", "dashed", "solid"), 
                        breaks=c("Control", "SmallPulse", "BigPulse"),
                        labels=c("Control", "Small Pulse", "Big Pulse")) +
  scale_shape_manual(values=c("square", "triangle", "circle"), 
                        breaks=c("Control", "SmallPulse", "BigPulse"),
                        labels=c("Control", "Small Pulse", "Big Pulse")) +
  theme(legend.position=c(0.3, 0.8)) +
  theme(axis.title.x = element_text(face="bold", size=20),
        axis.text.x  = element_text(face="bold", size=20)) +
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(face="bold", size=20)) +
  theme(legend.text = element_text(size = 22, face = "bold")) +
  theme(strip.text.y = element_text(size=16, face = "bold")) +
  theme(legend.key.width = unit(5, "line")) +
  theme(axis.line = element_line(colour = 'black', size = 1))
