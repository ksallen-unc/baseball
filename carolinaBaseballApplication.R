# written by Kate Allen, Nov 2019
# Project purpose: an application piece for my UNC Baseball analytics job, comparing MLB pitching + hitting pre + post DH introduction

# import statements
library(ggplot2)
library(rBaseball)
library(tidyverse)
library(readr)
library(knitr)
library(kableExtra)

# PITCHING STATS

# importing pitching data
pitchingdata <- read.csv("pitching_data.csv")

# separating out AL data
pitchingAL <- pitchingdata %>% filter(league == "AL")
pitchingALpreDH <- pitchingAL %>% filter(year <= "1972")
pitchingALpostDH <- pitchingAL %>% filter(year >= "1973")

# separating out NL data
pitchingNL <- pitchingdata %>% filter(league == "NL")
pitchingNLpreDH <- pitchingNL %>% filter(year <= "1972")
pitchingNLpostDH <- pitchingAL %>% filter(year >= "1973")

# CALCULATING PITCHING STATS OF INTEREST -- All Teams, Pre DH vs Post DH
# QUESTION: Does having a designated hitter allow AL teams to utilize "better" pitchers? (not just those that can hit well)
#           This effect would likely be cancelled out by the harder lineups teams would be facing (DHs better than pitchers at    
#           hitting)
#           No effect on NL because no interleague play until '93, so just comparing AL with NL for reference

# BABIP
AL_PitchingBABIP = round(mean(pitchingAL$BABIP), 3)               # 0.272
AL_PitchingBABIP_preDH = round(mean(pitchingALpreDH$BABIP), 3)     # 0.266
AL_PitchingBABIP_postDH = round(mean(pitchingALpostDH$BABIP), 3)   # 0.278

NL_PitchingBABIP = round(mean(pitchingNL$BABIP), 3)               # 0.277
NL_PitchingBABIP_preDH = round(mean(pitchingALpreDH$BABIP), 3)    # 0.266
NL_PitchingBABIP_postDH = round(mean(pitchingNLpostDH$BABIP), 3)  # 0.278

# ERA
AL_ERA = round(mean(pitchingAL$ERA), 3)               # 3.705
AL_ERA_preDH = round(mean(pitchingALpreDH$ERA), 3)    # 3.539
AL_ERA_postDH = round(mean(pitchingALpostDH$ERA), 3)  # 3.875

NL_ERA = round(mean(pitchingNL$ERA), 3)               # 3.610
NL_ERA_preDH = round(mean(pitchingNLpreDH$ERA), 3)    # 3.584
NL_ERA_postDH = round(mean(pitchingNLpostDH$ERA), 3)  # 3.875

# FIP
AL_FIP = round(mean(pitchingAL$FIP), 3)               # 3.771
AL_FIP_preDH = round(mean(pitchingALpreDH$FIP), 3)    # 3.660
AL_FIP_postDH = round(mean(pitchingALpostDH$FIP), 3)  # 3.885

NL_FIP = round(mean(pitchingNL$FIP), 3)               # 3.539
NL_FIP_preDH = round(mean(pitchingNLpreDH$FIP), 3)    # 3.460
NL_FIP_postDH = round(mean(pitchingNLpostDH$FIP), 3)  # 3.885
# Pitching Data -- tables

# Defining data
pitchingCategories <- c("National League", "American League")

preDH_pitchingBABIP <- c(AL_PitchingBABIP_preDH, NL_PitchingBABIP_preDH)
postDH_pitchingBABIP <- c(AL_PitchingBABIP_postDH, NL_PitchingBABIP_postDH)

preDH_ERA <- c(AL_ERA_preDH, NL_ERA_preDH)
postDH_ERA <- c(AL_ERA_postDH, NL_ERA_postDH)

preDH_FIP <- c(AL_FIP_preDH, NL_FIP_preDH)
postDH_FIP <- c(AL_FIP_postDH, NL_FIP_postDH)

preDHpitchingDataTable <- data.frame(pitchingCategories, preDH_pitchingBABIP, preDH_ERA, preDH_FIP)
postDHpitchingDataTable <- data.frame(pitchingCategories, postDH_pitchingBABIP, postDH_ERA, postDH_FIP)

preDHpitchingDataTable_manip <- kable(preDHpitchingDataTable, caption = "Pitching Statistics Pre DH", col.names = c("Category", "BABIP", "ERA", "FIP")) %>% kable_styling(c("striped", "bordered"))
preDHpitchingDataTable_manip

postDHpitchingDataTable_manip <- kable(postDHpitchingDataTable, caption = "Pitching Statistics Post DH", col.names = c("Category", "BABIP", "ERA", "FIP")) %>% kable_styling(c("striped", "bordered"))
postDHpitchingDataTable_manip

# Pitching data -- Histograms

# BABIP
# Pre DH
ggplot(data = preDHpitchingDataTable, aes(x=pitchingCategories, y=preDH_pitchingBABIP)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme_minimal() + 
  geom_label(aes(x = pitchingCategories, y = preDH_pitchingBABIP, label = preDH_pitchingBABIP), hjust = 0.5, vjust = 0, colour = 
"black", fill = NA, label.size = NA, family="Helvetica", size = 4) + 
  xlab("League") +
  ylab("BABIP") + 
  ggtitle("BABIP pre DH") + 
  coord_cartesian(ylim = c(0,0.285))


# Post DH
ggplot(data = postDHpitchingDataTable, aes(x=pitchingCategories, y=postDH_pitchingBABIP)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme_minimal() + 
  geom_label(aes(x = pitchingCategories, y = postDH_pitchingBABIP, label = postDH_pitchingBABIP), hjust = 0.5, vjust = 0, colour = 
"black", fill = NA, label.size = NA, family="Helvetica", size = 4) + 
  xlab("League") +
  ylab("BABIP") + 
  ggtitle("BABIP post DH") + 
  coord_cartesian(ylim = c(0,0.285))


# ERA
# Pre DH
ggplot(data = preDHpitchingDataTable, aes(x=pitchingCategories, y=preDH_ERA)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme_minimal() + 
  geom_label(aes(x = pitchingCategories, y = preDH_ERA, label = preDH_ERA), hjust = 0.5, vjust = 0, colour = 
"black", fill = NA, label.size = NA, family="Helvetica", size = 4) + 
  xlab("League") +
  ylab("ERA") + 
  ggtitle("ERA pre DH") + 
  coord_cartesian(ylim = c(0,3.9))


# Post DH
ggplot(data = postDHpitchingDataTable, aes(x=pitchingCategories, y=postDH_ERA)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme_minimal() + 
  geom_label(aes(x = pitchingCategories, y = postDH_ERA, label = postDH_ERA), hjust = 0.5, vjust = 0, colour = 
"black", fill = NA, label.size = NA, family="Helvetica", size = 4) + 
  xlab("League") +
  ylab("ERA") + 
  ggtitle("ERA post DH") + 
  coord_cartesian(ylim = c(0,3.9))


# FIP
# Pre DH
ggplot(data = preDHpitchingDataTable, aes(x=pitchingCategories, y=preDH_FIP)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme_minimal() + 
  geom_label(aes(x = pitchingCategories, y = preDH_FIP, label = preDH_FIP), hjust = 0.5, vjust = 0, colour = 
"black", fill = NA, label.size = NA, family="Helvetica", size = 4) + 
  xlab("League") +
  ylab("FIP") + 
  ggtitle("FIP pre DH") + 
  coord_cartesian(ylim = c(0,3.9))


# Post DH
ggplot(data = postDHpitchingDataTable, aes(x=pitchingCategories, y=postDH_FIP)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme_minimal() + 
  geom_label(aes(x = pitchingCategories, y = postDH_FIP, label = postDH_FIP), hjust = 0.5, vjust = 0, colour = 
"black", fill = NA, label.size = NA, family="Helvetica", size = 4) + 
  xlab("League") +
  ylab("FIP") + 
  ggtitle("FIP post DH") + 
  coord_cartesian(ylim = c(0,3.9))


# BATTING STATS

# importing batting data
battingdata <- read.csv("batting_data.csv")
# separating out AL data
battingAL <- battingdata %>% filter(league == "AL")
battingALpreDH <- battingAL %>% filter(year <= "1972")
battingALpostDH <- battingAL %>% filter(year >= "1973")
# separating out NL data
battingNL <- battingdata %>% filter(league == "NL")
battingNLpreDH <- battingNL %>% filter(year <= "1972")
battingNLpostDH <- battingNL %>% filter(year >= "1973")
# PLOTTING DATA OF INTEREST -- Looking at Trends over Time

# AVG
ggplot(data=battingAL, mapping=aes(x=year, y=AVG)) + 
  geom_point() + 
  xlab("Year") + 
  ylab("AVG") + 
  ggtitle("American League AVG") + 
  coord_cartesian(ylim = c(0.21, 0.29))


ggplot(data=battingNL, mapping=aes(x=year, y=AVG)) + 
  geom_point() + 
  xlab("Year") + 
  ylab("AVG") + 
  ggtitle("National League AVG") +
  coord_cartesian(ylim = c(0.21, 0.29))


# wOBA
ggplot(data=battingAL, mapping=aes(x=year, y=wOBA)) + 
  geom_point() + 
  xlab("Year") + 
  ylab("wOBA") + 
  ggtitle("American League wOBA") +
  coord_cartesian(ylim = c(0.26, 0.36))


ggplot(data=battingNL, mapping=aes(x=year, y=wOBA)) + 
  geom_point() + 
  xlab("Year") + 
  ylab("wOBA") + 
  ggtitle("National League wOBA") + 
  coord_cartesian(ylim = c(0.26, 0.36))


# RBI, BABIP
# OBP
ggplot(data=battingAL, mapping=aes(x=year, y=OBP)) + 
  geom_point() + 
  xlab("Year") + 
  ylab("OBP") + 
  ggtitle("American League OBP") + 
  coord_cartesian(ylim = c(0.27, 0.36))


ggplot(data=battingNL, mapping=aes(x=year, y=OBP)) + 
  geom_point() + 
  xlab("Year") + 
  ylab("OBP") + 
  ggtitle("National League OBP") + 
  coord_cartesian(ylim = c(0.27, 0.36))


# RBI
ggplot(data=battingAL, mapping=aes(x=year, y=RBI)) + 
  geom_point() + 
  xlab("Year") + 
  ylab("RBI") + 
  ggtitle("American League RBI")


ggplot(data=battingNL, mapping=aes(x=year, y=RBI)) + 
  geom_point() + 
  xlab("Year") + 
  ylab("RBI") + 
  ggtitle("National League RBI")


# BABIP
ggplot(data=battingAL, mapping=aes(x=year, y=BABIP)) + 
  geom_point() + 
  xlab("Year") + 
  ylab("BABIP") + 
  ggtitle("American League BABIP")


ggplot(data=battingNL, mapping=aes(x=year, y=BABIP)) + 
  geom_point() + 
  xlab("Year") + 
  ylab("BABIP") + 
  ggtitle("National League BABIP")


# CALCULATING BATTING STATS OF INTEREST -- All Teams, Pre DH vs Post DH
#         Would expect no changes in NL statistics because no interleague play until '93

# AVG should increase for AL w DH
AL_AVG = round(mean(battingAL$AVG),3)                 # 0.253
AL_preDH_AVG = round(mean(battingALpreDH$AVG), 3)     # 0.245
AL_postDH_AVG = round(mean(battingALpostDH$AVG), 3)   # 0.262

NL_AVG = round(mean(battingNL$AVG),3)                 # 0.254
NL_preDH_AVG = round(mean(battingNLpreDH$AVG),3)      # 0.252
NL_postDH_AVG = round(mean(battingNLpostDH$AVG),3)    # 0.257

# wOBA should increase for AL w DH
AL_wOBA = round(mean(battingAL$wOBA),3)               # 0.316
AL_preDH_wOBA = round(mean(battingALpreDH$wOBA), 3)   # 0.309
AL_postDH_wOBA = round(mean(battingALpostDH$wOBA), 3) # 0.323

NL_wOBA = round(mean(battingNL$wOBA),3)               # 0.312
NL_preDH_wOBA = round(mean(battingNLpreDH$wOBA),3)    # 0.309
NL_postDH_wOBA = round(mean(battingNLpostDH$wOBA),3)  # 0.314

# OBP should increase for AL w DH
AL_OBP = round(mean(battingAL$OBP),3)                 # 0.321
AL_preDH_OBP = round(mean(battingALpreDH$OBP), 3)     # 0.315
AL_postDH_OBP = round(mean(battingALpostDH$OBP), 3)   # 0.327

NL_OBP = round(mean(battingNL$OBP),3)               # 0.319
NL_preDH_OBP = round(mean(battingNLpreDH$OBP),3)    # 0.315
NL_postDH_OBP = round(mean(battingNLpostDH$OBP),3)  # 0.323

# RBI should increase for AL w DH
AL_RBI = round(mean(battingAL$RBI),3)                 # 615.418
AL_preDH_RBI = round(mean(battingALpreDH$RBI), 3)     # 599.11
AL_postDH_RBI = round(mean(battingALpostDH$RBI), 3)   # 632.22

NL_RBI = round(mean(battingNL$RBI),3)               # 601.941
NL_preDH_RBI = round(mean(battingNLpreDH$RBI),3)    # 604.993
NL_postDH_RBI = round(mean(battingNLpostDH$RBI),3)  # 598.533

# BABIP should not change at all for AL w DH
AL_BABIP = round(mean(battingAL$BABIP),3)                 # 0.276
AL_preDH_BABIP = round(mean(battingALpreDH$BABIP), 3)     # 0.270
AL_postDH_BABIP = round(mean(battingALpostDH$BABIP), 3)   # 0.282

NL_BABIP = round(mean(battingNL$BABIP),3)               # 0.282
NL_preDH_BABIP = round(mean(battingNLpreDH$BABIP),3)    # 0.281
NL_postDH_BABIP = round(mean(battingNLpostDH$BABIP),3)  # 0.283
# HISTOGRAMS -- Batting Data

# Defining data
battingCategories <- c("AVG", "wOBA", "OBP", "BABIP")

ALPreDHbattingData <- c(AL_preDH_AVG, AL_preDH_wOBA, AL_preDH_OBP, AL_preDH_BABIP)
ALPostDHbattingData <- c(AL_postDH_AVG, AL_postDH_wOBA, AL_postDH_OBP, AL_postDH_BABIP)

NLPreDHbattingData <- c(NL_preDH_AVG, NL_preDH_wOBA, NL_preDH_OBP, NL_preDH_BABIP)
NLPostDHbattingData <- c(NL_postDH_AVG, NL_postDH_wOBA, NL_postDH_OBP, NL_postDH_BABIP)

# Table of data
alBattingDataTable <- data.frame(battingCategories, ALPreDHbattingData, ALPostDHbattingData)
alBattingDataTable_manip <- kable(alBattingDataTable, caption = "American League Batting Statistics Pre and Post DH", col.names = c("Category",
                           "Pre DH",
                           "Post DH")) %>% kable_styling(c("striped", "bordered"))
                           
alBattingDataTable_manip

nlBattingDataTable <- data.frame(battingCategories, NLPreDHbattingData, NLPostDHbattingData)
nlBattingDataTable_manip <- kable(nlBattingDataTable, caption = "National League Batting Statistics Pre and Post DH", col.names = c("Category",
                           "Pre DH",
                           "Post DH")) %>% kable_styling(c("striped", "bordered"))
nlBattingDataTable_manip

# Histograms

# American League
# Pre DH
ggplot(data = alBattingDataTable, aes(x=battingCategories, y=ALPreDHbattingData)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme_minimal() +  
  geom_label(aes(x = battingCategories, y = ALPreDHbattingData, label = ALPreDHbattingData), hjust = .5,  vjust = 0, colour = "black", fill = NA, label.size = NA, family="Helvetica", size = 4) +
  xlab("Statistic") + 
  ylab("Value") + 
  ggtitle("American League Batting Stats Pre-DH") + 
  coord_cartesian(ylim = c(0, 0.330)) +
  scale_fill_brewer( palette = "Blues")


# Post DH
ggplot(data = alBattingDataTable, aes(x=battingCategories, y=ALPostDHbattingData)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme_minimal() + 
  geom_label(aes(x = battingCategories, y = ALPostDHbattingData, label = ALPostDHbattingData), hjust = .5,  vjust = 0, colour = "black", fill = NA, label.size = NA, family="Helvetica", size = 4) +
  xlab("Statistic") + 
  ylab("Value") + 
  ggtitle("American League Batting Stats Post-DH") + 
  coord_cartesian(ylim = c(0, 0.330)) +
  scale_fill_brewer( palette = "Blues")


# National League
# Pre DH
ggplot(data = nlBattingDataTable, aes(x=battingCategories, y=NLPreDHbattingData)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme_minimal() + 
  geom_label(aes(x = battingCategories, y = NLPreDHbattingData, label = NLPreDHbattingData), hjust = .5,  vjust = 0, colour = "black", fill = NA, label.size = NA, family="Helvetica", size = 4) +
  xlab("Statistic") + 
  ylab("Value") + 
  ggtitle("National League Batting Stats Pre-DH") + 
  coord_cartesian(ylim = c(0, 0.330)) +
  scale_fill_brewer(palette = "Blues")


# Post DH
ggplot(data = nlBattingDataTable, aes(x=battingCategories, y=NLPostDHbattingData)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme_minimal() + 
  geom_label(aes(x = battingCategories, y = NLPostDHbattingData, label = NLPostDHbattingData), hjust = .5,  vjust = 0, colour = "black", fill = NA, label.size = NA, family="Helvetica", size = 4) +
  xlab("Statistic") + 
  ylab("Value") + 
  ggtitle("National League Batting Stats Post-DH") + 
  coord_cartesian(ylim = c(0, 0.330))
