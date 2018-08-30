# Alpha diversity analyses

# Sample completeness per site-----------------------------------------------------
#Data
repliflies_site <- 
  cleanflies %>% 
  ##Pool different trapping methods by locality (site)
  dplyr::select(Locality,Species, Abundance) %>% 
  group_by(Locality,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 

#Make site names row names
repliflies_site <- 
  data.frame(repliflies_site[,2:334], 
             row.names = repliflies_site$Locality)

#Compute completeness
completeness <- 
  iNEXT(t(repliflies_site), 
        q=0, 
        datatype = "abundance")
##View values
DataInfo(t(repliflies_site), 
         datatype = "abundance")

#Plot
pdf("completeness_site.pdf", 
    height = 8,
    width = 15)
ggiNEXT(completeness)
dev.off()

# Sample completeness per moisture regime per site-----------------------------------------------------
#Moosonee,ON
##Data
replifliesMO <- 
  cleanflies %>% 
  filter(Locality== "Moosonee") %>% 
  ###Pool replicates per moisture regime
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
##Make moisture regime row names
replifliesMO <- 
  data.frame(replifliesMO[,2:163], 
             row.names = replifliesMO$Moisture_Regime)
##Compute completeness
completeness <- 
  iNEXT(t(replifliesMO), 
        q=0, 
        datatype = "abundance")
##Plot
plot1 <- 
  ggiNEXT(completeness) + 
  ggtitle("Moosonee")+ 
  xlim(0, 1400) + 
  ylim(0,175)

#Churchill, MB
##Data
replifliesCH <- 
  cleanflies %>% 
  filter(Locality== "Churchill") %>% 
  ###Pool replicates per moisture regime
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
##Make moisture regime row names
replifliesCH <- 
  data.frame(replifliesCH[,2:83], 
             row.names = replifliesCH$Moisture_Regime)
##Compute completeness
completeness <- 
  iNEXT(t(replifliesCH), 
        q=0, 
        datatype = "abundance")
##Plot
plot2 <- 
  ggiNEXT(completeness) + 
  ggtitle("Churchill")+ 
  xlim(0, 1400) + 
  ylim(0,175)

#Aulavik National Park, NU
##Data
replifliesAU <- 
  cleanflies %>% 
  filter(Locality== "Aulavik_National_Park") %>% 
  ###Pool replicates per moisture regime
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
##Make moisture regime row names
replifliesAU <- 
  data.frame(replifliesAU[,2:13], 
             row.names = replifliesAU$Moisture_Regime)
##Compute completeness
completeness <- 
  iNEXT(t(replifliesAU), 
        q=0, 
        datatype = "abundance")
##Plot
plot3 <- 
  ggiNEXT(completeness) + 
  ggtitle("Aulavik National Park")+ 
  xlim(0, 1400) + 
  ylim(0,175)

#Goose Bay, NL
##Data
replifliesGB <- 
  cleanflies %>% 
  filter(Locality== "Goose_Bay") %>% 
  ###Pool replicates per moisture regime
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
##Make moisture regime row names
replifliesGB <- 
  data.frame(replifliesGB[,2:21], 
             row.names = replifliesGB$Moisture_Regime)
##Compute completeness
completeness <- 
  iNEXT(t(replifliesGB), 
        q=0, 
        datatype = "abundance")
##Plot
plot4 <- 
  ggiNEXT(completeness) + 
  ggtitle("Goose Bay")+
  xlim(0, 1400) + 
  ylim(0,175)

#Kugluktuk, NU
##Data
replifliesKG <- 
  cleanflies %>% 
  filter(Locality== "Kugluktuk") %>% 
  ###Pool replicates per moisture regime
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
##Make moisture regime row names
replifliesKG <- 
  data.frame(replifliesKG[,2:29], 
             row.names = replifliesKG$Moisture_Regime)
##Compute completeness
completeness <- 
  iNEXT(t(replifliesKG), 
        q=0, 
        datatype = "abundance")
##Plot
plot5 <- 
  ggiNEXT(completeness) + 
  ggtitle("Kugluktuk")+ 
  xlim(0, 1400) + 
  ylim(0,175)

#Cambridge Bay, NU
##Data
replifliesCB <- 
  cleanflies %>% 
  filter(Locality== "Cambridge_Bay") %>% 
  ###Pool replicates per moisture regime
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
##Make moisture regime row names
replifliesCB <- 
  data.frame(replifliesCB[,2:13], 
             row.names = replifliesCB$Moisture_Regime)
##Compute completeness
completeness <- 
  iNEXT(t(replifliesCB), 
        q=0, 
        datatype = "abundance")
##Plot
plot6 <- 
  ggiNEXT(completeness) + ggtitle("Cambridge Bay")+ xlim(0, 1400) + ylim(0,175)

#Norman Wells, NT
##Data
replifliesNW <- 
  cleanflies %>% 
  filter(Locality== "Norman_Wells") %>% 
  ###Pool replicates per moisture regime
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
##Make moisture regime row names
replifliesNW <- 
  data.frame(replifliesNW[,2:97], 
             row.names = replifliesNW$Moisture_Regime)
##Completeness
completeness <- 
  iNEXT(t(replifliesNW), 
        q=0, 
        datatype = "abundance")
##Plot
plot7 <- 
  ggiNEXT(completeness) + 
  ggtitle("Norman Wells")+ 
  xlim(0, 1400) + 
  ylim(0,175)

#Schefferville, QC
##Data
replifliesSC <- 
  cleanflies %>% 
  filter(Locality== "Schefferville") %>% 
  ###Pool replicates per moisture regime
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
##Make moisture regime row names
replifliesSC <- 
  data.frame(replifliesSC[,2:40], 
             row.names = replifliesSC$Moisture_Regime)
##Compute completeness
completeness <- 
  iNEXT(t(replifliesSC), 
        q=0, 
        datatype = "abundance")
##Plot
plot8 <- 
  ggiNEXT(completeness) + ggtitle("Schefferville")+ xlim(0, 1400) + ylim(0,175)

#Iqaluit, NU
##Data
replifliesIQ<- 
  cleanflies %>% 
  filter(Locality== "Iqaluit") %>%
  ###Pool replicates per moisture regime
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
##Make moisture regime row names
replifliesIQ <- 
  data.frame(replifliesIQ[,2:12], 
             row.names = replifliesIQ$Moisture_Regime)
##Compute completeness
completeness <- 
  iNEXT(t(replifliesIQ), 
        q=0, 
        datatype = "abundance")
##Plot
plot9 <- 
  ggiNEXT(completeness) + 
  ggtitle("Iqaluit")+ 
  xlim(0, 1400) + 
  ylim(0,175)

#Yellowknife, NT
##Data
replifliesYK<- 
  cleanflies %>% 
  filter(Locality== "Yellowknife") %>%
  ###Pool replicates per moisture regime
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0)
##Make moisture regime row names
replifliesYK <- 
  data.frame(replifliesYK[,2:72],
             row.names = replifliesYK$Moisture_Regime)
##Compute completeness
completeness <- 
  iNEXT(t(replifliesYK), 
        q=0, 
        datatype = "abundance")
##Plot
plot10 <- 
  ggiNEXT(completeness) + 
  ggtitle("Yellowknife")+ 
  xlim(0, 1400) + 
  ylim(0,175)

#Tombstone Mountains, YT
##Data
replifliesTM <- 
  cleanflies %>% 
  filter(Locality== "Tombstone_Mountains") %>% 
  ###Pool replicates per moisture regime
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
##Make moisture regime row names
replifliesTM <- 
  data.frame(replifliesTM[,2:66], 
             row.names = replifliesTM$Moisture_Regime)
##Compute completeness
completeness <- 
  iNEXT(t(replifliesTM), 
        q=0, 
        datatype = "abundance")
##Plot
plot11 <- 
  ggiNEXT(completeness) + ggtitle("Tombstone Mountains")+ xlim(0, 1400) + ylim(0,175)

#Printing plots
pdf("completeness_moisture.pdf", height = 30, width =30)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, 
             plot7, plot8, plot9, plot10, plot11, 
             ncol =3)
dev.off()


# Diversity measurements per site -----------------------------------------
#Compute raw richness
Richness_site <- 
  specnumber(repliflies_site, 
             MARGIN = 1)

#Compute estimated richness (Chao1 index)
##Compute index
Chao1_site <- 
  t(estimateR(repliflies_site))
##Only keep column with index
Chao1_site <- 
  Chao1_site[,2]

#Compute Shannon index
Shannon_site <- 
  diversity(repliflies_site, 
            index = "shannon", 
            MARGIN = 1, 
            base = exp(1))

#Compute abundance
siteabundance <- 
  cleanflies %>% 
  ##Pool different trapping methods by replicate
  dplyr::select(Locality, Abundance) %>% 
  group_by(Locality) %>%
  summarise_all(funs(sum))

#Bind all measurement vectors
siteindices <- 
  cbind.data.frame(siteabundance[,2], 
                   Richness_site,
                   Shannon_site,
                   Chao1_site)


# Testing for effect of environmental variables on diversity measuremenets ----------------------------------------------------
#Observed Richness
##Model
model_richness <- 
  glmmPQL(Richness~ 
            coldest_quarter*t_range + Moisture_Regime,
          random =~1|Locality/moisture, 
          family = "quasipoisson"(link="sqrt"),
          data = replicate_analysis)
##Assumptions
plot(model_richness)
qqnorm(residuals(model_richness))
qqline(residuals(model_richness), col = "red")
##Summary and tests
summary(model_richness)
Anova(model_richness)
##R-squared
r.squaredGLMM(model_richness)
##Plot
###visreg
visreg(model_richness)
###ggeffects
####Data
effect_richness <- 
  ggeffect(model_richness,
           type = "re",
           ##selecting  restricted set of t_range value for computation and visibility
           terms = c("coldest_quarter", "t_range [42, 46.3, 50, 53.1]"),
           ci.lvl = 0.95)
####Graphics
col <- 
  ifelse(effect_richness$group ==42,
         "deepskyblue4",
         ifelse(effect_richness$group ==46.3,
                "gold4",
                ifelse(effect_richness$group ==50,
                       "darkorange3", "firebrick4")))
####Rescale
effect_richness[,2:4] <- 
  effect_richness[,2:4] +1
####Plot
plot_richness <- 
  plot(effect_richness,
       ci = T)+
  geom_point(data = replicate_analysis,
             mapping = aes(x = coldest_quarter, 
                           y = Richness +1), 
             colour = c("grey30", "grey80")[replicate_analysis$Moisture_Regime], 
             fill = c("grey30", "grey80")[replicate_analysis$Moisture_Regime],
             pch = 16,
             cex=2) +
  labs(color = "Annual T range (°C)") +
  ggtitle("") +
  scale_y_continuous(trans="log",
                     breaks = c(0,10,70)) +
  xlab("Minimum T of coldest quarter (°C)") +
  ylab("Observed species richness") + 
  scale_color_manual(labels = c(42, 46.3, 50, 53.1), 
                     values = c("deepskyblue4", "gold4",
                                "darkorange3", "firebrick4")) +
  scale_fill_manual(values = c("deepskyblue4", "gold4",
                               "darkorange3", "firebrick4")) +
  theme(legend.position = c(0.1,0.9),
        legend.text = element_text(size = rel(0.6)),
        legend.key.size =  unit(0.2, "in"),
        legend.title = element_text(size = rel(0.7)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Chao1
##Model
model_chao1 <- 
  glmmPQL(Chao1~ 
            coldest_quarter*t_range + Moisture_Regime,
          random =~1|Locality/moisture,
          family = "quasipoisson"(link="sqrt"),
          data = replicate_analysis)
##Assumptions
plot(model_chao1)
qqnorm(residuals(model_chao1))
qqline(residuals(model_chao1), col = "red")
##Summary and tests
summary(model_chao1)
Anova(model_chao1)
##R-squared
r.squaredGLMM(model_chao1)
##Plot
###visreg
visreg(model_chao1)
###ggeffects
####Data
effect_chao1 <- 
  ggeffect(model_chao1,
           type = "re",
           ##selecting  restricted set of t_range value for computation and visibility
           terms = c("coldest_quarter", "t_range [42, 46.3, 50, 53.1]"),
           ci.lvl = 0.95)
####Graphics
col <- 
  ifelse(effect_chao1$group ==42,
         "deepskyblue4",
         ifelse(effect_chao1$group ==46.3,
                "gold4",
                ifelse(effect_chao1$group ==50,
                       "darkorange3", "firebrick4")))
####Rescale
effect_chao1[,2:4] <- 
  effect_chao1[,2:4] +1
####Plot
plot_chao1 <- 
  plot(effect_chao1,
       ci = T)+
  geom_point(data = replicate_analysis,
             mapping = aes(x = coldest_quarter, 
                           y = Chao1 +1), 
             colour = c("grey30", "grey80")[replicate_analysis$Moisture_Regime], 
             fill = c("grey30", "grey80")[replicate_analysis$Moisture_Regime],
             pch = 16,
             cex=2) +
  labs(color = "Annual T range (°C)") +
  ggtitle("") +
  scale_y_continuous(trans="log",
                     breaks = c(0,10,150)) +
  xlab("Minimum T of coldest quarter (°C)") +
  ylab("Estimated species richness") + 
  scale_color_manual(labels = c(42, 46.3, 50, 53.1), 
                     values = c("deepskyblue4", "gold4",
                                "darkorange3", "firebrick4")) +
  scale_fill_manual(values = c("deepskyblue4", "gold4",
                               "darkorange3", "firebrick4")) +
  theme(legend.position = c(0.1,0.9),
        legend.text = element_text(size = rel(0.6)),
        legend.key.size =  unit(0.2, "in"),
        legend.title = element_text(size = rel(0.7)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Abundance
##Model
model_abundance <- 
  glmmPQL(Abundance~ 
            coldest_quarter*t_range + Moisture_Regime, 
          random = ~1|Locality/moisture,
          family = "quasipoisson"(link ="sqrt"),
          data = replicate_analysis)
##Assumptions
plot(model_abundance)
qqnorm(residuals(model_abundance))
qqline(residuals(model_abundance), col = "red")
##Summary and tests
summary(model_abundance)
Anova(model_abundance)
##R-squared
r.squaredGLMM(model_abundance)
##Plot
###visreg
visreg(model_abundance)
###ggeffects
####Data
effect_abundance <- 
  ggeffect(model_abundance,
           type = "re",
           ##selecting  restricted set of t_range value for computation and visibility
           terms = c("coldest_quarter", "t_range [42, 46.3, 50, 53.1]"),
           ci.lvl = 0.95)
####Graphics
col <- 
  ifelse(effect_abundance$group ==42,
         "deepskyblue4",
         ifelse(effect_abundance$group ==46.3,
                "gold4",
                ifelse(effect_abundance$group ==50,
                       "darkorange3", "firebrick4")))
####Rescale
effect_abundance[,2:4] <- 
  effect_abundance[,2:4] +3
####Plot
plot_abundance <- 
  plot(effect_abundance,
       ci = T)+
  geom_point(data = replicate_analysis,
             mapping = aes(x = coldest_quarter, 
                           y = Abundance + 3), 
             colour = c("grey30", "grey80")[replicate_analysis$Moisture_Regime], 
             fill = c("grey30", "grey80")[replicate_analysis$Moisture_Regime],
             pch = 16,
             cex=2) +
  labs(color = "Annual T range (°C)") +
  ggtitle("") +
  scale_y_continuous(trans="log",
                     breaks = c(0,10,350)) +
  xlab("Minimum T of coldest quarter (°C)") +
  ylab("Abundance") + 
  scale_color_manual(labels = c(42, 46.3, 50, 53.1), 
                     values = c("deepskyblue4", "gold4",
                                "darkorange3", "firebrick4")) +
  scale_fill_manual(values = c("deepskyblue4", "gold4",
                               "darkorange3", "firebrick4")) +
  theme(legend.position = c(0.1,0.9),
        legend.text = element_text(size = rel(0.6)),
        legend.key.size =  unit(0.2, "in"),
        legend.title = element_text(size = rel(0.7)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Print plots in tiff format ---------------------------------------------------------
tiff("richness_plot.tiff",
     width = 4800,
     height = 3200, 
     units = "px", 
     res = 800)
plot_richness
dev.off()

tiff("chao1_plot.tiff",
     width = 4800,
     height = 3200, 
     units = "px", 
     res = 800)
plot_chao1
dev.off()

tiff("abundance_plot.tiff",
     width = 4800,
     height = 3200, 
     units = "px", 
     res = 800)
plot_abundance
dev.off()
