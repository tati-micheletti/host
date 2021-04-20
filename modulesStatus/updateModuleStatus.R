# Plotting module's state
# Current status of each family of modules and respective modules (each 
# horizontal bar). 
# For each numeric 'version' (e.g., 0, 1, 2), there are several ecological 
# milestones: design phase (D), 
# working prototype (P), 
# tested locally (T), 
# robust (R), 
# validated locally (V), and
# maintenance phase (M). See appendix for version detailing.
# 
# To update module status, please modify the table and re-run the script

library("Require")
Require("reproducible")
Require("data.table")
Require("ggplot2")

wd <- checkPath(file.path(getwd(), "modulesStatus"), create = TRUE)

DT <- prepInputs(url = "https://drive.google.com/file/d/1aNYmbXec5g_pOM1DZPqFyi60n4Vud39r/view?usp=sharing",
                 targetFile = "moduleStatusTable_FY2.csv", 
                 destinationPath = wd,
                 fun = "data.table::fread")
DT[, versionDescription := NULL] # Simplifying

# Make the combination of version and milestone as an ordered factor
# DT[, "Version_and_milestone" := factor(paste0(version, milestone), 
#                                        levels = c("0D", "0P", "0T", "0R", 
#                                                   "1P", "1T", "1R", "1M", 
#                                                   "2T", "2R", "2M"))]
names(DT)[names(DT) == "colors"] <- "plotColors"
# composedMilestone <- paste0(rep(0:maxVersion, each = 6), c("D","P","T","R","V","M"))
composedMilestone <- c("0D", "0P", "0T", "0R", "1P", "1T", "1R", "1M", "2T", "2R", "2M")
specComponents <- c("LBMR","Biomass_regeneration","Boreal_LBMRDataPrep","gmcsDataPrep","PSP_Clean","Biomass_regenerationPM","scfmLandcoverInit","scfmRegime","scfmDriver","scfmIgnition","scfmEscape","scfmSpread","fireSense_NWT_DataPrep","LBMR2LCC_DataPrep","fireSense_NWT","fireSense_IgnitionPredict","fireSense_SpreadFit","fireSense_EscapePredict","fireSense_EscapeFit","fireSense_SpreadPredict","fireSense_dataPrepFit","fireSense_dataPrepPredict","fireSense_FrequencyFit","caribouPopulationGrowth","caribouRSF","caribouRSF_NT","caribouIEK","birdsNWT","waterfowl","PriorityPlaces","MPB","SBW","development","spadesCBM","wc3Harvest","birdDensityTrends","comm_metricsNWT")
DT[, composedMilestone  := factor(paste0(version, milestone), levels = composedMilestone)]
# DT[, composedMilestone  := paste0(version, milestone)]
DT[, groupOrFamily  := factor(groupOrFamily, unique(DT$groupOrFamily))]
DT[specificComponent == "umbrellasAndTradeoffs", specificComponent := "PriorityPlaces"]
DT[, specificComponent  := factor(specificComponent, levels = specComponents)]

data.table::setkey(DT, specificComponent)

## this version shades in the regions of the graph corresponding to "version" and has the correspondent colors to the hex stickers
p <- ggplot() + 
  geom_col(data = DT[Year == 2019],
           mapping = aes(x = specificComponent, 
                         y = composedMilestone, 
                         fill = groupOrFamily),
           position = position_dodge2(reverse = TRUE),
           alpha = 1) +
  geom_col(data = DT[Year == 2021],
           mapping = aes(x = specificComponent, 
                         y = composedMilestone, 
                         fill = groupOrFamily),
           position = position_dodge2(reverse = TRUE),
           alpha = 0.7) +
  coord_flip() +
  geom_hline(yintercept = c(8, 11), lty = 3) +
  ylab(label = "Version and milestone") +
  xlab(label = "Modules") +
  theme(legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        legend.position = "right",
        legend.background = element_blank()) +
  scale_y_discrete(limits = factor(composedMilestone, 
                                   levels = composedMilestone)) +
  scale_x_discrete(limits = factor(rev(specComponents), 
                                   levels = rev(specComponents))) +
  scale_fill_manual("Module's families", values = c("vegetation dynamics - LandR" = "#3A8426", 
                                                    "fire - fireSense" = "#962A2A",
                                                    "fire - SCFM" = "#CC3737", 
                                                    "caribou" = "#E2C026", 
                                                    "birds" = "#637DB5", 
                                                    "biodiversity metrics" = "#3D3D3D", 
                                                    "trends" = "#030077",
                                                    "harvesting" = "#88C95C", 
                                                    "hotspots" = "#5D5BA0", 
                                                    "insects" = "#9B831A", 
                                                    "carbon" = "#939393", 
                                                    "anthropogenic disturbances" = "#07051E"))

p

ggsave(filename = file.path(wd, "modulesStatus.png"),
       plot = p,
       device = "png",
       scale = 1,
       width = NA,
       height = NA,
       units = "cm",
       dpi =  300)

# ## this version shades in the regions of the graph corresponding to "version"
# rects <- data.frame(ystart = c(0, 6, 12), yend = c(6, 12, 18),
#                     col = c("alpha", "beta", "release")) ## TODO: better names
# q <- ggplot() + 
#   geom_rect(data = rects,
#             mapping = aes(xmin = -Inf, xmax = Inf, ymin = ystart, ymax = yend, fill = col),
#             alpha = 0.3) +
#   geom_col(data = DT,
#            mapping = aes(x = specificComponent, y = composedMilestone, fill = groupOrFamily),
#            position = position_dodge2(reverse = TRUE)) +
#   coord_flip() +
#   scale_fill_discrete(drop = FALSE) +
#   scale_y_discrete(drop = FALSE) ## TODO: split legend in two: shaded version region and module group
# q
