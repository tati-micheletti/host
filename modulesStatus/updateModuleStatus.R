
# To update module status, please modify the table (https://docs.google.com/spreadsheets/d/1wlsgzXzZ_G1dbk4KD992OFnPnkW8xMmycpq-tYADnIE/edit?usp=sharing) and re-run the script

library("Require")
Require("reproducible")
Require("data.table")
Require("ggplot2")
Require("googlesheets4")
Require("googledrive")

# Deprecated. Using googlesheets instead
# DT <- prepInputs(url = "https://drive.google.com/file/d/1aNYmbXec5g_pOM1DZPqFyi60n4Vud39r/view?usp=sharing",
#                  targetFile = "moduleStatusTable_FY2.csv", 
#                  destinationPath = wd,
#                  fun = "data.table::fread")

DT <- data.table(read_sheet("https://docs.google.com/spreadsheets/d/1wlsgzXzZ_G1dbk4KD992OFnPnkW8xMmycpq-tYADnIE/edit?usp=sharing"))

wd <- checkPath(file.path(getwd(), "modulesStatus"), create = TRUE)

DT[, versionDescription := NULL] # Simplifying

names(DT)[names(DT) == "colors"] <- "plotColors"
maxVersion <- 3
devel <- c("D","T","R")
composedMilestone <- paste0(rep(0:maxVersion, each = length(devel)), devel)
DT[, composedMilestone  := factor(paste0(version, milestone), levels = composedMilestone)]
DT[, subjects  := factor(subjects, unique(DT$subjects))]
DT[, nature  := factor(nature, c("module", "data"))]
subs <- as.character(unique(DT[["subjects"]]))
 
## this version shades in the regions of the graph corresponding to "version" and has the correspondent colors to the hex stickers
p <- ggplot() + 
  geom_col(data = DT[Year == 2019],
           mapping = aes(x = subjects, 
                         y = composedMilestone, 
                         fill = subjects),
           position = position_dodge2(reverse = TRUE),
           alpha = 1) +
  geom_col(data = DT[Year == 2021],
           mapping = aes(x = subjects, 
                         y = composedMilestone, 
                         fill = subjects),
           position = position_dodge2(reverse = TRUE),
           alpha = 0.6) +
  geom_col(data = DT[Year == 0],
           mapping = aes(x = subjects, 
                         y = composedMilestone, 
                         fill = subjects),
           position = position_dodge2(reverse = TRUE),
           alpha = 0.4) +
  coord_flip() +
  geom_hline(yintercept = c(3, 6, 9), lty = 3) +
  ylab(label = "Version and milestone") +
  theme(legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        legend.position = "none",
        axis.title.y = element_blank(),
        legend.background = element_blank(), 
        text = element_text(size = 18)) +
  scale_y_discrete(limits = factor(composedMilestone, 
                                   levels = composedMilestone)) +
  scale_x_discrete(limits = factor(rev(subs), 
                                   levels = rev(subs))) +
  facet_grid(. ~ nature) +
  scale_fill_manual("Module's families", values = c("Forest Dynamics (LandR)" = "#3A8426",
                                                    "Wildfire (fireSense)" = "#962A2A",
                                                    "Wildfire (SCFM)" = "#CC3737",
                                                    "Caribou Population Growth" = "#FFB523",
                                                    "Caribou Resource Selection" = "#FFE228",
                                                    "Caribou Indigenous Knowledge" = "#FF783A",
                                                    "Landbirds" = "#60387F",
                                                    "Waterfowl" = "#637DB5",
                                                    "Harvesting" = "#88C95C",
                                                    "Priority Places" = "#5D5BA0",
                                                    "Mountain Pine Beetle" = "#7A6427",
                                                    "Spruce Budworm" = "#603800",
                                                    "Carbon" = "#939393",
                                                    "Anthropogenic Disturbances" = "#07051E"))

p

ggsave(filename = file.path(wd, "modulesStatus.png"),
       plot = p,
       device = "png",
       scale = 1,
       width = 30,
       height = 15,
       units = "cm",
       dpi =  300)

drive_upload(file.path(wd, "modulesStatus.png"), as_id("1wmHMgQrwtsUejOMJ2IAegRW3S71SHMkd"))

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

# specComponents <- c("LBMR","Biomass_regeneration","Boreal_LBMRDataPrep","gmcsDataPrep","PSP_Clean","Biomass_regenerationPM","scfmLandcoverInit","scfmRegime","scfmDriver","scfmIgnition","scfmEscape","scfmSpread","fireSense_NWT_DataPrep","LBMR2LCC_DataPrep","fireSense_NWT","fireSense_IgnitionPredict","fireSense_SpreadFit","fireSense_EscapePredict","fireSense_EscapeFit","fireSense_SpreadPredict","fireSense_dataPrepFit","fireSense_dataPrepPredict","fireSense_FrequencyFit","caribouPopulationGrowth","caribouRSF","caribouRSF_NT","caribouIEK","birdsNWT","waterfowl","PriorityPlaces","MPB","SBW","development","spadesCBM","wc3Harvest","birdDensityTrends","comm_metricsNWT")
# # DT[, specificComponent  := factor(specificComponent, levels = specComponents)]

