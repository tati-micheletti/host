#Hotspot plot 
library(ggplot2)
library(viridis)
P <- ggplot(data.frame(x = rnorm(5000), y = rnorm(5000)), aes(x = x, y = y)) +
  geom_hex() + coord_fixed() +
  scale_fill_viridis() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())
png(filename = file.path(getwd(),"hotspot.png"), width = 3000, height = 3000)
print(P)
dev.off()
