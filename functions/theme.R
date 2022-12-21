# Got it from Tiago Ventura

# ggplot theme ------------------------------------------------------------

library(extrafont)

my_font <- "Times"
my_bkgd <- "white"
#my_bkgd <- "#f5f5f2"
pal <- RColorBrewer::brewer.pal(9, "Spectral")

my_theme <- theme(text = element_text(family = my_font, color = "#22211d"),
                  rect = element_rect(fill = my_bkgd),
                  plot.background = element_rect(fill = my_bkgd, color = NA),
                  panel.background = element_rect(fill = my_bkgd, color = NA),
                  panel.border = element_rect(color="black"),
                  strip.background = element_rect(color="black", fill="white"),
                  legend.background = element_rect(fill = my_bkgd, color = NA),
                  legend.key = element_rect(size = 6, fill = "white", colour = NA),
                  legend.key.size = unit(1, "cm"),
                  legend.text = element_text(size = 14, family = my_font),
                  legend.title = element_text(size = 14),
                  plot.title = element_text(size = 22, face = "bold"),
                  plot.subtitle = element_text(size = 16, family=my_font),
                  axis.title= element_text(size = 22),
                  axis.text = element_text(size = 14),
                  axis.title.x = element_text(hjust = 1),
                  strip.text = element_text(family = my_font, color = "#22211d",
                                            size = 16, face = "italic"),
                  plot.caption = element_text(size = 12, family = my_font),
                  plot.margin =  unit(c(1,1,1,1), "cm"))