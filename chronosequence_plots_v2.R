library(ggplot2)
library(xlsx)
library(gridExtra)
library(cowplot)
library(survminer)


setwd("C:/Users/Sarah/Google Drive/Hog Island/Hog Island/2017 Data/chronosequence analysis")

chrono_data <- read.xlsx("means3 1990s and 2017 chronosequence data.xlsx", 1)


#cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## error bars still smaller than points on some? 
template_top <- function(ylabel = NULL, yvariable = NULL, SEvariable =  NULL, xlabel = NULL){
  ggplot(chrono_data)  + 
    geom_point(x = chrono_data$Age, y = yvariable, size = 3.5, aes(color = factor(chrono_data$Dataset), 
                                                                   shape = factor(as.character(chrono_data$Initiation.Year)), stroke = 1.5)) +
    scale_shape_manual(values = c(15:19,4, 7, 8)) + scale_fill_manual(values = c("gray", "black"))+ 
    scale_color_manual(values = c("gray", "black"), name = "Sampling Year", labels = c("2017", "1995")) +
    labs(x =  xlabel, y = ylabel, shape = "Initiation Year")  + 
    geom_errorbar(data = chrono_data, mapping = aes(x = chrono_data$Age, ymin = yvariable- SEvariable, 
                                                    ymax = yvariable + SEvariable), width = 0.1)+ facet_grid(chrono_data$Elevation) + 
    coord_trans(x = "log10") + theme_classic() +
    theme( panel.spacing = unit(-0.5, "lines"),plot.margin=unit(c(-0.1,0,0,0),"cm"), text = element_text(size = 20), axis.text.x=element_blank(), axis.text.y = element_text(size = 15), 
           legend.position = "bottom", panel.border = element_rect(linetype = "solid", fill = NA), strip.background = element_rect(color = NA) ) }

template_middle <- function(ylabel = NULL, yvariable = NULL, SEvariable =  NULL){
  ggplot(chrono_data)  + 
    geom_point(x = chrono_data$Age, y = yvariable, size = 3.5, aes(color = factor(chrono_data$Dataset), 
                                                                   shape = factor(as.character(chrono_data$Initiation.Year)), stroke = 1.5)) +
    scale_shape_manual(values = c(15:19,4, 7, 8)) + scale_fill_manual(values = c("gray", "black"))+ 
    scale_color_manual(values = c("gray", "black"), name = "Sampling Year", labels = c("2017", "1995")) +
    labs(x = NULL, y = ylabel, shape = "Initiation Year")  + 
    geom_errorbar(data = chrono_data, mapping = aes(x = chrono_data$Age, ymin = yvariable- SEvariable, 
                                                    ymax = yvariable + SEvariable), width = 0.1)+ facet_grid(chrono_data$Elevation) + 
    coord_trans(x = "log10") + theme_classic() +
    theme( panel.spacing = unit(0, "lines"), plot.margin=unit(c(-0.1,0,-0.15,0),"cm"), text = element_text(size = 20), axis.text.x = element_blank(), axis.text.y = element_text(size = 15), legend.position = "none",
           strip.text.x = element_blank(), panel.border = element_rect(linetype = "solid", fill = NA), strip.background = element_rect(color = NA) ) }

template_bottom <- function(ylabel = NULL, yvariable = NULL, SEvariable =  NULL){
  ggplot(chrono_data)  + 
    geom_point(x = chrono_data$Age, y = yvariable, size = 3.5, aes(color = factor(chrono_data$Dataset), 
                                                                   shape = factor(as.character(chrono_data$Initiation.Year)), stroke = 1.5)) +
    scale_shape_manual(values = c(15:19,4, 7, 8)) + scale_fill_manual(values = c("gray", "black"))+ 
    scale_color_manual(values = c("gray", "black"), name = "Sampling Year", labels = c("2017", "1995")) +
    labs(x ="Marsh Age", y = ylabel, shape = "Initiation Year")  + 
    geom_errorbar(data = chrono_data, mapping = aes(x = chrono_data$Age, ymin = yvariable- SEvariable, 
                                                    ymax = yvariable + SEvariable), width = 0.1)+ facet_grid(chrono_data$Elevation) + 
    coord_trans(x = "log10") + theme_classic() +
    theme( panel.spacing = unit(-0.5, "lines"), plot.margin=unit(c(0,0,-0.1,0),"cm"), text = element_text(size = 20), axis.text = element_text(size = 15), legend.position = "none",
           strip.text.x = element_blank(), panel.border = element_rect(linetype = "solid", fill = NA), strip.background = element_rect(color = NA) ) }

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


Eh_plot <- template_top(ylabel = "Eh", yvariable = chrono_data$Mean.Eh_adj., SEvariable = chrono_data$Std.Err.Eh_adj.)
mylegend <- get_legend(Eh_plot)




#################################### Sediment Properties ##############################################################################
OM_plot <- template_top(ylabel = "% OM", yvariable = chrono_data$Mean..OM.0.10., SEvariable = chrono_data$Std.Err..OM.0.10.) + theme(legend.position="none")
Sed_P <- template_middle(ylabel = "% P", yvariable = chrono_data$Mean.Sed.P.0.10....., SEvariable = chrono_data$Std.Err.Sed.P.0.10.....)
Sed_N <- template_bottom(ylabel = "% N", yvariable = chrono_data$Mean.sed_.N.surface., SEvariable = chrono_data$Std.Err.sed_.N.surface.)

#IT AIN'T PRETTY BUT IT WORKS (~~FINALLY~~)
x <- ggarrange(OM_plot, Sed_P, Sed_N, ncol = 1, nrow = 3, align = "v")
sediment_nutrients<- grid.arrange(x, mylegend, ncol = 1, heights = c(9,1))


#################################### Invertebrates ##############################################################################
Littoraria <- template_top(ylabel = "L. irrorota", yvariable = chrono_data$Mean.Littoraria., SEvariable = chrono_data$Std.Err.Littoraria.)+ theme(legend.position="none")
Ilyanassa <- template_middle(ylabel = "I. obsoleta", yvariable = chrono_data$Mean.Ilyanassa., SEvariable = chrono_data$Std.Err.Ilyanassa.)
Uca <- template_middle(ylabel = "Uca spp.", yvariable = chrono_data$Mean.Uca., SEvariable = chrono_data$Std.Err.Uca.)
Geuk <- template_bottom(ylabel = "Geukensia spp.", yvariable = chrono_data$Mean.Geukensia., SEvariable = chrono_data$Std.Err.Geukensia.)

y <- ggarrange(Littoraria, Ilyanassa, Uca, Geuk, ncol = 1, nrow = 4, align = "v")
invertebrates <- grid.arrange(y, mylegend, ncol = 1, heights = c(9,1))

#################################### other ##############################################################################

Elevation <- template(ylabel = "Elevation", yvariable = chrono_data$Mean.Elev_NAD88., SEvariable = chrono_data$Std.Err.Elev_NAD88.)
Elevation


Sand <- template(ylabel = "% Sand", yvariable = chrono_data$Mean..sand.0.10., SEvariable = chrono_data$Std.Err..sand.0.10.)
Sand


##### version with different fills for each data set
# template <- function(ylabel = NULL, yvariable = NULL, SEvariable =  NULL){
#   ggplot(chrono_data)  + 
#     geom_point(x = chrono_data$Age, y = yvariable, size = 3, aes(color = factor(chrono_data$Dataset), 
#                                                                  shape = factor(as.character(chrono_data$Initiation.Year)), fill = factor(chrono_data$Dataset)), stroke = 1) +
#     scale_shape_manual(values = c(21:25, 7, 8)) + scale_fill_manual(values = c("gray", "black"))+ 
#     scale_color_manual(values = c("gray", "black"), name = "Dataset", labels = c("New", "Old")) +
#     labs(x = "Marsh Age (years)", y = ylabel, shape = "Initiation Year")  + 
#     geom_errorbar(data = chrono_data, mapping = aes(x = chrono_data$Age, ymin = yvariable- SEvariable, 
#                                                     ymax = yvariable + SEvariable), width = 0.1)+ facet_grid(chrono_data$Elevation) + coord_trans(x = "log10") + theme_classic() +
#     theme(text = element_text(size = 25), axis.text = element_text(size = 15), panel.border = element_rect(linetype = "solid", fill = NA)) }