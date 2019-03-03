library(ggplot2)
library(xlsx)
library(gridExtra)
library(cowplot)
library(survminer)


setwd("C:/Users/Sarah/Google Drive/Hog Island/Hog Island/2017 Data/chronosequence analysis")


#cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## error bars still smaller than points on some? 
template <- function(ylabel = NULL, yvariable = NULL, SEvariable =  NULL, xlabel = NULL){
  ggplot(avg_chrono_data)  + 
    geom_point(x = avg_chrono_data$Age, y = yvariable, size = 3.5, aes(color = factor(avg_chrono_data$Dataset), 
                                                                   shape = factor(as.character(avg_chrono_data$Dataset)), stroke = 1.5)) +
    scale_shape_manual(values = c(16,17), name = "Sampling Year", labels = c("2017", "1995") , guide = guide_legend(reverse = TRUE)) + 
    scale_fill_manual(values = c("gray", "black"))+ 
    scale_color_manual(values = c("gray", "black"), name = "Sampling Year", labels = c("2017", "1995"), guide = guide_legend(reverse = TRUE)) +
    labs(x =  xlabel, y = ylabel)  + 
    geom_errorbar(data = SE_chrono_data, mapping = aes(x = SE_chrono_data$Age, ymin = yvariable- SEvariable, 
           ymax = yvariable + SEvariable), width = 0.1)+ facet_grid(SE_chrono_data$Elevation) + 
    coord_trans(x = "log10") + theme_classic() +
    theme( panel.spacing = unit(-0.5, "lines"),plot.margin=unit(c(-0.1,0,0,0),"cm"), text = element_text(size = 20), 
           axis.text = element_text(size = 15), axis.text.x = element_text(size = 15, margin = margin(r = 500, l = 500, unit = "pt")), axis.text.y = element_text(size = 15), 
           legend.position = "bottom", panel.border = element_rect(linetype = "solid", fill = NA), 
           strip.text.x = element_text(size = 20), strip.background = element_rect(color = NA) ) }

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


### My not very elegant solution to placements ######
#top: + theme(legend.position="none",  axis.text.x=element_blank())
#middle: + theme(legend.position="none", plot.margin=unit(c(-0.1,0,-0.15,0),"cm"), strip.text.x = element_blank(), axis.text.x=element_blank())
#bottom:  + theme(legend.position="none", plot.margin=unit(c(0,0,-0.1,0),"cm"), strip.text.x = element_blank()) 

Eh_plot <- template(ylabel = "Eh", yvariable = avg_chrono_data$Eh_adj, SEvariable = SE_chrono_data$Eh_adj)
mylegend <- get_legend(Eh_plot)

Biomass <- template(ylabel = expression("g "~ m^{-2}), yvariable = avg_chrono_data$Sept.Biomass..g.m.2., SEvariable = SE_chrono_data$Sept.Biomass..g.m.2.)


#################################### Sediment Properties ##############################################################################
OM_plot <- template(ylabel = "% OM", yvariable = chrono_data$Mean..OM.0.10., SEvariable = chrono_data$Std.Err..OM.0.10.) + theme(legend.position="none")
Sed_P <- template(ylabel = "% P", yvariable = chrono_data$Mean.Sed.P.0.10....., SEvariable = chrono_data$Std.Err.Sed.P.0.10.....)
Sed_N <- template_bottom(ylabel = "% N", yvariable = chrono_data$Mean.sed_.N.surface., SEvariable = chrono_data$Std.Err.sed_.N.surface.)

#IT AIN'T PRETTY BUT IT WORKS (~~FINALLY~~)
x <- ggarrange(OM_plot, Sed_P, Sed_N, ncol = 1, nrow = 3, align = "v")
sediment_nutrients<- grid.arrange(x, mylegend, ncol = 1, heights = c(9,1))


#################################### Invertebrates ##############################################################################
Littoraria <- template(ylabel = italic("L. irrorota"), yvariable = avg_chrono_data$Littoraria, SEvariable = SE_chrono_data$Littoraria)+ theme(legend.position="none",  axis.text.x=element_blank())
Ilyanassa <- template(ylabel = italic("I. obsoleta"), yvariable = avg_chrono_data$Ilyanassa, SEvariable = SE_chrono_data$Ilyanassa) + theme(legend.position="none", plot.margin=unit(c(-0.1,0,-0.15,0),"cm"), strip.text.x = element_blank(), axis.text.x=element_blank())
Uca <- template(ylabel = italic("Uca spp."), yvariable = avg_chrono_data$Uca, SEvariable = SE_chrono_data$Uca) + theme(legend.position="none", plot.margin=unit(c(-0.1,0,-0.15,0),"cm"), strip.text.x = element_blank(),  axis.text.x=element_blank())
Geuk <- template(ylabel = italic("Geukensia spp."), yvariable = avg_chrono_data$Geukensia, SEvariable = SE_chrono_data$Geukensia) + theme(legend.position="none", plot.margin=unit(c(0,0,-0.1,0),"cm"), strip.text.x = element_blank()) 

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