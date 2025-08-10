
install.packages("https://cran.r-project.org/src/contrib/Archive/ggh4x/ggh4x_0.2.8.tar.gz", 
                 repos = NULL, type = "source")
# https://cran.r-project.org/src/contrib/Archive/ggh4x/ggh4x_0.2.8.tar.gz
# https://cran.r-project.org/src/contrib/Archive/ggh4x/ggh4x_0.3.0.tar.gz


#remotes::install_github("teunbrand/ggh4x")
# install.packages("marker")
# devtools::install_github("teunbrand/legendry")

# devtools::install_github("shalom-lab/pcolor")
# remotes::install_github("JohnCoene/marker")

library(ggplot2)
library(ggpubr)
library(cowplot)
library(dplyr)
library(grid)
library(legendry) #guide_axis_nested
library(psych)
library(ggfun)
library(ggh4x)
library(pcolor)

# .libPaths()


# for forest  -------------------------------------------------------------

Outcome_order <- c(
    "Overall",
    "20-39","40-59","≥60",
    "Male","Female",
    "Mexican American","Non-Hispanic White","Non-Hispanic Black","Other races",
    "Below high school","High school","College or above",
    "No insurance", "Public insurance", "Private insurance",
    "≤1.3","1.3-3.5","≥3.5",
    "Unemployment", "Employment",
    "<7","≥7",
    "No","Yes" 
  )
Xlevel <- unique(clear_Res$Variable)
  # calculated the variables
forest <- clear_Res %>% mutate(
    Variable = factor(Variable,levels = rev(Xlevel)),
    Subgroup = factor(Subgroup,levels = rev(Outcome_order)), 
    Levels = factor(Levels, levels = c("Q4", "Q3", "Q2", "Q1")),
    OR = as.numeric(OR),
    Lower = as.numeric(Lower),
    Upper = as.numeric(Upper),
    # P_for_interaction = as.numeric(P_for_interaction),
    # P_for_interaction = ifelse(is.na(P_for_interaction), " ", sprintf('%.03f', P_for_interaction))
  )

  library(pcolor)
  #pcolor(8)
  dotColor <- c('#7FBFCF','#108064','#F2B5BE','#C45221')
  barColor <- c('#7FBFCF','#108064','#F2B5BE','#C45221')
  
  color1 <- "#DAEBBF" ## #EBF5D0
  color2 <- "#E9F0F2"
  # 绘制森林图 
  forest_plot <-
    ggplot(forest, aes(x = interaction(Subgroup, Variable, sep = "!") , y = OR)) + ##interaction(Subgroup, Variable)
    ggpubr::theme_pubr() +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(aes(ymin = Lower, ymax = Upper, color = Levels), size = 1, position = position_dodge(width = 0.6), alpha = 1) +
    geom_point(aes(fill = Levels, shape = Levels), size = 3, color = 'white', stroke = 0.5, position = position_dodge(width = 0.6)) +
    scale_shape_manual(values = c(21, 21, 21, 21), breaks = c('Q1','Q2', 'Q3', 'Q4')) +
    scale_color_manual(values = barColor, breaks = c('Q1','Q2', 'Q3', 'Q4')) +
    scale_fill_manual(values = dotColor, breaks = c('Q1','Q2', 'Q3', 'Q4')) +
    
    coord_flip() +
    scale_x_discrete(name = " ", guide = ggh4x::guide_axis_nested(delim = "!" )) + #
    
    
    scale_y_continuous(name = "OR(95%CI)") +
    guides(shape = guide_legend(title = "Quantile", nrow = 1, keywidth = unit(1, "cm"), keyheight = unit(0.5, "cm")),
           color = guide_legend(title = "Quantile", nrow = 1, keywidth = unit(1, "cm"), keyheight = unit(0.5, "cm")),
           fill = guide_legend(title = "Quantile", nrow = 1, keywidth = unit(1, "cm"), keyheight = unit(0.5, "cm"))) +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.spacing = unit(0.1, "cm"),
      legend.key.size = unit(0.5, "cm"),
      legend.text = element_text(size = 14, family="serif"),# for legend text 
      legend.title = element_text(size = 14, family="serif"),  # for legend tttle
      legend.key.width = unit(2, "cm"),    # 设置图例项的宽度
      legend.spacing.x = unit(0.1, "cm"),
      legend.spacing.y = unit(0.2, "cm"),
      axis.title.x = element_text(size = 14, face = "bold", family="serif"),  # x title , family = "serif"
      axis.title.y = element_text(size = 14, face = "bold", family="serif"),  # y title
      axis.text.x = element_text(size = 14, family="serif"),  # x label
      axis.text.y = element_text(size = 14, family="serif"),  # y label
      axis.ticks.y = element_line(size=NA),
      axis.line.y = element_line(colour = "black"),
      axis.ticks = element_line(colour = "black") ##修改小竖线的
      
    ) + 
    
    annotate("rect", xmin = 0, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill=color1) +
    annotate("rect", xmin = 2.5, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill=color2) +
    annotate("rect", xmin = 4.5, xmax = 6.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill=color1) +
    annotate("rect", xmin = 6.5, xmax = 8.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill=color2) +
    annotate("rect", xmin = 8.5, xmax = 10.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill=color1) +
    annotate("rect", xmin = 10.5, xmax = 13.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill=color2) +
    annotate("rect", xmin = 13.5, xmax = 16.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill=color1) +
    annotate("rect", xmin = 16.5, xmax = 19.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill=color2) +
    annotate("rect", xmin = 19.5, xmax = 23.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill=color1) +
    annotate("rect", xmin = 23.5, xmax = 26.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill=color2) +
    annotate("rect", xmin = 26.5, xmax = 28.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill=color1) +
    annotate("rect", xmin = 28.5, xmax = 29.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill=color2)
  
  print(forest_plot)
  
  
  y <- ggplot()+theme_void()
  combined_plot <- plot_grid(forest_plot, y,
                              nrow = 1, 
                              rel_widths = c(3, 0.52))
  # 添加文本并调整字体
  final_plot <- ggdraw(combined_plot) +
    draw_grob(textGrob("Variables", x = 0.05, y = 0.98, hjust = 0, gp = gpar(fontsize = 14, fontfamily = "serif"))) +
    draw_grob(textGrob("Subgroup", x = 0.22, y = 0.98, hjust = 0, gp = gpar(fontsize = 14, fontfamily = "serif"))) +
    draw_grob(textGrob("P for interaction", x = 0.85, y = 0.98, hjust = 0, gp = gpar(fontsize = 14, fontfamily = "serif"))) +
    
    draw_grob(textGrob("0.692", x = 0.88, y = 0.90, hjust = 0, gp = gpar(fontsize = 14, fontfamily = "serif"))) +
    draw_grob(textGrob("0.918", x = 0.88, y = 0.826, hjust = 0, gp = gpar(fontsize = 14, fontfamily = "serif"))) +
    draw_grob(textGrob("0.526", x = 0.88, y = 0.716, hjust = 0, gp = gpar(fontsize = 14, fontfamily = "serif"))) +
    draw_grob(textGrob("0.843", x = 0.88, y = 0.605, hjust = 0, gp = gpar(fontsize = 14, fontfamily = "serif"))) +
    draw_grob(textGrob("0.742", x = 0.88, y = 0.51, hjust = 0, gp = gpar(fontsize = 14, fontfamily = "serif"))) +
    draw_grob(textGrob("0.869", x = 0.88, y = 0.415, hjust = 0, gp = gpar(fontsize = 14, fontfamily = "serif"))) +
    draw_grob(textGrob("0.132", x = 0.88, y = 0.335, hjust = 0, gp = gpar(fontsize = 14, fontfamily = "serif"))) +
    draw_grob(textGrob("0.246", x = 0.88, y = 0.27, hjust = 0, gp = gpar(fontsize = 14, fontfamily = "serif"))) +
    
    draw_grob(textGrob("0.716", x = 0.88, y = 0.205, hjust = 0, gp = gpar(fontsize = 14, fontfamily = "serif"))) +
    draw_grob(textGrob("0.757", x = 0.88, y = 0.15, hjust = 0, gp = gpar(fontsize = 14, fontfamily = "serif"))) +
    draw_grob(textGrob("0.323", x = 0.88, y = 0.08, hjust = 0, gp = gpar(fontsize = 14, fontfamily = "serif"))) +
    draw_plot(
      ggplot() +
        geom_rect(aes(xmin = 0, xmax = 1.5, ymin = 0.97, ymax = 0.972), fill = "black", alpha = 0.9) +
        theme_void(),
      x = -0.1,
      y = 0.966,
      width = 1.3,
      height = 0.001
    )
  
  print(final_plot) 
  
  cairo_pdf("Figures/SubgroupVIMForest.pdf", 
            family = "serif", width = 10, height = 14)
  print(final_plot)
  dev.off()
  
  
  tiff("Figures/SubgroupVIMForestXXXX.tiff", width = 10, height = 14, units = "in", res = 300)
  print(final_plot)
  dev.off()
  








