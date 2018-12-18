ggplot_missing <- function(x){
  x %>% is.na %>% melt %>%
    ggplot(data = ., aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_manual(name = "", values = c("#D3D3D3", "#910000"), labels = c("Info", "NA")) +
    scale_y_reverse () +
    xlab("Column") +
    ylab("Row") +
    ggtitle("Missing Values Map") +
    theme_minimal() +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="bottom")
}
missingMap <- ggplot_missing(data)
missingMap