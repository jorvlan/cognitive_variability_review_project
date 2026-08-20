
#---------------------------------------------------------------------------------
### SCRIPT FOR PLOTTING EXAMPLE TIME SERIES BASED ON LOWEST, MIDDLE, AND ###
### HIGHEST MEASURES ###
## ALINE KORVER, 28 JULY 2025

library(reshape2)

# This function can be used to plot 9 example time series with the 3 lowest, 
# 3 mid, and 3 highest scores for a certain measure.

# insert a time series matrix, a vector with measures, and the name of the 
# measure to be displayed in the plot
plot_meas =  function (ts,meas, meas_name) {
  
  # get the 4 lowest, ~ middle, and highest scores on the measure
  low = sort(meas)[1:3]
  mid = sort(meas)[((length(meas)/2)-1):((length(meas)/2)+2)][1:3]
  high = sort(meas, decreasing = T)[1:3]
  
  # get indices of these measures
  ordered_indices = order(meas)
  low_index = ordered_indices[1:3]
  mid_start = floor(length(meas) / 2) - 1
  mid_index = ordered_indices[mid_start:(mid_start + 2)]
  high_index = order(meas, decreasing = TRUE)[1:3]
  indices = c(low_index, mid_index, high_index)
  
  # generate titles object for plot
  titles = as.character(round(c(low, mid, high), digits = 2))
  titles_plot = setNames(titles, 1:9)
  
  # get plot data for the 12 selected time series and melt, add ID
  plot_dat = ts[indices,]
  plot_dat$ID = as.factor(1:9)
  plot_melt = reshape2::melt(plot_dat, id.vars = "ID")
  
  # return plot with the 12 time series
  plot = ggplot(data=plot_melt) + geom_line(aes(x=variable, y=value, group = ID, col=ID)) + ggtitle(meas_name) + facet_wrap(~ID, nrow=3, labeller = labeller(ID = titles_plot)) + xlab("Trials") + ylab("Accuracy") + theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank())
  return(plot)
}

