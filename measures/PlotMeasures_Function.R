
#---------------------------------------------------------------------------------
### SCRIPT FOR PLOTTING EXAMPLE TIME SERIES BASED ON LOWEST, MIDDLE, AND ###
### HIGHEST MEASURES ###
## ALINE KORVER, 28 JULY 2025

library(reshape2)

# This function can be used to plot 9 example time series with the 3 low, 
# 3 mid, and 3 high scores for a certain measure.

# insert a time series matrix, a vector with measures, and the name of the 
# measure to be displayed in the plot
plot_meas =  function (ts_df,meas, meas_name) {
  
  # make sure ts_df is a data frame
  ts_df = as.data.frame(ts_df)
  
  # get the 3 indices for the Q1, middle, and Q3 scores
  ordered_meas = order(meas)
  n = length(meas)
  Q1 = round(n*.25)
  Q2 = round(n*.50)
  Q3 = round(n*.75)
  low_index = ordered_meas[Q1:(Q1+2)]
  mid_index = ordered_meas[Q2:(Q2+2)]
  high_index = ordered_meas[Q3:(Q3+2)]
  indices = c(low_index, mid_index, high_index)
  
  # get the values 
  sorted_meas = sort(meas)
  low = sorted_meas[Q1:(Q1+2)]
  mid= sorted_meas[Q2:(Q2+2)]
  high = sorted_meas[Q3:(Q3+2)]

  
  # generate titles object for plot
  titles = as.character(round(c(low, mid, high), digits = 2))
  titles_plot = setNames(titles, 1:9)
  
  # get plot data for the 12 selected time series and melt, add ID
  plot_dat = ts_df[indices,]
  plot_dat$ID = as.factor(1:9)
  plot_melt = reshape2::melt(plot_dat, id.vars = "ID")
  
  # return plot with the 12 time series
  plot = ggplot(data=plot_melt) + geom_line(aes(x=variable, y=value, group = ID, col=ID)) + ggtitle(meas_name) + facet_wrap(~ID, nrow=3, labeller = labeller(ID = titles_plot)) + xlab("Trials") + ylab("Accuracy") + theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank())
  return(plot)
}
