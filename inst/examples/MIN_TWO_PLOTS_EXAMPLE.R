library(curvaUE)
M <- example_trial("rice")
configurations <- plot_configurations(M,min_plots=2)
stopifnot(nrow(configurations)==35)
print(subset(configurations,n_plots==2,
             select=c(width,length,area,n_plots,cv)))
