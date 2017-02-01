if (FALSE){

library(ggplot2)
library(GGally)

lm_or_resid <- function(data, mapping, ..., line_color = "red", line_size = 1) {
  if (as.character(mapping$y) != "Residual") {
    return(ggally_smooth_lm(data, mapping, ...))
  }
  
  # make residual data to display
  resid_data <- data.frame(
    x = data[[as.character(mapping$x)]],
    y = residuals[[as.character(mapping$x)]]
  )
  

  # calculate a consistent y range for all residuals
  y_range <- range(unlist(residuals))

    ggplot(data = data, mapping = mapping) +
    geom_hline(yintercept = 0, color = line_color, size = line_size) +
    ylim(y_range) +
    geom_point(data = resid_data, mapping = aes(x = x, y = y), ...)
}

swiss <- datasets::swiss

# add a 'fake' column
swiss$Residual <- seq_len(nrow(swiss))

# calculate all residuals prior to display
residuals <- lapply(swiss[2:6], function(x) {
  summary(lm(Fertility ~ x, data = swiss))$residuals
})
# calculate a consistent y range for all residuals
#y_range <- range(unlist(residuals))

# plot the data
ggduo(
  swiss,
  2:6, c(3,7),
  types = list(continuous = lm_or_resid)
)

library(diags)
library(stringr)

dirVPA="/home/laurie/Desktop/kobe/inputs/bfte/2014/vpa/reported/med"

r1=diags:::diags.vpa2box(file.path(dirVPA,"MINUS0.R"))

names(r1)

dat=data=subset(r1,name==r1$name[1])
mdl=lm(obs~hat,dat)
ggnostic(mdl)


mdl=lm(obs~hat*name+name,data=r1)
ggnostic(mdl)
}