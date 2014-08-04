#'  actMap
#'  
#'  Activity map: 3D plot of exploratory path taken by rodent in the activity chamber.
#'  
#'  @param subject Subject ID corresponding to file with activity time series.
#'  @param session Session number corresponding to folder where file is located.
#'  @param file_path Optional specification of file path where session folders are located.
#'  Defaults to current working directory.
#'  @param time Optional specification of a time vector specifying range in minutes to plot. Default
#'  is the entire 10 minutes: c(0,10)
#'  @return plot3D object
#'  @author Jason Shumake
#'  @details
#'  WARNING: make sure you have first run "fixData" or have otherwise interpolated missing X,Y
#'  coordinates. Otherwise you will get false labeling of zones for time points where rat is
#'  "off grid", i.e., not breaking any infrared beams, which can happen when the rat rears upright
#'  into a corner. 
#'  @importFrom plot3D lines3D
#'  @export

actMap = function(subject, session, time = c(0,10), file_path = getwd(), file_ext = "txt")
{
  time = time*60
  file = paste(file_path, "/Session", session, "/", subject, ".", file_ext, sep="")
  data = read.table(file,header=T)
  data$colvar = 0
  data$colvar[data$Zone=='dark'] = -1
  data$colvar[data$Zone=='light'] = 1
  if (typeof(data$Time)=="integer") stop("Data is unprocessed. First run 'fixData'.")
  data = subset(data, Time >= time[1] & Time <= time[2])
  return (with (data, lines3D(X, Y, Z, xlim = c(1,16), ylim = c(1,16), zlim = c(0,1.5*max(Z)),
                              colvar = colvar, clim = c(-1,1), colkey = F)))
}
