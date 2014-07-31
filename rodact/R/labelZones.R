#'  labelZones
#'  
#'  Labels every time sample in subject's file to indicate whether the subject is in the light or
#'  dark zone, or in the doorway between the two.
#'  
#'  @param subject Subject ID corresponding to file with activity time series.
#'  @param session Session number corresponding to folder where file is located.
#'  @param type Options are "LD" (default) for light-dark, "L" for all light, or "D" for all dark.
#'  @param file_path Optional specification of file path where session folders are located.
#'  Defaults to current working directory.
#'  @param file_ext Optional specification of file extension if something other than "txt".
#'  @param inferZones Should the function infer the location of the dark box? Defaults to TRUE.
#'                    If set to FALSE, must specify darkZone parameter.
#'  @param darkZone If the position of the dark box is known, specify any Y coordinate that falls
#'         within its space. Not used if inferZones is set to TRUE.
#'  @return overwrites exisiting files, adding a zone label to each line. See details.
#'  @author Jason Shumake
#'  @details
#'  WARNING 1: this function will overwrite the original files that are being passed to it. If you
#'  want to maintain an ulnaltered copy of the original files, back them up elsewhere before using
#'  this function.
#'  
#'  WARNING 2: make sure you have first run "fixData" or have otherwise interpolated missing X,Y
#'  coordinates. Otherwise you will get false labeling of zones for time points where rat is
#'  "off grid", i.e., not breaking any infrared beams, which can happen when the rat rears upright
#'  into a corner. 
#'  
#'  Labels every time sample in subject's file to indicate whether the subject is in the light or
#'  dark zone, or in the doorway between the two. Recommended that for future experiments you 
#'  make note of the Y coordinates where you place the dark box. If you are unsure of the coordinates
#'  of the dark box, the default behavior is that the location of the dark box will be inferred
#'  from the maximum likelihood of the rat's location over the test session. This is almost
#'  invariably an accurate inference except in cases where the rat has received an experimental
#'  manipulation that causes a drastic reduction in fear/anxiety. At most, rats may equalize their
#'  exploration of both light and dark chambers. A marked preference for the light zone is exceedingly
#'  rare.
#'  
#'  If "L" is passed to the <type> parameter, all time samples are labeled as "light".
#'  If "D" is passed to the <type> parameter, all time samples are labeled as "dark".
#'  
#'  @export

labelZones = function(subject, session, type = "LD", file_path = getwd(), file_ext = "txt",
                      inferZones = TRUE, darkZone = 9)
{
  if (!type %in% c("LD","L","D")) stop("Invalid argument for type parameter.")
  file = paste(file_path, "/Session", session, "/", subject, ".", file_ext, sep="")
  data = read.table(file,header=T)
  if (typeof(data$Time)=="integer") stop("Data is unprocessed. First run 'fixData'.")
  data$Zone = "light"
  #if you are not sure which side of box you placed the dark zone, it can be inferred from
  #the animal's mean Y position. virtually all rats spend more time in the dark zone than in
  #the light zone.
  if (inferZones) darkZone = mean(data$Y)
  
  if (type == "LD")
  {
    if (darkZone<9)
    {
      for (i in 1:nrow(data))
      {
        if (data$X[i] >= 6.5 & data$X[i] <= 10.5 & data$Y[i] >= 6 & data$Y[i] < 9)
          data$Zone[i]="doorway"
        else if (data$Y[i] < 9)
          data$Zone[i]="dark"
      }
    }
    if (darkZone>9)
    {
      for (i in 1:nrow(data))
      {
        if (data$X[i] >= 6.5 & data$X[i] <= 10.5 & data$Y[i] > 6 & data$Y[i] <= 9)
          data$Zone[i]="doorway"
        else if (data$Y[i] > 9)
          data$Zone[i]="dark"
      }
    }
  }
  if (type == "D") data$Zone = "dark"
  write.table(data,file=file,sep="\t",row.names=FALSE)
}
