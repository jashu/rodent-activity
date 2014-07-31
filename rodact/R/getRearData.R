#'  getRearData
#'  
#'  Computes rearing counts, duration, and summary stats for a single subject.
#'  
#'  @param subject subject ID corresponding to file with activity time series.
#'  @param session session number corresponding to folder where file is located.
#'  @param file_path optional specification of file path where session folders are located.
#'  Defaults to current working directory.
#'  @param file_ext optional specification of file extension if something other than "txt".
#'  @return data frame containing subject, session, zone, rearing count, start time,
#'  duration, minimum rearing duration, maximum rearing duration, mean rearing duration, and
#'  rearing variability score)
#'  @author Jason Shumake
#'  @details
#'  WARNING: this function should only be run on files that are analysis ready, i.e.,
#'  you've already run "fixData" and "labelZones" on all files. 
#'  @export


getRearData <- function(subject, session, file_path = getwd(), file_ext = "txt")
{
  file = paste(file_path, "/Session", session, "/", subject, ".", file_ext ,sep="")
  data = read.table(file,header=T)
  if (typeof(data$Time) == "integer") stop ("Data is unprocessed. First run 'fixData'.")
  if (typeof(data$Zone) == "NULL") stop ("Zones are not labeled. First run 'labelZones'.")
  data$Zone=as.character(data$Zone)
  #Count total number of rears
  rearCount = 0
  if (data$Z[1] > 0)
  {
    rearCount = 1
  }
  for (i in 2:nrow(data))
  {
    if (data$Z[i]>0 & data$Z[i-1]==0)
        {
        rearCount = rearCount + 1
        }
  }
  #Compute start time and duration of each rear
  Zone = vector("character",rearCount)
  Start = vector("numeric",rearCount)
  Duration = vector("numeric", rearCount)
  Count=vector("numeric",rearCount)
  rearCount = 0
  if (data$Z[1] > 0)
  {
    rearCount = 1
    Count[1] = 1
    Start[1]= .05
    Duration[1]=.05
    Zone[1] = data$Zone[1]
  }
  for (i in 2:nrow(data))
    {
    if (data$Z[i]>0)
      {
      if (data$Z[i-1]==0)
        {
        rearCount = rearCount + 1
        Count[rearCount] = rearCount
        Start[rearCount]=data$Time[i]
        Zone[rearCount] = data$Zone[i]
        }
      Duration[rearCount]=Duration[rearCount]+.05
      }
      
}
  #Write data frame with summary stats
  rearData = data.frame(Subject=subject,Session=session,Zone=Zone,RearCount=Count,RearStart=Start,
                        RearDuration=Duration)
  #If animal was rearing as the session ended, do not count last time point
  if (data$Z[nrow(data)]>0)
    rearData=rearData[1:(nrow(rearData)-1),]  
  #Remove erroneous data
  rearData = subset(rearData,RearDuration<9)
  #Create summary stats by zone
  if ("dark" %in% rearData$Zone)
  {
    rearData$minRearDur[rearData$Zone=="dark"] = min(rearData$RearDuration[rearData$Zone=="dark"])
    rearData$maxRearDur[rearData$Zone=="dark"] = max(rearData$RearDuration[rearData$Zone=="dark"])
    rearData$meanRearDur[rearData$Zone=="dark"] = mean(rearData$RearDuration[rearData$Zone=="dark"])
    rearData$varRearDur[rearData$Zone=="dark"] = var(rearData$RearDuration[rearData$Zone=="dark"])
  }
  if ("light" %in% rearData$Zone)
  {
    rearData$minRearDur[rearData$Zone=="light"] = min(rearData$RearDuration[rearData$Zone=="light"])
    rearData$maxRearDur[rearData$Zone=="light"] = max(rearData$RearDuration[rearData$Zone=="light"])
    rearData$meanRearDur[rearData$Zone=="light"] = mean(rearData$RearDuration[rearData$Zone=="light"])
    rearData$varRearDur[rearData$Zone=="light"] = var(rearData$RearDuration[rearData$Zone=="light"])
  }

return (rearData)
}

