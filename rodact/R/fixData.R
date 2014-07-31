#'  fixData
#'  
#'  Performs standard fixes for raw XYZ time-series data exported from Activity Monitor. 
#'  
#'  @param subject subject ID corresponding to file with activity time series.
#'  @param session session number corresponding to folder where file is located.
#'  @param file_path optional specification of file path where session folders are located.
#'  Defaults to current working directory.
#'  @param file_ext optional specification of file extension if something other than "txt".
#'  @return overwrites exisiting files with new information. See details.
#'  @author Jason Shumake
#'  @details
#'  This function performs several standard fixes to files exported by Activity Monitor
#'  that are in the tabular form of Time followed by spatial coordrinates X, Y, and Z:
#'  
#'  1)  Converts the time units (corresponding to a sampling period of 50 ms) into seconds.
#'  
#'  2)  Interpolates X,Y coordinates for sampling periods in which the rat goes off the grid
#'      of infrared beams, which can occur when the rat stands upright in one corner. Med Associates
#'      software scores these events as "jumps", but it is extremely rare that these off-grid
#'      moments are due to the rat actually jumping. Interpolating from the last known X,Y position
#'      is a very reasonable assumption for filling in missing X,Y coordinates.
#'      
#'  3)  Med Associates records as a Z coordinate the number assigned to the particular beam that is
#'      broken. However, these numbers do not reflect differences in vertical position. All
#'      of these beams are at the same height, and any number greater than zero reflects a boolean
#'      event: the rat has reared up on its hind legs. Therefore, all numbers greater than 0 are
#'      changed to 1.
#'      
#'  PREREQUISITES:
#'  This function works with the file names and directory structure created by using the
#'  LD_Translator, which is a Python package that takes .Export files containing multiple subjects
#'  and creates single files for each subject and assigns them to folders corresponding to each
#'  session run.
#'  
#'  1)  Run the LD_Translator according to its instructions.
#'  
#'  2)  Set your R working directory to the folder which contains the "Session" folders (Session1, 
#'      Session2. etc.), or pass the location of this directory to the function parameter "file_path".    
#'  
#'  @export


fixData = function(subject, session, file_path = getwd(), file_ext = "txt")
{
  file = paste(file_path, "/Session", session, "/", subject, ".", file_ext ,sep="")
  data = read.table(file,header=T)
  if (typeof(data$Time) == "double") stop("File already processed.")
  #Change time units to seconds:
  data$Time=as.numeric(data$Time)*.05
  #Correct time points where rat is off-grid:
  for (i in 2:nrow(data))
  {
    if (data$X[i]==0)
      data$X[i]=data$X[i-1]
    if (data$Y[i]==0)
      data$Y[i]=data$Y[i-1]
  }
  #Set Z coordinates to the same value if any upper-level beam is broken:
  data$Z[data$Z>0]=1
  #Rewrite files with correct data:
  write.table(data,file=file,sep="\t",row.names=FALSE)
}