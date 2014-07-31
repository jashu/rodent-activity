import os

SUBJECTLINE ="Subject\tSession\tStartDate\tStartTime\tChamber\n"

def getFiles():
	
	files = os.listdir("Files/")
	#print files
	useFiles = [f for f in files if ".export" in f.lower() or ".txt" in f.lower()]
	
	return useFiles
	#print useFiles

def getData(f):
	f = "Files/"+f
	infile = open(f).read().strip().split('\n')

	#print infile

	sessionList = []
	sessionData = []
	for line in infile:
		if "Experiment Title:" in line: 
			sessionList.append(sessionData)
			sessionData = []
		else:
			sessionData.append(line)
	# add last line
	sessionList.append(sessionData)

	metaDataDict = {}
	dataDict = {}
	for session in sessionList:
		subject = ''
		cn = ''
		for lineIdx,line in enumerate(session):

			if "Chamber Number:" in line:
				col = "Chamber Number:".find(":")
				cn = line[col+1:].strip()
		
			elif "Subject ID:" in line:
				col = "Subject ID:".find(":")
				subject = line[col+1:].strip()
				if subject in metaDataDict:
					metaDataDict[subject][0] += 1
				else:
					metaDataDict[subject] = [1]
		
			elif "Start Date:" in line:
				col = "Start Date:".find(":")
				sd = line[col+1:].strip()
				metaDataDict[subject].append(sd)

			elif "Start Time:" in line:
				col = "Start Time:".find(":")
				st = line[col+1:].strip()
				metaDataDict[subject].append(st)
				metaDataDict[subject].append(cn)

			elif "Time" in line and "X" in line and "Y" in line and "Z" in line:
				data = session[lineIdx:]
				if subject in dataDict:
					dataDict[subject].append(data)	
				else:
					dataDict[subject] = [data]	
	#print dataDict
	return (metaDataDict,dataDict)
 
def chronologSort(metaData,data):
	
	subjects =  metaData.keys()
	
	for subject in subjects:
		first = metaData[subject][0]
		outList = [first]
		outData = []
		subjectInfo = metaData[subject][1:]
		#make 2d list split by third element
		splitted = []
		for idx,elem in enumerate(subjectInfo):
			if len(elem)==1:
				splitted.append(subjectInfo[idx-2:idx+1])
		#print splitted
		#print subject +":"+ str(subjectInfo)
		# if len is 3 means only one session, do nothing, else
		if len(subjectInfo) > 3 :
			dates = [subjectInfo[i] for i in range(0,len(subjectInfo),3)]
			#otherwise check if dates are all the same
			setDates = set(dates)
			if len(setDates) == 1 :
				times = [subjectInfo[i] for i in range(1,len(subjectInfo),3)]
				#print times
				sortedTime = sorted(times)
				# if sorted is the same as times, is chronological, else
				if sortedTime != times:
					#indexes of times which need to be flipped
					idxs = [times.index(t) for t in sortedTime]
					for idx in idxs:
						# append the appropriate indexes in order
						outList.append(splitted[idx])
						outData.append(data[subject][idx])

					#flatten the nested list
					flatOutList = [outList[0]] + [elem for ls in outList[1:] for elem in ls]
			
					#print flatOutList
					
					#update values in dictionaries to return
					metaData[subject] = flatOutList
					
					data[subject] = outData

					#print subject + ":" + str(idxs)
			else:
				sortedDates = sorted(dates)
				#print subject + " wasn't run the same day."

				# all sessions were run on unique days
				if len(setDates) == len(dates) and sortedDates != dates:
					
					idxs = [dates.index(d) for d in sortedDates]
					for idx in idxs:
						outList.append(splitted[idx])
						outData.append(data[subject][idx])
					
					flatOutList = [outList[0]] + [elem for ls in outList[1:] for elem in ls]
					#out structs
					metaData[subject] = flatOutList
					
					data[subject] = outData
					# If some days subject was run multiple times, find those days in order and then sort by time	
	return metaData,data

def sessionError(subject):
	print subject +" has more than 3 sessions."

def writeOut(metaData,data):
	global SUBJECTLINE
	#data = {subject: [...]}
	subjects = sorted(data.keys())
	#print subjects
	for subject in subjects:
		if metaData[subject][0] > 3:
			sessionError(subject)
		else:
			#print metaData[subject][0]
			subjectInfo = metaData[subject][1:]
			#accumulator for meta data session
			accu = 1
			#print subject + ":"+ str(subjectInfo)
			for idx,elem in enumerate(subjectInfo):
				if len(elem) == 1:
					SUBJECTLINE += subject+'\t'+str(accu)+'\t' + "\t".join(subjectInfo[idx-2:idx+1]) +'\n'
					accu += 1
			#print subjectInfo
			#write out subject data
			subjectData = data[subject]
			#accumulator for subject data sessions
			accu2 = 1
			for session in subjectData:
				outf = open("Session"+str(accu2)+"/"+subject+".txt","w")
				session.pop(1)
				for line in session:
					outf.write(line)
				outf.close()
				accu2 += 1

def sortMeta():
	global SUBJECTLINE
	#print SUBJECTLINE
	metaDataf = open("SessionInfo.txt","w")	
	sessMeta = SUBJECTLINE.split('\n')
	header = sessMeta[0]
	data = sorted(sessMeta[1:])
	metaDataf.write(header)
	metaDataf.write("\n".join(data))

def main ():

	files = getFiles()

	for f in files:
		# MetaData dictionary subject: [numSessions, startDate, startTime, chambernum, ...]
		# data dictionary subject : [subjectData1, subjectData2...]
		metaData,data = getData(f)
		sortedMetaData,sortedData = chronologSort(metaData,data)
		writeOut(sortedMetaData,sortedData)

	#sort and write out metaData
	sortMeta()

main()