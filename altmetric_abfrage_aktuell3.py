#imports
import csv
import json
import requests
import sys
import os

#global variables
ind_of_doi = 0
error_log = 'Failed requests:\nRequests:'
success_requests = 0
number_dois = 0
relevant = 0
__location__ = os.path.realpath(os.path.join(os.getcwd(), os.path.dirname(__file__)))

#returns the name of a file
#does not check if file actually exists
#first searches in command line, then demands if necessary
def getInputFile():
	if len(sys.argv) == 2:
		return str(sys.argv[1])
	else:
		print("Please specify your input file. \nExample: test.csv")
		filename = input("Enter name of file with original data: ")
		return filename

def getDelimiter():
	print("Please specify the delimiter used in the input file.")
	delim=input("Enter ',' or ';': ")
	while delim != ',' and delim != ';':
		print("Invalid input.")
		delim=input("Please enter ',' or ';': ")
	return delim
	

#returns a list of all the dois found in the file
#which corresponds to filename and should be utf8 encoded
#will break if file does not exist
def getAllDois(filename, delim):
	global ind_of_doi
	ind_of_doi = 0
	global __location__ 
	try:
		with open(os.path.join(__location__, filename),"r",encoding="utf8") as file:
			csv_reader = csv.reader(file, delimiter=delim)
			lines = list(csv_reader)
			dois = []
			
			
			while lines[0][ind_of_doi] != 'DOI':
				ind_of_doi = ind_of_doi + 1
				
			global number_dois
			
			for line_number in range(1,len(lines)):
				if lines[line_number][ind_of_doi] != '' and lines[line_number][ind_of_doi] != 'NULL':
					dois.append(lines[line_number][ind_of_doi])
					number_dois+=1
				else:
					dois.append('NULL')
			return lines, dois
			
	except (OSError, IOError) as e:
		print('ERROR: input file not found')
		input('(press enter to close program)')
		sys.exit(1)
	except (UnicodeDecodeError):
		print("Decoding Error, trying to decode other format")
		with open(os.path.join(__location__, filename), "r",encoding="ANSI") as file:
			csv_reader = csv.reader(file, delimiter=delim)
			lines = list(csv_reader)
			dois = []

			while lines[0][ind_of_doi] != 'DOI':
				ind_of_doi = ind_of_doi + 1
			
			for line_number in range(1,len(lines)):
				if lines[line_number][ind_of_doi] != '' and lines[line_number][ind_of_doi] != 'NULL':
					dois.append(lines[line_number][ind_of_doi])
					number_dois+=1
				else:
					dois.append('NULL')
			return lines, dois


#returns name of a file with the altmetric scores 
#corresponding to the dois provided as a list
#sends requests to altmetric server
def getAltmetricScores(filename,lines,dois,treshold):
	tablehead = lines[0]
	categories=['cited_by_posts_count', 'cited_by_fbwalls_count', 'cited_by_feeds_count', 'cited_by_gplus_count', 'cited_by_msm_count', 'cited_by_peer_review_sites_count', 'cited_by_policies_count', 'cited_by_rdts_count', 'cited_by_rh_count', 'cited_by_tweeters_count', 'cited_by_videos_count', 'cited_by_wikipedia_count', 'added_on', 'published_on']
	tablehead += categories
	datalist = [tablehead]
	counter=0
	for doi in dois:
		counter+=1
		if doi=='NULL':
			continue
		#send a get request to altmetric
		get_request = "https://api.altmetric.com/v1/doi/" + doi
		
		try:
			response = requests.get(get_request)
		except (Exception):
			print('Request failed: Invalid server response')
			print('trying again...')
			suc = False
			while suc == False:
				try:
					suc = True
					response = requests.get(get_request)
				except (Exception):
					suc = False
			print('Finally received a valid response')
		
		if response is None or response.status_code != 200:
			global error_log
			error_log = error_log + '\n' + 'Problem getting the data from the altmetric server.\nServer response: ' + str(response.status_code) + "\nRequested doi: " + str(get_request)
			
		else:
			global success_requests
			success_requests += 1
			parsed_json = json.loads(response.text)
			try:
				if int(parsed_json["cited_by_msm_count"])>= treshold:
					scores=[]
					for cate in categories:
						try:
							scores.append(parsed_json[cate])
						except(Exception):
							scores.append("NULL")
					relline=lines[counter]+scores
					datalist.append(relline)
			except(KeyError):
				continue
	createErrorLog(filename)
	global relevant
	relevant=len(datalist)-1
	name_output_file = 'altmetric_scores_' + filename
	output_file = open(os.path.join(__location__, name_output_file), "w", encoding="utf8")
	csv_writer = csv.writer(output_file, delimiter=',', quotechar='"', quoting=csv.QUOTE_ALL)
	for row in datalist:
		csv_writer.writerow(row)
	output_file.close()
	return name_output_file

#creates a textfile containing error log
#all failed requests are written into error log	test
def createErrorLog(filename):
	global error_log
	global location
	only_name = filename.rsplit(".", 1)[0]
	name_log_file = 'Failed_requests_' + only_name + '.txt'
	log_file = open(os.path.join(__location__, name_log_file),"w")
	log_file.write(error_log)
	log_file.close()

#main function
#
if __name__ == "__main__":

	input_file = getInputFile()
	while input_file == "":
		input_file = getInputFile()
	delim=getDelimiter()
	treshold=int(input('Please enter threshold for msm-score as integer: '))
	#get all the dois from the input file
	data = getAllDois(input_file, delim)
	all_dois = data[1]
	lines=data[0]
	print('finished searching for dois in ' + input_file)
	print('found ' + str(number_dois))
	
	#sending get requests and writing received data into all_scores_file
	print('sending requests to altmetric server...')
	all_scores_file = getAltmetricScores(input_file,lines,all_dois,treshold)
	print('finished getting data from altmetric server')
	print(str(success_requests) + ' requests were successful')
	print(str(relevant) + ' papers are relevant')
	print('created ' + str(all_scores_file))
	
	input('press enter to continue')
	