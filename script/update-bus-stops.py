import json
import urllib
from urlparse import urlparse
import httplib2 as http #External library if __name__=="__main__":
    #Authentication parameters
headers = {
    'AccountKey' : os.environ['ACCOUNT_KEY'],
    'accept' : 'application/json'
} #this is by default

with open("bus_stops.csv","w+") as outfile:
    #API parameters
    skip = 0

    print 'Downloading bus stops...'
    while True:
        print 'Page %d\n' % (skip / 500 + 1)
        uri = 'http://datamall2.mytransport.sg/' #Resource URL
        path = 'ltaodataservice/BusStops?$skip=%d' % skip

        target = urlparse(uri + path)
        print target.geturl()
        method = 'GET'
        body = ''
        #Get handle to http
        h = http.Http()
        #Obtain results
        response, content = h.request(
            target.geturl(),
            method,
            body,
            headers)
        #Parse JSON to print
        jsonObj = json.loads(content)
        #Saving jsonObj["d"]

        if len(jsonObj['value']) == 0:
            break

        for obj in jsonObj['value']:
            outfile.write("{BusStopCode}\t{RoadName}\t{Description}\n".format(**obj))

        skip += 500
