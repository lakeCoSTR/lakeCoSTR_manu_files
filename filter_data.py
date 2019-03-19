import os,csv
from datetime import datetime,timedelta
import pytz

inspace = r'D:\Box Sync\IDS_Lutz\Bethel\dropbox'
infile = r'sunapee_highfrequency_record_23Oct2018.csv'
outfile = r'sunapee_outrows.csv'
thermocline = 11

count = 0
early, earlyRow = None, None
#available depths
#[2.0, 0.5, 1.0, 1.5, 2.5, 3.0, 1.25]

os.chdir(inspace)

timezone = pytz.timezone("America/New_York")
utc = pytz.timezone("UTC")
epoch = timezone.localize(datetime.fromtimestamp(0))
def convert_datetime(dt):
    dto = datetime.strptime(dt,"%Y-%m-%d %H:%M:%S")
    aware = timezone.localize(dto)
    return (aware - epoch).total_seconds()*1000.0, dto.hour

def earlyTime(rt,x): #rt=ms, x=early
    if x == None:
        x,earlyrow = rt,row
    elif rt<x:
        x = rt
    return rt,x

with open(infile,'rb') as incsv, open(outfile,'wb') as outcsv:
    reader = csv.reader(incsv)
    writer = csv.writer(outcsv)
    header = reader.next()
    for row in reader:
        rowtime = row[0]       
        rowdepth = float(row[3])
        
        ms,dayhour = convert_datetime(rowtime)
        ms,early = earlyTime(ms,early)

        
        
        count +=1

read_time = (datetime.fromtimestamp(early/1000.0))

print count
print read_time
print depth

