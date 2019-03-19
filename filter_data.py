import os,csv
from datetime import datetime,timedelta
import pytz
from time import strftime

print "\nStart: ",strftime("%x %X"),"\n"

inspace = r'D:\Box Sync\IDS_Lutz\Bethel\dropbox'

infile = r'sunapee_highfrequency_record_23Oct2018.csv'
geefile = r'temp_stats_sunapee_gee.csv'

outfile = r'temp_sunapee_paired.csv'
thermocline = 11

count = 0
early, earlyRow = None, None
#available depths
#[2.0, 0.5, 1.0, 1.5, 2.5, 3.0, 1.25]

os.chdir(inspace)

day1 = 1*24*60*60*1000.0
day2 = 2*24*60*60*1000.0
day3 = 3*24*60*60*1000.0
day4 = 4*24*60*60*1000.0
day5 = 5*24*60*60*1000.0
day6 = 6*24*60*60*1000.0
day7 = 7*24*60*60*1000.0

timezone = pytz.timezone("America/New_York")
utc = pytz.timezone("UTC")
epoch = timezone.localize(datetime.fromtimestamp(0))
def convert_datetime(dt):
    dto = datetime.strptime(dt,"%Y-%m-%d %H:%M:%S")
    aware = timezone.localize(dto)
    return (aware - epoch).total_seconds()*1000.0, dto.hour #two things returned

def earlyTime(rt,x): #rt=ms, x=early
    if x == None:
        x,earlyrow = rt,row
    elif rt<x:
        x = rt
    return rt,x

with open(infile,'rb') as incsv, open(geefile,'rb') as geecsv, open(outfile,'wb') as outcsv:
    reader = csv.reader(incsv)
    geereader = csv.reader(geecsv)
    writer = csv.writer(outcsv)

    geeheader = geereader.next()
    header = reader.next()
    newheader = geeheader[:-1]+["excel_time_l","excel_time_v","days_removed"]+header
    writer.writerow(newheader)
    
    for each in geereader:
        landsattime = float(each[3])/1000.0
        vaportime = float(each[11])/1000.0
        landsatexceltime = (landsattime/86400.0)+25569
        vaporexceltime = (vaportime/86400.0)+25569

        for row in reader:
        
            rowtime = row[0]       
            rowdepth = float(row[3])
            newrow = each[:-1] + [landsatexceltime,vaporexceltime]
            
            ms,dayhour = convert_datetime(rowtime)
            #ms,early = earlyTime(ms,early)

            if abs(landsattime-ms) <= day1:
                newrow += ['1']+row
                writer.writerow(newrow)
            elif abs(landsattime-ms) <= day2:
                newrow += ['2']+row
                writer.writerow(newrow)
            elif abs(landsattime-ms) <= day3:
                newrow += ['3']+row
                writer.writerow(newrow)
            elif abs(landsattime-ms) <= day4:
                newrow += ['4']+row
                writer.writerow(newrow)
            elif abs(landsattime-ms) <= day5:
                newrow += ['5']+row
                writer.writerow(newrow)
            elif abs(landsattime-ms) <= day6:
                newrow += ['6']+row
                writer.writerow(newrow)
            elif abs(landsattime-ms) <= day7:
                newrow += ['7']+row
                writer.writerow(newrow)
            else:
                writer.writerow(newrow)

#read_time = (datetime.fromtimestamp(ms/1000.0))
                
print "\nFinish: ",strftime("%x %X"),"\n"
