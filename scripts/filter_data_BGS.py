#some edits made to run in RStudio 1.4. OG script by CH. Run R_library.R before running this to activate reticulate. At this time, the code is not running - likely needs extensive syntax edits for RStudio. Just run the OG script in PyCharm for now.

import os, csv
from datetime import datetime
import pytz
from time import strftime

print("\nStart: ", strftime("%x %X"), "\n")

inspace = r'data/'

infile = r'sunapee_highfrequency_record_22Jul2019.csv'
geefile = r'temp_stats_sunapee_gee.csv'

outfile = r'temp_sunapee_paired.csv'

# available depths
# [2.0, 0.5, 1.0, 1.5, 2.5, 3.0, 1.25]

os.chdir(inspace)

min20 = 20 * 60 * 1000.0
day1 = 1 * 24 * 60 * 60 * 1000.0
day2 = 2 * 24 * 60 * 60 * 1000.0
day3 = 3 * 24 * 60 * 60 * 1000.0
day4 = 4 * 24 * 60 * 60 * 1000.0
day5 = 5 * 24 * 60 * 60 * 1000.0
day6 = 6 * 24 * 60 * 60 * 1000.0
day7 = 7 * 24 * 60 * 60 * 1000.0

timezone = pytz.timezone("America/New_York")
utc = pytz.timezone("UTC")
epoch = timezone.localize(datetime.fromtimestamp(0))


def convert_datetime(dt):
    dto = datetime.strptime(dt, "%Y-%m-%d %H:%M:%S")
    aware = timezone.localize(dto)
    return (aware - epoch).total_seconds() * 1000.0, dto.hour  # two things returned



def _ss(data):
    """Return sum of square deviations of sequence data"""
    n = len(data)
    c = sum(data) / (n * 1.0)
    ss = sum((x - c) ** 2 for x in data)
    return ss


def stdev(data, ddof=0):
    """Calculates the population std dev by default; specifiy ddof=1 to compute the sample stddev"""
    n = len(data)
    if n < 2:
        raise ValueError('variance requires at least two data points')
    ss = _ss(data)
    pvar = ss / (n - ddof)
    return pvar ** 0.5


with open(infile, 'rb') as incsv, open(geefile, 'rb') as geecsv, open(outfile, 'wb') as outcsv:
    reader = csv.reader(incsv)
    geereader = csv.reader(geecsv)
    writer = csv.writer(outcsv)

    geeheader = geereader.next()
    header = reader.next()
    newheader = geeheader[:-1] + ["excel_time_l", "excel_time_v", "days_removed", "avg_temp", "t_std_dev", "count",
                                  "avg_depth", "d_std_dev"]
    writer.writerow(newheader)

    for each in geereader:
        scene = each[0]
        landsattime = float(each[3])
        vaportime = float(each[7])
        landsatexceltime = (landsattime / 1000.0 / 86400.0) + 25569
        vaporexceltime = (vaportime / 1000.0 / 86400.0) + 25569

        measurements = []
        depth = []

        newrow = each[:-1] + [landsatexceltime, vaporexceltime, "0"]

        reader.next()
        with open(scene+'.csv','wb') as copycsv:
            copywriter = csv.writer(copycsv)
            copywriter.writerow(header)

            for row in reader:

                rowtime = row[0]
                rowdepth = float(row[3])
                temperature = float(row[4])
                flag = row[5]
                try:
                    buff125 = int(row[6])
                except:
                    buff125 = row[6]
                try:
                    buff500 = int(row[7])
                except:
                    buff500 = row[7]

                # if flag == "depth is within 0.5m" or rowdepth <= 1.5:
                if rowdepth <= 1.5:

                    ms, dayhour = convert_datetime(rowtime)

                    if abs(landsattime - ms) <= min20:
                        # newrow += ['0',ms]+row
                        # writer.writerow(newrow)
                        measurements.append(temperature)
                        depth.append(rowdepth)
                        copywriter.writerow(row)

            print (measurements),
            # scratch = raw_input('\npress anything to continue')

            count = len(measurements)

            try:
                t_avg = sum(measurements) / (count * 1.0)
                d_avg = sum(depth) / (count * 1.0)
                t_result = stdev(measurements)
                d_result = stdev(depth)
                print (t_avg, t_result, '\n')
            except ZeroDivisionError:
                t_avg = 0
                d_avg = 0
                t_result = 0
                d_result = 0
                print (t_avg, t_result, '\n')

            newrow += [t_avg, t_result, count, d_avg, d_result]

            writer.writerow(newrow)

        incsv.seek(0)

# read_time = (datetime.fromtimestamp(ms/1000.0))

print ("\nFinish: ", strftime("%x %X"), "\n")
