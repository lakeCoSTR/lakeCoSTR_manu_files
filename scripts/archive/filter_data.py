import os
from datetime import datetime, timedelta
import pytz
from time import strftime
import pandas as pd

print("\nStart: ", strftime("%x %X"), "\n")

inspace = r"D:\Box Sync\IDS_Lutz\Bethel\Manuscript"

infile = r'all_temp_data_v2021Apr21.csv'
geefile = r'temp_stats_sunapee_1985_2020_05-11.csv'

outfile = r'temp_sunapee_paired_2021_0428.csv'

timewindow = 30  # number of minutes +/-
max_depth = 1.5  # sensor depth in m

"""
End user-input

Define functions
"""

os.chdir(inspace)
min_ms = timewindow * 60 * 1000.0
pd.options.mode.chained_assignment = None  # suppresses the false positive pandas "SettingWithCopyWarning"

# timezone = pytz.timezone("America/New_York")
timezone = pytz.timezone("US/Eastern")
dst_transitions = timezone._utc_transition_times
utc = pytz.timezone("UTC")
epoch = timezone.localize(datetime.fromtimestamp(0))


# Our data does not observe DST, but pytz expects it, so we adjust the time
def convert_datetime(dt, dst_obs=False, dtformat="%Y-%m-%d %H:%M:%S"):
    '''converts string format insitu time to epoch ms'''
    dto = datetime.strptime(dt, dtformat)

    dst_dates = []
    for each in dst_transitions:
        if each.year == dto.year:
            dst_dates.append(each)

    if dst_dates[0] <= dto <= dst_dates[1]:
        if not dst_obs:
            dto = datetime.strptime(dt, dtformat) + timedelta(hours=1)

    aware = timezone.localize(dto)
    return (aware - epoch).total_seconds() * 1000.0


def getExceltime(t):
    return (t / 1000.0 / 86400.0) + 25569


"""
Harmonize datasets
"""
colsToUse = ["datetime", "depth_m", "temp_degC", "source"]
print(strftime("%x %X"), "importing insitu data...")
insitu_csv = pd.read_csv(infile)  # , usecols=colsToUse)
shallow = insitu_csv[(insitu_csv.depth_m <= max_depth)]  # filter dataframe by depth
shallow["datetime_ms"] = shallow["datetime"].apply(convert_datetime)  # add a column to convert time

gee_csv = pd.read_csv(geefile)
# gee_csv.columns

gee_csv = gee_csv.drop([".geo"], axis=1)

for index, each in gee_csv.iterrows():
    scene = gee_csv.loc[index, "system:index"].split("1_")[-1]
    landsattime = gee_csv.loc[index, "landsat_time"]
    # print(strftime("%x %X"), index, scene)

    same_time = shallow[
        (abs(landsattime - shallow.datetime_ms) <= min_ms)]  # continue to winnow down records to +- 20min

    gee_csv.loc[index, "scene"] = scene
    gee_csv.loc[index, "avg_temp"] = same_time["temp_degC"].mean()
    gee_csv.loc[index, "t_std_dev"] = same_time["temp_degC"].std()
    gee_csv.loc[index, "avg_depth"] = same_time["depth_m"].mean()
    gee_csv.loc[index, "d_std_dev"] = same_time["depth_m"].std()
    gee_csv.loc[index, "count"] = same_time.shape[0]

    if same_time.shape[0] > 0:
        same_time.to_csv(os.path.join(inspace, scene + ".csv"))

print(strftime("%x %X"),"exporting harmonized csv...")
gee_csv.to_csv(os.path.join(inspace, outfile))

print("\nFinish: ", strftime("%x %X"), "\n")
