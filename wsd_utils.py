import plotly
import plotly.graph_objs as go
import re
import os
import sys
import json
import wapi
import shutil
import pandas as pd
import numpy as np
import datetime as dt
from datetime import datetime, timedelta, timezone
from wapi.util import CurveException
from wapi.auth import AuthFailedException
from wapi.session import MetadataException
import dash_core_components as dcc
import dash_html_components as html
import isodate
import win32api, win32con, win32process

USER = os.getlogin()

secrets_json = f'C:\\Users\\{USER}\\WATTSIGHT\\JSON\\SECRETS.json'

with open(secrets_json, 'r', encoding='UTF-8') as fin:
    ws_secrets = json.load(fin)

USERNAME = ws_secrets['Alpiq']['USERNAME']
PASSWORD = ws_secrets['Alpiq']['PASSWORD']
URL = ws_secrets['Alpiq']['PROXY_URL']
PORT = ws_secrets['Alpiq']['PROXY_PORT']

WS_CLIENT_ID = ws_secrets['Wattsight']['CLIENT_ID']
WS_CLIENT_SECRET = ws_secrets['Wattsight']['CLIENT_SECRET']

os.environ['http_proxy'] = f'http://{URL}:{PORT}'
os.environ['https_proxy'] = f'https://{USERNAME}:{PASSWORD}@{URL}:{PORT}'

try:
    ws_session = wapi.Session(client_id = WS_CLIENT_ID, 
                            client_secret = WS_CLIENT_SECRET)
except:
    os.environ['https_proxy'] = ''
    os.environ['http_proxy'] = ''


def SetCoreAffinity(cpu):
    if cpu == 0:
        mask = 0
    if cpu == 1:
        mask = 2
    if cpu == 2:
        mask = 4
    if cpu == 3:
        mask = 8
    if cpu == 4:
        mask = 16
    if cpu == 5:
        mask = 32
    if cpu == 6:
        mask = 64
    if cpu == 7:
        mask = 128
        
    pid  = win32api.GetCurrentProcessId()
    handle = win32api.OpenProcess(win32con.PROCESS_ALL_ACCESS, True, pid)
    win32process.SetProcessAffinityMask(handle, mask)


def ReplaceFavIco(user, country_code):
    
    favico = f'C:\\Users\\{user}\\WATTSIGHT\\PYLIB\\DISPLAY\\assets\\favicon.ico'
    if os.path.isfile(favico):
        os.remove(favico)
    ico = f'C:\\Users\\{user}\\WATTSIGHT\\PYLIB\\DISPLAY\\ico\\{country_code}.ico'
    new_ico =  f'C:\\Users\\{user}\\WATTSIGHT\\PYLIB\\DISPLAY\\assets\\{country_code}.ico'
    shutil.copy(ico, f'C:\\Users\\{user}\\WATTSIGHT\\PYLIB\\DISPLAY\\assets')
    os.rename(new_ico, favico)


def GetCurveNameRegex(pattern, curve_list):
    
    res = list(filter(re.compile(pattern).match, curve_list))[0]
    
    return res

def GetCurveNames(curves_json, country_code, category, freq, actual_name):
    
    country_code = country_code.upper()
    category = category.upper()
    freq = freq.upper()
    
    cat_list = list(curves_json[f'{country_code}_{category}_{freq}'].values())

    ac = actual_name.strip().lower()
    no = GetCurveNameRegex(r'.*n$', cat_list)
    mo = GetCurveNameRegex(r'.*\becmonthly\b.*', cat_list)
    ec = GetCurveNameRegex(r'.*\bec00\b.*', cat_list)
    ecs = GetCurveNameRegex(r'.*\bec00ens\b.*', cat_list)
    gf = GetCurveNameRegex(r'.*\bgfs00\b.*', cat_list)
    gfs = GetCurveNameRegex(r'.*\bgfs00ens\b.*', cat_list)

    return [no, mo, ec, ecs, gf, gfs, ac]


def GetNormal(WS_CLIENT_ID, 
              WS_CLIENT_SECRET, 
              curve_name, 
              freq, 
              func, 
              tz,
              data_from,
              data_to):
    
    ws_session = wapi.Session(client_id = WS_CLIENT_ID, client_secret = WS_CLIENT_SECRET)
    no_curve = ws_session.get_curve(name = curve_name)
    no_ts = no_curve.get_data(frequency = f'{freq}', 
                              function = f'{func}',
                              output_time_zone = f'{tz}',
                              data_from = data_from,
                              data_to = data_to)
    
    no_s = no_ts.to_pandas()
    no_s = no_s.ffill().where(no_s.bfill().notnull())
    no_s = no_s.dropna()  
    
    no_df = no_s.to_frame()
    no_df.reset_index(inplace = True, drop = False)
    no_df.columns = ['times', 'values']
    
    return no_df


def GetMonthly(WS_CLIENT_ID, 
               WS_CLIENT_SECRET, 
               curve_name, 
               freq, 
               func, 
               tz):
    
    ws_session = wapi.Session(client_id = WS_CLIENT_ID, client_secret = WS_CLIENT_SECRET)
    mo_curve = ws_session.get_curve(name = curve_name)
    mo_ts = mo_curve.get_latest(frequency = f'{freq}', 
                                function = f'{func}',
                                output_time_zone = f'{tz}')
    mo_s = mo_ts.to_pandas()
    mo_s = mo_s.ffill().where(mo_s.bfill().notnull())
    mo_s = mo_s.dropna()  
    
    mo_df = mo_s.to_frame()
    mo_df.reset_index(inplace = True, drop = False)
    mo_df.columns = ['times', 'values']
    
    return mo_df


def GetCategoryBundle(WS_CLIENT_ID, WS_CLIENT_SECRET,
                      curves_json, 
                      country_code, 
                      category, 
                      freq, 
                      func, 
                      tz, 
                      actual_name,
                      data_from_nor,
                      data_to_nor):
    
    curve_names = GetCurveNames(curves_json = curves_json, 
                                country_code = country_code, 
                                category = category, 
                                freq = freq, 
                                actual_name = actual_name)

    no_df = GetNormal(WS_CLIENT_ID = WS_CLIENT_ID,  
                      WS_CLIENT_SECRET = WS_CLIENT_SECRET, 
                      curve_name = curve_names[0], 
                      freq = freq, 
                      func = func, 
                      tz = tz, 
                      data_from = data_from_nor, 
                      data_to = data_to_nor)

    mo_df = GetMonthly(WS_CLIENT_ID = WS_CLIENT_ID,  
                       WS_CLIENT_SECRET = WS_CLIENT_SECRET, 
                       curve_name = curve_names[1], 
                       freq = freq, 
                       func = func, 
                       tz = tz)
    
    return curve_names, no_df, mo_df


def GetAvaCat(pattern, ava_list):
    
    ava = list(filter(re.compile(pattern).match, ava_list))

    if len(ava) == 1:
        ava = ava[0]
    else:
        ava = False
        
    return ava


def GetRelative(WS_CLIENT_ID,
                WS_CLIENT_SECRET,
                offset, 
                duration, 
                freq, 
                func, 
                tz,
                data_from,
                data_to,
                names_list):
    
    ws_session = wapi.Session(client_id = WS_CLIENT_ID, client_secret = WS_CLIENT_SECRET)
    
    if names_list[0][:3] == 'con':
        ec_short = ' '.join(names_list[2].split(' ')[2:3])[:-2]
        ec_ens_short = ' '.join(names_list[2].split(' ')[2:3])[:-2] + '_ens'
        gf_short = ' '.join(names_list[4].split(' ')[2:3])[:-2]
        gf_ens_short = ' '.join(names_list[4].split(' ')[2:3])[:-2] + '_ens'
        
    else: 
        ec_short = ' '.join(names_list[2].split(' ')[3:4])[:-2]
        ec_ens_short = ' '.join(names_list[2].split(' ')[3:4])[:-2] + '_ens'
        gf_short = ' '.join(names_list[4].split(' ')[3:4])[:-2]
        gf_ens_short = ' '.join(names_list[4].split(' ')[3:4])[:-2] + '_ens'   

    if names_list[0][:2] == 'tt':
        actu_short = 'backcast'
    else:
        actu_short = 'actuals'
    
    ec_s = GetRelativeForecast(ws_session = ws_session, 
                                forecast = names_list[2], 
                                offset = offset, 
                                duration = duration, 
                                freq = freq, 
                                func = func, 
                                tz = tz, 
                                issue_date_from = data_from, 
                                issue_date_to = data_to)
    
    ec_ens_s = GetRelativeForecast(ws_session = ws_session, 
                                   forecast = names_list[3], 
                                   offset = offset, 
                                   duration = duration, 
                                   freq = freq, 
                                   func = func, 
                                   tz = tz, 
                                   issue_date_from = data_from, 
                                   issue_date_to = data_to)


    gf_s = GetRelativeForecast(ws_session = ws_session, 
                               forecast = names_list[4], 
                               offset = offset, 
                               duration = duration, 
                               freq = freq, 
                               func = func, 
                               tz = tz, 
                               issue_date_from = data_from, 
                               issue_date_to = data_to)
    
    gf_ens_s = GetRelativeForecast(ws_session = ws_session, 
                                   forecast = names_list[5], 
                                   offset = offset, 
                                   duration = duration, 
                                   freq = freq, 
                                   func = func, 
                                   tz = tz, 
                                   issue_date_from = data_from, 
                                   issue_date_to = data_to)
    
    
    ac_s = GetActualHistory(ws_session = ws_session, 
                            curve_name = names_list[6], 
                            freq = freq, 
                            func = func, 
                            tz = tz, 
                            data_from = data_from, 
                            data_to = data_to)

    s_list = [ac_s, ec_s, ec_ens_s, gf_s, gf_ens_s]
    
    for i in range(len(s_list)):
        s_list[i] = s_list[i][ac_s.index]
        
    ac_df = ac_s.to_frame()
    ac_df.reset_index(inplace=True, drop=False)
    ac_df.columns = ['times', 'values']

    ec_df = ec_s.to_frame()
    ec_df.reset_index(inplace=True, drop=False)
    ec_df.columns = ['times', 'values']

    ec_ens_df = ec_ens_s.to_frame()
    ec_ens_df.reset_index(inplace=True, drop=False)
    ec_ens_df.columns = ['times', 'values']

    gf_df = gf_s.to_frame()
    gf_df.reset_index(inplace=True, drop=False)
    gf_df.columns = ['times', 'values']

    gf_ens_df = gf_ens_s.to_frame()
    gf_ens_df.reset_index(inplace=True, drop=False)
    gf_ens_df.columns = ['times', 'values']
    
    return ac_df, ec_df, ec_ens_df, gf_df, gf_ens_df, actu_short, ec_short, ec_ens_short, gf_short, gf_ens_short



def GetLatestAvailSeries(ws_session, 
                         curve_name, 
                         freq = None, 
                         func = None, 
                         tz = None, 
                         data_from = None, 
                         data_to = None):

    curve_name = curve_name.strip()
    curve = ws_session.get_curve(name = curve_name)
    
    if curve.curve_type == 'INSTANCES' or curve.curve_type == 'TAGGED_INSTANCES':
    
        meta_ts = curve.get_latest(with_data = False)
        issue_date = meta_ts.issue_date
        ts = curve.get_instance(issue_date = issue_date,
                                data_from = data_from,
                                data_to = data_to,
                                frequency = freq, 
                                function = func,
                                output_time_zone = tz)
        s = ts.to_pandas()
        s = s.ffill().where(s.bfill().notnull())
        s = s.dropna() 
    
        ava_df = s.to_frame()
        ava_df.reset_index(inplace = True, drop = False)
        ava_df.columns = ['times', 'values']
    
    elif curve.curve_type == 'TIME_SERIES' or curve.curve_type == 'TAGGED':
        
        ts = curve.get_data(data_from = data_from, 
                            data_to = data_to,
                            frequency = freq,
                            function = func, 
                            output_time_zone = tz)
        
        s = ts.to_pandas()
        s = s.ffill().where(s.bfill().notnull())
        s = s.dropna()
        
        ava_df = s.to_frame()
        ava_df.reset_index(inplace = True, drop = False)
        ava_df.columns = ['times', 'values']        
    
    return ava_df


def GetLatestForecastSeries(ws_session, 
                            curve_name, 
                            freq = None, 
                            func = None, 
                            tz = None):

    now = pd.Timestamp.now(tz = 'CET')
    today = now.floor('D')
    
    yesterday = today - pd.Timedelta(days = 1)
    yesterday =  yesterday.date()
    
    before_yesterday = today - pd.Timedelta(days = 2)
    before_yesterday = before_yesterday.date()
    
    before_before_yesterday = today - pd.Timedelta(days = 3)
    before_before_yesterday = before_before_yesterday.date()
    
    tomorrow = today + pd.Timedelta(days = 1)
    tomorrow = tomorrow.date()
    
    
    curve_name = curve_name.strip()
    now = pd.Timestamp.now(tz = 'CET').floor('H')
    match = re.search(r'ec\d{2}|gfs\d{2}', curve_name)
    if match is None:
        print(f'[{curve_name}] is not a valid EC or GFS curve name.')
        return
    
    time_zones = ['ART', 'CEGT', 'CET', 'EET', 'MSK', 'PST', 'TRT', 'UTC', 'WEGT', 'WET']
    functions = ['AVERAGE', 'DIVIDE', 'LAST', 'MAX', 'MIN', 'SAME', 'SUM']
    frequencies = ['D', 'H', 'H12', 'H3', 'H6', 'M', 'MIN', 'MIN15', 'MIN30', 'MIN5', 'Q', 'S', 'W', 'Y']
    
    if tz and tz not in time_zones:
        print(f'Invalid time zone [{tz}].\r\nMust be in {time_zones}\r\n')
        return
        
    if func and func not in functions:
        print(f'Invalid function [{func}].\r\nMust be in {functions}\r\n')
        return
        
    if freq and freq not in frequencies:
        print(f'Invalid frequency [{freq}].\r\nMust be in {frequencies}\r\n')
        return


    try:
        test_curve = ws_session.get_curve(name = curve_name)
        del test_curve
        
    except MetadataException as e:
        print(f'[{curve_name}] is not a valid curve name: {e}')
        return 
    
    
    issues = ['00', '06', '12', '18']
    issue_list = []

    match = None
    match = re.search(r'ec\d{2}', curve_name)
    if match:
        match = None
        match = re.search(r'ec\d{2}ens', curve_name)
        if match:
            issue_list = [f'ec{issue}ens' for issue in issues]
            regexpat = r'ec\d{2}ens'
        else:
            issue_list = [f'ec{issue}' for issue in issues]
            regexpat = r'ec\d{2}'
    else:
        match = None
        match = re.search(r'gfs\d{2}ens', curve_name)
        if match:
            issue_list = [f'gfs{issue}ens' for issue in issues]
            regexpat = r'gfs\d{2}ens'
        else:
            issue_list = [f'gfs{issue}' for issue in issues]
            regexpat = r'gfs\d{2}'

    part_list = curve_name.split(' ')

    pos = 0
    for part in part_list:
        match = None
        match = re.search(regexpat, part)
        if match:
            part_list.pop(pos)
            break
        pos += 1

    curve_list = []

    for i in range(len(issue_list)):

        part_list.insert(pos, issue_list[i])
        curve_name = ''
        curve_name = curve_name.join(part + ' ' for part in part_list).strip()
        part_list.pop(pos)
        
        try:
            curve = ws_session.get_curve(name = curve_name)
        except MetadataException:
            pass
        else:
            curve_list.append(curve)

    curves_created = []
    curve_names = []
    issue_list = []

    for i in range(len(curve_list)):

        temp_meta = curve_list[i].get_latest(with_data = False, 
                                             output_time_zone = tz,
                                             issue_date_from = before_before_yesterday,
                                             issue_date_to = tomorrow)

        if temp_meta:
            curve_names.append(curve_list[i].name)
            curves_created.append(temp_meta.created)
            issue_list.append(temp_meta.issue_date)


    latest_created = max(curves_created)
    idx = curves_created.index(latest_created)
    latest_name = curve_names[idx]
    latest_issue = issue_list[idx]
    latest_curve = curve_list[idx]
    
    curves_created.pop(idx)
    previous_created = max(curves_created)
    curves_created.insert(idx, latest_created)
    
    previous_idx = curves_created.index(previous_created)
    previous_issue = issue_list[previous_idx]
    previous_curve = curve_list[previous_idx]
        
    latest_ts = latest_curve.get_instance(issue_date = latest_issue,
                                          data_from = now,
                                          frequency = freq, 
                                          function = func,
                                          output_time_zone = tz)
    
    previous_ts = previous_curve.get_instance(issue_date = previous_issue,
                                              data_from = now,
                                              frequency = freq, 
                                              function = func, 
                                              output_time_zone = tz)   
    if not latest_ts or not previous_ts:
        
        latest_issue = pd.Timestamp(latest_issue) - pd.Timedelta(days = 1)
        previous_issue = pd.Timestamp(latest_issue) - pd.Timedelta(days = 1)
        
        latest_issue = str(latest_issue.date())
        latest_ts = latest_curve.get_instance(issue_date = latest_issue,
                                              data_from = now,
                                              frequency = freq, 
                                              function = func,
                                              output_time_zone = tz) 
        
        previous_issue = str(previous_issue.date())
        previous_ts = previous_curve.get_instance(issue_date = previous_issue,
                                                  data_from = now,
                                                  frequency = freq, 
                                                  function = func, 
                                                  output_time_zone = tz)  
    
        if not latest_ts or not previous_ts:
            
            latest_issue = pd.Timestamp(latest_issue) - pd.Timedelta(days = 1)
            previous_issue = pd.Timestamp(latest_issue) - pd.Timedelta(days = 1)
            
            latest_issue = str(latest_issue.date())
            latest_ts = latest_curve.get_instance(issue_date = latest_issue,
                                                  data_from = now,
                                                  frequency = freq, 
                                                  function = func,
                                                  output_time_zone = tz)
            
            previous_issue = str(previous_issue.date())
            previous_ts = previous_curve.get_instance(issue_date = previous_issue,
                                                      data_from = now,
                                                      frequency = freq, 
                                                      function = func, 
                                                      output_time_zone = tz)                 
    
    if len(latest_ts.points) <= 2 or len(previous_ts.points) <= 2:
        
        latest_issue = pd.Timestamp(latest_issue) - pd.Timedelta(days = 1)
        previous_issue = pd.Timestamp(latest_issue) - pd.Timedelta(days = 1)
        
        latest_issue = str(latest_issue.date())
        latest_ts = latest_curve.get_instance(issue_date = latest_issue,
                                              data_from = now,
                                              frequency = freq, 
                                              function = func,
                                              output_time_zone = tz)
        
        previous_issue = str(previous_issue.date())
        previous_ts = previous_curve.get_instance(issue_date = previous_issue,
                                                  data_from = now,
                                                  frequency = freq, 
                                                  function = func, 
                                                  output_time_zone = tz)  
    
        if len(latest_ts.points) <= 2 or len(previous_ts.points) <= 2:
            
            latest_issue = pd.Timestamp(latest_issue) - pd.Timedelta(days = 1)
            previous_issue = pd.Timestamp(latest_issue) - pd.Timedelta(days = 1)
            
            latest_issue = str(latest_issue.date())
            latest_ts = latest_curve.get_instance(issue_date = latest_issue,
                                                  data_from = now,
                                                  frequency = freq, 
                                                  function = func,
                                                  output_time_zone = tz)
            
            previous_issue = str(previous_issue.date())
            previous_ts = previous_curve.get_instance(issue_date = previous_issue,
                                                      data_from = now,
                                                      frequency = freq, 
                                                      function = func, 
                                                      output_time_zone = tz)      

    previous_s = previous_ts.to_pandas()
    previous_s = previous_s.ffill().where(previous_s.bfill().notnull())
    previous_s = previous_s.dropna()
    
    latest_s = latest_ts.to_pandas()
    latest_s = latest_s.ffill().where(latest_s.bfill().notnull())
    latest_s = latest_s.dropna()   
    
    if max(previous_s.index) > max(latest_s.index) and len(latest_s.index) < len(previous_s.index):
        
        previous_extra = previous_s[previous_s.index > max(latest_s.index)]
        latest_s = latest_s.append(previous_extra)
        latest_s.sort_index(ascending = False, inplace = True)
    
    latest_name = latest_ts.name
    latest_df = latest_s.to_frame()
    latest_df.reset_index(inplace = True, drop = False)
    latest_df.columns = ['times', 'values']
    
    return latest_df, latest_name


def GetRelativeForecast(ws_session, 
                        forecast, 
                        offset, 
                        duration, 
                        freq, 
                        func, 
                        tz, 
                        issue_date_from, 
                        issue_date_to):
    
    name_parts = forecast.split(' ')
    name_parts[3] = name_parts[3][:2]
    avg_name = ' '.join(name_parts)
    
    if not isinstance(offset, int):
        print(f'Offset must be an integer of the number of hours. Got[{offset}].')
        return

    if not isinstance(duration, int):
        print(f'Duration must be an integer of the number of hours. Got [{duration}].')
        return

    if not isinstance(forecast, str):
        print(f'[{forecast}] is not a string!')
        return
    
    if forecast == '':
        print('Curve name is empty!')
        return
    
    forecast = forecast.strip()
    
    if forecast[-1] != 'f' and forecast[-1] != 'af':
        print(f'[{forecast}] is not a forecast curve (data type F or AF)!')
        return

    match = re.search(r'\d{4}-([1-9]|0[1-9]|1[0-2])-([1-9]|0[1-9]|[1-3][0-9])', issue_date_from)
    if match is None:
        print(f'[{issue_date_from}] in wrong format, must be YYYY-M-D')
        return

    try:
        curve = ws_session.get_curve(name = forecast)
    except Exception as e:
        print(f'[{forecast}] cannot be retrieved from Wattsight: {e}')
        return

    iso_offset = isodate.duration_isoformat(pd.Timedelta(hours = offset))
    iso_duration = isodate.duration_isoformat(pd.Timedelta(hours = duration))
    
    relative_ts = curve.get_relative(data_offset = iso_offset, 
                                     data_max_length = iso_duration, 
                                     frequency = freq,
                                     function = func,
                                     output_time_zone = tz,
                                     issue_date_from = issue_date_from,
                                     issue_date_to = issue_date_to)    
    
    relative_s = relative_ts.to_pandas()
    relative_s = relative_s.ffill().where(relative_s.bfill().notnull())
    relative_s = relative_s.dropna()   
    
    relative_s.name = avg_name
    
    return relative_s


def GetRelativeAverage(ws_session, 
                       for_list, 
                       offset, 
                       duration, 
                       freq, 
                       func, 
                       tz, 
                       issue_date_from, 
                       issue_date_to):
    
    name_parts = for_list[0].split(' ')
    name_parts[3] = name_parts[3][:2]
    name = ' '.join(name_parts)
    
    rel_list = []
    len_list = []

    for forecast in for_list:
        
        rel_s = GetRelativeForecast(ws_session = ws_session, 
                                    forecast = forecast, 
                                    offset = offset, 
                                    duration = duration, 
                                    freq = freq, 
                                    func = func, 
                                    tz = tz, 
                                    issue_date_from = issue_date_from, 
                                    issue_date_to = issue_date_to)
        rel_list.append(rel_s)
        len_list.append(len(rel_s))  
        
    min_len = min(len_list)
    idx = len_list.index(min_len)
    min_s = rel_list[idx]
    
    for i in range(len(rel_list)):
        rel_list[i] = rel_list[i][min_s.index]

    value_list = []
    for ec_s in rel_list:
        value_list.append(list(ec_s.values))
    avg_array = np.array(value_list)
    avg_values = list(np.average(avg_array, axis=0)) 
    
    avg_rel_s = pd.Series(avg_values, index=rel_list[0].index)
    avg_rel_s.name = name   
    
    return avg_rel_s



def GetActualHistory(ws_session, 
                     curve_name, 
                     freq, 
                     func, 
                     tz, 
                     data_from, 
                     data_to):
    
    actual_curve = ws_session.get_curve(name = curve_name)

    actual_ts = actual_curve.get_data(data_from = data_from, 
                                      data_to = data_to,
                                      frequency = freq,
                                      function = func, 
                                      output_time_zone = tz)
    
    actual_s = actual_ts.to_pandas()
    actual_s = actual_s.ffill().where(actual_s.bfill().notnull())
    actual_s = actual_s.dropna()   

    return actual_s


def GetTitleTable(category, 
                  unit, 
                  freq):
    
    title_table = html.Table([ 
                    html.Tbody([
                        html.Tr([
                            html.Td([f'{category.upper()}'],
                                    style={
                                        'display': 'inline-block',
                                        'vertical-align': 'bottom',
                                        'line-height': 30,
                                        'height': 30,
                                        'padding': 0,
                                        'padding-top': 5,
                                        'padding-left': 40,
                                        'margin': 0,
                                        'font-size': 30,
                                        'font-weight': 'bold',
                                        'color': 'navy',
                                        'background-color': '#ecf0f1'}),

                            html.Td([f'({unit})'],
                                    style={
                                        'display':'inline-block',
                                        'vertical-align':'bottom',
                                        'line-height': 30,
                                        'height': 30,
                                        'padding': 0,
                                        'padding-left': 8,
                                        'padding-top': 5,
                                        'margin': 0,
                                        'font-size': 18,
                                        'color':'navy',
                                        'background-color': '#ecf0f1'}),
                        
                            html.Td([f'[{freq}]'], 
                                        style={
                                        'display':'inline-block',
                                        'vertical-align': 'bottom',
                                        'line-height': 30,
                                        'height': 30,
                                        'padding': 0,
                                        'padding-top': 5,
                                        'padding-left': 8,
                                        'margin': 0,
                                        'font-size': 18,
                                        'color': 'navy',
                                        'background-color': '#ecf0f1'})
                            ], style={'width':'100%',
                                      'display':'flex',
                                      'flex-direction':'row'})                                                  
                        ])
                    ], style={
                        'cellspacing': 0,
                        'cellpadding': 0,
                        'border': 0,
                        'width': '100%',
                        'background-color': '#ecf0f1',
                        'height': 33}
                    )
    
    return title_table


def GetLayout(x_range, 
              y_range):
    
    figure_layout = {
        'autosize': False,
        'height': 450,
        'hovermode': 'x unified',
        'xaxis':  {'showgrid': True, 
                   'range': [min(x_range), max(x_range)],
                   'showline': True,
                   'linewidth': 1.3,
                   'linecolor': 'navy',
                   'mirror':True},
        'yaxis': {'showgrid': True, 
                  'gridcolor': 'darkgray', 
                  'range': [min(y_range), max(y_range)], 
                  'automargin': True,
                   'showline': True,
                   'linewidth': 1.3,
                   'linecolor': 'navy',
                   'mirror':True},
        'plot_bgcolor': '#fbfcfc',
        'paper_bgcolor': '#ecf0f1',
        'margin': {'l': 30, 
                   'r': 30,
                   'b': 50,
                   't': 20},
        'legend_orientation': "h",
        'legend': {
            'x': 0.5, 'y': 1.1, 'font': {
                'size':11}, 
            'xanchor':'center'}}
    
    return figure_layout


def DisplayAvaCat(WS_CLIENT_ID,
                  WS_CLIENT_SECRET,
                  freq,
                  func,
                  tz,
                  unit,
                  curve_name,
                  data_from,
                  data_to):
    
    ws_session = wapi.Session(client_id = WS_CLIENT_ID, 
                              client_secret = WS_CLIENT_SECRET)
    x_range = []
    y_range = []
    color = 'blue'

    ava_df = GetLatestAvailSeries(ws_session = ws_session, 
                                  curve_name = curve_name, 
                                  freq = freq, 
                                  func = func, 
                                  tz = tz, 
                                  data_from = data_from, 
                                  data_to = data_to)
    
    X = ava_df['times'].to_list()
    Y = ava_df['values'].to_list()
    if len(X) > 2:
        x_range.append(min(X))
        x_range.append(max(X))
    if len(Y) > 2:
        y_range.append(min(Y))
        y_range.append(max(Y))  
    short_name = 'ror'
    
    data = plotly.graph_objs.Scatter(
        x = X,
        y = Y,
        name = short_name,
        mode = 'lines',
        line = dict(shape = 'linear', 
                    color = color, 
                    width = 1.4
                    )
        )
    
    return {'data': [data], 'layout': go.Layout(GetLayout(x_range, y_range))}



def DisplayAvail(WS_CLIENT_ID,
                 WS_CLIENT_SECRET,
                 freq,
                 func,
                 tz,
                 unit,
                 curve_list,
                 data_from,
                 data_to):
    
    ws_session = wapi.Session(client_id = WS_CLIENT_ID, client_secret = WS_CLIENT_SECRET)
    
    x_range = []
    y_range = []
    data_list = []
    
    col_nuc = 'crimson'
    col_t_coa = 'gray'
    col_t_oil = 'black'
    col_t_lig = 'sienna'
    col_t_bio = 'forestgreen'
    col_h_tot = 'blue'
    col_h_pum = 'deepskyblue'
    
    pat_gwh = r'.*\bgwh\b.*'
    
    for curve_name in curve_list:
    
        ava_df = GetLatestAvailSeries(ws_session = ws_session, 
                                      curve_name = curve_name, 
                                      freq = freq, 
                                      func = func, 
                                      tz = tz, 
                                      data_from = data_from, 
                                      data_to = data_to)
        
        X = ava_df['times'].to_list()
        Y = ava_df['values'].to_list()
        
        match_gwh = None
        match_gwh = re.search(pat_gwh, curve_name)
        
        if match_gwh:
            Y = [y * 1000 for y in Y]
        
        if len(X) > 2:
            x_range.append(min(X))
            x_range.append(max(X))
        if len(Y) > 2:    
            y_range.append(min(Y))
            y_range.append(max(Y))
            
        if 'nuc' in curve_name:
            color = col_nuc
            short_name = 'nuclear'
        if 'coal' in curve_name:
            color = col_t_coa
            short_name = 'coal'
        if 'gas' in curve_name:
            color = 'LightGreen'
            short_name = 'gas'
        if 'oil' in curve_name:
            color = col_t_oil
            short_name = 'oil'
        if 'lignite' in curve_name:
            color = col_t_lig
            short_name = 'lignite'
        if 'bio' in curve_name:
            color = col_t_bio
            short_name = 'biomass'
        if 'tot' in curve_name:
            color = col_h_tot
            short_name = 'tot'
        if 'pump' in curve_name:
            color = col_h_pum
            short_name = 'pump'
        if 'res' in curve_name:
            color = col_h_pum
            short_name = 'res'
        if 'chp' in curve_name:
            color = col_t_oil
            short_name = 'chp'
        if 'inf' in curve_name:
            color = 'blue'
            short_name = 'inf'
        if 'ror' in curve_name:
            color = 'CadetBlue'
            short_name = 'ror'
        
        data = plotly.graph_objs.Scatter(
            x = X,
            y = Y,
            name = short_name,
            mode = 'lines',
            line = dict(shape = 'linear', 
                        color = color, 
                        width = 1.4
                        )
            )
        data_list.append(data)
        
    return {'data': data_list, 'layout': go.Layout(GetLayout(x_range, y_range))}
        

def DisplayForecast(WS_CLIENT_ID, 
                    WS_CLIENT_SECRET, 
                    freq, 
                    func,
                    tz, 
                    no_df,
                    mo_df,
                    ec_curve_name, 
                    ec_ens_curve_name,
                    gf_curve_name,
                    gf_ens_curve_name):
    
    ws_session = wapi.Session(client_id = WS_CLIENT_ID, client_secret = WS_CLIENT_SECRET)

    no_short_name = 'normal'
    mo_short_name = 'monthly'
    
    ec_df, ec_name = GetLatestForecastSeries(ws_session, ec_curve_name, freq, func, tz)
    gf_df, gf_name = GetLatestForecastSeries(ws_session, gf_curve_name, freq, func, tz)
    ec_ens_df, ec_ens_name = GetLatestForecastSeries(ws_session, ec_ens_curve_name, freq, func, tz)
    gf_ens_df, gf_ens_name = GetLatestForecastSeries(ws_session, gf_ens_curve_name, freq, func, tz)
    
    if ec_name[:3] == 'con':
        ec_short = ' '.join(ec_name.split(' ')[2:3])
        gf_short = ' '.join(gf_name.split(' ')[2:3])
        ec_ens_short = ' '.join(ec_ens_name.split(' ')[2:3])
        gf_ens_short = ' '.join(gf_ens_name.split(' ')[2:3])
    else:
        ec_short = ' '.join(ec_name.split(' ')[3:4])
        gf_short = ' '.join(gf_name.split(' ')[3:4])
        ec_ens_short = ' '.join(ec_ens_name.split(' ')[3:4])
        gf_ens_short = ' '.join(gf_ens_name.split(' ')[3:4])

    #ec_times = ec_df['times'].to_list()
    
    # for time in ec_times:
    #     mo_df.loc[mo_df['times'] == time, 'values'] = float(ec_df.loc[ec_df['times'] == time, 'values'])
    
    data_list = []
    x_range = []
    y_range = []
    
    # ec
    X = ec_df['times'].to_list()
    Y = ec_df['values'].to_list()
    if len(X) > 2:
        x_range.append(min(X))
        x_range.append(max(X))
    if len(Y) > 2:
        y_range.append(min(Y))
        y_range.append(max(Y))  
    
    data = plotly.graph_objs.Scatter(
        x = X,
        y = Y,
        name = ec_short,
        mode = 'lines',
        line = dict(shape = 'linear', 
                    color = 'blue', 
                    width = 1.4
                    )
        )
    data_list.append(data)      
    

    # ec ens
    X = ec_ens_df['times'].to_list()
    Y = ec_ens_df['values'].to_list()
    
    if len(Y) > 2:
        y_range.append(min(Y))
        y_range.append(max(Y))  
    
    data = plotly.graph_objs.Scatter(
        x = X,
        y = Y,
        name = ec_ens_short,
        mode = 'lines',
        line = dict(shape = 'linear', 
                    color = 'blue', 
                    width = 1.4,
                    dash = 'dash'
                    )
        )
    
    data_list.append(data)
    
    # gf
    X = gf_df['times'].to_list()
    Y = gf_df['values'].to_list()
    if len(X) > 2:
        x_range.append(min(X))
        x_range.append(max(X))
    if len(Y) > 2:
        y_range.append(min(Y))
        y_range.append(max(Y))  
    
    data = plotly.graph_objs.Scatter(
        x = X,
        y = Y,
        name = gf_short,
        mode = 'lines',
        line = dict(shape = 'linear', 
                    color = 'deeppink', 
                    width = 1.4
                    )
        )
    data_list.append(data)  
    
    # gf ens
    X = gf_ens_df['times'].to_list()
    Y = gf_ens_df['values'].to_list()
    if len(Y) > 2:
        y_range.append(min(Y))
        y_range.append(max(Y))  

    data = plotly.graph_objs.Scatter(
        x = X,
        y = Y,
        name = gf_ens_short,
        mode = 'lines',
        line = dict(shape = 'linear', 
                    color = 'deeppink', 
                    width = 1.4,
                    dash = 'dash'
                    )
        )
    data_list.append(data)  
       
    
    # monthly
    mo_df = mo_df[mo_df['times'] >= min(x_range)]
    
    X = mo_df['times'].to_list()
    Y = mo_df['values'].to_list()
    
    
    if len(Y) > 2:
        y_range.append(min(Y))
        y_range.append(max(Y))
    
    data = plotly.graph_objs.Scatter(
        x = X,
        y = Y,
        name = mo_short_name,
        mode = 'lines',
        line = dict(shape = 'linear', 
                    color = 'green', 
                    width = 1.4
                    )
        )
    data_list.append(data)
    
    
    # normal
    no_df = no_df[no_df['times'] >= min(x_range)]
    
    X = no_df['times'].to_list()
    Y = no_df['values'].to_list()
    
    
    if len(Y) > 2:
        y_range.append(min(Y))
        y_range.append(max(Y))
    
    data = plotly.graph_objs.Scatter(
        x = X,
        y = Y,
        name = no_short_name,
        mode = 'lines',
        line = dict(shape = 'linear', 
                    color = 'orange', 
                    width = 1.2,
                    dash = 'dashdot'
                    )
        )
    data_list.append(data)
    
    return {'data': data_list, 'layout': go.Layout(GetLayout(x_range, y_range))}




def DisplayRelativeActuals(freq, 
                           func, 
                           tz, 
                           cc, 
                           offset,
                           duration,
                           ac_df,
                           ec_df,
                           ec_ens_df,
                           gf_df,
                           gf_ens_df,
                           actu_short,
                           ec_short,
                           ec_ens_short,
                           gf_short,
                           gf_ens_short):

    data_list = []
    y_range = []
    x_range = []
        

    # ec
    X = ec_df['times'].to_list()
    Y = ec_df['values'].to_list()
    if len(X) > 2:
        x_range.append(min(X))
    if len(Y) > 2:
        y_range.append(min(Y))
        y_range.append(max(Y))
    
    data = plotly.graph_objs.Scatter(
        x = X,
        y = Y,
        name = ec_short,
        mode = 'lines',
        line = dict(shape = 'linear', 
                    color = 'blue', 
                    width = 1.4
                    )
        )
    data_list.append(data)      
    

    # ec ens
    X = ec_ens_df['times'].to_list()
    Y = ec_ens_df['values'].to_list()
    if len(X) > 2:
        x_range.append(min(X))
    if len(Y) > 2:
        y_range.append(min(Y))
        y_range.append(max(Y))
        
    data = plotly.graph_objs.Scatter(
        x = X,
        y = Y,
        name = ec_ens_short,
        mode = 'lines',
        line = dict(shape = 'linear', 
                    color = 'blue', 
                    width = 1.4,
                    dash = 'dash'
                    )
        )
    data_list.append(data)      
    

    # gf
    X = gf_df['times'].to_list()
    Y = gf_df['values'].to_list()
    if len(X) > 2:
        x_range.append(min(X))
    if len(Y) > 2:
        y_range.append(min(Y))
        y_range.append(max(Y)) 
    
    data = plotly.graph_objs.Scatter(
        x = X,
        y = Y,
        name = gf_short,
        mode = 'lines',
        line = dict(shape = 'linear', 
                    color = 'deeppink', 
                    width = 1.4
                    )
        )
    data_list.append(data)  
    
    # gf ens
    X = gf_ens_df['times'].to_list()
    Y = gf_ens_df['values'].to_list()
    if len(X) > 2:
        x_range.append(min(X))
    if len(Y) > 2:
        y_range.append(min(Y))
        y_range.append(max(Y))
        
    data = plotly.graph_objs.Scatter(
        x = X,
        y = Y,
        name = gf_ens_short,
        mode = 'lines',
        line = dict(shape = 'linear', 
                    color = 'deeppink', 
                    width = 1.4,
                    dash = 'dash'
                    )
        )
    data_list.append(data) 


    # actuals
    X = ac_df['times'].to_list()
    Y = ac_df['values'].to_list()
    
    if len(X) > 2:
        x_range.append(max(X))
    if len(Y) > 2:
        y_range.append(min(Y))
        y_range.append(max(Y))  
        
    data = plotly.graph_objs.Scatter(
        x = X,
        y = Y,
        name = actu_short,
        mode = 'lines',
        line = dict(shape = 'linear', 
                    color = 'green', 
                    width = 1.4))
    
    data_list.append(data)
    
    
    return {'data': data_list, 'layout': go.Layout(GetLayout(x_range, y_range))}