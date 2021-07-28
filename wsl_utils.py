import re
import sys
import os
import itertools
import sqlite3
import json
import wapi
from xlsxwriter import exceptions as xe, Workbook
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import datetime as dt
from wapi.session import MetadataException
from wapi.auth import AuthFailedException 
from wapi.util import CurveException
import requests
import string
import ctypes
import time
import isodate

USER = os.getlogin()

secrets_json = f'C:\\Users\\{USER}\\WATTSIGHT\\JSON\\SECRETS.json'

with open(secrets_json, 'r', encoding='UTF-8') as fin:
    ws_secrets = json.load(fin)

# USERNAME = ws_secrets['USERNAME']
# PASSWORD = ws_secrets['PASSWORD']
# URL = ws_secrets['PROXY_URL']
# PORT = ws_secrets['PROXY_PORT']

WS_CLIENT_ID = ws_secrets['Wattsight']['CLIENT_ID']
WS_CLIENT_SECRET = ws_secrets['Wattsight']['CLIENT_SECRET']

ws_session = wapi.Session(client_id = WS_CLIENT_ID, client_secret = WS_CLIENT_SECRET)

# os.environ['http_proxy'] = f'http://{URL}:{PORT}'
# os.environ['https_proxy'] = f'https://{USERNAME}:{PASSWORD}@{URL}:{PORT}'
# try:

# except:
#     os.environ['https_proxy'] = ''
#     os.environ['http_proxy'] = ''


def GetNestedDictValues(d):
    for v in d.values():
        if isinstance(v, dict):
            yield from GetNestedDictValues(v)
        else:
            yield v

def CreateWsCurvesDb(db_path):
    
    db_path = os.path.abspath(db_path)
    
    conn = sqlite3.connect(db_path)
    conn.execute('PRAGMA encoding = "UTF-8"')
    conn.execute("PRAGMA journal_mode = WAL")
    conn.execute("PRAGMA busy_timeout = 8000")

    script = f"\'\'\'CREATE TABLE IF NOT EXISTS metadata \
                    (curve_id INTEGER NOT NULL, \
                    table_name TEXT NOT NULL, \
                    curve_name TEXT NOT NULL, \
                    curve_type TEXT NOT NULL, \
                    data_type TEXT NOT NULL, \
                    unit TEXT NOT NULL, \
                    frequency TEXT, \
                    function TEXT, \
                    issue_date TEXT, \
                    created TEXT, \
                    modified TEXT, \
                    data_from TEXT, \
                    data_to TEXT, \
                    data_size INTEGER, \
                    begin TEXT, \
                    end TEXT, \
                    PRIMARY KEY(curve_id, table_name));\'\'\'"
    conn.executescript(eval(script))
    conn.close()
    
    return True

def CreateHistoryDb(db_path):
    
    db_path = os.path.abspath(db_path)
    
    conn = sqlite3.connect(db_path)
    conn.execute('PRAGMA encoding = "UTF-8"')
    conn.execute("PRAGMA journal_mode = WAL")
    conn.execute("PRAGMA busy_timeout = 8000")

    script = f"\'\'\'CREATE TABLE IF NOT EXISTS metadata \
              (curve_id INTEGER NOT NULL, \
              table_name TEXT NOT NULL, \
              curve_name TEXT NOT NULL, \
              curve_type TEXT NOT NULL, \
              data_type TEXT NOT NULL, \
              unit TEXT NOT NULL, \
              frequency TEXT, \
              function TEXT, \
              data_from TEXT, \
              data_to TEXT, \
              data_size INTEGER, \
              PRIMARY KEY(curve_id, table_name));\'\'\'"
    conn.executescript(eval(script))
    conn.close()
    
    return True



def CreateHistoryTables(secrets, 
                        db_path, 
                        curve_names, 
                        data_from,
                        offset,
                        duration, 
                        freq = None, 
                        func = None, 
                        tz = None): 

    
    with open(secrets, 'r', encoding='UTF-8') as json_file:
        ws_secrets = json.load(json_file)
    
    WS_CLIENT_ID = ws_secrets['Wattsight']['CLIENT_ID']
    WS_CLIENT_SECRET = ws_secrets['Wattsight']['CLIENT_SECRET']
    
    ws_session = wapi.Session(client_id = WS_CLIENT_ID, client_secret = WS_CLIENT_SECRET)
    
    if isinstance(curve_names, dict):
        curve_names = list(GetNestedDictValues(curve_names)) 
    
    db_path = os.path.abspath(db_path)
    
    all_data_types = ['A', 'AF', 'N', 'F']
    time_zones = ['ART', 'CEGT', 'CET', 'EET', 'MSK', 'PST', 'TRT', 'UTC', 'WEGT', 'WET']
    functions = ['AVERAGE', 'DIVIDE', 'LAST', 'MAX', 'MIN', 'SAME', 'SUM']
    frequencies = ['D', 'H', 'H12', 'H3', 'H6', 'M', 'MIN', 'MIN15', 'MIN30', 'MIN5', 'Q', 'S', 'W', 'Y']
    
    if tz and tz not in time_zones:
        print(f'Invalid time zone [{tz}].\r\nMust be in {time_zones}\r\n')
        return False
        
    if func and func not in functions:
        print(f'Invalid function [{func}].\r\nMust be in {functions}\r\n')
        return False
        
    if freq and freq not in frequencies:
        print(f'Invalid frequency [{freq}].\r\nMust be in {frequencies}\r\n')
        return False
    
    table_names = [x.strip().replace(' ','_').replace('/', '').replace('>','').replace('-','') for x in curve_names]

    now = pd.Timestamp.now(tz = 'CET')
    today = now.floor('D')
    
    data_to = today + dt.timedelta(days = 1)
    issue_date_to = data_to
    
    conn = sqlite3.connect(db_path)
    
    for i in range(len(curve_names)):
        try:
            curve = ws_session.get_curve(name = curve_names[i])
            issue_date_from = str(pd.Timestamp(curve.accessRange['begin'], tz = tz).date())
            data_from = pd.Timestamp(curve.accessRange['begin'], tz = tz) + dt.timedelta(days = 1) 
        except CurveException:
            print(f'No access to [{curve_names[i]}]')
            continue
        except MetadataException:
            print(f'This curve was not found [{curve_names[i]}]')
            continue
        
        curve_id = int(curve.id)
        curve_name = curve.name
        table_name = table_names[i]
        data_type = curve.data_type
        curve_unit = str(curve.unit)
        curve_type = curve.curve_type
        
        if freq:
            frequency = freq
        else:
            frequency = curve.frequency
            
        if func:
            function = func
        else:
            function = 'None'
            

        if curve_type == 'TIME_SERIES' or curve_type == 'TAGGED':

            ts = curve.get_data(frequency = freq, 
                                function = func,
                                output_time_zone = tz,
                                data_from = data_from,
                                data_to = data_to)
            
        elif curve_type == 'INSTANCES' or curve_type == 'TAGGED_INSTANCES':
            
            ts = curve.get_relative(data_offset = offset,
                                    data_max_length = duration,
                                    issue_date_from = issue_date_from,
                                    issue_date_to = issue_date_to,
                                    frequency = freq, 
                                    function = func,
                                    output_time_zone = tz,
                                    data_from = data_from,
                                    data_to = data_to)
        
        else:
            print(f'Curve type [{data_type}] is not compatible! \r\nMust be in {all_data_types}\r\n')
            continue      
                            
        if ts and len(ts.points) > 2:
            s = ts.to_pandas()
            s = s.ffill().where(s.bfill().notnull())
            s = s.dropna()
            index0 = s.index.strftime('%Y-%m-%dT%H:%M:%S%z').to_list() 
            s.index = [f'{x[:-2]}:{x[-2:]}' for x in index0]
            
            data_from_d = min(s.index)
            data_to_d = max(s.index)
            data_size = len(s.index)
            
            data_from_d = pd.Timestamp(data_from_d, tz = tz).strftime('%Y-%m-%dT%H:%M:%S%z')
            data_from_d = f'{data_from_d[:-2]}:{data_from_d[-2:]}'   
            
            data_to_d = pd.Timestamp(data_to_d, tz = tz).strftime('%Y-%m-%dT%H:%M:%S%z')
            data_to_d = f'{data_to_d[:-2]}:{data_to_d[-2:]}' 
            
            new_list = list(zip(s.index.values.tolist(), s.values.tolist()))
            
            try:
                cur = conn.cursor()

                sql_comm = eval(f"\'\'\'CREATE TABLE IF NOT EXISTS '{table_name}' (times TEXT NOT NULL, 'values' REAL, PRIMARY KEY(times));\'\'\'") 
                cur.executescript(sql_comm)
                conn.commit()

                # replace or insert new values in curve's table
                sql_comm = eval(f"\'\'\'INSERT OR REPLACE INTO '{table_name}' VALUES (?, ?);\'\'\'")
                cur.executemany(sql_comm, new_list)
                conn.commit()

                # replace new values in metadata table
                sql_comm = eval(f"\'\'\'INSERT OR REPLACE INTO metadata(curve_id, table_name, curve_name, curve_type, data_type, unit, frequency, function, data_from, data_to, data_size) VALUES({curve_id}, '{table_name}', '{curve_name}', '{curve_type}', '{data_type}', '{curve_unit}', '{frequency}', '{function}', '{data_from_d}', '{data_to_d}', {data_size});\'\'\'")
                cur.executescript(sql_comm)
                conn.commit()

                cur.close()
                data_from_d = datetime.strftime(datetime.strptime(data_from_d, '%Y-%m-%dT%H:%M:%S%z'), '%Y-%m-%d %H:%M')
                data_to_d = datetime.strftime(datetime.strptime(data_to_d, '%Y-%m-%dT%H:%M:%S%z'), '%Y-%m-%d %H:%M')
                
                curve_name_short = " ".join(curve_name.split(" ")[:-4])
                event_time_short = datetime.strftime(now.time(), '%H:%M:%S')
               
                
                if len(f'{table_name}') < 24:
                    print(f"[{event_time_short}]  {curve_name_short}  {data_size}")                
                elif len(f'[{event_time_short}]  {table_name}') < 32:
                    print(f"[{event_time_short}]  {curve_name_short}  {data_size}")
                elif len(f'[{event_time_short}]  {table_name}') > 39:
                    print(f"[{event_time_short}]  {curve_name_short}  {data_size}")
                else:
                    print(f"{curve_name_short}  {data_size}")  
                
            except Exception as e:
                print(f'Something went wrong while filling {table_name}:\r\n{e}')
                if cur:
                    cur.close()
                if conn:
                    conn.close()
                return False
        else:
            print(f'[{curve_name}] is empty!!!')
            continue       
       
    conn.close()
    return True


def CreateWsCurvesTables(secrets, 
                         db_path, 
                         curve_names, 
                         freq = None, 
                         func = None, 
                         tz = None): 
    
    with open(secrets, 'r') as json_file:
        ws_secrets = json.load(json_file)
    
    WS_CLIENT_ID = ws_secrets['Wattsight']['CLIENT_ID']
    WS_CLIENT_SECRET = ws_secrets['Wattsight']['CLIENT_SECRET']
    
    ws_session = wapi.Session(client_id = WS_CLIENT_ID, 
                              client_secret = WS_CLIENT_SECRET)
    
    if isinstance(curve_names, dict):
        curve_names = list(GetNestedDictValues(curve_names)) 
    
    db_path = os.path.abspath(db_path)
    
    all_data_types = ['A', 'AF', 'N', 'F']
    time_zones = ['ART', 'CEGT', 'CET', 'EET', 'MSK', 'PST', 'TRT', 'UTC', 'WEGT', 'WET']
    functions = ['AVERAGE', 'DIVIDE', 'LAST', 'MAX', 'MIN', 'SAME', 'SUM']
    frequencies = ['D', 'H', 'H12', 'H3', 'H6', 'M', 'MIN', 'MIN15', 'MIN30', 'MIN5', 'Q', 'S', 'W', 'Y']
    
    if tz and tz not in time_zones:
        print(f'Invalid time zone [{tz}].\r\nMust be in {time_zones}\r\n')
        return False
        
    if func and func not in functions:
        print(f'Invalid function [{func}].\r\nMust be in {functions}\r\n')
        return False
        
    if freq and freq not in frequencies:
        print(f'Invalid frequency [{freq}].\r\nMust be in {frequencies}\r\n')
        return False
    
    table_names = [x.strip().replace(' ','_').replace('/', '').replace('>','').replace('-','') for x in curve_names]

    now = pd.Timestamp.now(tz = 'CET')
    today = now.floor('D')
    
    tomorrow = today + dt.timedelta(days = 1)
    tomorrow = tomorrow.date()
    
    data_from = datetime.now().date()
    data_to = data_from + dt.timedelta(days = 1095)
    if data_to.year - datetime.now().year > 3:
        data_to = data_to - dt.timedelta(days = 1)
        data_to = f'{data_to.year}-01-01'
    if data_to.year - datetime.now().year <= 2:
        data_to = data_to + dt.timedelta(days = 1)
        data_to = f'{data_to.year}-01-01'
    else:
        data_to = f'{data_to.year}-01-01'

    data_from = now.floor('H')
    data_to = pd.Timestamp(data_to)
    
    conn = sqlite3.connect(db_path)
    
    for i in range(len(curve_names)):
        try:
            curve = ws_session.get_curve(name = curve_names[i])
        except CurveException:
            print(f'No access to [{curve_names[i]}]')
            continue
        except MetadataException:
            print(f'This curve was not found [{curve_names[i]}]')
            continue
        
        curve_id = int(curve.id)
        curve_name = curve.name
        table_name = table_names[i]
        data_type = curve.data_type
        curve_unit = str(curve.unit)
        curve_type = curve.curve_type
        
        if freq:
            frequency = freq
        else:
            frequency = curve.frequency
            
        if func:
            function = func
        else:
            function = 'None'
            
        try:
            created = pd.Timestamp(curve.created).strftime('%Y-%m-%dT%H:%M:%S%z')
            created = f'{created[:-2]}:{created[-2:]}'
        except AttributeError:
            created = ''
            pass
        if not created:
            created = ''
            
        try:
            modified = pd.Timestamp(curve.modified).strftime('%Y-%m-%dT%H:%M:%S%z')
            modified = f'{modified[:-2]}:{modified[-2:]}'
        except AttributeError:
            created = ''
            pass
        if not created:
            created = ''

        begin = ''
        end = ''
        
        if curve_type == 'TIME_SERIES' or curve_type == 'TAGGED':

            ts = curve.get_data(frequency = freq, 
                                function = func,
                                output_time_zone = tz,
                                data_from = data_from,
                                data_to = data_to)
            issue_date = ''
            
        elif curve_type == 'INSTANCES' or curve_type == 'TAGGED_INSTANCES':
            
            latest = curve.get_latest(with_data = False,
                                      issue_date_to = tomorrow)
            
            issue_date = latest.issue_date

            ts = curve.get_instance(issue_date = issue_date,
                                    frequency = freq, 
                                    function = func,
                                    output_time_zone = tz,
                                    data_from = data_from,
                                    data_to = data_to) 
        
        else:
            print(f'Curve type [{data_type}] is not compatible! \r\nMust be in {all_data_types}\r\n')
            continue      
                            
        if ts and len(ts.points) > 2:
            s = ts.to_pandas()
            s = s.ffill().where(s.bfill().notnull())
            s = s.dropna()
            index0 = s.index.strftime('%Y-%m-%dT%H:%M:%S%z').to_list() 
            s.index = [f'{x[:-2]}:{x[-2:]}' for x in index0]
            
            data_from_d = min(s.index)
            data_to_d = max(s.index)
            data_size = len(s.index)
            
            data_from_d = pd.Timestamp(data_from_d, tz = tz).strftime('%Y-%m-%dT%H:%M:%S%z')
            data_from_d = f'{data_from_d[:-2]}:{data_from_d[-2:]}'   
            
            data_to_d = pd.Timestamp(data_to_d, tz = tz).strftime('%Y-%m-%dT%H:%M:%S%z')
            data_to_d = f'{data_to_d[:-2]}:{data_to_d[-2:]}' 
            
            new_list = list(zip(s.index.values.tolist(), s.values.tolist()))
            
            try:
                cur = conn.cursor()

                sql_comm = eval(f"\'\'\'CREATE TABLE IF NOT EXISTS '{table_name}' (times TEXT NOT NULL, 'values' REAL, PRIMARY KEY(times));\'\'\'") 
                cur.executescript(sql_comm)
                conn.commit()

                # replace or insert new values in curve's table
                sql_comm = eval(f"\'\'\'INSERT OR REPLACE INTO '{table_name}' VALUES (?, ?);\'\'\'")
                cur.executemany(sql_comm, new_list)
                conn.commit()

                # replace new values in metadata table
                sql_comm = eval(f"\'\'\'INSERT OR REPLACE INTO metadata(curve_id, table_name, curve_name, curve_type, data_type, unit, frequency, function, issue_date, created, modified, data_from, data_to, data_size, begin, end) VALUES({curve_id}, '{table_name}', '{curve_name}', '{curve_type}', '{data_type}', '{curve_unit}', '{frequency}', '{function}', '{issue_date}', '{created}', '{modified}', '{data_from_d}', '{data_to_d}', {data_size}, '{begin}', '{end}');\'\'\'")
                cur.executescript(sql_comm)
                conn.commit()

                cur.close()
                data_from_d = datetime.strftime(datetime.strptime(data_from_d, '%Y-%m-%dT%H:%M:%S%z'), '%m-%d %H:%M')
                data_to_d = datetime.strftime(datetime.strptime(data_to_d, '%Y-%m-%dT%H:%M:%S%z'), '%Y-%m-%d %H:%M')
                
                if len(f'{table_name}') < 24:
                    print(f"{curve_name}\t\t\t\t{data_size}")                
                elif len(f'{table_name}') < 32:
                    print(f"{curve_name}\t\t\t{data_size}")
                elif len(f'{table_name}') > 39:
                    print(f"{curve_name}\t{data_size}")
                else:
                    print(f"{curve_name}\t\t{data_size}")  
                
            except Exception as e:
                print(f'Something went wrong while filling {table_name}:\r\n{e}')
                if cur:
                    cur.close()
                if conn:
                    conn.close()
                return False
        else:
            print(f'[{curve_name}] is empty!!!')
            continue       
       
    conn.close()
    return True

def ExportDfToExcelSheets(list_df, list_sheetNames, path):
    if len(locals()) != 3:
        print('Not enough arguments! You must provide two lists and a filepath.')
        return
    
    if (not isinstance(list_df, list)) or (not isinstance(list_sheetNames, list)):
        print('Two first arguments must be lists!')
        return
    
    if len(list_df) != len(list_sheetNames):
        print(f'Both lists length are not equal! Got {len(list_df)} dataframes and {len(list_sheetNames)} sheet names.')
        return
    
    path = path.replace("/", "\\")
    path = os.path.abspath(path)
    filename = path.split('\\')[-1]
    
    if '.' not in path:
        print(f'[{path}] is not a valid file path!')
        return
    
    extensions = ['xls', 'xlsx']
    if path.split('.')[1] not in extensions:
        print(f'[{filename}is not an Excel file! Extension must be in {extensions}')
        return
    
    drives = ctypes.cdll.kernel32.GetLogicalDrives()
    drives_list =list(itertools.compress(string.ascii_uppercase, 
                                         map(lambda x:ord(x) - ord('0'), bin(drives)[:1:-1])))

    dir_path = path[:-len(filename)]
    if (len(dir_path) != 0) and (dir_path[0] not in drives_list):
        print(f'[{dir_path[0:3]}] is not a valid drive!')
        return

    if len(dir_path) != 0 and os.path.isdir(dir_path) == False:
        print(f'[{dir_path}] is not a valid directory path!')
        return
        
    wb = Workbook(path)
    bold_font = wb.add_format({'bold': True})
    
    for i in range(len(list_df)):
        if not isinstance(list_df[i], pd.core.frame.DataFrame):
            print(f'Not a list of pandas dataframes!')
            return
        
        idx_len = int(len(list_df[i].index))
        col_len = int(len(list_df[i].columns))
       
        sheet = wb.add_worksheet(list_sheetNames[i])
        
        sheet.set_row(0, None, bold_font)
        
        try:
            for col in list_df[i].columns:

                if not isinstance(col, str):
                    list_df[i].rename(columns={list_df[i].columns[j]: str(col)})

            for j in range(len(list_df[i].columns)):
                sheet.write_string(0, j, list_df[i].columns[j])
            
            for row in range(idx_len):
                for col in range(col_len):
                    if not isinstance(list_df[i].loc[row][col], str):
                        sheet.write_string(row+1, col, 'NA')
                    sheet.write_string(row+1, col, str(list_df[i].loc[row][col]))
                    
        except Exception as e:
            print(f'Something went wrong while filling cells with content: {e}')
            return
                
       
        max_col_sizes = [max([len(str(s)) for s in list_df[i][col].values] + [len(col)]) for col in list_df[i].columns]

        max_col_sizes[0] += 2
        
        for k, width in enumerate(max_col_sizes):
            sheet.set_column(k, k, width)
        
    try:
        wb.close()
    except xe.FileCreateError as e:
        
        if len(dir_path) == 0:
            dir_path = os.getcwd()
            dir_path = dir_path.replace("/", "\\")
        print(f'Permission Denied on [{dir_path}], or the Excel file is open.')
        
    except xe.FileSizeError as e:
        print('Filesize too big, require ZIP64 extensions. Use workbook.use_zip64().')
        return 

def GetWsEventsUrl(l):

    base_url = 'https://api.wattsight.com/api/events?'
    
    if isinstance(l, list):
        for i in l:
            if not isinstance(i, int):
                print(f'{i} is not an integer!')
                return
        
        for i in l:
            base_url = base_url + f'&id={i}' 
        return base_url
    
    elif isinstance(l, dict):

        if not all(isinstance(key, int) for key in l):
            print(f'Keys of dict are not all integer!')
            return
    
        for k in l.keys():
            base_url = base_url + f'&id={k}'
        
        return base_url
    
    else:
        if not isinstance(l, int):
            print(f'{l} is not an integer!')
            return
        
        base_url = base_url + f'&id={l}' 
        return base_url

def GetWsToken(client_id, client_secret):
    
    token_url = 'https://auth.wattsight.com/oauth2/token'
    client_id = client_id
    client_secret = client_secret
    data = {'grant_type': 'client_credentials'}
    
    ws_token = requests.post(token_url, 
                             data = data, 
                             allow_redirects = False, 
                             auth = (client_id, client_secret))
    
    tok = ws_token.json()['access_token']
    
    return tok

def GetDictAsDf(dic, colnames = None):
    
    if colnames and len(dic.keys()) != len(colnames):
        print(f"Length of dictionary ({len(dic.keys())}) doesn't match length of colnames ({len(colnames)})")
        return        
    
    df = pd.DataFrame.from_dict(dic, orient='columns')
    if colnames:
        df.columns = colnames
   
    return df

def ListDifference(l1, l2):
    return list(set(l1) - set(l2))
