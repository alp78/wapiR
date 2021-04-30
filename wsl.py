import sys
import os
import json
from multiprocessing import Manager, process, cpu_count
from concurrent.futures import ProcessPoolExecutor
from multiprocessing_logging import install_mp_handler
from datetime import datetime, timedelta
import datetime as dt
import time
import sseclient
import requests
import wapi
from wapi.session import MetadataException
from wapi.auth import AuthFailedException 
from wapi.util import CurveException
import pandas as pd
import isodate
import logging
import sqlite3
from wsl_utils import GetWsToken, GetWsEventsUrl
from wsl_utils import CreateWsCurvesDb, CreateWsCurvesTables, ListDifference
from wsl_utils import ExportDfToExcelSheets, GetDictAsDf, CreateHistoryDb, CreateHistoryTables

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
    ws_session = wapi.Session(client_id = WS_CLIENT_ID, client_secret = WS_CLIENT_SECRET)
except:
    os.environ['https_proxy'] = ''
    os.environ['http_proxy'] = ''

# create today's log file if not already exists (one per day)
log_today = str(pd.Timestamp.now(tz = "CET").date()).replace('-', '')
logfolder = f'C:\\Users\\{USER}\\WATTSIGHT\\WSLOGS'
if not os.path.exists(logfolder):
        os.makedirs(logfolder)      
logfile = f"{logfolder}\\{log_today}_WSLOG.txt"

# setup the logger
logger = logging.getLogger()
logger.setLevel(logging.INFO)

formatter = logging.Formatter('%(message)s')

ch = logging.StreamHandler()
ch.setLevel(logging.INFO)
ch.setFormatter(formatter)
logger.addHandler(ch)

fh = logging.FileHandler(logfile)
fh.setLevel(logging.INFO)
logger.addHandler(fh)

install_mp_handler()

# process each event individually (only if [modify], and in case of ensemble, only [Avg] tag)
def ProcessWsEvent(ws_session, 
                   cnt, 
                   batch_dic, 
                   conn, 
                   freq, 
                   func, 
                   curve_id, 
                   curve_name, 
                   issue_date, 
                   begin, 
                   end,
                   event_time,
                   tz,
                   series_event):

    try:
        curve = ws_session.get_curve(name = curve_name)

        begin_from = pd.Timestamp(begin, tz = tz)
        end_to = pd.Timestamp(end, tz = tz) + dt.timedelta(hours=1)

        ts = curve.get_instance(issue_date = issue_date, 
                                data_from = begin_from, 
                                data_to = end_to, 
                                frequency = freq, 
                                function = func, 
                                output_time_zone = tz)
        
        issue_date = ts.issue_date

        s = ts.to_pandas()
        s = s.ffill().where(s.bfill().notnull())
        s = s.dropna()
        index0 = s.index.strftime('%Y-%m-%dT%H:%M:%S%z').to_list() 
        s.index = [f'{x[:-2]}:{x[-2:]}' for x in index0]
        
        new_list = list(zip(s.index.values.tolist(), s.values.tolist()))
        
        table_name = curve_name.strip().replace(' ','_').replace('/','').replace('>','').replace('-', '')
        curve_name = ts.name
        curve_type = curve.curve_type
        curve_unit = curve.unit
        data_type = curve.data_type
        
        if freq:
            frequency = freq
        else:
            frequency = curve.frequency
        if func:
            function = func
        else:
            function = 'None'
        
        created = pd.Timestamp(ts.created).strftime('%Y-%m-%dT%H:%M:%S%z')
        created = f'{created[:-2]}:{created[-2:]}'
        
        modified = pd.Timestamp(ts.modified).strftime('%Y-%m-%dT%H:%M:%S%z')
        modified = f'{modified[:-2]}:{modified[-2:]}'
           
        begin = pd.Timestamp(begin).strftime('%Y-%m-%dT%H:%M:%S%z')
        begin = f'{begin[:-2]}:{begin[-2:]}'
        begin_d = datetime.strftime(datetime.strptime(begin, '%Y-%m-%dT%H:%M:%S%z'), '%m-%d %H:%M')
        
        end = pd.Timestamp(end).strftime('%Y-%m-%dT%H:%M:%S%z')
        end = f'{end[:-2]}:{end[-2:]}'
        end_d = datetime.strftime(datetime.strptime(end, '%Y-%m-%dT%H:%M:%S%z'), '%m-%d %H:%M')
        
        ev_size = len(s.index)

        
    except Exception as e:
        logger.info(f'Formatting error while processing event!!!!: {e}')
    
    with conn:
        sql_comm = eval(f"\'\'\'INSERT OR REPLACE INTO '{table_name}' VALUES (?, ?);\'\'\'")
        conn.executemany(sql_comm, new_list)
        
        cur = conn.cursor()
        query = f"SELECT times from '{table_name}' ORDER BY times DESC LIMIT 1"
        cur.execute(query)
        data_from_d = cur.fetchone()[0]
    
        query = f"SELECT times from '{table_name}' ORDER BY times ASC LIMIT 1"
        cur.execute(query)
        data_to_d = cur.fetchone()[0]  
        
        query = f"SELECT COUNT(times) from '{table_name}'"
        cur.execute(query)
        data_size = cur.fetchone()[0]  
        cur.close()
        
        sql_comm = eval(f"\'\'\'INSERT OR REPLACE INTO metadata(curve_id, table_name, curve_name, curve_type, data_type, unit, frequency, function, issue_date, created, modified, data_from, data_to, data_size, begin, end) VALUES({curve_id}, '{table_name}', '{curve_name}', '{curve_type}', '{data_type}', '{curve_unit}', '{frequency}', '{function}', '{issue_date}', '{created}', '{modified}', '{data_from_d}', '{data_to_d}', {data_size}, '{begin}', '{end}');\'\'\'")
        conn.executescript(sql_comm)
        
    if series_event == False:
        if len(f'{table_name}') < 32:
            logger.info(f"\t{event_time} - {curve_id} \t{table_name}\t\t\t{begin_d} >>> {end_d} - {ev_size}\r\n")
        elif len(f'{table_name}') > 37:
            logger.info(f"\t{event_time} - {curve_id} \t{table_name}\t{begin_d} >>> {end_d} - {ev_size}\r\n")
        else:
            logger.info(f"\t{event_time} - {curve_id} \t{table_name}\t\t{begin_d} >>> {end_d} - {ev_size}\r\n")


def GoMultiListenWsForecast(dic_q, 
                            db_path, 
                            timeout, 
                            token_start,
                            process_start, 
                            tok, 
                            WS_CLIENT_ID, 
                            WS_CLIENT_SECRET, 
                            func,
                            tz):
    
    ws_session = wapi.Session(client_id = WS_CLIENT_ID, 
                              client_secret = WS_CLIENT_SECRET)
    
    proc = process.current_process()
    
    batch_dic = dic_q.get(True, 0.05)

    run = 1
    
    batch_key = list(batch_dic.keys())[0]
    
    cat = ' '.join(batch_key.split('_')[:2])
    freq = batch_key[-1]
    batch_dic = batch_dic[batch_key]

    batch_url = GetWsEventsUrl(l = batch_dic)

    conn = sqlite3.connect(db_path, check_same_thread = False)
    
    while True:

        if int(time.time()) - process_start > timeout:
            break
        
        if int(time.time()) - token_start > 2700:
            try:
                ws_session = wapi.Session(client_id = WS_CLIENT_ID, 
                                          client_secret = WS_CLIENT_SECRET)
            except AuthFailedException as e:
                logger.info(f'Cannot refresh wapi session: {e}')
                sys.exit(1)
            try:
                tok = GetWsToken(client_id = WS_CLIENT_ID, 
                                 client_secret = WS_CLIENT_SECRET)
                
                token_start = time.time()
            except KeyError as e:
                logger.info(f'Cannot refresh access token: {e}')
                sys.exit(1)
                
            token_refresh_time = str(datetime.fromtimestamp(time.time()).strftime('%m-%d %H:%M'))
            logger.info(f'\r\n\t!!!!! [{token_refresh_time}] Token refreshed !!!!!\r\n')

        header = {'Authorization': 'Bearer ' + tok}
        
        response = requests.get(batch_url, headers = header, stream = True)
        client = sseclient.SSEClient(response)
        
        start_listen = str(datetime.fromtimestamp(time.time()).strftime('%m-%d %H:%M'))
        logger.info(f"\r\n\t###############\t\t[{start_listen}] RUN {run}\t\t[{cat}]\t ###############\r\n")

        ev_count = 0
        event_list = []
        event_dict_list = []
        cnt_list = []
        ev_len = 0
        series_event = False
        for event in client.events():
            
            cnt = json.loads(event.data)

            if 'tag' in cnt and cnt['tag'] != 'Avg':
                continue
            
            event_time = str(datetime.fromtimestamp(time.time()).strftime('%m-%d %H:%M'))

            evdict = str(event.__dict__).replace('"', "'").replace("'", '"'). replace('"{', '{').replace('}"', '}').replace(' ', '').replace('None', '"None"')
            evdict = json.loads(evdict)

            event_type = evdict['data']['operation']

            if event_type == 'modify': 
                ev_count +=1
                curve_id = int(evdict['data']['id'])
                
                begin = evdict['data']['range']['begin']
                end = evdict['data']['range']['end']
                
                event_str = f'{str(curve_id)}_{begin}_{end}'
                event_str = event_str.replace(':', '').replace('/', '').replace(' ', '_').replace('-', '').replace('T', '').replace('Z','').replace('+','')
                ev_len = len(event_str)

                event_str = event_str  + f'{ev_count}'
                
                if event_str not in event_list:
                    event_list.append(event_str)
                    event_dict_list.append(evdict)
                    cnt_list.append(cnt)
                    try:
                        if event_str[:ev_len] == event_list[-2][:ev_len]:
                            series_event = True
                            continue
                        else:
                            if series_event == True:
                                if ev_count % 2 == 0:
                                    time.sleep(1)
                                    
                                cnt = cnt_list[-1]
                                evdict = event_dict_list[-1]
                                curve_id = int(evdict['data']['id'])
                                
                                begin = evdict['data']['range']['begin']
                                end = evdict['data']['range']['end']
                                curve_name = batch_dic[curve_id]
                                issue_date = evdict['data']['issue_date']

                                ProcessWsEvent(ws_session = ws_session, 
                                               cnt = cnt, 
                                               batch_dic = batch_dic, 
                                               conn = conn, 
                                               freq = freq, 
                                               func = func, 
                                               curve_id = curve_id, 
                                               curve_name = curve_name, 
                                               issue_date = issue_date, 
                                               begin = begin, 
                                               end = end,
                                               event_time = event_time,
                                               tz = tz,
                                               series_event = series_event)
                                
                                series_event = False
                                
                            else:
                                if ev_count % 2 == 0:
                                    time.sleep(1)
                    
                                curve_name = batch_dic[curve_id]
                                issue_date = evdict['data']['issue_date']

                                ProcessWsEvent(ws_session = ws_session, 
                                               cnt = cnt, 
                                               batch_dic = batch_dic, 
                                               conn = conn, 
                                               freq = freq, 
                                               func = func, 
                                               curve_id = curve_id, 
                                               curve_name = curve_name, 
                                               issue_date = issue_date, 
                                               begin = begin, 
                                               end = end,
                                               event_time = event_time,
                                               tz = tz,
                                               series_event = series_event)  
                
                    except IndexError as e:
                        if ev_count % 2 == 0:
                            time.sleep(1)
            
                        curve_name = batch_dic[curve_id]
                        issue_date = evdict['data']['issue_date']

                        ProcessWsEvent(ws_session = ws_session, 
                                       cnt = cnt, 
                                       batch_dic = batch_dic, 
                                       conn = conn, 
                                       freq = freq, 
                                       func = func, 
                                       curve_id = curve_id, 
                                       curve_name = curve_name, 
                                       issue_date = issue_date, 
                                       begin = begin, 
                                       end = end,
                                       event_time = event_time,
                                       tz = tz,
                                       series_event = series_event)  
            else:
                curve_name = batch_dic[curve_id]
                issue_date_d = datetime.strftime(datetime.strptime(evdict['data']['issue_date'], '%Y-%m-%dT%H:%M:%S%z'), '%Y-%m-%d')
                table_name = curve_name.strip().replace(' ','_').replace('/','').replace('>','').replace('-', '')

                if len(f'{table_name}') < 24:
                    logger.info(f"\t[{event_time}] - {curve_id} \t{table_name}\t\t\t[{issue_date_d}] !!! [{event_type}] !!!\r\n")             
                elif len(f'{table_name}') < 32:
                    logger.info(f"\t[{event_time}] - {curve_id} \t{table_name}\t\t\t[{issue_date_d}] !!! [{event_type}] !!!\r\n")
                elif len(f'{table_name}') > 39:
                    logger.info(f"\t[{event_time}] - {curve_id} \t{table_name}\t[{issue_date_d}] !!! [{event_type}] !!!\r\n")
                else:
                    logger.info(f"\t[{event_time}] - {curve_id} \t{table_name}\t\t[{issue_date_d}] !!! [{event_type}] !!!\r\n")
         
        run+=1
    proc.terminate()
    return


def main(timeout, func, country_code, tz, watt_start, offset, duration):
    
    USER = os.getlogin()

    db_dir = f'C:\\Users\\{USER}\\WATTSIGHT\\SQLDB'
    db_name = f'{country_code}_FOR.db'
    db_path = os.path.join(db_dir, db_name)
    db_his_name = f'{country_code}_HIS.db'
    db_his_path = os.path.join(db_dir, db_his_name)
    
    secrets = f'C:\\Users\\{USER}\\WATTSIGHT\\JSON\\SECRETS.json'

    try:
        with open(secrets) as json_file:
            my_secrets = json.load(json_file)
            WS_CLIENT_ID = my_secrets['Wattsight']['CLIENT_ID']
            WS_CLIENT_SECRET = my_secrets['Wattsight']['CLIENT_SECRET']
    except Exception as e:
        logger.info(f'Error while retrieving secrets from [{secrets}]: {e}\r\n')
        sys.exit(1)

    try:
        ws_session = wapi.Session(client_id = WS_CLIENT_ID, client_secret = WS_CLIENT_SECRET)
    except AuthFailedException as e:
        logger.info(f'Error while connecting to Wattisght API: {e}\r\n')
        sys.exit(1)


    logger.info(f'\tUser:     \t{USER}')
    logger.info(f'\tCountry:   \t{country_code}')
    logger.info(f'\tDatabase: \t{db_path}')
    logger.info(f'\tSecrets:  \t{secrets}')
    logger.info(f'\tTimeout:  \t{str(dt.timedelta(seconds=timeout))}\r\n')

    
    if not input("\tContinue ? (y/n):\r\n\r\n").lower().strip()[:1] == "y": sys.exit(0)


    ### initialize HISTORIC curves
    his_json = (f'C:\\Users\\{USER}\\WATTSIGHT\\JSON\\{country_code}_HIS.json')
       
    if not os.path.isfile(his_json): 
        logger.info(f'Curves json file missing: [{his_json}]\r\n')
        sys.exit(0)
        
    with open(his_json, 'r', encoding='UTF-8') as fin:
        jdata = json.load(fin)
        
    if not input(f"\tUpdate HISTORIC curves with latest data ? (y/n):\r\n\r\n").lower().strip()[:1] == "n":
        
        now = pd.Timestamp.now(tz = tz)
        today = now.floor('D')
        comm = f"SELECT MIN(data_to) FROM metadata;"
        conn = sqlite3.connect(db_his_path)
        cur = conn.cursor()
        cur.execute(comm)
        data_from = cur.fetchone()[0]

        data_from = pd.Timestamp(data_from, tz = tz)
        data_to = today + dt.timedelta(days = 1)
        
        issue_date_from = data_from - dt.timedelta(days = 1)
        
        issue_date_from = issue_date_from.date()
        issue_date_to = data_to.date()
        
        for key in jdata.keys():
            freq = key[-1]
            logger.info(f'\t \r\n')
            logger.info(f'\t{key} \r\n')
            for v in jdata[key].values():
                
                curve_name = v
                time.sleep(1)
                if curve_name[-1] != 'n':
                    try:
                        curve = ws_session.get_curve(name = curve_name)
                        
                    except CurveException:
                        logger.info(f'No access to [{v}]')
                        continue
                    except MetadataException:
                        logger.info(f'This curve was not found [{v}]')
                        continue
                
                    curve_type = curve.curve_type
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

                    table_name = curve_name.strip().replace(' ','_').replace('/','').replace('>','').replace('-', '')
                    curve_id = int(curve.id)
                    
                    if ts and len(ts.points) > 2:
                        
                        s = ts.to_pandas()
                        s = s.ffill().where(s.bfill().notnull())
                        s = s.dropna()
                        index0 = s.index.strftime('%Y-%m-%dT%H:%M:%S%z').to_list() 
                        s.index = [f'{x[:-2]}:{x[-2:]}' for x in index0]
                        
                        
                        curve_unit = curve.unit
                        data_type = curve.data_type

                        if freq:
                            frequency = freq
                        else:
                            frequency = curve.frequency
                        if func:
                            function = func
                        else:
                            function = ''
                        
                        data_from_d = min(s.index)
                        data_to_d = max(s.index)

                        
                        data_from_d = pd.Timestamp(data_from_d, tz = tz).strftime('%Y-%m-%dT%H:%M:%S%z')
                        data_from_d = f'{data_from_d[:-2]}:{data_from_d[-2:]}'   
                        
                        data_to_d = pd.Timestamp(data_to_d, tz = tz).strftime('%Y-%m-%dT%H:%M:%S%z')
                        data_to_d = f'{data_to_d[:-2]}:{data_to_d[-2:]}' 
                        
                        new_list = list(zip(s.index.values.tolist(), s.values.tolist()))
                        
                        try:
                            # replace or insert new values in curve's table
                            sql_comm = eval(f"\'\'\'INSERT OR REPLACE INTO '{table_name}' VALUES (?, ?);\'\'\'")
                            cur.executemany(sql_comm, new_list)
                            conn.commit()

                            comm = f"SELECT COUNT(times) FROM '{table_name}';"
                            cur.execute(comm)
                            data_size = cur.fetchone()[0]

                            # replace new values in metadata table
                            sql_comm = eval(f"\'\'\'INSERT OR REPLACE INTO metadata(curve_id, table_name, curve_name, curve_type, data_type, unit, frequency, function, data_from, data_to, data_size) VALUES({curve_id}, '{table_name}', '{curve_name}', '{curve_type}', '{data_type}', '{curve_unit}', '{frequency}', '{function}', '{data_from_d}', '{data_to_d}', {data_size});\'\'\'")
                            cur.executescript(sql_comm)
                            conn.commit()

                            data_size = len(s.index)

                            data_from_d = datetime.strftime(datetime.strptime(data_from_d, '%Y-%m-%dT%H:%M:%S%z'), '%Y-%m-%d %H:%M')
                            data_to_d = datetime.strftime(datetime.strptime(data_to_d, '%Y-%m-%dT%H:%M:%S%z'), '%Y-%m-%d %H:%M')
                            
                            if len(f'{table_name}') < 24:
                                logger.info(f"\t{curve_id} \t{table_name}\t\t\t\t{data_from_d} >>> {data_to_d} - {data_size}")                
                            elif len(f'{table_name}') < 32:
                                logger.info(f"\t{curve_id} \t{table_name}\t\t\t{data_from_d} >>> {data_to_d} - {data_size}")
                            elif len(f'{table_name}') > 39:
                                logger.info(f"\t{curve_id} \t{table_name}\t{data_from_d} >>> {data_to_d} - {data_size}")
                            else:
                                logger.info(f"\t{curve_id} \t{table_name}\t\t{data_from_d} >>> {data_to_d} - {data_size}")  
                            
                        except Exception as e:
                            logger.info(f'Something went wrong while filling {table_name}:\r\n{e}')
                            if cur:
                                cur.close()
                            if conn:
                                conn.close()
                            return False
                    else:

                        comm = f"SELECT COUNT(times) FROM '{table_name}';"
                        cur.execute(comm)
                        data_size = cur.fetchone()[0]
 
                        comm = f"SELECT MIN(times) FROM '{table_name}';"
                        cur.execute(comm)
                        data_from_d = cur.fetchone()[0]                       

                        comm = f"SELECT MAX(times) FROM '{table_name}';"
                        cur.execute(comm)
                        data_to_d = cur.fetchone()[0]   
                        
                        data_from_d = datetime.strftime(datetime.strptime(data_from_d, '%Y-%m-%dT%H:%M:%S%z'), '%Y-%m-%d %H:%M')
                        data_to_d = datetime.strftime(datetime.strptime(data_to_d, '%Y-%m-%dT%H:%M:%S%z'), '%Y-%m-%d %H:%M')                  
                        
                        if len(f'{table_name}') < 24:
                            logger.info(f"\t{curve_id} \t{table_name}\t\t\t\tUP TO DATE - {data_from_d} >>> {data_to_d} - {data_size}")                
                        elif len(f'{table_name}') < 32:
                            logger.info(f"\t{curve_id} \t{table_name}\t\t\tUP TO DATE - {data_from_d} >>> {data_to_d} - {data_size}")
                        elif len(f'{table_name}') > 39:
                            logger.info(f"\t{curve_id} \t{table_name}\tUP TO DATE - {data_from_d} >>> {data_to_d} - {data_size}")
                        else:
                            logger.info(f"\t{curve_id} \t{table_name}\t\tUP TO DATE - {data_from_d} >>> {data_to_d} - {data_size}")  
                        continue
        cur.close()              
        conn.close()
        logger.info('\r\n')
         
        conn = sqlite3.connect(db_his_path, isolation_level = None)
        conn.execute("VACUUM;")
        conn.close()      



    ### initialize FORECAST curves
    
    listen_json = (f'C:\\Users\\{USER}\\WATTSIGHT\\JSON\\{country_code}_FOR.json')
       
    if not os.path.isfile(listen_json): 
        logger.info(f'Curves json file missing: [{listen_json}]\r\n')
        sys.exit(0)
        
    with open(listen_json, 'r', encoding='UTF-8') as fin:
        jdata = json.load(fin)
        
    if not input(f"\tInitialize FORECAST curves with latest data ? (y/n):\r\n\r\n").lower().strip()[:1] == "n":
        
        now = pd.Timestamp.now(tz = 'CET')
        today = now.floor('D')
        data_from = now.floor('H')
        
        data_from_d = pd.Timestamp(data_from).strftime('%Y-%m-%dT%H:%M:%S%z')
        data_from_d = f'{data_from_d[:-2]}:{data_from_d[-2:]}'   
        
        tomorrow = today + dt.timedelta(days = 1)
        tomorrow = tomorrow.date()
        
        data_to = data_from + dt.timedelta(days = 1095)
        
        if data_to.year - datetime.now().year > 3:
            data_to = data_to - dt.timedelta(days = 1)
            data_to = f'{data_to.year}-01-01'
        if data_to.year - datetime.now().year <= 2:
            data_to = data_to + dt.timedelta(days = 1)
            data_to = f'{data_to.year}-01-01'
        else:
            data_to = f'{data_to.year}-01-01'
        
        data_from = pd.Timestamp(data_from)
        data_to = pd.Timestamp(data_to)
        
        conn = sqlite3.connect(db_path)
        
        for key in jdata.keys():
            freq = key[-1]
            logger.info(f'\t \r\n')
            logger.info(f'\t{key} \r\n')
            for v in jdata[key].values():
                
                curve_name = v
                time.sleep(1)
                if curve_name[-1] != 'n':
                    try:
                        curve = ws_session.get_curve(name = curve_name)
                        
                    except CurveException:
                        logger.info(f'No access to [{v}]')
                        continue
                    except MetadataException:
                        logger.info(f'This curve was not found [{v}]')
                        continue
                    
                    latest = curve.get_latest(with_data = False,
                                              issue_date_to = tomorrow)
                    
                    issue_date = latest.issue_date
                    
                    ts = curve.get_instance(issue_date = issue_date,
                                            frequency = freq, 
                                            function = func,
                                            time_zone = tz,
                                            data_from = data_from,
                                            data_to = data_to)
                    
                    while True:
                        
                        if not ts or (ts and len(ts.points) <= 5):
                            
                            issue_date = pd.Timestamp(issue_date) - dt.timedelta(days = 1)
                            issue_date = issue_date.date()
                            ts = curve.get_instance(issue_date = issue_date,
                                                    frequency = freq, 
                                                    function = func,
                                                    time_zone = tz,
                                                    data_from = data_from,
                                                    data_to = data_to)
                        else:
                            break

                    if ts and len(ts.points) > 2:
                        
                        s = ts.to_pandas()
                        s = s.ffill().where(s.bfill().notnull())
                        s = s.dropna()
                        
                        index0 = s.index.strftime('%Y-%m-%dT%H:%M:%S%z').to_list() 
                        s.index = [f'{x[:-2]}:{x[-2:]}' for x in index0]
                        data_to_d = max(s.index)
                        data_size = len(s.index)
                    
                        data_to_d = pd.Timestamp(data_to_d).strftime('%Y-%m-%dT%H:%M:%S%z')
                        data_to_d = f'{data_to_d[:-2]}:{data_to_d[-2:]}'
                        
                        new_list = list(zip(s.index.values.tolist(), s.values.tolist()))

                        table_name = curve_name.strip().replace(' ','_').replace('/','').replace('>','').replace('-', '')
                        curve_type = curve.curve_type
                        curve_id = int(curve.id)
                        curve_unit = curve.unit
                        data_type = curve.data_type

                        if freq:
                            frequency = freq
                        else:
                            frequency = curve.frequency
                        if func:
                            function = func
                        else:
                            function = ''

                        created = pd.Timestamp(ts.created).strftime('%Y-%m-%dT%H:%M:%S%z')
                        created = f'{created[:-2]}:{created[-2:]}'

                        modified = pd.Timestamp(ts.modified).strftime('%Y-%m-%dT%H:%M:%S%z')
                        modified = f'{modified[:-2]}:{modified[-2:]}'
                        begin = ''
                        end = ''

                        try:
                            cur = conn.cursor()
                            comm = f"SELECT name FROM sqlite_master WHERE type='table' AND name = '{table_name}';"
                            cur.execute(comm)
                            res = cur.fetchall()
                            
                            if len(res) > 0:
                                
                                comm = f"DELETE FROM '{table_name}';"                                                 
                                cur.execute(comm)
                                conn.commit()
                                
                                sql_comm = eval(f"\'\'\'INSERT OR REPLACE INTO '{table_name}' VALUES (?, ?);\'\'\'")
                                cur.executemany(sql_comm, new_list)
                                conn.commit()

                                sql_comm = eval(f"\'\'\'INSERT OR REPLACE INTO metadata(curve_id, table_name, curve_name, curve_type, data_type, unit, frequency, function, issue_date, created, modified, data_from, data_to, data_size, begin, end) VALUES({curve_id}, '{table_name}', '{curve_name}', '{curve_type}', '{data_type}', '{curve_unit}', '{frequency}', '{function}', '{issue_date}', '{created}', '{modified}', '{data_from_d}', '{data_to_d}', {data_size}, '{begin}', '{end}');\'\'\'")
                                cur.executescript(sql_comm)
                                conn.commit()
                                
                            cur.close()
                        except Exception as e:
                            logger.info(f'Something went wrong while filling {table_name}:\r\n{e}')
                            if cur:
                                cur.close()
                            continue
                        
                        data_from_dd = datetime.strftime(datetime.strptime(data_from_d, '%Y-%m-%dT%H:%M:%S%z'), '%m-%d %H:%M')
                        data_to_d = datetime.strftime(datetime.strptime(data_to_d, '%Y-%m-%dT%H:%M:%S%z'), '%Y-%m-%d %H:%M')
                        
                        if len(f'{table_name}') < 24:
                            logger.info(f"\t{curve_id} \t{table_name}\t\t\t\t{data_from_dd} >>> {data_to_d} - {data_size}")                
                        elif len(f'{table_name}') < 32:
                            logger.info(f"\t{curve_id} \t{table_name}\t\t\t{data_from_dd} >>> {data_to_d} - {data_size}")
                        elif len(f'{table_name}') > 39:
                            logger.info(f"\t{curve_id} \t{table_name}\t{data_from_dd} >>> {data_to_d} - {data_size}")
                        else:
                            logger.info(f"\t{curve_id} \t{table_name}\t\t{data_from_dd} >>> {data_to_d} - {data_size}")  
   
                    else:
                        logger.info(f'\t{curve_name} is empty !!\r\n')
                else:
                    cur = conn.cursor()
                    table_name = curve_name.strip().replace(' ','_').replace('/','').replace('>','').replace('-', '')
                    comm = f"DELETE FROM '{table_name}' WHERE times < '{data_from_d}';"
                    cur.execute(comm)
                    conn.commit()
                    cur.close()
    
        conn.close()
        logger.info('\r\n')
         
        conn = sqlite3.connect(db_path, isolation_level = None)
        conn.execute("VACUUM;")
        conn.close()         
    
    logger.info(f'\r\n\t!!!! Press [Ctrl+Break] to interrupt !!!!\r\n\r\n')
     
    dic_list = []
    for k in jdata.keys():
        dic_f = jdata[k]
        dic_f = {int(k):v for k,v in dic_f.items() if 'ecmonthly' not in v and v[-1] != 'n'}
        dic_f = {k : dic_f} 
        dic_list.append(dic_f)
    
    manager = Manager()
    dic_q = manager.Queue()
    number_of_cpus = cpu_count()  
    
    for dic in dic_list:
        dic_q.put(dic)
    
    try:
        tok = GetWsToken(WS_CLIENT_ID, WS_CLIENT_SECRET)

        token_start = int(time.time())
    except KeyError as e:
        logger.info(f'Error while retrieving access token: {e}\r\n')
        sys.exit(1)
    
    process_start = int(time.time())
    
    with ProcessPoolExecutor(max_workers = number_of_cpus) as go_listen:
        for _ in range(dic_q.qsize()):
            time.sleep(1)
            
            go_listen.submit(GoMultiListenWsForecast,
                             dic_q = dic_q,
                             db_path = db_path,
                             timeout = timeout,
                             process_start = process_start,
                             token_start = token_start,
                             tok = tok,
                             WS_CLIENT_ID = WS_CLIENT_ID,
                             WS_CLIENT_SECRET = WS_CLIENT_SECRET,
                             func = func,
                             tz = tz)
        
        go_listen.shutdown(wait = True)
        
    total_time = int(time.time()) - process_start    
    logger.info(f'\r\n\r\n\t!!!! TIMEOUT {str(dt.timedelta(seconds=total_time))} !!!!!\r\n\r\n')                
    sys.exit(0)


if __name__ == '__main__':

    USER = os.getlogin()
    country_codes = ['DE', 'FR', 'CH', 'BE', 'NL', 'ES', 'IT', 'AT', 'DK', 'NO', 'SE', 'UK', 'CZ']
    countries = ['Germany', 'France', 'Switzerland', 'Belgium', 'Netherlands', 'Spain', 'Italy', 'Austria', 'Denmark', 'Norway', 'Sweden', 'UK', 'Czechia']
    
    func = 'AVERAGE'
    tz = 'CET'
    data_from_watt = '2016-10-01'
    data_from_watt = pd.Timestamp(data_from_watt, tz = 'CET')
    
    offset = 24
    duration = 24*7
    
    iso_offset = isodate.duration_isoformat(dt.timedelta(hours = offset))
    iso_duration = isodate.duration_isoformat(dt.timedelta(hours = duration))

    if len(sys.argv) != 3:
        logger.info(f'\r\n\t!! Country code in {country_codes} and Timeout in [seconds] must be provided !!\r\n\r\n\t')
        sys.exit(0)  
    
    timeout = sys.argv[2]

    if not isinstance(timeout, int):
        try:
            timeout = int(timeout)   
        except Exception as e:
            logger.info(f'\r\n\t!! Timeout must be indicated as integer (seconds) !!\r\n')
            sys.exit(0)
    
    country_code = sys.argv[1]
    
    if not isinstance(country_code, str) or len(country_code) != 2:
        
        logger.info(f'\r\n\t!! Invalid country code [{country_code}] invalid\r\n\tMust be in [{country_codes}] !!\r\n')
        sys.exit(0)
        
    country_code = country_code.upper()
    
    if country_code not in country_codes:
        
        logger.info(f'\r\n\t!! Invalid country code [{country_code}] invalid\r\n\tMust be in [{country_codes}] !!\r\n')
        sys.exit(0)
        
    secrets = f'C:\\Users\\{USER}\\WATTSIGHT\\JSON\\SECRETS.json'

    logger.info('\r\n')
    
    db_dir = f'C:\\Users\\{USER}\\WATTSIGHT\\SQLDB'
    db_name = f'{country_code}_FOR.db'
    db_path = os.path.join(db_dir, db_name)
    db_path = os.path.abspath(db_path)
    
    db_his_name = f'{country_code}_HIS.db'
    db_his_path = os.path.join(db_dir, db_his_name)
    db_his_path = os.path.abspath(db_his_path)
    
    
    
    #### HISTORY DB
    db_his_created = False
    
    if not os.path.isfile(f'{db_his_path}'):  
        idx = country_codes.index(country_code)
        if not input(f"\tCreate new HISTORIC database for {countries[idx]} ? (y/n):\r\n\r\n").lower().strip()[:1] == "y": sys.exit(0)
        
        try:
            if not os.path.exists(db_dir):
                os.makedirs(db_dir)
            
            db_ok = CreateHistoryDb(db_his_path)
            
            if not db_ok:
                logger.info(f'Error while creating the SQLite db [{db_his_path}]: {e}\r\n')
                sys.exit(1)
            else:
               db_his_created = True
                
        except Exception as e:
            logger.info(f'Error while creating the SQLite db [{db_his_path}]: {e}\r\n')
            sys.exit(1)   
            
        logger.info(f'\r\n\tCreating tables and fetching latest data... Do not interrupt!!\r\n\r\n')
    

        json_his = f'C:\\Users\\{USER}\\WATTSIGHT\\JSON\\{country_code}_HIS.json'
        
        if not os.path.isfile(json_his): 
            logger.info(f'Curves json file missing: {json_his}\r\n')
            sys.exit(0)
                
        try:
            all_frequencies = ['D', 'H', 'H12', 'H3', 'H6', 'M', 'MIN', 'MIN15', 'MIN30', 'MIN5', 'Q', 'S', 'W', 'Y']
            
            with open(json_his, 'r', encoding='UTF-8') as fin:
                jdata = json.load(fin)
                
            freq_list = []
            for k in jdata.keys():
                if k[-1] not in freq_list:
                    freq_list.append(k[-1])
  
            len_diff = ListDifference(freq_list, all_frequencies)
            if len(len_diff) > 0:
                logger.info(f'Invalid frequencies: {len_diff}.\r\nMust be in {all_frequencies}')
                sys.exit(1)                 
            
            freq_dic = {}
            name_list = []
            for freq in freq_list:
                freq_dic[freq] = []

            for k, v in jdata.items():
                 freq_dic[k[-1]] = freq_dic[k[-1]] + list(jdata[k].values())

        except Exception as e:
            logger.info(f'Error while formatting table names: {e}\r\n')
            sys.exit(1)            
            
        try:
            for k, v in freq_dic.items():
                logger.info(k)
                created_table = CreateHistoryTables(secrets = secrets, 
                                                    db_path = db_his_path, 
                                                    curve_names = v, 
                                                    data_from = data_from_watt, 
                                                    offset = iso_offset,
                                                    duration = iso_duration,
                                                    freq = k, 
                                                    func = func, 
                                                    tz = tz)
                if not created_table:
                    
                    logger.info(f'Tables for {k} cannot be created:\r\n')
                    sys.exit(1)

        except Exception as e:
            logger.info(f'Error while creating tables in [{db_his_path}]:\r\n{e}\r\n')
            sys.exit(1)  


    if db_his_created:
        logger.info(f'\r\n\r\n\r\n\t[{country_code}] Database path: [{db_his_path}]\r\n')    
    
    logger.info('\r\n')
    

    ### FORECAST DB
    db_created = False

    if not os.path.isfile(f'{db_path}'):  
        idx = country_codes.index(country_code)
        if not input(f"\tCreate new FORECAST database for {countries[idx]} ? (y/n):\r\n\r\n").lower().strip()[:1] == "y": sys.exit(0)
        
        try:
            if not os.path.exists(db_dir):
                os.makedirs(db_dir)
            
            db_ok = CreateWsCurvesDb(db_path)
            
            if not db_ok:
                logger.info(f'Error while creating the SQLite db [{db_path}]: {e}\r\n')
                sys.exit(1)
            else:
               db_created = True
                
        except Exception as e:
            logger.info(f'Error while creating the SQLite db [{db_path}]: {e}\r\n')
            sys.exit(1)   
            
        logger.info(f'\r\n\tCreating tables and fetching latest data... Do not interrupt!!\r\n\r\n')
    

        json_for = f'C:\\Users\\{USER}\\WATTSIGHT\\JSON\\{country_code}_FOR.json'
        
        if not os.path.isfile(json_for): 
            logger.info(f'Curves json file missing: {json_for}\r\n')
            sys.exit(0)
                
        try:
            all_frequencies = ['D', 'H', 'H12', 'H3', 'H6', 'M', 'MIN', 'MIN15', 'MIN30', 'MIN5', 'Q', 'S', 'W', 'Y']
            
            with open(json_for, 'r', encoding = 'UTF-8') as fin:
                jdata = json.load(fin)
                
            freq_list = []
            for k in jdata.keys():
                if k[-1] not in freq_list:
                    freq_list.append(k[-1])
  
            len_diff = ListDifference(freq_list, all_frequencies)
            if len(len_diff) > 0:
                logger.info(f'Invalid frequencies: {len_diff}.\r\nMust be in {all_frequencies}')
                sys.exit(1)                 
            
            freq_dic = {}
            name_list = []
            for freq in freq_list:
                freq_dic[freq] = []

            for k, v in jdata.items():
                 freq_dic[k[-1]] = freq_dic[k[-1]] + list(jdata[k].values())

        except Exception as e:
            logger.info(f'Error while formatting table names: {e}\r\n')
            sys.exit(1)            
            
        try:
            for k, v in freq_dic.items():
                
                created_table = CreateWsCurvesTables(secrets = secrets, 
                                                     db_path = db_path, 
                                                     curve_names = v, 
                                                     freq = k, 
                                                     func = func, 
                                                     tz = tz)
                if not created_table:
                    
                    logger.info(f'Tables for {k} cannot be created:\r\n')

                    sys.exit(1)

        except Exception as e:
            logger.info(f'Error while creating tables in [{db_path}]:\r\n{e}\r\n')
            sys.exit(1)  

    if db_created:
        logger.info(f'\r\n\r\n\r\n\t[{country_code}] Database path: [{db_path}]\r\n')

    
    xlsx_file_for = f'C:\\Users\\{USER}\\WATTSIGHT\\EXCEL\\{country_code}_FOR.xlsx'     
    json_for = f'C:\\Users\\{USER}\\WATTSIGHT\\JSON\\{country_code}_FOR.json' 
       
    if not os.path.isfile(f'{xlsx_file_for}'): 
        
        if not os.path.isfile(json_for): 
            logger.info(f'Curves json file missing: [{json_for}]\r\n')
            sys.exit(0)

        with open(json_for, 'r', encoding='UTF-8') as fin:
            jdata = json.load(fin)
         
        df_list = []
        sh_list = []

        for key in jdata.keys():
            
            id_list = list(jdata[key].keys())
            curve_names = list(jdata[key].values())
            table_names = [x.strip().replace(' ', '_').replace('/', '').replace('>', '').replace('-', '') for x in curve_names]
            
            frequencies = []
            frequencies = frequencies + len(curve_names) * [f'{key[-1]}']
            
            excel_dic = {'ID': id_list, 'CURVE_NAME': curve_names, 'TABLE_NAME': table_names, 'FREQUENCY': frequencies}
            
            df = GetDictAsDf(excel_dic)
            df_list.append(df)
            sh_list.append(key)
        try:
            ExportDfToExcelSheets(df_list, sh_list, xlsx_file_for) 
            
        except Exception as e:
            logger.info(f'Error creating tables names Excel file: {e}\r\n')
            sys.exit(1)
        
        logger.info(f'\r\n\t[{country_code}_FOR.db] tables reference: {xlsx_file_for}\r\n')   
        
        
    xlsx_file_his = f'C:\\Users\\{USER}\\WATTSIGHT\\EXCEL\\{country_code}_HIS.xlsx'     
    json_his = f'C:\\Users\\{USER}\\WATTSIGHT\\JSON\\{country_code}_HIS.json'
       
    if not os.path.isfile(f'{xlsx_file_his}'): 
        
        if not os.path.isfile(json_his): 
            logger.info(f'Curves json file missing: [{json_his}]\r\n')
            sys.exit(0)

        with open(json_his, 'r', encoding='UTF-8') as fin:
            jdata = json.load(fin)
         
        df_list = []
        sh_list = []

        for key in jdata.keys():
            
            id_list = list(jdata[key].keys())
            curve_names = list(jdata[key].values())
            table_names = [x.strip().replace(' ', '_').replace('/', '').replace('>', '').replace('-', '') for x in curve_names]
            
            frequencies = []
            frequencies = frequencies + len(curve_names) * [f'{key[-1]}']
            
            excel_dic = {'ID': id_list, 'CURVE_NAME': curve_names, 'TABLE_NAME': table_names, 'FREQUENCY': frequencies}
            
            df = GetDictAsDf(excel_dic)
            df_list.append(df)
            sh_list.append(key)
            
        try:
            ExportDfToExcelSheets(df_list, sh_list, xlsx_file_his) 
        except Exception as e:
            logger.info(f'Error creating tables names Excel file: {e}\r\n')
            sys.exit(1)
        
        logger.info(f'\r\n\t[{country_code}_HIS.db] tables reference: {xlsx_file_his}\r\n')    

   
    main(timeout, func, country_code, tz, data_from_watt, iso_offset, iso_duration)
    sys.exit(0)
