import os
import sys
import re
import dash
from waitress import serve
from dash.dependencies import Output, Input
import dash_core_components as dcc
import dash_html_components as html
import wapi
import datetime as dt
from datetime import datetime, timedelta, timezone
import random
import webbrowser
import pandas as pd
import json
import logging
from pathlib import Path
from wsd_utils import GetCategoryBundle, DisplayRelativeActuals, GetRelative, ReplaceFavIco
from wsd_utils import DisplayAvaCat, DisplayAvail, DisplayForecast, GetTitleTable, GetAvaCat, SetCoreAffinity

cpu_count = os.cpu_count()

SetCoreAffinity(1)

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
    ws_session = wapi.Session(client_id = WS_CLIENT_ID,  client_secret = WS_CLIENT_SECRET)
except:
    os.environ['https_proxy'] = ''
    os.environ['http_proxy'] = ''

log = logging.getLogger('werkzeug')
log.setLevel(logging.ERROR)

frequencies = ['D', 'H', 'H12', 'H3', 'H6', 'M', 'MIN', 'MIN15', 'MIN30', 'MIN5', 'Q', 'S', 'W', 'Y']
countries = ['Germany', 'France', 'Switzerland', 'Belgium', 'Netherlands', 'Spain', 'Italy', 'Austria', 'Denmark', 'Norway', 'Sweden', 'United Kingdom', 'Czech Republic']

CCs = ['DE', 'FR', 'CH', 'BE', 'NL', 'ES', 'IT', 'AT', 'DK', 'NO', 'SE', 'UK', 'CZ']

CC =  sys.argv[1].upper()
country_code = CC.lower()
idx = CCs.index(CC)
country = countries[idx]

ReplaceFavIco(user = USER, country_code = CC)

external_stylesheets = ['dash_style.css']

json_for = f'C:\\Users\\{USER}\\WATTSIGHT\\JSON\\{CC}_FOR.json'

with open(json_for, 'r', encoding='UTF-8') as fin:
    curves_json = json.load(fin)

port = random.randrange(8000, 9000, 10)
func = 'AVERAGE'
tz = 'CET'

update_time = 30000
ava_time = 21600000
rva_time = 86400000

data_from_ava = str(datetime.now().date())
data_to_ava = str(datetime.now().date() + dt.timedelta(days = 91))

data_from_his = str(datetime.now().date() - dt.timedelta(days = 30))
data_to_his = str(datetime.now().date() + dt.timedelta(days = 1))

data_from_nor = str(datetime.now().date() - dt.timedelta(days = 30))
data_to_nor = str(datetime.now().date() + dt.timedelta(days = 30))

shadow_col = '#797D7F'

inf_unit = 'GWh'
for_unit = 'MWh/h'
ror_unit = 'MWh/h'
ava_unit = 'MW'
wea_unit = '°C'

ava_freq = 'H'
for_freq = 'H'

offset = 24
duration = 24*7

wnd_actu = f'pro {country_code} wnd mwh/h cet min15 s'
wnd_cat = 'WND'

wnd_names, wnd_no_df, wnd_mo_df = GetCategoryBundle(WS_CLIENT_ID = WS_CLIENT_ID, 
                                                    WS_CLIENT_SECRET = WS_CLIENT_SECRET,
                                                    curves_json = curves_json, 
                                                    country_code = country_code, 
                                                    category = wnd_cat, 
                                                    freq = for_freq, 
                                                    func = func, 
                                                    tz = tz, 
                                                    actual_name = wnd_actu,
                                                    data_from_nor = data_from_nor,
                                                    data_to_nor = data_to_nor)

spv_actu = f'pro {country_code} spv mwh/h cet min15 s'
spv_cat = 'SPV'

spv_names, spv_no_df, spv_mo_df = GetCategoryBundle(WS_CLIENT_ID = WS_CLIENT_ID, 
                                                    WS_CLIENT_SECRET = WS_CLIENT_SECRET,
                                                    curves_json = curves_json, 
                                                    country_code = country_code, 
                                                    category = spv_cat, 
                                                    freq = for_freq, 
                                                    func = func, 
                                                    tz = tz, 
                                                    actual_name = spv_actu,
                                                    data_from_nor = data_from_nor,
                                                    data_to_nor = data_to_nor)

con_actu = f'con {country_code} mwh/h cet min15 a'
con_cat = 'CON'

con_names, con_no_df, con_mo_df = GetCategoryBundle(WS_CLIENT_ID = WS_CLIENT_ID, 
                                                    WS_CLIENT_SECRET = WS_CLIENT_SECRET,
                                                    curves_json = curves_json, 
                                                    country_code = country_code, 
                                                    category = con_cat, 
                                                    freq = for_freq, 
                                                    func = func, 
                                                    tz = tz, 
                                                    actual_name = con_actu,
                                                    data_from_nor = data_from_nor,
                                                    data_to_nor = data_to_nor)

wea_actu = f'tt {country_code} con °c cet min15 s'
wea_cat = 'WEA'

wea_names, wea_no_df, wea_mo_df = GetCategoryBundle(WS_CLIENT_ID = WS_CLIENT_ID, 
                                                    WS_CLIENT_SECRET = WS_CLIENT_SECRET,
                                                    curves_json = curves_json, 
                                                    country_code = country_code, 
                                                    category = wea_cat, 
                                                    freq = for_freq, 
                                                    func = func, 
                                                    tz = tz, 
                                                    actual_name = wea_actu,
                                                    data_from_nor = data_from_nor,
                                                    data_to_nor = data_to_nor)

ava_list = list(curves_json[f'{CC}_AVA_H'].values())

ava_ror = GetAvaCat(r'.*\bror\b.*', ava_list)

ava_inf = GetAvaCat(r'^inf\b.*', ava_list)

app = dash.Dash(__name__, external_stylesheets = external_stylesheets)

app.config.suppress_callback_exceptions = False
app.css.config.serve_locally = True
app.title = f'{country} Forecasts'


app.layout = html.Div([
    
    dcc.Interval(
        id = 'for_update', 
        interval = update_time,
        n_intervals = 0),
        
    dcc.Interval(
        id = 'ava_update', 
        interval = ava_time,
        n_intervals = 0),
    
    dcc.Interval(
        id = 'rva_update', 
        interval = rva_time,
        n_intervals = 0),     

    html.Div([
        
        html.Div([
            
            html.Div([
                
                GetTitleTable('WND F', for_unit, for_freq),
                
                dcc.Graph(
                    id = 'wnd_forecast',
                    animate = True,
                    animation_options = { 'frame': { 'redraw': True}},
                    responsive = True,
                    config = {'displayModeBar': False})
                
                ], style = {'display': 'inline-block', 
                            'width': '100%',
                            'heigth': '100%',
                            'box-shadow': f'2px 2px 2px {shadow_col}', 
                            'margin-right': 30, 
                            'margin-left': 30, 
                            'margin-top': 30, 
                            'margin-bottom': 15,
                            'border': '5px solid',
                            'border-radius': 5,
                            'border-color': '#ecf0f1'})
                            
            ], style = {'display': 'flex',
                        'flex-direction' : 'row', 'height': '535px',
                        'width': '100%'}),
            

        html.Div([
            
            html.Div([
            
                GetTitleTable('WND H', for_unit, for_freq),
                    
                dcc.Graph(
                    id = 'rva_wnd',
                    responsive = True,
                    config = {'displayModeBar': False})
                    
                ],  style = {'display': 'inline-block',
                            'width': '100%',
                            'heigth': '100%',
                            'box-shadow': f'2px 2px 2px {shadow_col}', 
                            'margin-right': 30, 
                            'margin-left': 30, 
                            'margin-top': 15, 
                            'margin-bottom': 15,
                            'border': '5px solid',
                            'border-radius': 5,
                            'border-color': '#ecf0f1'})  
                        
            ], style = {'display': 'flex',
                        'flex-direction' : 'row', 'height': '520px',
                        'width': '100%'}),
        
        html.Div([ 
                            
            html.Div([
                    
                GetTitleTable('SPV F', for_unit, for_freq),
                
                dcc.Graph(
                    id = 'spv_forecast',    
                    animate = True,
                    animation_options = { 'frame': { 'redraw': True}},
                    responsive = True,
                    config={'displayModeBar': False})
            
                ], style = {'display': 'inline-block',
                            'width': '100%',
                            'heigth': '100%',                                
                            'box-shadow': f'2px 2px 2px {shadow_col}', 
                            'margin-right': 30, 
                            'margin-left': 30, 
                            'margin-top': 15, 
                            'margin-bottom': 15,
                            'border': '5px solid',
                            'border-radius': 5,
                            'border-color': '#ecf0f1'})

            ], style = {'display': 'flex',
                        'flex-direction' : 'row', 'height': '520px',
                        'width': '100%'}),
        
        html.Div([
            
            html.Div([
            
                GetTitleTable('SPV H', for_unit, for_freq),
                    
                dcc.Graph(
                    id = 'rva_spv',
                    responsive = True,
                    config = {'displayModeBar': False})
                    
                ], style = {'display': 'inline-block',
                            'width': '100%',
                            'heigth': '100%',                                
                            'box-shadow': f'2px 2px 2px {shadow_col}', 
                            'margin-right': 30, 
                            'margin-left': 30, 
                            'margin-top': 15, 
                            'margin-bottom': 15,
                            'border': '5px solid',
                            'border-radius': 5,
                            'border-color': '#ecf0f1'})

            ], style = {'display': 'flex',
                        'flex-direction' : 'row', 'height': '520px',
                        'width': '100%'}),
        
        html.Div([
            
            html.Div([
                
                GetTitleTable('CON F', for_unit, for_freq),
                    
                dcc.Graph(
                    id = 'con_forecast',    
                    animate = True,
                    animation_options = { 'frame': { 'redraw': True}},
                    responsive = True,
                    config = {'displayModeBar': False})
                
                ], style = {'display': 'inline-block',
                            'width': '100%',
                            'heigth': '100%',                                
                            'box-shadow': f'2px 2px 2px {shadow_col}', 
                            'margin-right': 30, 
                            'margin-left': 30, 
                            'margin-top': 15, 
                            'margin-bottom': 15,
                            'border':'5px solid',
                            'border-radius': 5,
                            'border-color': '#ecf0f1'})

            ], style = {'display': 'flex',
                        'flex-direction' : 'row', 'height': '520px',
                        'width': '100%'}),
        
        html.Div([
            
            html.Div([
                    
                GetTitleTable('CON H', for_unit, for_freq),
                    
                dcc.Graph(
                    id = 'rva_con',
                    responsive = True,
                    config = {'displayModeBar': False})
                    
                ], style = {'display': 'inline-block',
                            'width': '100%',
                            'heigth': '100%',                                
                            'box-shadow': f'2px 2px 2px {shadow_col}', 
                            'margin-right': 30, 
                            'margin-left': 30, 
                            'margin-top': 15, 
                            'margin-bottom': 15,
                            'border': '5px solid',
                            'border-radius': 5,
                            'border-color': '#ecf0f1'})
            
            ], style = {'display': 'flex',
                        'flex-direction' : 'row', 'height': '520px',
                        'width': '100%'}),
        
        html.Div([
            
            html.Div([
                
                GetTitleTable('WEA F', wea_unit, for_freq),
                    
                dcc.Graph(
                    id = 'wea_forecast',
                    animate = True,
                    animation_options = { 'frame': { 'redraw': True}},
                    responsive = True,
                    config={'displayModeBar': False})
                
                ], style = {'display': 'inline-block', 
                            'width': '100%',
                            'heigth': '100%',                                                      
                            'box-shadow': f'2px 2px 2px {shadow_col}', 
                            'margin-right': 30, 
                            'margin-left': 30, 
                            'margin-top': 15, 
                            'margin-bottom': 15,
                            'border':'5px solid',
                            'border-radius': 5,
                            'border-color': '#ecf0f1'})
            
            ], style = {'display': 'flex',
                        'flex-direction' : 'row', 'height': '520px',
                        'width': '100%'}),
        
        html.Div([
            
            html.Div([
                    
                GetTitleTable('WEA H', wea_unit, for_freq),
                    
                dcc.Graph(
                    id = 'rva_wea',
                    responsive = True,
                    config = {'displayModeBar': False})
                    
                ], style = {'display': 'inline-block',
                            'width': '100%',
                            'heigth': '100%',                                
                            'box-shadow': f'2px 2px 2px {shadow_col}', 
                            'margin-right': 30, 
                            'margin-left': 30, 
                            'margin-top': 15, 
                            'margin-bottom': 15,
                            'border': '5px solid',
                            'border-radius': 5,
                            'border-color': '#ecf0f1'})

            ], style = {'display': 'flex',
                        'flex-direction' : 'row', 'height': '520px',
                        'width': '100%'}),
        
        html.Div([
            
            html.Div([
                
                GetTitleTable('AVA F', ava_unit, ava_freq),
                    
                dcc.Graph(
                    id = 'ava_forecast',    
                    animate = True,
                    animation_options = { 'frame': { 'redraw': True}},
                    responsive = True,
                    config = {'displayModeBar': False})
                    
                ], style = {'display': 'inline-block',
                            'width': '100%',
                            'heigth': '100%',                                
                            'box-shadow': f'2px 2px 2px {shadow_col}', 
                            'margin-right': 30, 
                            'margin-left': 30, 
                            'margin-top': 15, 
                            'margin-bottom': 30,
                            'border': '5px solid',
                            'border-radius': 5,
                            'border-color': '#ecf0f1'})
            
            ], style = {'display': 'flex',
                        'flex-direction' : 'row', 'height': '535px',
                        'width': '100%'})
        
        ], style = {'display': 'flex',    
                    'flex-direction': 'column', 
                    'background-color': '#B3B6B7'})
    ])

@app.callback(Output('wnd_forecast', 'figure'),
            [Input('for_update', 'n_intervals')])

def call_back_wnd(self): 

    if cpu_count > 2:
        SetCoreAffinity(2)

    return DisplayForecast(WS_CLIENT_ID = WS_CLIENT_ID, 
                           WS_CLIENT_SECRET = WS_CLIENT_SECRET, 
                           freq = for_freq,
                           func = func,
                           tz = tz,
                           no_df = wnd_no_df,
                           mo_df = wnd_mo_df,
                           ec_curve_name = wnd_names[2],
                           ec_ens_curve_name = wnd_names[3],
                           gf_curve_name = wnd_names[4],
                           gf_ens_curve_name = wnd_names[5])
    
@app.callback(Output('rva_wnd', 'figure'),
            [Input('rva_update', 'n_intervals')])

def call_back_rva_wnd(self): 
   
    if cpu_count > 3: 
        SetCoreAffinity(3)

    wnd_ac_df, wnd_ec_df, wnd_ec_ens_df, wnd_gf_df, wnd_gf_ens_df, wnd_actu_short, wnd_ec_short, wnd_ec_ens_short, wnd_gf_short, wnd_gf_ens_short = GetRelative(WS_CLIENT_ID = WS_CLIENT_ID, 
                                                                                                                                                                WS_CLIENT_SECRET = WS_CLIENT_SECRET, 
                                                                                                                                                                offset = offset, 
                                                                                                                                                                duration = duration, 
                                                                                                                                                                freq = for_freq, 
                                                                                                                                                                func = func, 
                                                                                                                                                                tz = tz, 
                                                                                                                                                                data_from = data_from_his, 
                                                                                                                                                                data_to = data_to_his,
                                                                                                                                                                names_list = wnd_names)
    
    return DisplayRelativeActuals(freq = for_freq, 
                                  func = func, 
                                  tz = tz, 
                                  cc = CC, 
                                  offset = offset,
                                  duration = duration,
                                  ac_df = wnd_ac_df,
                                  ec_df = wnd_ec_df,
                                  ec_ens_df = wnd_ec_ens_df,
                                  gf_df = wnd_gf_df,
                                  gf_ens_df = wnd_gf_ens_df,
                                  actu_short = wnd_actu_short,
                                  ec_short = wnd_ec_short,
                                  ec_ens_short = wnd_ec_ens_short,
                                  gf_short = wnd_gf_short,
                                  gf_ens_short = wnd_gf_ens_short)

@app.callback(Output('spv_forecast', 'figure'),
            [Input('for_update', 'n_intervals')])

def call_back_spv(self): 
    
    if cpu_count > 4:
        SetCoreAffinity(4)

    return DisplayForecast(WS_CLIENT_ID = WS_CLIENT_ID, 
                           WS_CLIENT_SECRET = WS_CLIENT_SECRET, 
                           freq = for_freq,
                           func = func,
                           tz = tz,
                           no_df = spv_no_df,
                           mo_df = spv_mo_df,
                           ec_curve_name = spv_names[2],
                           ec_ens_curve_name = spv_names[3],
                           gf_curve_name = spv_names[4],
                           gf_ens_curve_name = spv_names[5])


@app.callback(Output('rva_spv', 'figure'),
            [Input('rva_update', 'n_intervals')])

def call_back_rva_spv(self): 
    
    if cpu_count > 5:
        SetCoreAffinity(5)

    spv_ac_df, spv_ec_df, spv_ec_ens_df, spv_gf_df, spv_gf_ens_df, spv_actu_short, spv_ec_short, spv_ec_ens_short, spv_gf_short, spv_gf_ens_short = GetRelative(WS_CLIENT_ID = WS_CLIENT_ID, 
                                                                                                                                                                WS_CLIENT_SECRET = WS_CLIENT_SECRET, 
                                                                                                                                                                offset = offset, 
                                                                                                                                                                duration = duration, 
                                                                                                                                                                freq = for_freq, 
                                                                                                                                                                func = func, 
                                                                                                                                                                tz = tz, 
                                                                                                                                                                data_from = data_from_his, 
                                                                                                                                                                data_to = data_to_his,
                                                                                                                                                                names_list = spv_names)
    
    return DisplayRelativeActuals(freq = for_freq, 
                                  func = func, 
                                  tz = tz, 
                                  cc = CC, 
                                  offset = offset,
                                  duration = duration,
                                  ac_df = spv_ac_df,
                                  ec_df = spv_ec_df,
                                  ec_ens_df = spv_ec_ens_df,
                                  gf_df = spv_gf_df,
                                  gf_ens_df = spv_gf_ens_df,
                                  actu_short = spv_actu_short,
                                  ec_short = spv_ec_short,
                                  ec_ens_short = spv_ec_ens_short,
                                  gf_short = spv_gf_short,
                                  gf_ens_short = spv_gf_ens_short)


@app.callback(Output('con_forecast', 'figure'),
            [Input('for_update', 'n_intervals')])

def call_back_con(self): 

    if cpu_count > 5:
        SetCoreAffinity(6)

    return DisplayForecast(WS_CLIENT_ID = WS_CLIENT_ID, 
                           WS_CLIENT_SECRET = WS_CLIENT_SECRET,
                           freq = for_freq,
                           func = func,
                           tz = tz,
                           no_df = con_no_df,
                           mo_df = con_mo_df,
                           ec_curve_name = con_names[2],
                           ec_ens_curve_name = con_names[3],
                           gf_curve_name = con_names[4],
                           gf_ens_curve_name = con_names[5])
    
    
@app.callback(Output('rva_con', 'figure'),
            [Input('rva_update', 'n_intervals')])

def call_back_rva_con(self): 
    
    if cpu_count > 7:
        SetCoreAffinity(7)

    con_ac_df, con_ec_df, con_ec_ens_df, con_gf_df, con_gf_ens_df, con_actu_short, con_ec_short, con_ec_ens_short, con_gf_short, con_gf_ens_short = GetRelative(WS_CLIENT_ID = WS_CLIENT_ID, 
                                                                                                                                                                WS_CLIENT_SECRET = WS_CLIENT_SECRET, 
                                                                                                                                                                offset = offset, 
                                                                                                                                                                duration = duration, 
                                                                                                                                                                freq = for_freq, 
                                                                                                                                                                func = func, 
                                                                                                                                                                tz = tz, 
                                                                                                                                                                data_from = data_from_his, 
                                                                                                                                                                data_to = data_to_his,
                                                                                                                                                                names_list = con_names)
    
    return DisplayRelativeActuals(freq = for_freq, 
                                  func = func, 
                                  tz = tz, 
                                  cc = CC, 
                                  offset = offset,
                                  duration = duration,
                                  ac_df = con_ac_df,
                                  ec_df = con_ec_df,
                                  ec_ens_df = con_ec_ens_df,
                                  gf_df = con_gf_df,
                                  gf_ens_df = con_gf_ens_df,
                                  actu_short = con_actu_short,
                                  ec_short = con_ec_short,
                                  ec_ens_short = con_ec_ens_short,
                                  gf_short = con_gf_short,
                                  gf_ens_short = con_gf_ens_short)

    
@app.callback(Output('wea_forecast', 'figure'),
            [Input('for_update', 'n_intervals')])

def call_back_wea(self): 
    
    if cpu_count > 2:    
        SetCoreAffinity(2)

    return DisplayForecast(WS_CLIENT_ID = WS_CLIENT_ID, 
                           WS_CLIENT_SECRET = WS_CLIENT_SECRET,
                           freq = for_freq,
                           func = func,
                           tz = tz,
                           no_df = wea_no_df,
                           mo_df = wea_mo_df,
                           ec_curve_name = wea_names[2],
                           ec_ens_curve_name = wea_names[3],
                           gf_curve_name = wea_names[4],
                           gf_ens_curve_name = wea_names[5])

@app.callback(Output('rva_wea', 'figure'),
            [Input('rva_update', 'n_intervals')])

def call_back_rva_wea(self): 
    
    if cpu_count > 3:    
        SetCoreAffinity(3)
    
    wea_ac_df, wea_ec_df, wea_ec_ens_df, wea_gf_df, wea_gf_ens_df, wea_actu_short, wea_ec_short, wea_ec_ens_short, wea_gf_short, wea_gf_ens_short = GetRelative(WS_CLIENT_ID = WS_CLIENT_ID, 
                                                                                                                                                                WS_CLIENT_SECRET = WS_CLIENT_SECRET, 
                                                                                                                                                                offset = offset, 
                                                                                                                                                                duration = duration, 
                                                                                                                                                                freq = for_freq, 
                                                                                                                                                                func = func, 
                                                                                                                                                                tz = tz, 
                                                                                                                                                                data_from = data_from_his, 
                                                                                                                                                                data_to = data_to_his,
                                                                                                                                                                names_list = wea_names)
   
    return DisplayRelativeActuals(freq = for_freq, 
                                  func = func, 
                                  tz = tz, 
                                  cc = CC, 
                                  offset = offset,
                                  duration = duration,
                                  ac_df = wea_ac_df,
                                  ec_df = wea_ec_df,
                                  ec_ens_df = wea_ec_ens_df,
                                  gf_df = wea_gf_df,
                                  gf_ens_df = wea_gf_ens_df,
                                  actu_short = wea_actu_short,
                                  ec_short = wea_ec_short,
                                  ec_ens_short = wea_ec_ens_short,
                                  gf_short = wea_gf_short,
                                  gf_ens_short = wea_gf_ens_short)


@app.callback(Output('ava_forecast', 'figure'),
            [Input('ava_update', 'n_intervals')])

def call_back_ava(self): 
    
    if cpu_count > 4:    
        SetCoreAffinity(4)

    return DisplayAvail(WS_CLIENT_ID = WS_CLIENT_ID, 
                        WS_CLIENT_SECRET = WS_CLIENT_SECRET, 
                        freq = ava_freq,
                        func = func,
                        tz = tz,
                        unit = ava_unit,
                        curve_list = ava_list,
                        data_from = data_from_ava,
                        data_to = data_to_ava)
    

    
if __name__ == '__main__':
    
    server = app.server
    
    url = f'http://127.0.0.1:{port}/'
    
    webbrowser.register('chrome',
                        None,
                        webbrowser.BackgroundBrowser("C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe"))
    webbrowser.get('chrome').open_new(url)

    serve(server, host='127.0.0.1', port = port, threads = 32)
