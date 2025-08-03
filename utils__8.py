"""
@author: Alicia Rohnacher, 2020
###################Functions for the analysis of measured data###################


read_stattionfile: load  station coordinates
    Input: csv file
    Output: list with coordinates

calc_utm: convert lon, lat to UTM
    Input: lon, lat
    Output: UTM coordinates (Zone 33N)

add_coordinates: append station coordinates to trace metadata
    Input: obspy stream
    Output obspy stream

geometry:

read_data:

calc_distance:

calc_baz:



array_analysis2_st: carry out array analysis
    Input: filter frequencies, start and end of timeseries, length of sliding window, info about instrument response, component, lag of sliding window
    Output: array with absolute power, relative power, slowness, backazimuth over time

array_timeseries_st: show temporal variation of parameter over time
    Input: stream, filter frequencies, length of sliding window, info about instrument response
    Output: -

array_hist_st: show polar plot with slowness/backazimuth
    Input: stream,start, end of window, first letters of Array name, network code, component, filter frequencies, length of sliding window, info about instrument response
    Output: -



"""
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import numpy as np
#import time, datetime
from obspy import UTCDateTime
from obspy.core.util.attribdict import AttribDict
import datetime
from datetime import datetime
from matplotlib.dates import num2date, date2num
from datetime import timedelta
from obspy import read, read_inventory
from array_analysis_obspy_edit import*
from obspy.imaging.cm import obspy_sequential
from matplotlib.colorbar import ColorbarBase
from matplotlib.colors import Normalize
import pyproj
from obspy.geodetics.base import gps2dist_azimuth
from math import radians, sin, cos, atan2, sqrt
import pandas as pd 
import sys

def read_station_file(filename):
    '''
    --> read coordinates of station
    filename:       string, filename (including path to filename) of csv file with station information (station,network,lat,lon,elevation)

    return:         list, coordinates of stations
    '''
    with open(filename,'r') as f: 
        dat=f.read().splitlines()
        dat=[i.split(',') for i in dat]
        
    return dat

def calc_utm(lon, lat):
    '''
    --> convert lat, lon to UTM coordinates
    lon:       float, longitude
    lat:       float, latitude

    return:    2 floats, UTM easting and nothing      
    '''
    if lon <20 and lon>10:
        zone="epsg:32633"#first epsg code is lon/lat (WGS84), second UTM zone 33N of Italy
    elif lon <-90 and lon>-96:
        zone="epsg:32615"#
    else: 
        print('No EPSG-Code is available for the given array coordinates and epsg:32633 is used.')
        zone="epsg:32633"#first epsg code is lon/lat (WGS84), second UTM zone 33N of Italy


    transformer = pyproj.Transformer.from_crs("epsg:4326",zone) 
    x, y = transformer.transform(lat,lon)
    return x,y
    
def add_coordinate(tr,stationfile):
    '''
    --> add station coordinates to obspy stream meta data (uses station info file!)
    tr:        obspy stream

    return:    obspy stream      
    '''
    stat_dat=read_station_file(stationfile)
    for s in tr:
        for stat in stat_dat:
            if stat[0]==s.stats.station: #if station of trace matches line in file --> add coordinates to the stream
                lat=float(stat[2])
                lon=float(stat[3])
                el=float(stat[4])
                x,y=calc_utm(lon, lat)
        
                s.stats.coordinates = AttribDict({
                        'latitude': lat,
                        'elevation': el/1000.0,
                        'longitude': lon,
                        'x': x,
                        'y': y})
            
    return tr

def geometry(tr,stationfile):
    '''
    --> add station coordinates to obspy stream meta data (uses station info file!)
    tr:        obspy stream

    return:    obspy stream      
    '''
    coord=[]
    stat_dat=read_station_file(stationfile)
    for s in tr:
        for stat in stat_dat:
            if stat[0]==s.stats.station: #if station of trace matches line in file --> add coordinates to the stream
                lat=float(stat[2])
                lon=float(stat[3])
                el=float(stat[4])
                x,y=calc_utm(lon, lat)
        
                s.stats.coordinates = AttribDict({
                        'latitude': lat,
                        'elevation': el/1000.0,
                        'longitude': lon,
                        'x': x,
                        'y': y})
                
    x0=tr[0].stats.coordinates['x']
    y0=tr[0].stats.coordinates['y']
    for s in tr:
        x=s.stats.coordinates['x']
        y=s.stats.coordinates['y']
        coord.append([x-x0,y-y0])
            
    return coord

def read_data(filename):
    headers=['time','lat','lon','z','mag']
    dtypes={'day':'str','time':'str','lat':'float','lon':'float','z':'float','mag':'float'}
    parse_dates=['time']
    data=pd.read_csv(filename, sep=',', header=None,names=headers,dtype=dtypes,parse_dates=parse_dates)
    return data


def calc_distance(lo1,la1,lo2,la2):
    lat1 = radians(la1)
    lon1 = radians(lo1)
    lat2 = radians(la2)
    lon2 = radians(lo2)

    dlon = lon2 - lon1
    dlat = lat2 - lat1

    a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
    c = 2 * atan2(sqrt(a), sqrt(1 - a))
    return(c)

def calc_baz(lo1,la1,lo2,la2):
    dist, baz,az=gps2dist_azimuth(la1,lo1,la2,lo2)
    return dist, baz,az

def array_analysis2_st(st, fmin, fmax, win_len=1,smax=1.5,win_frac=0.05):
    '''
    --> carries out the array analysis for all array traces at a given date and time
    --> uses obspy funtion array_analysis_obspy_edit
    fmin:       float, lower frequency corner of bandpass filter
    fmax:       float, upper frequency corner of bandpass filter
    start:      UTCDateTime object, define start of timeseries
    end:        UTCDateTime object, define end of timeseries
    win_len:    float, size of sliding window (should be smaller than duration of the timeseries)
    res:        boolean, remove instrument response?!
    dataless    string or default: None, information about instrument response
    comp:       string (1 letter), component (use wildcards for reading more than one component)
    win_frac:   float, fraction of sliding window to use for step
    '''
    

    #define slowness grid
    
    sllx=-smax
    slmx=smax
    slly=-smax
    slmy=smax
    sls=smax/50
    
    print('calculating fk-diagram with:')
    print('sll_x: ',sllx)
    print('slm_x: ',slmx)
    print('sll_y: ',slly)
    print('slm_y: ',slmy)
    print('sl_s: ',sls)
    
    start=st[0].stats.starttime
    end=st[0].stats.endtime
    print(start,end)

    #define relevant parameters
    kwargs = dict(
				# slowness grid: X min, X max, Y min, Y max, Slow Step
				sll_x=sllx, slm_x=slmx, sll_y=slly, slm_y=slmy, sl_s=sls,
				# sliding window properties
				win_len=win_len, win_frac=win_frac,
				# frequency properties
				frqlow=fmin, frqhigh=fmax, prewhiten=0,
				# restrict output
				semb_thres=-1e9, vel_thres=-1e9, timestamp='julsec',
				stime=UTCDateTime(start), etime=UTCDateTime(end), coordsys='lonlat', time_edit=True
    )
    print('fk processing for signal between {} and {}...'.format(start,end))
    
    # Execute array_processing
    out = array_processing_edit(st, **kwargs)
    return (out)

def array_timeseries_st(st, fmin,fmax,win_len=1,smax=1.5,stationfile="station_GR.csv",dataless=None,save=False):
    '''
    --> uses results of array analysis and show temporal variation in a Figure
    start:        UTCDateTime object, define start of timeseries
    end:          UTCDateTime object, define end of timeseries
    station_base: string (2 letters), define array station
    code:         string (2 letters), network code 
    comp:         string (1 letter), component (use wildcards for reading more than one component)
    fmin:         float, lower frequency corner of bandpass filter or highpass filter (if fmin=fmax=0 --> no filtering, if fmin !=0, fmax=0 --> highpass filtering)
    fmax:         float, upper frequency corner of bandpass filter or lowpass filter (if fmin=fmax=0 --> no filtering, if fmax !=0, fmin=0 --> lowpass filtering)
    win_len:      float, size of sliding window (should be smaller than duration of the timeseries)
    stationfile:
    res:          boolean, specify, whether instrument response should be removed
    dataless:     string, path and filename of dataless with information about the instrument response
    save:         boolean, saving Figure?
    '''
    st=add_coordinate(st,stationfile)
    for s in st:
        print(s.stats.coordinates)
    #print(st[0])
    sl_axlim=smax
    # carry out array analysis --> will always use 'AP??' as station base for this exercise!
    out=array_analysis2_st(st,fmin,fmax,win_len=win_len,smax=smax)
    t, rel_power, abs_power, baz, slow = out.T
    labels = ['rel.power', 'abs.power', 'baz in $\circ$', 'slow in s/km']
    #print(slow)
    
    
    #define Figure parameters (axis, tick format)
    xformatter = mdates.DateFormatter('%H:%M:%S.%f')
    xlocator = mdates.AutoDateLocator()
    fig = plt.figure(figsize=(8,4))
    ax=[None]*3
    j=0
    #convert time of array analysis (seconds after 01-01-1970 0:00:00 to a UTCDatetime objects)
    t_out=[]
    for k in range(len(out[:, 0])):
        t_out.append(UTCDateTime(out[k, 0]))
    
    #plot array results
    for i, lab in enumerate(labels):
        if i<2:
            continue
        if j==0:
            ax[j] = fig.add_subplot(3, 1, i-1)
        else:
            ax[j] = fig.add_subplot(3, 1, i-1, sharex=ax[0])
        ax[j].xaxis_date()
        ax[j].scatter(t_out, out[:, i + 1], c=out[:, 2], alpha=0.6, edgecolors='none', cmap=obspy_sequential)
        ax[j].set_ylabel(lab)
        ax[j].set_xlim([t_out[0], t_out[-1]])
        ax[j].set_ylim(out[:, i + 1].min(), out[:, i + 1].max())
        if i==2:
            ax[j].set_ylim([-190,190])
        if i==3:
            ax[j].set_ylim([0,sl_axlim])
        ax[j].xaxis.set_major_locator(xlocator)
        ax[j].xaxis.set_major_formatter(mdates.AutoDateFormatter(xlocator))
        ax[j].set_xticks([])
        j=j+1

    #plot seismogram
    i=2
    ax[i] = fig.add_subplot(3, 1, 3)#, sharex=ax[0])  
    ax[i].xaxis_date()
    ax[i].xaxis.set_major_locator(xlocator)
    ax[i].xaxis.set_major_formatter(mdates.AutoDateFormatter(xlocator))
    ax[i].plot(np.array(st[0].times("utcdatetime")),st[0].data*1e3, 'k',linewidth=0.7)
    ax[i].set_xlim(t_out[0], t_out[-1])
    ax[i].set_ylabel('velocity in mm/s')
    
    fig.subplots_adjust(left=0.15, top=0.95, right=0.95, bottom=0.2, hspace=0)
    if save:
        fig.savefig('Figures/array_timesseries_{}'.format(start.strftime('%y%m%d_%H%M%S'), dpi=300))
       
    plt.show()

def array_hist_st(st, start,end, fmin=0.125, fmax=4,win_len=1,smax=1.5,stationfile="station_GR.csv",res=False, dataless=None,save=False):
    '''
    --> uses results of array analysis and show polar histogram of slowness over the backazimuth
    st:           stream with data
    start:        UTCDateTime object, define start of timeseries
    end:          UTCDateTime object, define end of timeseries
    fmin:         float, lower frequency corner of bandpass filter or highpass filter (if fmin=fmax=0 --> no filtering, if fmin !=0, fmax=0 --> highpass filtering)
    fmax:         float, upper frequency corner of bandpass filter or lowpass filter (if fmin=fmax=0 --> no filtering, if fmax !=0, fmin=0 --> lowpass filtering)
    win_len:      float, size of sliding window (should be smaller than duration of the timeseries)
    smax:         float, maximum slowness
    res:          boolean, specify, whether instrument response should be removed
    dataless:     string, path and filename of dataless with information about the instrument response
    save:         boolean, saving Figure?
    '''
    st=add_coordinate(st,stationfile)
    st.detrend(type='simple')
    st.detrend(type='demean')
    st_cut=st.copy()
    st_cut.trim(start,end)
    sl_axlim=smax
    # carry out array analysis --> will always use 'AP??' as station base for this exercise!
    out=array_analysis2_st(st_cut,fmin,fmax,win_len=win_len,smax=smax)
    t, rel_power, abs_power, baz, slow = out.T

    baz[baz < 0.0] += 360
    
    # choose number of fractions in plot (desirably 360 degree/N is an integer!)
    N = 36
    smax=smax
    N2 = 30
    N2=len(slow)-1
    abins = np.arange(N + 1) * 360. / N
    sbins = np.linspace(0, smax, N2 + 1)
    
    # sum rel power in bins given by abins and sbins
    hist, baz_edges, sl_edges = \
        np.histogram2d(baz, slow, bins=[abins, sbins], weights=rel_power)
       
    cmap = obspy_sequential
    baz_edges = np.radians(baz_edges)
    
    # add polar and colorbar axes
    fig = plt.figure(figsize=(10,6))
    cax = fig.add_axes([0.8, 0.1, 0.03, 0.4])
    ax = fig.add_axes([0.2, 0.05, 0.5, 0.5], polar=True)
    
    ax.set_theta_direction(-1)
    ax.set_theta_zero_location("N")
    
    dh = abs(sl_edges[1] - sl_edges[0])
    dw = abs(baz_edges[1] - baz_edges[0])
    
    # circle through backazimuth
    for i, row in enumerate(hist):
        bars = ax.bar((i * dw) * np.ones(N2),
                      height=dh * np.ones(N2),
                      width=dw, bottom=dh * np.arange(N2),
                      color=cmap(row / hist.max()))
    
    ax.set_xticks(np.linspace(0, 2 * np.pi, 4, endpoint=False))
    ax.set_xticklabels([r'N', r'E', r'S', r'W'])
    
    # set slowness limits
    ax.set_ylim(0, smax)
    [i.set_color('grey') for i in ax.get_yticklabels()]
    ColorbarBase(cax, cmap=cmap,
                 norm=Normalize(vmin=hist.min(), vmax=hist.max()))
    ax=[None]*1
    
    tr=st[0].copy()
    tr.filter('bandpass',freqmin=fmin,freqmax=fmax)
    tr.trim(tr.stats.starttime+5, tr.stats.endtime-5)
    #create Figure
    i=0
    ax1= fig.add_axes([0.15, 0.65, 0.70, 0.2])
    time_ar=np.array(tr.times("matplotlib"))
    data=tr.data
    maximum=max(abs(data))
    ax1.plot(time_ar,data, 'k',linewidth=0.9)
    ax1.xaxis_date()
    titlestring = r'%s, filter %1.2f - %1.2f Hz' % (tr.stats.station,fmin,fmax)
    ax1.text(time_ar[15],maximum*0.80,titlestring,fontsize=7) 
    ax1.axvline(start,linestyle='--',color='0.7')
    ax1.axvline(end,linestyle='--',color='0.7')
    #ax1.axvline(UTCDateTime(t[0]),linestyle='--',color='0.4')
    #ax1.axvline(UTCDateTime(t[-1]),linestyle='--',color='0.4')
    ax1.set_xlabel(r'time')
    if res:
        ax1.set_ylabel(r'v/ (m/s)')
    else:
        ax1.set_ylabel(r'v/ counts')
    ax1.autoscale(enable=True, axis='x', tight=False)
    ax1.set_ylim(-maximum,maximum)
    fig.suptitle(r'seismic explosion detected with the array at %s' %start.strftime('%d.%m.%y %H:%M:%S'),y=0.89,fontsize=11)
    if save:
        fig.savefig("Figure/fk_{}_{}_{}-{}".format(start.strftime('%Y%m%d_%H%M%S'),win_len, fmin,fmax))
    plt.show()
    # Find the optimal baz and slowness
    
    max_idx = np.unravel_index(np.argmax(hist, axis=None), hist.shape)
    optimal_baz = (baz_edges[max_idx[0]] + baz_edges[max_idx[0] + 1]) / 2
    optimal_slow = (sl_edges[max_idx[1]] + sl_edges[max_idx[1] + 1]) / 2
    
    return optimal_baz, optimal_slow
