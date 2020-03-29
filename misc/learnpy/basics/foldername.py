# this is sample code for generating a folder structure 

class Folder: 
    server = ''
    asset = '' 
    tag = ''
folder_struct = object
folder_struct.server = 's1'


# example tag name: gbn.pp.fi-3221.pv 
# example server name: GBN-PI01

import datetime

server = 'GBN-PI01'
tag = 'gbn.pp.fi-3221.pv' # remove the .pv
dt = datetime.datetime.now() 
datafolder = '' ## provide string to be later made a Path object 

param_tuple = { 
    'server': server,
    'tag': tag,
    'year': dt.year, 
    'month': dt.month, 
    'day': dt.day, 
    'hour': dt.hour }

# string formatting: https://docs.python.org/2/library/stdtypes.html#string-formatting 
partitionName = '%(server)s/%(tag)s/%(year)04d/%(month)02d/%(day)02d/%(hour)02d' % param_tuple




from pathlib import Path, PurePath

p = PurePath('c:/data')
p / partitionName

import os
os.fspath(p/partitionName)
os.fspath(PurePath('.')/partitionName)

datafolder = Path.cwd() # or something like Path("c:\data")
datafolder = Path('c:\\data')
dataPartitionPath = datafolder / partitionName
print(dataPartitionPath.resolve())
# make dir to ensure it exists before writing any files to it 
dataPartitionPath.mkdir(parents=True, exist_ok=True)


dataPartitionPath.resolve()
Path('c:\\data').as_posix()
