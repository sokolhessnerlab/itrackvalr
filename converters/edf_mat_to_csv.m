% CSN Convert Completed Eyelink Data Format MAT to CSV

% Set up datapath
datapath = '/Users/metis/Projects/shlab/mounts/csn/data';

% List data directory contents and extract files starting with
% 'csntask_subjCSN', which signify behavioral task responses
dir_list = dir(datapath);
dir_contents = {dir_list.name};
filenames = dir_contents(startsWith(dir_contents, 'csntask_subjCSN'));

% Note incompletes (and non-participant)
incompletes = {'CSN003'
               'CSN013'
               'CSN014'
               'CSN024'
               'CSN028'
               'CSN031'
               'CSN046'
               'CSN999'};

% Navigate into data directory
