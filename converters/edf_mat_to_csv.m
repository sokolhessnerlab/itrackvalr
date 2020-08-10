% CSN Convert Completed Eyelink Data Format MAT to CSV

% Set root data path
datapath = '/Users/metis/Projects/shlab/mounts/csn/data';

% Set path to raw behavioral MAT files
raw_eyetracker_responses_path = fullfile(datapath, ...
                                         'raw', ...
                                         'eyetracker', ...
                                         'responses');

% Set path to incomplete participants list
incomplete_path = fullfile(datapath, ...
                           'raw', ...
                           'incomplete', ...
                           'participants.txt');

% Set output path for converted CSV files
output_path = fullfile(datapath, ...
                       'raw_csv', ...
                       'eyetracker');

% List data directory contents and extract files starting with
% 'CSN', which signify eyetracker responses
dir_list = dir(raw_eyetracker_responses_path);
dir_contents = {dir_list.name};
filenames = dir_contents(startsWith(dir_contents, 'CSN'));

% Import incompletes (and non-participant 999)
incompletes = importdata(incomplete_path);

% Navigate into data directory
cd(raw_eyetracker_responses_path)

for i = 1:length(filenames)

  fn = filenames{i};
  participant_id = string(regexp(fn, 'CSN\d{3}', 'match'));

  % Skip to next participant if current is incomplete
  if ismember(participant_id, incompletes)
    continue
  end

  etd = load(fn).Edf2Mat;

  %%%%
  %
  % Create a folder for each participant, and then
  % a CSV for each field in the Edf2Mat struct, i.e.:
  %
  %   CSNXXX/
  %     - fsample.csv
  %     - fevent.csv
  %     - ioevent.csv
  %     - recordings.csv
  %
  %%%%
  p_output_path = fullfile(output_path, participant_id);

  % create participant directory in output path if does not already exist
  if ~exist(p_output_path, 'dir')
    mkdir(output_path, participant_id);
  end
  
  % FSAMPLE struct to fsample.csv
  time = transpose(etd.FSAMPLE.time);
  gx = transpose(etd.FSAMPLE.gx);
  gx = gx(:,2);
  writetable(table(time, gx), ...
             fullfile(p_output_path, 'fsample.csv'), ...
             'Delimiter', ',', 'QuoteStrings', true);
  
  % FEVENT struct to ffevent.csv
  writetable(struct2table(etd.FEVENT), ...
             fullfile(p_output_path, 'fevent.csv'), ...
             'Delimiter', ',', 'QuoteStrings', true);

  % IOEVENT struct to ioevent.csv
  writetable(struct2table(etd.IOEVENT), ...
             fullfile(p_output_path, 'ioevent.csv'), ...
             'Delimiter', ',', 'QuoteStrings', true);

  % RECORDINGS struct to recordings.csv
  writetable(struct2table(etd.RECORDINGS), ...
             fullfile(p_output_path, 'recordings.csv'), ...
             'Delimiter', ',', 'QuoteStrings', true);

end
