% Convert Completed Eyelink Data Format MAT to CSV

% Set root data path
datapath = '/Volumes/shlab/Projects/CSN/data';

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

% Set output path `extracted` for converted CSV files
output_path = fullfile(datapath, ...
                       'extracted', ...
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

  % FSAMPLE struct to fsample.csv with specified fields

  % time (milliseconds)
  time = transpose(etd.FSAMPLE.time);

  % gaze position horizontal (pixels)
  gx = transpose(etd.FSAMPLE.gx);
  gx = gx(:,2); % right eye only

  % gaze position vertical (pixels)
  gy = transpose(etd.FSAMPLE.gy);
  gy = gy(:,2); % right eye only

  fsample_table = table(time, gx, gy);
  writetable(fsample_table, ...
             fullfile(p_output_path, 'fsample.csv'), ...
             'Delimiter', ',', 'QuoteStrings', true);

  % FEVENT struct to fevent.csv
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
