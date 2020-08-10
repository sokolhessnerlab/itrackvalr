% CSN Convert Completed Participants' MAT to CSV

% Set root data path
datapath = '/Users/metis/Projects/shlab/mounts/csn/data';

% Set path to raw behavioral MAT files
raw_behavioral_responses_path = fullfile(datapath, ...
                                         'raw', ...
                                         'behavioral', ...
                                         'responses');

% Set path to incomplete participants list
incomplete_path = fullfile(datapath, ...
                           'raw', ...
                           'incomplete', ...
                           'participants.txt');

% Set output path for converted CSV files
output_path = fullfile(datapath, ...
                       'raw_csv', ...
                       'behavioral');

% List data directory contents and extract files starting with
% 'csntask_subjCSN', which signify behavioral task responses
dir_list = dir(raw_behavioral_responses_path);
dir_contents = {dir_list.name};
filenames = dir_contents(startsWith(dir_contents, 'csntask_subjCSN'));

% Import incompletes (and non-participant 999)
incompletes = importdata(incomplete_path);

% Navigate into data directory
cd(raw_behavioral_responses_path)

% Convert each complete participant behavioral task response
% from .mat to .csv
for i = 1:length(filenames)

  fn = filenames{i};
  participant_id = string(regexp(fn, 'CSN\d{3}', 'match'));
  
  % Skip to next participant if current is incomplete
  if ismember(participant_id, incompletes)
    continue
  end
  
  bd = load(fn).subjdata;
  
  N_TRIALS = bd.nTrials;
  BASE_BOOL = true(N_TRIALS, 1);
  BASE_ONES = ones(N_TRIALS, 1);
  
  trial = transpose(1:N_TRIALS);
  clock_side = BASE_BOOL * bd.lr;
  p_signal = BASE_ONES * bd.pSignal;
  resp = bd.resps;
  step = bd.steps;
  img_index = bd.img_ind;

  participant_table = table(trial, clock_side, p_signal, resp, ...
                      step, img_index);
  
  participant_fn = participant_id + '.csv';

  writetable(participant_table, fullfile(output_path, participant_fn), ...
             'Delimiter', ',', 'QuoteStrings', true);
  
end

