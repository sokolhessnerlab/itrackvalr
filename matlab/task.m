function [subjdata] = csntask_4_et(subjid,et,isreal)
%
% function [subjdata] = contSCtask_11_et(subjid,et,isreal)
% 
% subjid must be a 3-character string (e.g. '003')
% et must be either 1 (use eyetracking) or 0 (don't use eyetracking)
% isreal must be either 1 (do the full study) or 0 (do an abbreviated study)
% 
% DEFAULTS: 
% subjid = '000'
% et = 1
% isreal = 1
%

%% CONTINUOUS SELF-CONTROL TASK:  Clock Task + Distractor

%% DESCRIPTION
%{
script to realize Mackworth's Clock Task in a continuous self-control experiment
uses code derived from clocktask_03.m for the clock 
(clocktask_03.m not required to use this task)

YYYY.MM.DD - UPDATES
2017.04.14 - KAS added edfmat conversion (calls on SR Research mex file "edfmat")
2017.04.12 - KAS updated task instructions
                changed task title to be study-specific "csntask" this
                (comes from the latest version of contsctask_15_et)
2017.04.01 - PSH added calibration check for Eyelink 1000+
2017.03.01 - KAS cleaned up script for best performance/readability
                removed reg/supr conditions
                updated task instructions
                updated output location (Desktop>ContSCtask>output)                
2016.01.07 - KAS updated for unnamed new eye-tracking study at DU
                updated to run w/ Eyelink 1000+ eyetracker
2012.02.22 - PSH updated to accomodate eyetracker, other images, etc.
2012.02.27 - PSH updated to include more explicit image instructions 
                and larger images (with the correct 4x3 WxH ratio)
2012.03.03 - PSH updated with example images, calibration check, 
                and regulation condition
2012.04.02 - PSH updated to record exact timestamp of response and drawing
                of step, as well as contstrain signal presence 
                (constant # of signals, constrain distribution)
2012.04.27 - PSH updated to constrain signal gaps (including start 
                and end of study)
2011.12.20 - created by PSH
%}

%% GENERAL INFO
%{
OPERATING SYSTEM:
script will identify operating system and run appropriate script for that
OS where necessary
(originally created for Mac, then converted to PC, so after changes for DU studies may need some additional testing to run on Mac)

EYETRACKING:
setup to run w/ Eyelink 1000+
script can be run with or w/o eyetracking
uses right eye for data collection

CLOCK & IMAGES:
randomly presents on either side of the screen

NUMBER OF TRIALS:
code included to set to run 15 trials as a test before the full 3600 image
task

%}

%{
isreal specifies whether this is a test run or an experimental run
isreal = 0 (i.e. a test run)
    nT = 15 (abridged number of trials)
    cursor can be accessed (& shows on screen during task)

isreal = 1 (i.e. a true experimental run)
    nT = 3600 (full number of trials)
    cursor is hidden
%}
    if nargin < 3
        isreal = 1; % assume real (unless otherwise specified)
    end 
    
%{
et=0 runs w/o eyetracker
et=1 runs w/ eyetracker
%}
    if nargin < 2
        et=1;    % assume eyetracker (unless otherwise specified)
    end

% Define Subject #
    if nargin < 1
        subjid = '000'; % assume default subjid 000 (unless otherwise specified)
    end     

    
%% DEFINE PATHS
% Define Host PC ADDRESS
    hostName = '100.1.1.1';     %address of the EyeLink 1000+ connection
    
% Determine OS & Define homepath, imagepath, & outputpath

% PATHS FOR TESTING ON PSH'S COMPUTER
%{
 homepath = '/Volumes/AHSS Psychology/shlab/Projects/CSN/Task/ContSCtask/';
 imgpath = 'images/';
 outputpath = 'output/';
%}

    if ispc
        homepath = 'C:\Users\Display\Desktop\ContSCtask\';  %location of task
        imgpath = 'images\';    %location of images
        eximgpath = 'exampleimgs\';
        outputpath = 'output\';     %location for saving output
    elseif ismac   
        homepath = '~/Desktop/ContSCtask/';
        imgpath = 'images/';
        eximgpath = 'exampleimgs/';
        outputpath = 'output/';
    else
        disp('Platform not supported (not recognized as Windows or OSX)')
    end     %end if loop (if ispc...)

% Change Directory to Homepath
    cd(homepath);
    
%% PREPARATION
%{ 
rng ('shuffle') seeds the random number generator based on the current time. if the seed is not shuffled, MATLAB will default to the same set of random numbers continuously
%}
rng('shuffle')   

% Establish Try
%{
the majority of the script is established in this try section. if anything contained within the try statement fails the catch section executes allowing the script to break gracefully rather than throwing an error & locking the user out of the screen
%} 
try
  
% Basic screen & keyboard prep
    KbName('UnifyKeyNames');
    Screen('Preference', 'SkipSyncTests', 1);   % skips sync tests for monitor relay timing (*ideally don't use this, mostly for use during testing w/ dual monitor setup)

% Create Experiment Window    
    [wind, rect] = Screen('OpenWindow',max(Screen('Screens')));

% Define Experiment Window
    Screen('BlendFunction', wind, GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);   % turn on aliasing to allow line to be smooth
    [width,height] = Screen('WindowSize',wind);
    blk = BlackIndex(wind);
    wht = WhiteIndex(wind);
    gry = GrayIndex(wind);
    Screen('TextFont',wind,'Monaco');
    txt_size = 25;

    if isreal
        HideCursor;
    end
    
% Define Response Keys
    resp_keys = {'space'};
    resp_key_codes = KbName(resp_keys);
    esc_key_code = KbName('ESCAPE');
    trig_key_code = KbName('Return');
    
% Define New Screen    
    DrawFormattedText(wind, 'Setting up...', 'center', 'center', blk);
    Screen(wind,'Flip');
    
% Define Clock
    % Define Clock Circle 
    circleratio = 1/3;  % set ratio of circle diameter to the smallest screen dimension
    dmtr = min(rect(rect~=0))/(circleratio^-1); % diameter = 1/2 smallest screen dimension

% Define Image Rectangle
%{
this task runs best when image dimension is 4x3
%}
    imgW = rect(3)*0.48;
    imgH = imgW*.75;    % set height to be 3/4 width

% Create Circle
    circlerect = [(rect(3)-dmtr)/2, (rect(4)-dmtr)/2 ...
        (rect(3)-dmtr)/2 + dmtr, (rect(4)-dmtr)/2 + dmtr];
% Create Image Rectangle    
    imgrect = [(rect(3)-imgW)/2, (rect(4)-imgH)/2 ...
        (rect(3)-imgW)/2 + imgW, (rect(4)-imgH)/2 + imgH];
    
% Set Trial Parameters
    % Define Number of Trials (also # of seconds)
    if isreal
        nT = 3600; % full experiment
    else
        nT = 15; %abridged for testing
    end

% Define "Trial" Length
    tTrial = 1; % length of a single "trial" in seconds

    % Define Double-Tick (aka "signal")    
    psig = 0.01; % probability of receiving a "signal", e.g. an increment of 2 (Mackworth was 0.01 [1/100] over 20 min, 0.00667 [1/150] over 30 min)
    stepstmp = ones(nT,1);
    stepstmp(1:ceil(nT*psig)) = 2; % make sure we have a constant NUMBER of signals, and at least 1
    while 1
        tmp = [randperm(nT)' stepstmp]; %randomly generate actual trial order
        tmp = sortrows(tmp,1);
        gaps = diff([1; find(tmp(:,2)==2); nT]); % find the distance between each signal (including the start & end of study)
        if max(gaps)<360 % if no signals are more than 360s or 6 minutes apart...
            steps = tmp(:,2);
            break
        end     % end if loop (if max(gaps)...)
        % disp('unsuccessful iteration') % for making sure that this was actually iterating 
    end     % end while loop (while 1...)
    steps = [steps zeros(size(steps))];
    resps = zeros(nT,2);
    exT = min(400,nT-1); % number of "practice" trials; just make it really high, they'll probably stop after 20 or so
    stepsEx = (mod(1:exT,4)==0) + 1; % make a big movement every 4th movement   

% Define Clock Line
    % Set Line parameters
    center = [rect(3)/2, rect(4)/2];
    linelength = dmtr/2 - dmtr*.05; % line is 45% of the diameter
    incrmnt = 3.41; % increment in degrees (doesn't fit evenly into 360 deg; will amount to gradual shifting over circles; close to 3.6 deg of original)
    pts = 0:incrmnt:(incrmnt*sum(steps(:,1))); % should be one longer than it needs to be
    pts = [sind(pts); -cosd(pts)]*linelength;
    origin = [0;0];
    lineW = 4; % line width
    
% Get the list of images ready
    image_names = dir([imgpath '*.jpg']); % get all the names
    
    disp(length(image_names)); % diplay image names in command window while loading
    WaitSecs(2);
    
    tic; % starts a stopwatch timer to measure image loading time
    img = zeros(length(image_names),1);
    for i = 1:length(image_names)
        imageID = fullfile(homepath,imgpath,image_names(i).name);
        % disp(imageID) % for use if images are having difficulty
        if ispc
            imgfile = imread(imageID);  % can't use sprintf on PC becuase of the / in the text file
        elseif ismac
            imgfile = imread(sprintf(imageID)); % for mac
        else
            disp('Platform not supported (not recognized as Windows or OSX)')
        end     %end if loop (if ispc...)
        img(i) = Screen(wind, 'MakeTexture',imgfile);
    end     %end for loop (for img = zeros...)
    toc % ends stopwatch timer and returns the elapsed time for loading images
    
    WaitSecs(2);
    
    % For when all images are 1s only and none repeat
    img_ind = img(randperm(length(image_names),nT));

% Define Clock & Image Position
%{
clock and images are randomly positioned for each participant. randomly (p = 0.5) assigns one to left and the other to right
%}
    % Randomly position the clock on the left or right & images vice versa
    lr = rand < 0.5;
    if lr == 0 % LEFT
        circlerect([1 3]) = circlerect([1 3]) - width/4; % CLOCK LEFT
        imgrect([1 3]) = imgrect([1 3]) + width/4; % IMAGE RIGHT
        pts(1,:) = pts(1,:) - width/4;
        origin(1) = origin(1) - width/4;
    elseif lr == 1 % RIGHT
        circlerect([1 3]) = circlerect([1 3]) + width/4; % CLOCK RIGHT
        imgrect([1 3]) = imgrect([1 3]) - width/4; % IMAGE LEFT
        pts(1,:) = pts(1,:) + width/4;
        origin(1) = origin(1) + width/4;
    end     %end if loop (for clock & image position)
    
    % Define Output Variables
%{
this section collects image index, image file name
**future goal:  add in ratings for all images and their categories once we have this info
%}
    subjdata = struct;
    subjdata.nTrials = nT;
    subjdata.pSignal = psig;
    subjdata.resps = resps;
    subjdata.steps = steps;
    subjdata.lr = lr;
    subjdata.img_ind = img_ind; % the numeric indices
    subjdata.image_names = image_names; % the text file names
    
% Waiting For Experimenter Screen
    % Display Text
    DrawFormattedText(wind, 'Waiting for experimenter...', 'center', 'center', blk);
    Screen(wind,'Flip');
    while 1;
        [keyIsDown,~,keyCode] = KbCheck(-1);
        if keyIsDown
            if keyCode(esc_key_code)
                error('Experiment aborted by user!');
            elseif any(keyCode(trig_key_code))
                break
            end
        end     % end if loop
    end     % end while loop

    
%% EXECUTION    

% Define Task Instructions  %this is what will be displayed to participants
    instrA{1} = sprintf('Shortly, you will see a white circle on one side of the screen. A black line will move around the circle like the hand of a clock.');
    instrA{2} = sprintf('The black line will move in small increments roughly every second, but every now and again (p = %.2f; or every 1 in %d movements), instead of the pointer moving in the normal small movement, it will move double the usual distance (a "double movement").\n\n\nPress the SPACE BAR as soon as you notice one of these double movements.\n\n\nThough you should press the SPACE BAR as quickly as possible, you have up to 8 seconds to indicate the presence of a double movement, should you suddenly remember having seen the signal.', psig, 1/psig);
    instrA{3} = sprintf('You will earn $1 for every hit (a response within 8 seconds of a double movement), and lose $1 for every false alarm (responding when no double movement has occurred within the past 8 movements).');
    instrA{4} = sprintf('In the EXAMPLE on the next screen (but NOT in the real study), every 4th movement is a double movement, just so you can get an idea of the size of a double movement.\n\n\nRemember that in the real study, only every 1 in %d movements will be a double movement!\n\n\nFor this example, you do not need to indicate double movements by pressing the space bar; just look for the difference between a single movement and a double movement. After 20 seconds you''ll have the option to continue, but you should take as long as you''d like to look at the double movements.',1/psig);

% Create Instruction Screen
    Screen('FillRect', wind, blk);
    Screen('TextSize', wind, txt_size);
    
    for iind = 1:length(instrA)
        
        DrawFormattedText(wind, 'Instructions', 'center', rect(4)*.1, wht);
        DrawFormattedText(wind, instrA{iind}, 'center', 'center', wht, 45);
        Screen('Flip',wind,[],1);
        
        WaitSecs(1);
                
        DrawFormattedText(wind, 'Press the space bar to continue when ready', 'center', rect(4)*.9, wht);
        Screen('Flip', wind);
        
        while 1;
            [keyIsDown,~,keyCode] = KbCheck(-1);
            if keyIsDown && any(keyCode(resp_key_codes))
                DrawFormattedText(wind, 'Instructions', 'center', rect(4)*.1, wht);
                Screen('Flip', wind);
                break
            elseif keyIsDown && keyCode(esc_key_code)
                error('Experiment aborted by user!');
            end     % end if loop
        end     % end while loop
        
    end     % end for loop
    
    
%% CLOCK EXAMPLE
    WaitSecs(0.5);
    while KbCheck(-1); % don't begin until all keys are released
    end
    
    ind = 0;
    for t = 1:exT
        brklp = false;
        ind = ind + stepsEx(t);
        DrawFormattedText(wind, 'In this example, every 4th movement is a double movement:', 'center', rect(4)*.1, wht, 45);
        if t > 20
            DrawFormattedText(wind, 'Press the space bar when you are ready to continue', 'center', rect(4)*.9, wht);
        end
        Screen('FillOval', wind, wht, circlerect);
        Screen('DrawLines', wind, [origin pts(:,ind)], lineW, blk, center, 1);
        Screen('Flip',wind);
        startT = GetSecs;
        while GetSecs-startT < tTrial;
            [keyIsDown, secs, keyCode] = KbCheck(-1);
            if keyIsDown && keyCode(esc_key_code)
                error('Experiment aborted by user!');
            elseif (t > 20) && keyIsDown && any(keyCode(resp_key_codes))
                brklp = true;
                break
            end
        end
        if brklp
            break
        end
    end
    
    Screen('Flip',wind);
    WaitSecs(1);

    
%% IMAGE EXAMPLE

% Define Image Instructions
    imgintro{1} = sprintf('REMEMBER: in the REAL study, double movements will only happen every 1 IN %d movements on average.',1/psig);
    imgintro{2} = sprintf('There will also be images on the other side of the screen from the circle.\n\n\nThese images include everything from pictures of accidents to plant life to erotic/sexual photos.\n\n\nEach image will only stay on the screen for one (1) second.');
    imgintro{3} = sprintf('To the extent that you care about making money today, you should watch the tip of the moving line to as great an extent as possible during the experiment (as you get paid for detecting the double movements).\n\n\nThe next screen will show you some example images.');
% Define Image Instruction Screen
for iind = 1:length(imgintro)
    DrawFormattedText(wind, 'Instructions', 'center', rect(4)*.1, wht);
    DrawFormattedText(wind, imgintro{iind}, 'center', 'center', wht, 45);
    Screen('Flip',wind,[],1);
    
    WaitSecs(1);
    
    DrawFormattedText(wind, 'Press the space bar to continue', 'center', rect(4)*.9, wht);
    Screen('Flip',wind);
    
    while 1;
        [keyIsDown, secs, keyCode] = KbCheck(-1);
        if keyIsDown && keyCode(esc_key_code)
            error('Experiment aborted by user!');
        elseif keyIsDown && any(keyCode(resp_key_codes))
            DrawFormattedText(wind, 'Instructions', 'center', rect(4)*.1, wht);
            Screen('Flip', wind);
            break
        end
    end
end
    
    WaitSecs(1);
        
% Prep Example Images
% Define Example Iamges
%{ 
these images will also display in the task itself. psh is okay with this.
%}  
%     eximg{1} = 'NAPS_ERO_female_015_v.jpg';  % arousing positive
%     eximg{2} = 'OASIS_cotton_swabs_2.jpg';  % neutral
%     eximg{3} = 'NAPS_BE_faces_021_h.jpg';  % arousing negative    
    eximg{1} = 'EM0593.jpg';  % arousing positive
    eximg{2} = 'EM0209.jpg';  % neutral
    eximg{3} = 'EM0319.jpg';  % arousing negative    
   
    for i = 1:length(eximg)
        imageID = fullfile(homepath,imgpath,eximgpath,eximg{i});
        %disp(imageID)   % for use if images are having difficulty
        imgfile = imread((imageID));
       
        eximgtxtr(i) = Screen(wind, 'MakeTexture', imgfile);
    end    % end for loop (for i = l:length...)
    
    for i = 1:length(eximgtxtr)
        Screen('DrawTexture',wind,eximgtxtr(i),[],imgrect);
        Screen('Flip',wind,[],1);
        
        WaitSecs(1);

        DrawFormattedText(wind,'Press the space bar to continue', 'center', rect(4)*.9, wht);
        Screen('Flip',wind);
        
        while 1;
            [keyIsDown, secs, keyCode] = KbCheck(-1);
            if keyIsDown && keyCode(esc_key_code)
                error('Experiment aborted by user!');
            elseif keyIsDown && any(keyCode(resp_key_codes))
                Screen('Flip',wind);
                break
            end % end if (if keyIsDown...)
        end % end while (while 1;)
        
    end % end for (for i = l:length...)
    
    WaitSecs(1);

%% QUIZ SCREEN
% Define Quiz Screen Instructions
    quizInstr = sprintf('You''ve now read all the instructions.\n\n\nTo make sure we''re on the same page, we now have a brief, straightforward quiz about the study.\n\n\nPlease let the experimenter know that you''re ready to begin!');
             
% Define Quiz Screen    
    Screen('TextSize', wind, txt_size);
    DrawFormattedText(wind, quizInstr, 'center', 'center', wht, 45);
    Screen('Flip', wind);
    
    while 1;
        [keyIsDown, secs, keyCode] = KbCheck(-1);
        if keyIsDown && keyCode(esc_key_code)
            error('Experiment aborted by user!');
        elseif keyIsDown && any(keyCode(trig_key_code))
            Screen('Flip', wind);
            break
        end
    end
    
    WaitSecs(1);    % to prevent the space bar press from contaminating the responses

%% BEGIN CALIBRATION SCREEN
% Define Begin Calibration Screen
    if(et)
        Screen('TextSize', wind, txt_size);
        DrawFormattedText(wind,'Ready to begin calibration!\n\nWaiting for Experimenter...', 'center', rect(4)*.1, wht, 45);
        Screen('Flip', wind);
        
        while 1;
            [keyIsDown, secs, keyCode] = KbCheck(-1);
            if keyIsDown && keyCode(esc_key_code)
                error('Experiment aborted by user!');
            elseif keyIsDown && any(keyCode(trig_key_code))
                Screen('Flip', wind);
                break
            end
        end
        
        WaitSecs(1);    % prevents space bar press from contaminating responses
        
%% CALIBRATION
       el=EyelinkInitDefaults(wind); 
       if EyelinkInit % Note that this acts both as a logical and as a function - calling it initializes the Eyelink connection
           
           for k = 1:9
               if ~exist(sprintf('CSN%sS%d.edf',subjid,k),'file')
                    fname = sprintf('CSN%sS%d.edf',subjid,k);
                    break
                end
            end
            
            Eyelink('command', 'link_sample_data = LEFT,RIGHT,GAZE,AREA');      % make sure that we get gaze data from the Eyelink
            Eyelink('command', 'file_sample_data = LEFT,RIGHT,GAZE,AREA');      % make sure that we get gaze data from the Eyelink % THIS MAY NEED TO BE LOWER (AFTER START REC)
            Eyelink('openfile', fname);     % open file to record data to (NAME MUST BE 1-8 CHAR, LETTER OR NUM ONLY)
            Eyelink('StartRecording');  % do this so calibration & validation results are output into the EDF file
            
% Calibrate Eye Tracker
            EyelinkDoTrackerSetup(el);
            
            WaitSecs(0.1);
            eye_used = el.RIGHT_EYE;    % use right eye
        else
            error('Eyelink initialization failed');
        end
        
        disp('passed calibration')
        
        Screen('FillRect', wind, blk);    % set background again (calibration replaces it)
        DrawFormattedText(wind, 'Calibration done.\n\nWaiting for the Experimenter', 'center', 'center', wht, 40);
        Screen('Flip', wind);
        while 1
            [keyIsDown,~,keyCode] = KbCheck(-1);
            if (keyIsDown && size(find(keyCode), 2) == 1);
                if any(keyCode(trig_key_code))
                    break;
                end
            end
        end
        Screen('Flip', wind);
        WaitSecs(1);
    end
    
    
    priorityLevel=MaxPriority(wind);
    Priority(priorityLevel);
  
%% EXPERIMENT BEGINNING SCREEN
% Define Experiment Beginning Screen
    Screen('FillRect', wind, blk);
    Screen('TextSize', wind, txt_size);
    DrawFormattedText(wind,'When you''re ready, press the space bar to begin the experiment!', 'center', rect(4)*.1, wht, 45);
    Screen('Flip', wind);
   
    while 1;
        [keyIsDown, secs, keyCode] = KbCheck(-1);
        if keyIsDown && keyCode(esc_key_code)
            error('Experiment aborted by user!');
        elseif keyIsDown && any(keyCode(resp_key_codes))
            Screen('Flip', wind);
            break
        end
    end
    
    WaitSecs(1); % to prevent the space bar press from contaminating the responses

%% INITIATE EXPERIMENT
% Define Experiment Screen
    Screen('FillOval', wind, wht, circlerect)
    Screen('FrameRect', wind, wht, imgrect)
    Screen('Flip', wind);
    WaitSecs(0.8);
    
    % Wait until all keys on keyboard are released:
    while KbCheck(-1); WaitSecs(0.05); end;
    
    
    
%% BEGIN TRIALS
% Open SUBJ Temp File    
    fid = fopen([homepath outputpath 'tmp_csntask_subjCSN' subjid '_' num2str(now) '.txt'],'w');
    ind = 0;    % line index for clock

% Initiate Eyelink    
    if et
        Eyelink('StartRecording');
    end

% Begin Trial Loop
    expBegin = GetSecs;
    for t = 1:nT
%         startT = GetSecs;
        ind = ind + steps(t,1);
        Screen('FillOval', wind, wht, circlerect);
        Screen('DrawLines', wind, [origin pts(:,ind)], lineW, blk, center,1);
        Screen('DrawTexture', wind, img_ind(t),[],imgrect);
        Screen('Flip', wind);
        steps(t,2) = GetSecs-expBegin;      % record the time of drawing of this line & image
        subjdata.resps = resps;
        subjdata.steps = steps;
        subjdata.expBegin = expBegin;
        subjdata.expEnd = GetSecs;
        while (GetSecs-expBegin) < (tTrial*t);      % CUMULATIVE TIMING
            [keyIsDown,secs,keyCode] = KbCheck(-1);
            if keyIsDown && keyCode(esc_key_code)
                error('Experiment aborted by user!');
            elseif keyIsDown && any(keyCode(resp_key_codes))        % if they push a response button
                resps(t,1) = 1;     % record that response
                resps(t,2) = secs-expBegin;      % and its overall time
            end     % end if loop (if keyIsDown...)
        end     % end while loop (while (GetSecs-...)
        fprintf(fid,'RESP,%d,STEP,%d,TIME,%0.3f,CTIME,%0.3f\n',resps(t,1),steps(t,1),GetSecs,GetSecs-expBegin); %print all the info about the trials to the output
    end
    
%% CALIBRATION CHECK
    if(et)
        
        DrawFormattedText(wind,'You''re done with the task portion of the study.\n\n\nBefore we finish, we need to validate the calibration once more. This should take less than a minute.\n\n\nPlease do not adjust your chair or stand up yet, to ensure accurate calculations during validation.\n\n\nThe experimenter will be in shortly to help you with this.', 'center', 'center', wht, 45);
        Screen('Flip', wind);
        
        beep; WaitSecs(0.25); 
        beep; WaitSecs(0.25); 
        beep; WaitSecs(1); 
        
        beep; WaitSecs(0.25); 
        beep; WaitSecs(0.25); 
        beep;
        
        while 1;
            [keyIsDown, secs, keyCode] = KbCheck(-1);
            if keyIsDown && keyCode(esc_key_code)
                error('Experiment aborted by user!');
            elseif keyIsDown && any(keyCode(trig_key_code))
                Screen('Flip', wind);
                break
            end
        end
        
        Screen('TextSize', wind, txt_size);
        DrawFormattedText(wind,'Ready for final validation!\n\nWaiting for Experimenter...', 'center', rect(4)*.1, wht, 45);
        Screen('Flip', wind);
        
        while 1;
            [keyIsDown, secs, keyCode] = KbCheck(-1);
            if keyIsDown && keyCode(esc_key_code)
                error('Experiment aborted by user!');
            elseif keyIsDown && any(keyCode(trig_key_code))
                Screen('Flip', wind);
                break
            end
        end
        
        WaitSecs(1);    % prevents space bar press from contaminating responses
        
        EyelinkDoTrackerSetup(el);
        
        disp('passed re-validation')
        
        Screen('FillRect', wind, blk);    % set background again (calibration replaces it)
        Screen('Flip',wind);

        WaitSecs(1);
    end
    
%% EXPERIMENT END SCREEN
    % Define Experiment End Screen
    Screen('TextSize', wind, txt_size);

    % Figure out participants' "HITS" for payment purposes
    dbmvst = steps(steps(:,1)==2,2);    % **** what are steps? SEE DEFINITION EARLIER IN SCRIPT
    respst = resps(resps(:,1)==1,2);    % **** record responses? SEE DEFINITION EARLIER IN SCRIPT
    ht = 0; fa = 0;
    for i = 1:length(respst)    % go response by response
        if any((dbmvst < respst(i)) & (dbmvst > (respst(i)-8)))
            % if any double movements occurred within 8s before the response...
            ht = ht + 1; % call it a hit
        else
            fa = fa + 1; % otherwise, it's a false alarm
        end     % end if loop (if any...)
    end     % end for loop (for i=l:length...)
    
    % Define Experiment End Screen Text
    otctxt = sprintf('You''re finished!\n\n%d hits - %d false alarms = $%d bonus.', ht, fa, max(ht-fa,0));
    DrawFormattedText(wind,otctxt, 'center', 'center', wht, 45);
    Screen('Flip', wind);

%% SAVE FILES & CLOSE OUT

% End Eyetracking Recording
    if et
        Eyelink('StopRecording'); WaitSecs(1);
        Eyelink('CloseFile'); WaitSecs(4);
        tmpt = GetSecs;
        while (GetSecs - tmpt) < 20     % try for 20 seconds
            
            % Import .edf Output File (eyetracking) & Convert to .mat
            %{
            Eyelink creates an .edf file with the eyetracking data
            script imports this then uses a mex file from SR Research to convert .edf to .mat
            %}
            recsucc = Eyelink('ReceiveFile')
            if recsucc > 0
                break
            end
        end
        Eyelink('Shutdown');        % disengage eyelink
        try
            system(sprintf('cd %s',homepath));
            system(sprintf('edfmat_win64 %s %.8s.mat',fname,fname)); % convert edf file to mat file
        catch
            fprintf('Conversion from EDF to MAT failed.\n');    % if asc to mat conversion fails, give this error message
        end     % end try loop (try edfmat...)
    end     % end if loop (if et...)
    
    fclose(fid);

% Save Output File (behavioral; .mat)
    save([outputpath, 'csntask_subjCSN' subjid '_' num2str(now) '.mat'], 'subjdata');
    
    while 1
        [keyIsDown,~,keyCode] = KbCheck(-1);
        if (keyIsDown && size(find(keyCode), 2) == 1);
            if any(keyCode(trig_key_code))
                break;
            end
        end
    end
    
    sca     % close the experiment window
    Priority(0);    % resets window priority level (gives control back to user)
    
    
% DEFINE CATCH SECTION
catch ME
%{
"catch" section executes in case of an error in the "try" section above
importantly, it closes the onscreen window if open
ME is MATLAB Exception (aka the error code thrown during the try block)
%}
    if et
        try
            Eyelink('StopRecording');
            Eyelink('CloseFile')
            Eyelink('ReceiveFile')
            system(sprintf('cd %s',homepath));
            system(sprintf('edfmat_win64 %s %.8s.mat',fname,fname)); % convert edf file to mat file
            Eyelink('Shutdown');
        catch
            Eyelink('Shutdown');
        end     % end try loop (try Eyelink...)
    end     % end if loop (if et...)
    
    try
        fclose(fid);
    end
    
    try
        save([outputpath,'csntask_subjCSN' subjid '_' num2str(now) '.mat'],'subjdata');     % save subjdata to outputpath w/ file name "csctask_subjCSCXXX_MATLABTIME.mat"
    end
    
    Priority(0);
    sca;
    rethrow(ME)
    
end %   try..catch..
end % end function
