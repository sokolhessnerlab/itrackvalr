% CSN Data Check & Basic Analysis

% Set up datapath
datapath = '/Users/sokolhessner/Documents/Dropbox/Academics/Research/CSN-CONTINUOUS_SC_NOREPS/databackups/Data-2017_10/';
cd(datapath);

% Identify subject IDs manually (incl. accounting for missing data)
% NOTE: needs to be updated to accommodate participants through 057

subjIDs = {
    'CSN001'
    'CSN002'
...    'CSN003' % did not complete (calib)
    'CSN004'
    'CSN005'
    'CSN006'
    'CSN007'
    'CSN008'
    'CSN009'
    'CSN010'
    'CSN011'
    'CSN012'
...    'CSN013' % ???
...    'CSN014' % did not complete (calib)
    'CSN015'
    'CSN016'
    'CSN017'
    'CSN018'
    'CSN019'
    'CSN020'
    'CSN021'
    'CSN022'
    'CSN023'
...    'CSN024' % illness
    'CSN025'
    'CSN026'
    'CSN027'
...    'CSN028' % did not complete (calib)
    'CSN029'
    'CSN030'
...    'CSN031' % did not complete (calib)
    'CSN032'
    'CSN033'
    'CSN034'
    'CSN035'
    'CSN036'
    'CSN037'
    'CSN038'
    'CSN039'
    'CSN040'
    'CSN041'
    'CSN042'
    'CSN043'
    'CSN044'
    'CSN045'
...    'CSN046' % did not complete (disturbing)
    };


% Basic numbers

nS = length(subjIDs);

subjIDsN = nan(length(subjIDs),1);
for s = 1:nS
    subjIDsN(s) = str2num(subjIDs{s}(4:6));
end




%%%%%%%%% Behavioral Analysis %%%%%%%%%%%%

% Prep, plot, and analyze the behavioral data
behav = struct;
behav.lr = [];
behav.resps = nan(3600,2,nS); % resps = whether participants pressed the space bar to indicate perception of a double-tick
behav.steps = nan(3600,2,nS); % clock movements
behav.img_ind = nan(3600,1); % images

HT = zeros(nS,1); % hit (said double movement, was a double movement)
MI = zeros(nS,1); % miss (said nothing, was a double movement)
FA = zeros(nS,1); % false alarm (said double movement, was nothing)
CR = zeros(nS,1); % correct rejection (said nothing, was nothing)

% H/M/F/C by half of the study
HThalf = zeros(nS,2); 
MIhalf = zeros(nS,2);
FAhalf = zeros(nS,2);
CRhalf = zeros(nS,2);

% reaction times
rts = nan(36,nS);
rtsHalf = nan(36,2,nS); % ... incl. by half

bRTtime = nan(2,nS); % regression beta values for RTs by time
pRTtime = nan(2,nS); % regression p-values for RTs by time
tbl = nan(0,3); 
tbl2 = nan(0,3);

for s = 1:nS
    fn = dir(sprintf('csntask_subj%s*.mat',subjIDs{s}));
    load(fn(end).name);
    behav.lr(s) = subjdata.lr; % clock left or right for this participant
    behav.resps(:,:,s) = subjdata.resps;
    behav.steps(:,:,s) = subjdata.steps;
    behav.img_ind(:,:,s) = subjdata.img_ind;
    
    % Figure out participants' "HITS" (code taken from payment calc)
    dbmvst = behav.steps(behav.steps(:,1,s)==2,2,s);
    respst = behav.resps(behav.resps(:,1,s)==1,2,s);
    htfa = zeros(length(respst),1); % 1 = hit, 0 = FA
    htmi = nan(length(dbmvst),1); % 1 = hit, 0 = Miss
    
    for i = 1:length(dbmvst) % go trial-by-trial
        if any((respst > dbmvst(i)) & (respst < (dbmvst(i)+8))) % look w/in allowable response window
            HT(s) = HT(s) + 1; % increment this participant's hit counter
            htmi(i) = 1; % mark this movement as a hit
            rind = find((respst > dbmvst(i)) & (respst < (dbmvst(i)+8)),1);
            htfa(rind) = 1; % mark this response as a hit
            rts(i,s) = respst(rind)-dbmvst(i);
            
            if respst(rind) < (3600/2) % if first half
                HThalf(s,1) = HThalf(s,1) + 1;
                rtsHalf(i,1,s) = rts(i,s);
            else
                HThalf(s,2) = HThalf(s,2) + 1;
                rtsHalf(i,2,s) = rts(i,s);
            end
            
            respst(rind) = nan; % erase this response so it's not double-counted
            
        else % if there weren't responses in the 8s after this double movement... 
            htmi(i) = 0; % mark this double movement as missed
            MI(s) = MI(s) + 1; % increase this participant's miss counter
            if dbmvst(i) < (3600/2) % if first half
                MIhalf(s,1) = MIhalf(s,1) + 1;
            else
                MIhalf(s,2) = MIhalf(s,2) + 1;
            end
        end
    end
    
    % Calculate FAs on the basis of responses remaining after all the hits
    % were removed (see above code)
    FA(s) = sum(isfinite(respst));
    FAhalf(s,1) = sum(isfinite(respst(respst<1800)));
    FAhalf(s,2) = sum(isfinite(respst(respst>=1800)));
    
    % Calculate CRs on the basis of FAs
    CR(s) = (3600-36) - FA(s); % the number of non-signals - FAs
    CRhalf(s,1) = (1800-sum(dbmvst<1800)) - FAhalf(s,1);
    CRhalf(s,2) = (1800-sum(dbmvst>=1800)) - FAhalf(s,2);
    
    % Predict reaction times for hits with trial number (aka time) for
    % individual participants
    [b,stats] = robustfit(dbmvst(isfinite(rts(:,s)))/3600*2-1,rts(isfinite(rts(:,s)),s));
    bRTtime(:,s) = b;
    pRTtime(:,s) = stats.p;
    
    % pull out RTs into tables
    tbl = [tbl; rts(isfinite(rts(:,s)),s) dbmvst(isfinite(rts(:,s))) repmat(s,HT(s),1)];
    tbl2 = [tbl2; htmi dbmvst repmat(s,36,1)];
    
end

[~,p] = ttest(bRTtime(2,:)) % are the individual RFX estimates above 0? (yes)

% Do multi-level regression predicting RTs w/time
tbl(:,2) = tbl(:,2)/3600*2-1; % normalize time from -1 to 1
lme = fitlme(table(tbl(:,1),tbl(:,2),tbl(:,3),'VariableNames',{'RT','Time','SUBJ'}),...
    'RT~Time+(1+Time|SUBJ)');
% RTs have a strong positive relationship with time-in-study

% Do multi-level regression predicting p(hit) w/time
tbl2(:,2) = tbl2(:,2)/3600*2-1; % normalize time from -1 to 1
glme = fitglme(table(tbl2(:,1),tbl2(:,2),tbl2(:,3),'VariableNames',{'htmi','Time','SUBJ'}),...
    'htmi~Time+(1+Time|SUBJ)','Distribution','Binomial');
% time-in-study has a strong negative effect on p(hit)

% Calculate signal detection metrics
pHT = HT./(HT + MI);
pMI = MI./(HT + MI);
pCR = CR./(CR + FA);
pFA = FA./(CR + FA);
dprime = norminv(pHT) - norminv(pFA);
dprime(isinf(dprime)) = norminv(pHT(isinf(dprime))) - norminv(1/3600); % pad any no-FA people with 1/2N standard correction

% sig. det. metrics by half of the study
pHThalf = HThalf./(HThalf + MIhalf);
pMIhalf = MIhalf./(HThalf + MIhalf);
pCRhalf = CRhalf./(CRhalf + FAhalf);
pFAhalf = FAhalf./(CRhalf + FAhalf);
dprimehalf = norminv(pHThalf) - norminv(pFAhalf);
dprimehalf(isinf(dprimehalf)) = norminv(pHThalf(isinf(dprimehalf))) - norminv(1/1800); % pad any no-FA people with 1/2N standard correction

% Set up plotting
figN = 0;

% Hits & False alarms
figN = figN + 1;
figure(figN); clf
subplot(1,3,1:2)
plot(pFA,pHT,'ro')
xlim([0 max(pFA)*1.2])
ylim([0 1])
xlabel('p(False Alarm)')
ylabel('p(Hit)')
title(sprintf('ROC Curve (N = %d)',nS))

subplot(1,3,3)
boxplot(HT-FA,'widths',.5)
% plot(ones(size(HT))+randn(size(HT))*.03,HT-FA,'ko')
plotSpread(HT-FA,'distributionMarkers',{'o'},'distributionColors',{'k'})
ylabel('Hits - False Alarms (Payment)')
set(gca,'XTickLabel','')

% Change in detection metrics by half of study
% d-prime
figN = figN + 1;
figure(figN); clf;
subplot(1,4,1)
[~,p] = ttest(dprimehalf(:,1),dprimehalf(:,2));
plot(dprimehalf(:,1),dprimehalf(:,2),'ko')
title(sprintf('D-prime by Study Half (p = %.3f)',p))
xlabel('First Half');
ylabel('Second Half');
xl = xlim; yl = ylim; 
minl = min([xl yl]);
maxl = max([xl yl]);
hold on; plot([minl maxl],[minl maxl],'k')

% hits
subplot(1,4,2)
[~,p] = ttest(pHThalf(:,1),pHThalf(:,2));
plot(pHThalf(:,1),pHThalf(:,2),'bo')
title(sprintf('p(Hit) by Study Half (p = %.3f)',p))
xlabel('First Half')
ylabel('Second Half')
xl = xlim; yl = ylim;
minl = min([xl yl]);
maxl = max([xl yl]);
hold on; plot([minl maxl],[minl maxl],'k')

% false alarms
subplot(1,4,3)
[~,p] = ttest(pFAhalf(:,1),pFAhalf(:,2));
plot(pFAhalf(:,1),pFAhalf(:,2),'ro')
title(sprintf('p(False Alarm) by Study Half (p = %.3f)',p))
xlabel('First Half')
ylabel('Second Half')
xl = xlim; yl = ylim;
minl = min([xl yl]);
maxl = max([xl yl]);
hold on; plot([minl maxl],[minl maxl],'k')

% mean RTs
subplot(1,4,4)
mrtsHalf = squeeze(nanmean(rtsHalf));
[~,p] = ttest(mrtsHalf(1,:),mrtsHalf(2,:));
plot(mrtsHalf(1,:),mrtsHalf(2,:),'gx')
title(sprintf('RT by Study Half (p = %.3f)',p))
xlabel('First Half')
ylabel('Second Half')
xl = xlim; yl = ylim;
minl = min([xl yl]);
maxl = max([xl yl]);
hold on; plot([minl maxl],[minl maxl],'k')
set(gcf,'Position',[100 100 1100 225])

% Boxplots of detection metrics
% d-prime
figN = figN + 1;
figure(figN);clf;
subplot(1,3,1)
boxplot(dprime); hold on;
plot(ones(size(dprime))+randn(size(dprime))*.03,dprime,'ko')
set(gca,'XTickLabel','')
ylabel('d''')

% hits
subplot(1,3,2)
boxplot(pHT); hold on;
plot(ones(size(pHT))+randn(size(pHT))*.03,pHT,'ko')
ylabel('p(Hit)')
set(gca,'XTickLabel','')
ylim([0 1])

% FAs
subplot(1,3,3)
boxplot(pFA); hold on;
plot(ones(size(pFA))+randn(size(pFA))*.03,pFA,'ko')
ylabel('p(False Alarm)')
set(gca,'XTickLabel','')
ylim([0 (max(pFA)*1.1)])

% Boxplot of mean reaction times
figN = figN + 1;
figure(figN); clf;
boxplot(rts); hold on;
plot(1:nS,nanmean(rts),'ko','markersize',10)
xlabel('Participants')
ylabel('Reaction time (s)')
set(gca,'XTickLabel',subjIDs)
legend('Means')

% Load & process questionnaires
ptqD = importdata('CSNQuestionnaireCoding_ptq.csv',',',1);
pssD = importdata('CSNQuestionnaireCoding_pss.csv',',',1);
erqD = importdata('CSNQuestionnaireCoding_erq.csv',',',1);
nfcD = importdata('CSNQuestionnaireCoding_nfc.csv',',',1);
dem = importdata('CSN_agegender.csv',',',1);

male = dem.data(subjIDsN,2);
age = dem.data(subjIDsN,3);

% Perceived stress scale
pssrev = [4 5 7 8]; pssfor = [1:3 6 9 10]; % reverse & forward-coded items from the PSS
pss = sum(pssD.data(:,pssfor+1),2) + (sum(-pssD.data(:,pssrev+1)+4,2));
pss = pss(subjIDsN);

% Emotion Regulation Questionnaire
erqRind = [1, 3, 5, 7, 8, 10]; erqSind = [2, 4, 6, 9]; % reappraisal & suppression items from the ERQ
erqR = sum(erqD.data(:,erqRind+1),2);
erqS = sum(erqD.data(:,erqSind+1),2);
erqR = erqR(subjIDsN);
erqS = erqS(subjIDsN);

% Need for Cognition
nfcfor = [1 2 6 10 11 13 14 15 18]; nfcrev = [3 4 5 7 8 9 12 16 17]; % reverse & forward-coded items from the NFC
nfc = sum(nfcD.data(:,nfcfor+1),2) + (sum(-nfcD.data(:,nfcrev+1)+6,2));
nfc = nfc(subjIDsN);

diffi = ptqD.data(subjIDsN,2); % 1-9 very easy to very difficult
tired = ptqD.data(subjIDsN,3); % 1-9 extremely to not at all tired
ctrl = ptqD.data(subjIDsN,4); % 1-7 uncontrolled to completely intentional
attrM = ptqD.data(subjIDsN,5); % 1-7 not at all to very attracted to males
attrF = ptqD.data(subjIDsN,6); % 1-7 not at all to very attracted to females
sexor = ptqD.data(subjIDsN,7); % 1 = hetero only; 2 = hetero mostly; 3 = hetero somewhat more; 4 = bisexual; 5 = gay somewhat more; 6 = gay mostly; 7 = gay only.

% PSS/ERQ/NFC correlations
[r,p] = corr(pss,nfc,'rows','complete') % n.s.
[r,p] = corr(pss,erqR,'rows','complete') % n.s.
[r,p] = corr(pss,erqS,'rows','complete') % n.s.
[r,p] = corr(nfc,erqR,'rows','complete') % n.s.
[r,p] = corr(nfc,erqS,'rows','complete') % -0.32, p = 0.054 (also trend w/ Spearman)
[r,p] = corr(erqR,erqS,'rows','complete') % -0.47, p = 0.003 (also sig w/ Spearman)

% PSS & difficulty/tired/controlled
[r,p] = corr(pss,diffi,'rows','complete') % n.s.
[r,p] = corr(pss,tired,'rows','complete') % n.s.
[r,p] = corr(pss,ctrl,'rows','complete') % n.s.

% NFC & difficulty/tired/controlled
[r,p] = corr(nfc,diffi,'rows','complete') % n.s.
[r,p] = corr(nfc,tired,'rows','complete') % n.s.
[r,p] = corr(nfc,ctrl,'rows','complete') % n.s.

% ERQ reappraisal & difficulty/tired/controlled
[r,p] = corr(erqR,diffi,'rows','complete') % -0.37, p = 0.02 (n.s. w/ Spearman)
[r,p] = corr(erqR,tired,'rows','complete') % n.s.
[r,p] = corr(erqR,ctrl,'rows','complete') % 0.33, p = 0.04 (n.s. w/ Spearman)

% ERQ suppression & difficulty/tired/controlled
[r,p] = corr(erqS,diffi,'rows','complete') % n.s.
[r,p] = corr(erqS,tired,'rows','complete') % 0.40, p = 0.01 (also sig w/ Spearman)
[r,p] = corr(erqS,ctrl,'rows','complete') % n.s.

% Difficulty/Tired/Controlled
[r,p] = corr(diffi,tired,'rows','complete') % n.s.
[r,p] = corr(diffi,ctrl,'rows','complete') % n.s. (trend neg?)
[r,p] = corr(ctrl,tired,'rows','complete') % n.s.




%%%%%%%%% Eye-tracking (ET) Analysis %%%%%%%%%%%%

% Prep, plot, and analyze the eye tracking data
figN = figN + 1;

w = 1280; h = 1024; % the size of the screen (see etd.FEVENT info).
imgW = w*.48; imgH = imgW*.75; % size of the images were relative to screen size
circleratio = 1/3;  % set ratio of circle diameter to the smallest screen dimension
dmtr = min([w h])/(circleratio^-1); % diameter of the clock
center = [w/2, h/2]; % center of the screen

npxppx = 16; % number of real data pixels per plot pixel for purposes of visualization (must be factor of 2)
imgmtx = nan(h/npxppx,w/npxppx,nS);
nout = nan(nS);

%{
 To find the correct cutoffs...
 1) look at etd.RECORDINGS(3).time
 2) Go 3,600 seconds forward of that.
%}

for s = 1:nS
    tic; % time this process
    figure(figN);clf; 
    set(gcf, 'Position', [100, 100, 1000, 1000]);
    fn = dir(sprintf('%s*.mat',subjIDs{s}));
    etd = load(fn(end).name); etd = etd.Edf2Mat; % load the eyetracking (ET) data
    
    imgRect = [(w-imgW)/2, (h-imgH)/2, (w-imgW)/2 + imgW, (h-imgH)/2 + imgH];
    pts = [sind(0:1:360); -cosd(0:1:360)]*dmtr/2; % the points defining the circle of the clock
    pts(1,:) = pts(1,:) + center(1); pts(2,:) = pts(2,:) + center(2);
    if behav.lr(s) == 0 % LEFT
        imgRect([1 3]) = imgRect([1 3]) + w/4; % IMAGE RIGHT
        pts(1,:) = pts(1,:) - w/4;
    elseif behav.lr(s) == 1 % RIGHT
        imgRect([1 3]) = imgRect([1 3]) - w/4; % IMAGE LEFT
        pts(1,:) = pts(1,:) + w/4;
    end
    
    % identify eyetracking data samples
    ind = (etd.FSAMPLE.gx(2,:) < 10^7) & ... % error code in data is a value > 10^7
        (etd.FSAMPLE.time > etd.RECORDINGS(3).time) &... % ET samples extend beyond start & end, so pick out only the right ones
        (etd.FSAMPLE.time < (etd.RECORDINGS(3).time+3600000));
    
    etx = etd.FSAMPLE.gx(2,ind); % x-values
    ety = -etd.FSAMPLE.gy(2,ind)+h; % y-values; because 0,0 is in upper left for eyelink
    ett = etd.FSAMPLE.time(ind) - etd.FSAMPLE.time(find(ind,1)); % time; 0 to end in ms
            % NO ERRORS                     CORRECT TIME
	
    % Makes downsampled large image matrix/person using npxppx
%     for i = 1:w/npxppx
%         for j = 1:h/npxppx
%             imgmtx(j,i,s) = sum((etd.FSAMPLE.gx(2,ind)<(npxppx*i)) & (etd.FSAMPLE.gx(2,ind)>(npxppx*(i-1))) & ...
%                 (etd.FSAMPLE.gy(2,ind)<(npxppx*j)) & (etd.FSAMPLE.gy(2,ind) > (npxppx*(j-1))));
%         end
%     end
%     nout(s) = length(etd.FSAMPLE.time) - sum(sum(imgmtx(:,:,s)));
%     imgmtx(:,:,s) = imgmtx(:,:,s)/sum(sum(imgmtx(:,:,s))); % normalize the image matrix

    scatter(etx,ety,16,'filled','Markerfacealpha',1/50); hold on; % plot eye-tracking data
    plot([0 w w 0 0],[0 0 h h 0],'k') % add box
    plot([w/2 w/2;0 w]',[0 h; h/2 h/2]','k') % add cross-lines
    plot([imgRect(1) imgRect(3) imgRect(3) imgRect(1) imgRect(1)],...
        [imgRect(2) imgRect(2) imgRect(4) imgRect(4) imgRect(2)],'k');
    plot(pts(1,:),pts(2,:),'k') % add the clock circle
    axis equal
    title(subjIDs{s})
%     saveas(gcf,sprintf('./eyetracePNGs/%s_eyetrace.png',subjIDs{s}));
    pause(.01);
    fprintf('%d RECORDING events; %d seconds.\n',length(etd.RECORDINGS),round(max(ett)/1000));
    toc
end

% Plot eyetracking data over time for a single participant
ndiv = 400; % Into how many "moments" do you want to divide the ET samples?
for t = 1:ndiv
    figure(figN);clf; 
    
    nsamp = length(ett);
    ind2 = ((1:nsamp)>((t-1)*nsamp/ndiv) & (1:nsamp)<((t)*nsamp/ndiv));
    
    scatter(etx(ind2),ety(ind2),16,'filled','Markerfacealpha',1/50); hold on;
    plot([0 w w 0 0],[0 0 h h 0],'k') % add box
    plot([w/2 w/2;0 w]',[0 h; h/2 h/2]','k') % add cross-lines
    plot([imgRect(1) imgRect(3) imgRect(3) imgRect(1) imgRect(1)],...
        [imgRect(2) imgRect(2) imgRect(4) imgRect(4) imgRect(2)],'k');
    plot(pts(1,:),pts(2,:),'k')
    axis equal
    ylim([-500 2000])
    xlim([-100 2500])
    text(500,1500,sprintf('%.2f seconds',ett(find(ind2,1))/1000));
    title(subjIDs{s})
    fprintf('%d Samples\n',sum(ind2))
    pause; % wait for keyboard input before making the next 
end

% left/right swap the right images so all images line up
% (i.e. so clocks are all in the same place across participants)
dims = size(imgmtx);
for s = 1:nS
    if behav.lr(s) == 1 
        imgmtx(:,:,s) = imgmtx(:,dims(2):-1:1,s);
    end
end

% Plot the mean image matrix (if you've made it)
figN = figN + 1;
figure(figN); clf;
cm = colormap('jet');
heatmap(mean(imgmtx,3),'colormap',cm,'GridVisible','off')


% save('csn_datacheck_02_output.mat','imgmtx','nout')


% figN = figN + 1;
% imgRect = [(w-imgW)/2, (h-imgH)/2 (w-imgW)/2 + imgW, (h-imgH)/2 + imgH];
% pts = [sind(0:1:360); -cosd(0:1:360)]*dmtr/2;
% pts(1,:) = pts(1,:) + center(1); pts(2,:) = pts(2,:) + center(2);
% imgRect([1 3]) = imgRect([1 3]) + w/4; % IMAGE RIGHT
% pts(1,:) = pts(1,:) - w/4;
% 
% figure(figN); clf;
% imagesc(mean(imgmtx,3))
% colormap('jet');
% hold on
% % plot([0 w/npxppx w/npxppx 0 0],[0 0 h/npxppx h/npxppx 0],'k') % add box
% plot(pts(1,:)/npxppx,pts(2,:)/npxppx,'k')

% Pull out the 4 'times'. 
fourts = double([etd.RECORDINGS(1).time etd.RECORDINGS(2).time etd.RECORDINGS(3).time etd.RECORDINGS(4).time]);

% etd.RECORDINGS has the 4 "events" - calibration, validation, task, and
% re-validation, including the timestamp of when they occurred. 

% In etd.FSAMPLE...

% px and py are empty (pupil position?), as are hx and hy (may be eye rotation angle?)

% gx and gy seem to have gaze data (including the sine wave); row 1 has
% null values; row 2 has gaze location (UNITS?), with occasional v.
% large error values (> 10^7)

% pa may be pupil AREA

% rx and ry have data! But what?
%   resolution, i.e. how many pixels occupy 1 degree of visual angle given
%   their position.

% etd.IOEVENT just has some button presses, but no times, etc. 

% etd.FEVENT has the main info tags, including output of calibration and 
% validation. 


% Figure out which participants to drop from analysis for excessively poor 
% eyetracking quality.
secondVal = cell(nS,2); % the second (post-task) validation ("re-validation")
firstVal = cell(nS,2); % the first (pre-task) validation
for s = 1:nS
    fn = dir(sprintf('%s*.mat',subjIDs{s}));
    etd = load(fn(end).name); etd = etd.Edf2Mat;
    nev = length(etd.FEVENT);
    reval = 1;
    for i = nev:-1:1
        if length(etd.FEVENT(i).message) > 30
            if reval && strcmp(etd.FEVENT(i).message(1:27),'!CAL VALIDATION HV9 R RIGHT')
                secondVal{s,1} = etd.FEVENT(i).message;
                secondVal{s,2} = etd.FEVENT(i).sttime;
                reval = 0;
            end
            if ~reval
                if ((secondVal{s,2} - etd.FEVENT(i).sttime)/1000 > 3500) && strcmp(etd.FEVENT(i).message(1:27),'!CAL VALIDATION HV9 R RIGHT')
                    firstVal{s,1} = etd.FEVENT(i).message;
                    firstVal{s,2} = etd.FEVENT(i).sttime;
                    break
                end
            end
        end
    end
    fprintf('Done with %s.\n',subjIDs{s});
end

valqual = nan(nS,2); % 1 = good, 0 = fair, -1 = poor; First and second
valerr = nan(nS,2,2); % avg & max error; first & second
for s = 1:nS
    if strcmp(firstVal{s,1}(29:32),'GOOD')
        valqual(s,1) = 1;
    elseif strcmp(firstVal{s,1}(29:32),'FAIR')
        valqual(s,1) = 0;
    elseif strcmp(firstVal{s,1}(29:32),'POOR')
        valqual(s,1) = -1;
    end
    
    if strcmp(secondVal{s,1}(29:32),'GOOD')
        valqual(s,2) = 1;
    elseif strcmp(secondVal{s,1}(29:32),'FAIR')
        valqual(s,2) = 0;
    elseif strcmp(secondVal{s,1}(29:32),'POOR')
        valqual(s,2) = -1;
    end
    
    valerr(s,1,1) = str2num(firstVal{s,1}(40:43));
    valerr(s,2,1) = str2num(firstVal{s,1}(50:53));
    
    valerr(s,1,2) = str2num(secondVal{s,1}(40:43));
    valerr(s,2,2) = str2num(secondVal{s,1}(50:53));
end
