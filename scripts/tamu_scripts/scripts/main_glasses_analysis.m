% Clearing console, workplace variables, and closing all plots
close all; clc; clear;

%% Input variables

% Size of the window (in minutes) where the samples will be computed.
% For the whole DT task, please set it to 50
window_size_minutes = 50;

%% Initialization

% Loading batching data
load('data/B_facs_cached.mat');

% FACS columns (needed for filtering the dataframe)
columns = {'Participant_ID', 'Group', 'Treatment', 'Task', 'F_Seconds', 'Time', 'F_Angry', 'F_Disgusted', 'F_Afraid', 'F_Happy', 'F_Sad', 'F_Surprised', 'F_Neutral'};

% Retrieve the participants from the batching condition
participants = unique(df_batching.Participant_ID);

% Initializing with and without glasses filter
filter_with_glasses = zeros(length(df_batching.Group), 1);
filter_without_glasses = zeros(length(df_batching.Group), 1);

% Participants who worn glasses during the experiment
with_glasses = {'T021', 'T051', 'T077', 'T079', 'T139', 'T144'};

%% Building filters

% Here we are building two filters, one that will filter the rows for participants
% with glasses, and another one, that will handle the participants without glasses
for pp = 1:length(participants)
    
    % From all rows in the dataframe, we return true (1) for the rows of
    % the current participant
    rows_participant = strcmp(df_batching.Participant_ID, participants{pp});
    
    % We iteratively create the filters by first checking if the current
    % participant belongs to the with_glasses set, that we initialized
    % above. We then "add" the rows for that participant to one of the
    % filters, depending to which group the participant belongs to
    if ismember(participants(pp), with_glasses)
        filter_with_glasses = filter_with_glasses | rows_participant;
    else
        filter_without_glasses = filter_without_glasses | rows_participant;
    end
end

%% Validity check

% We know that # rows for participants with glasses + # rows for
% participants without glasses should add up to the total numbers of rows
% in the batching data frame;
disp('Validity check:');
disp(['filter_with_glasses: ', num2str(sum(filter_with_glasses))]);
disp(['Size filter_without_glasses: ', num2str(sum(filter_without_glasses))]);
disp(['filter_with_glasses + filter_without_glasses: ', num2str(sum(filter_with_glasses) + sum(filter_without_glasses))]);
disp(['Size of batching dataframe: ', num2str(height(df_batching))]);

% Throws an error if fails
assert(height(df_batching) == sum(filter_with_glasses) + sum(filter_without_glasses));

%% Bar plots

% Filtering dataframes
df_with_glasses = df_batching(filter_with_glasses, columns);
df_without_glasses = df_batching(filter_without_glasses, columns);

% Compute non overlapping averages for both dataframes
averaged_df_with_glasses = nonOverlappingAverage(df_with_glasses, window_size_minutes);
averaged_df_without_glasses = nonOverlappingAverage(df_without_glasses, window_size_minutes);

% Legends and titles for plots. I used the size of angry here, but I could have used
% the size of any other emotion, since the size of all emotions are the same
legends = {'With Glasses', 'W/o Glasses '};
title_str = ['#Samples With Glasses: ', num2str(length(averaged_df_with_glasses.F_Angry)),...
             ' #Samples W/o Glasses: ', num2str(length(averaged_df_without_glasses.F_Angry))];

% Plots the pairwise emotion (emotion for with glasses vs. without glasses)
barplotPairwiseEmotions(averaged_df_with_glasses, averaged_df_without_glasses, title_str, legends);

% Plots the positive, negative, and neutral emotion (with glasses vs. without glases)
barPlotNegativePositiveNeutral(averaged_df_with_glasses, averaged_df_without_glasses, title_str, legends);