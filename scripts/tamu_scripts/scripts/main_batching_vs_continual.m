% Clearing console, workplace variables, and closing all plots
close all; clc; clear;

%% Input variables

% Size of the window (in minutes) where the samples will be computed.
% For the whole DT task, please set it to 50
window_size_minutes = 50;

%% Initialization

% Loading batching data
load('data/B_facs_cached.mat');

% Loading continual data
load('data/C_facs_cached.mat');

% FACS columns (needed for filtering the dataframe)
columns = {'Participant_ID', 'Group', 'Treatment', 'Task', 'F_Seconds', 'Time', 'F_Angry', 'F_Disgusted', 'F_Afraid', 'F_Happy', 'F_Sad', 'F_Surprised', 'F_Neutral'};

%% Bar Plots

% Computing the average emotion for each dataframe
averaged_df_batching = nonOverlappingAverage(df_batching, window_size_minutes);
averaged_df_continual = nonOverlappingAverage(df_continual, window_size_minutes);

% Configuring legends and title of the plots
legends = {'Batching ', 'Continual'};
title_str = ['#Samples Batching: ', num2str(length(averaged_df_batching.F_Angry)),...
             ' #Samples Continual: ', num2str(length(averaged_df_continual.F_Angry))];

% Plotting pairwise emotion for batching vs. continual
barplotPairwiseEmotions(averaged_df_batching, averaged_df_continual, title_str, legends);

% Plotting negative, positive, neutral emotions for batching vs. continual
barPlotNegativePositiveNeutral(averaged_df_batching, averaged_df_continual, title_str, legends);