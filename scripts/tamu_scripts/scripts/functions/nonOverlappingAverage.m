function averaged = nonOverlappingAverage(df, window_duration_minutes)
%NONOVERLAPPINGAVERAGE Computes, for each participant, the average emotion
%likelihood for a given window size

    %% Initialization
    columns = {'Participant_ID', 'Group', 'Treatment', 'Task', 'F_Seconds', 'Time', 'F_Angry', 'F_Disgusted', 'F_Afraid', 'F_Happy', 'F_Sad', 'F_Surprised', 'F_Neutral'};
    
    %% Initializing emotion buckets
    averaged.F_Angry = [];
    averaged.F_Disgusted = [];
    averaged.F_Afraid = [];
    averaged.F_Happy = [];
    averaged.F_Sad = [];
    averaged.F_Surprised = [];
    averaged.F_Neutral = [];
    
    %% Preparing to iterate over participants
    
    % Computing window size in seconds
    window_size = window_duration_minutes * 60;
    
    % Retrieving participants in this dataframe
    participants = unique(df.Participant_ID);
    
    %% iterating over all participants
    for i = 1:length(participants)
        %% Retrieves rows for the current participant only
        filtered_df = df(strcmp(df.Participant_ID, participants{i}), columns);
        
        %% Add the current average emotion to the emotion bucket
        averaged.F_Angry = [averaged.F_Angry; computeAveragedValues(filtered_df, 'F_Angry', window_size)];
        averaged.F_Disgusted = [averaged.F_Disgusted; computeAveragedValues(filtered_df, 'F_Disgusted', window_size)];
        averaged.F_Afraid = [averaged.F_Afraid; computeAveragedValues(filtered_df, 'F_Afraid', window_size)];
        averaged.F_Happy = [averaged.F_Happy; computeAveragedValues(filtered_df, 'F_Happy', window_size)];
        averaged.F_Sad = [averaged.F_Sad; computeAveragedValues(filtered_df, 'F_Sad', window_size)];
        averaged.F_Surprised = [averaged.F_Surprised; computeAveragedValues(filtered_df, 'F_Surprised', window_size)];
        averaged.F_Neutral = [averaged.F_Neutral; computeAveragedValues(filtered_df, 'F_Neutral', window_size)];
    end
end

%% Computes non overlapping averages using a sliding window approach
function averaged = computeAveragedValues(df, emotion_str, window_size)
    %% Initializing emotion
    averaged = [];
    
    %% Filtering by emotion
    emotion = df.(emotion_str);
    time = df.F_Seconds - df.F_Seconds(1);
    
    %% Start, end of each window
    start = time(1);
    finish = start + window_size;
    
    %% lambda function to compute the mean of valid data
    my_mean = @(data) mean(data(~isnan(data)));
    
    %% Computing mean of each window
    while finish <= time(end)
        filter = time > start & time <= finish;
        cur = my_mean(emotion(filter));
        
        averaged = [averaged; cur];
        
        start = finish;
        finish = start + window_size;
    end
end