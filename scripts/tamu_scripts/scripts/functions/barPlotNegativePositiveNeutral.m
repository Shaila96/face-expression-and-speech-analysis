function barPlotNegativePositiveNeutral(df1, df2, title_str, legends)
    %% Negative
    
    % Returns the maximum score obtained by either angry, sad, or disgusted
    % row each row of the dataframe df1. Then computes the mean and stddev
    neg_df1 = max(max(max(df1.F_Angry, df1.F_Sad), df1.F_Disgusted), df1.F_Afraid);
    [neg_df1_mean, neg_df1_std] = buildMeansStdSingleEmotion(neg_df1);
     
    % Performs the same for dataframe df2
    neg_df2 = max(max(max(df2.F_Angry, df2.F_Sad), df2.F_Disgusted), df2.F_Afraid);
    [neg_df2_mean, neg_df2_std] = buildMeansStdSingleEmotion(neg_df2);
    
    %% Positive
    
    % Returns the positive emotion (i.e., happy) and computes the mean and
    % stddev for dataframe df1
    pos_df1 = df1.F_Happy;
    [pos_df1_mean, pos_df1_std] = buildMeansStdSingleEmotion(pos_df1);
    
    % Performs the same for df2
    pos_df2 = df2.F_Happy;
    [pos_df2_mean, pos_df2_std] = buildMeansStdSingleEmotion(pos_df2);
    
    %% Neutral
    
    % Returns the neutral emotion and computes the mean and stddev for
    % dataframe df1
    neu_df1 = df1.F_Neutral;
    [neu_df1_mean, neu_df1_std] = buildMeansStdSingleEmotion(neu_df1);
    
    % Performs the same for df2
    neu_df2 = df2.F_Neutral;
    [neu_df2_mean, neu_df2_std] = buildMeansStdSingleEmotion(neu_df2);
    
    %% Statistical tests
    [~, p_neg] = myTTest(neg_df1, neg_df2);
    [~, p_pos] = myTTest(pos_df1, pos_df2);
    [~, p_neu] = myTTest(neu_df1, neu_df2);
    
    %% Plotting
    data = [neg_df1_mean, neg_df2_mean;
            pos_df1_mean, pos_df2_mean;
            neu_df1_mean, neu_df2_mean];
        
    err = [neg_df1_std, neg_df2_std;
           pos_df1_std, pos_df2_std;
           neu_df1_std, neu_df2_std];
        
    figure('units','normalized','outerposition',[0 0 1 1]);
    bar(data); hold on;
    set(gca,'xticklabel',{'Negative','Positive','Neutral'});
    
    %% Errors bars
    nbars = 3;
    for i = 1:nbars
        x = [i - 0.15; i + 0.15];
        er = errorbar(x, data(i,:), 0.5*err(i,:), '.');
        er.Color = [0 0 0]; 
        er.LineStyle = 'none';  
    end
    
    %% Annotating bar plot with statistical tests
    
    dim_angry = [.245 .6 .1 .3];
    t = annotation('textbox',dim_angry,'String',num2str(p_neg),'FitBoxToText','on');
    paintRedIfDifferent(t, p_neg);
    
    dim_disgusted = [.495 .6 .1 .3];
    t = annotation('textbox',dim_disgusted,'String',num2str(p_pos),'FitBoxToText','on');
    paintRedIfDifferent(t, p_pos);
    
    dim_afraid = [.765 .6 .1 .3];
    t = annotation('textbox',dim_afraid,'String',num2str(p_neu),'FitBoxToText','on');
    paintRedIfDifferent(t, p_neu);
    
    %% Setting up title, legends, limits, and font size
    title(title_str);
    legend(legends,'Location',[.520 .8 .01 .03]);
    ylim([-0.05 1]);
    set(gca,'fontsize', 14);
    ylabel('Likelihood');
end

%% Computes the mean and standard deviation of the data
function [data_mean, data_std] = buildMeansStdSingleEmotion(data)
    data = data(~isnan(data));
    data_mean = mean(data);
    data_std = std(data);
end

%% If data is statistically different, paint text box red
function paintRedIfDifferent(annot, p_val)
    if p_val < 0.05
        annot.Color = 'r';
    end
end