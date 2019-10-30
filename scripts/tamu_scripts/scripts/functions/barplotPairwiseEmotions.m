function barplotPairwiseEmotions(df1, df2, title_str, legends_str)
%BARPLOTPAIRWISEEMOTIONS Plots the pairwise emotion for df1 and df2

    %% Statistical test
    
    % Computes the t test for each emotion
    [~, p_angry] = myTTest(df1.F_Angry, df2.F_Angry);
    [~, p_disgusted] = myTTest(df1.F_Disgusted, df2.F_Disgusted);
    [~, p_afraid] = myTTest(df1.F_Afraid, df2.F_Afraid);
    [~, p_happy] = myTTest(df1.F_Happy, df2.F_Happy);
    [~, p_sad] = myTTest(df1.F_Sad, df2.F_Sad);
    [~, p_surprised] = myTTest(df1.F_Surprised, df2.F_Surprised);
    [~, p_neutral] = myTTest(df1.F_Neutral, df2.F_Neutral);

    %% Bar plot
    
    % Compute the mean and standard deviation of each emotion for df1
    [df1_means, def1_errors] = buildMeansStd(df1);
    
    % Compute the mean and standard deviation of each emotion for df2
    [df2_means, def2_errors] = buildMeansStd(df2);
    
    % Aggregating the means and standard deviation
    data = [df1_means; df2_means]';
    error_low = [def1_errors; def2_errors]';
    
    % Creating figure and updating some settings
    figure('units','normalized','outerposition',[0 0 1 1]);
    bar(data); hold on;
    set(gca,'xticklabel',{'Angry','Disgusted','Afraid','Happy','Sad','Surprised','Neutral'});
    
    %% Creating error bars
    nbars = 7;
    for i = 1:nbars
        x = [i - 0.15; i + 0.15];
        
        % I am showing half stddev below the mean, and half above the mean
        er = errorbar(x, data(i,:), 0.5*error_low(i,:), '.');
        
        er.Color = [0 0 0]; 
        er.LineStyle = 'none';  
    end
    
    %% Plot annotation: annotating plots with p-values obtained above
    
    dim_angry = [.165 .6 .1 .3];
    t = annotation('textbox',dim_angry,'String',num2str(p_angry),'FitBoxToText','on');
    paintRedIfDifferent(t, p_angry);
    
    dim_disgusted = [.275 .6 .1 .3];
    t = annotation('textbox',dim_disgusted,'String',num2str(p_disgusted),'FitBoxToText','on');
    paintRedIfDifferent(t, p_disgusted);
    
    dim_afraid = [.385 .6 .1 .3];
    t = annotation('textbox',dim_afraid,'String',num2str(p_afraid),'FitBoxToText','on');
    paintRedIfDifferent(t, p_afraid);
    
    dim_happy = [.495 .6 .1 .3];
    t = annotation('textbox',dim_happy,'String',num2str(p_happy),'FitBoxToText','on');
    paintRedIfDifferent(t, p_happy);
    
    dim_sad = [.605 .6 .1 .3];
    t = annotation('textbox',dim_sad,'String',num2str(p_sad),'FitBoxToText','on');
    paintRedIfDifferent(t, p_sad);
    
    dim_surprised = [.715 .6 .1 .3];
    t = annotation('textbox',dim_surprised,'String',num2str(p_surprised),'FitBoxToText','on');
    paintRedIfDifferent(t, p_surprised);
    
    dim_neutral = [.825 .6 .1 .3];
    t = annotation('textbox',dim_neutral,'String',num2str(p_neutral),'FitBoxToText','on');
    paintRedIfDifferent(t, p_neutral);
    
    %% Adding legends, setting limits, and font size of the plot
    legend(legends_str,'Location',[.520 .8 .01 .03]);
    ylim([-0.05 1.15]);
    ylabel('Likelihood');
    title(title_str);
    set(gca,'fontsize', 14);
end

%% If differences are statistically different, change the textbox to red
function paintRedIfDifferent(annot, p_val)
    if p_val < 0.05
        annot.Color = 'r';
    end
end