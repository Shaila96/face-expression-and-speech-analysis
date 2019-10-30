function [h, p] = myTTest(dist1, dist2)
%MYTTEST T-Test of the log likelihood

    % Removes the NaN for both distributions
    dist1 = dist1(~isnan(dist1));
    dist2 = dist2(~isnan(dist2));
    
    % Computes the log of both distributions
    log_dist1 = log(dist1);
    log_dist2 = log(dist2);

    % Computes the two-sample t-test of the log distributions 
    [h, p] = ttest2(log_dist1, log_dist2);
end

