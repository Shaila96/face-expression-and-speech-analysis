function res = convertCellToMatrix(df)
%CONVERTCELLTOMATRIX Converts from the emotion cells from cell to matrix.
% This is needed because when using read_table to load a table, since there
% are both rows with NA (char) and the probability (double) all values in
% that column are loaded as a cell array. Converting to matrices makes it
% easier for calculations

    df.F_Angry = cell2matEmotion(df.F_Angry);
    df.F_Disgusted = cell2matEmotion(df.F_Disgusted);
    df.F_Afraid = cell2matEmotion(df.F_Afraid);
    df.F_Happy = cell2matEmotion(df.F_Happy);
    df.F_Sad = cell2matEmotion(df.F_Sad);
    df.F_Surprised = cell2matEmotion(df.F_Surprised);
    df.F_Neutral = cell2matEmotion(df.F_Neutral);
    res = df;
end

%% From each emotion, we convert a cell column to a column vector
function res = cell2matEmotion(emotion)
    res = zeros(length(emotion),1);
    res(strcmp('NA', emotion)) = NaN;
    res(~strcmp('NA', emotion)) = str2double(emotion(~strcmp('NA', emotion)));
end