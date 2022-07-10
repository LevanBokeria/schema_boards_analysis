%% Description

% This script will load the data and for each participant, each condition,
% and separately for all-PAs/near-PAs/far-PAs will fit two different
% learning curve models:

% 2 parameter model:

% 3 parameter model:

%% Estimate learning rates
clear; clc;
dbstop if error;

warning('off','MATLAB:table:RowsAddedExistingVars')

saveData = 1;

%% Load and prepare the dataset
opts = detectImportOptions('./results/mean_by_rep_long_all_types.csv');
opts = setvartype(opts,{'border_dist_closest'},'char');

df = readtable('./results/mean_by_rep_long_all_types.csv',opts);

% Get only the needed accuracy types
indices = strcmp(df.border_dist_closest,'all');
df = df(indices,:);

all_ptp = unique(df.ptp);
n_ptp   = length(all_ptp);

all_conditions        = unique(df.condition);
all_hidden_pa_types   = unique(df.hidden_pa_img_type);
all_border_dist_types = unique(df.border_dist_closest);

%% Start the for loop
params_two   = [250,0.1];
params_three = [250,0.1,200];
plotFMSEstimation = 0;

tbl = table;

ctr = 1;
for iPtp = 1:n_ptp
    iPtp
    
    for iCond = 1:length(all_conditions)
        all_conditions{iCond}
        
        for iType = 1:length(all_hidden_pa_types)
            iType
            
%             if strcmp(all_conditions{iCond},'no_schema') | strcmp(all_conditions{iCond},'random_loc')
%                 
%                 if strcmp(all_hidden_pa_types{iType},'near') | strcmp(all_hidden_pa_types{iType},'far')
%                     
%                     continue;
%                     
%                 end
%                 
%             end
            
            curr_ptp  = all_ptp{iPtp};
            curr_cond = all_conditions{iCond};
            curr_type = all_hidden_pa_types{iType};
            
            % Get the data
            y = df.mouse_error_mean(strcmp(df.ptp,curr_ptp) &...
                strcmp(df.condition,curr_cond) & ...
                strcmp(df.hidden_pa_img_type,curr_type));
            
            % Now fit the data
            
            % 1. The 2 parameter model
            [out_two_params,  fval_two_param] = est_learning_rate(y',params_two,plotFMSEstimation,'two_parameters');
            [out_three_params,fval_three_param] = est_learning_rate(y',params_three,plotFMSEstimation,'three_parameters');
            
            % Save in a table
            tbl.ptp{ctr} = curr_ptp;
            tbl.condition{ctr} = curr_cond;
            tbl.hidden_pa_img_type{ctr} = curr_type;
            tbl.sse_two_param(ctr) = fval_two_param;
            tbl.intercept_two_param(ctr) = out_two_params(1);
            tbl.learning_rate_two_param(ctr) = out_two_params(2);
            tbl.sse_three_param(ctr) = fval_three_param;
            tbl.intercept_three_param(ctr) = out_three_params(1);
            tbl.learning_rate_three_param(ctr) = out_three_params(2);
            tbl.asymptote_three_param(ctr) = out_three_params(1) - out_three_params(3);
            
            ctr = ctr + 1;
        end %itype
    end % iCond
end %iPtp

%% Save the table
if saveData
    writetable(tbl,'./results/learning_rate_fits_matlab.csv');
end
