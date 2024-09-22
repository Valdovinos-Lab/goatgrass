%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Developer: Fernanda S. Valdovinos
% Project: Goatgrass removal (Nelson et al 2024)
% Rebecca Nelson run simulations.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Runs the Valdovinos et al's (2013) model for the goatgrass project
% Modifications specific to the goatgrass project:
% 1. The adjacency matrix used is the combined network (control + restored)
%    of 50 pollinator and 20 plant species, sorted by degree from less to
%    most connected. Goatgrass is added in the first row but with all
%    zeros as it is wind pollinated. Thus, total number of plant species is 21.
% 2. Goatgrass is the first plant in the plant vector and adjacency matrix.
% 3. Goatgrass is a wind pollinated plant, so its population dynamic equation
%    (particularly seed production) is assumed to be independent of the 
%    plant-pollinatornetwork and animal abundances.
% 4. To make the goatgrass' seed production similar to the other plants in
%    the community (in absence of better information), it's assumed to be
%    equal to that of the average plant of the network (all of which are assumed
%    to be animal pollinated, determined by the plant-pollinator netwoloark).
% 5. To remove goatgrass from the system, all what is needed is to set its
%    initial abundance as zero in IntegrateValdovinos2013_goatgrass.m,
%    by uncommenting line 85.
% 6. Prameter values can be changed in IntegrateValdovinos2013_goatgrass.m,
%    specifically variance of plant parameters (across species, varp), which
%    is set to zero to see more clearly the effects of the network vs
%    the effect of goatgrass.
%    This variance chould be changed back to 0.1 if needed/desire. For
%    example, when needing many replicates of the same type of simulations,
%    to get means and standard deviations to compare treatments.
% Another parameter to change to compare different scenarios of competition
%    strenghts between Lasthenia and goatgrass is u_21, which I included in
%    Valdovinos2013_rhs_goatgrass.m (i.e., direclty in the equations). I
%    should probably polish this parameter choice and move it where all the
%    parameters are. Regardless, right now the effect of goatgrass on
%    Lasthenia via compatition for seed recruitment is set as 10 times higher
%    than the strength of competition with any other plant species, and
%    between any other pair of plant species. This can be easily changed by
%    changing u_21=u(1)*10 to u_21=u(1)*1.
% 7. Another parameter to change to compare different scenarios of mortality, 
%    is determining muAP as 1, 2, 3, 4. Four differnt types of mortality
%    scenarios I have characterized over the many years working with this model.
%    muAP=1 -> high pollinator mortality.
%    muAP=2 -> high plant mortality.
%    muAP=3 -> low mortality for plants and pollinators.
%    muAP=4 -> high mortality for plants and pollinators.

% Outputs that can be obtained after runing this code in the Matlab command
% window (by calling them):
% 1. Final abundances of plants (plantsf), floral rewards (nectarf), and
%   animals (animalsf).
% 2. alphasf: Final matrix with preferences.
% 3. M_V: Matrix of total visits by each pollinator to each plant species. 
% 4. sPolServ_perP: Sum of all pollination services received by each plant
%    species on a per-capita (per-plant) basis.
% 5. sN_extractj_perA: Sum of all rewards extracted by each animal species
%    on a per-capita (per-animal) basis.
% 6. meansigma_perP: Mean visit quality received by each plant species on a
%    per-capita (per-plant) basis.
% 7. sVisits_perP: Sum of all visits received by each plant species on a
%    per-capita (per-plant) basis.
% 8. sVisitsP: Total visits received by each plant species.
% 9. meansigma_perA: Mean quality of visits performed by each pollinator
%    species on a per-capita (per-animal) basis.
% 10. sVisits_perA: Sum of all visits received by each pollinator species 
%     on a per-capita (per-animal) basis.
% 11. sVisitsA: Total visits performed by each animal species.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Last modifications made during the month of November 2023, Davis.
% Fernanda Valdovinos introduced all the modifications indicated above,
% which meant creating this function RunValdovinos2013_singleM_goatgrass.m,
% IntegrateValdovinos2013_goatgrass.m, and Valdovinos2013_rhs_goatgrass.m
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Last modification 08/03/2019, Ann Arbor
% Cleaning up my codes
% Only run the dynamics, without species invasions or removals
% Runs the dynamics for only one matrix.
% Outputs the whole time-series for any variable as well as final values

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% 5-23-24 updates: ran the simulation for each of the four scenarios of
% mortality both with and without goatgrass in the system. Saved the
% outputs of interest as .csv files. 

% 5-31-24 update: removed attempted empirical 'visits' to goatgrass from the .csv file
% of input data. Reran the simulation for each of the four scenarios of
% mortality both with and without goatgrass in the system. For Mortality
% scenario 3 both with and without GG, also ran these with and without AF. 

% 6-5-24 update: ran simulations for mortality scenarios 1-4 without and with
% GG with and without AF. I also extracted sVisitsA as output. 

%6-26-24 update: ran simulations with w values based on empirical cover
%values for plant species. In this case, w is the inverse of the mean cover
%for each plant species for all plots and years combined. In this current
%version, w is thus inverse the mean cover for both the restored and
%control plots combined. One unknown plant in the network is not identified to
%species (UNK). For UNK, I took the mean of the w-values for all of the
%other forbs in the network. On an initial run, plantsf abundances appear similar in
%value to the empirical means. I then proceeded with running all of the
%pervious mortality scenarios, AF, with and without goatgrass using these
%empirically informed values for w. 

%8-20-24 update: ran with variance of plants set to .1 so that there can be
%multiple replicates. created a for loop to run the simulation 500 times.

%Global Variables & Setup
global J_pattern network_metadata

% Model Parameters 
r_i=1;
frG=1;
muAP=3;
sem=0;
numRuns = 500; % Number of simulation runs

% Preallocate storage for results
plantsf_all = zeros(20, numRuns); % 20 plant species, adjust size as needed
sVisitsP_all = zeros(20, numRuns); % 20 plant specie
sVisits_perP_all = zeros(20, numRuns); % 20 plant species

animalsf_all = zeros(50, numRuns); % 50 animal species, adjust size as needed
sVisitsA_all = zeros(50, numRuns); % 50 animal species
sVisits_perA_all = zeros(50, numRuns); % 50 animal species

% Load the data
In = load('goatgrass_network_full.csv'); % Update this if the data source or format changes
[plant_qty, animal_qty] = size(In);

% Initialize J_pattern
J_pattern = J_zero_pattern(In);

% Which pollinator exhibits adaptive foraging
vectG = frG * ones(1, animal_qty);

% Simulation Loop
for i = 1%:numRuns
    % Set random seed for reproducibility
    rng(sem + i); % Modern MATLAB function for random seed

    [t, y, plantsf, nectarf, animalsf, alphasf] = IntegrateValdovinos2013_goatgrass(vectG, In, muAP);
    [t2, y2, plantsf2, nectarf2, animalsf2, alphasf2] = IntegrateValdovinos2013_goatgrassR(plantsf, nectarf, animalsf, alphasf, 0);
    
    % Calculate Metrics
    [M_V, sPolServ_perP, sN_extractj_perA, meansigma_perP, sVisits_perP, sVisitsP, meansigma_perA, sVisits_perA, sVisitsA] = calValMechs(alphasf2, plantsf2, animalsf2, nectarf2, network_metadata);

    % Store Results
    plantsf_all(:, i) = plantsf; % Adjust indexing based on actual dimensions
    animalsf_all(:, i) = animalsf; % Adjust indexing based on actual dimensions
    sVisits_perP_all(:, i) = sVisits_perP; % Adjust indexing based on actual dimensions
    sVisits_perA_all(:, i) = sVisits_perA;
    sVisitsA_all(:, i) = sVisitsA; % Adjust indexing based on actual dimensions
    sVisitsP_all(:, i) = sVisitsP;

%rand('seed',sem+r_i);
%In=load('goatgrass_network_full.csv'); % Already sorted by degree
%load(sprintf('%dm.mat',dataset)); % Already sorted by degree
%In=cell2mat(m1200(r_i));% change for every dataset!!!!    

%goatgrass network full gives full network with goatgrass as all zeros.
%goatgrass.csv gives goatgrass twice both with attempts visits and with
%zeroz. 

%In=[1 1;1 0];
%[rows, cols]= size(In);
%J_pattern = J_zero_pattern(In) ;

%vectG=frG*ones(1,cols);
    
%[t, y, plantsf, nectarf, animalsf, alphasf]=IntegrateValdovinos2013_goatgrass(vectG,In,muAP);

% To get visits and other useful measurments
%[M_V, sPolServ_perP, sN_extractj_perA, meansigma_perP, sVisits_perP, sVisitsP,...
 %   meansigma_perA, sVisits_perA, sVisitsA]= calValMechs(alphasf,plantsf,animalsf,nectarf,network_metadata);

% Extract plant simulation output as .csv
%filename = 'plantsf1withoutGGwithAFwithw.csv';
%writematrix(plantsf, filename);

% Extract pollinator simulation output as .csv
%filename = 'animalsf1withoutGGwithAFwithw.csv';
%writematrix(animalsf, filename);

%filename = 'sVisits_perP1withoutGGwithAFwithw.csv';
%writematrix(sVisits_perP, filename);

%filename = 'sVisitsP1withoutGGwithAFwithw.csv';
%writematrix(sVisitsP, filename);

%filename = 'sVisitsA1withoutGGwithAFwithw.csv';
%writematrix(sVisitsA, filename);

%network matrix for network robustness;
%filename = 'M_V1withoutGGwithoutAF.csv';
%writematrix(M_V, filename);

%naming convention for files:
%%name of response variable + Mortality Scenario # (1-4) + with vs without
%%GG in the model and with vs without AF
% "with w" means that this is the output for empirically-informed w values
% based on empirical plant abundances. If there is no meniton of with w in the
% name, this means that the default abundance values from metanetwork data
% were used. 

% Plotting trajectories
%[plants, nectar, animals] = unpack2(y,network_metadata);

%figure
%subplot (3,1,1)
%plot(t,plants)
 %title('Plant Trajectories')
%subplot (3,1,2)
%plot(t,nectar)
 %title('Nectar Trajectories')
%subplot (3,1,3)
%plot(t,animals)
 %title('Animal Trajectories')
end
% % Create Tables and Export to CSV
% plantsf_table = array2table(plantsf_all', 'VariableNames', strcat('Plant_', arrayfun(@num2str, 1:20, 'UniformOutput', false)));
% animalsf_table = array2table(animalsf_all', 'VariableNames', strcat('Animal_', arrayfun(@num2str, 1:50, 'UniformOutput', false)));
% sVisits_perP_table = array2table(sVisits_perP_all', 'VariableNames', strcat('Plant_', arrayfun(@num2str, 1:20, 'UniformOutput', false)));
% sVisits_perA_table = array2table(sVisits_perA_all', 'VariableNames', strcat('Animal_', arrayfun(@num2str, 1:50, 'UniformOutput', false)));
% sVisitsA_table = array2table(sVisitsA_all', 'VariableNames', strcat('Animal_', arrayfun(@num2str, 1:50, 'UniformOutput', false)));
% sVisitsP_table = array2table(sVisitsP_all', 'VariableNames', strcat('Plant_', arrayfun(@num2str, 1:20, 'UniformOutput', false)));
% 
% % Save tables as CSV files
% % update name with either GG or noGG
% writetable(plantsf_table, 'plantsf_all_runs.GG.csv');
% writetable(animalsf_table, 'animalsf_all_runs.GG.csv');
% writetable(sVisits_perP_table, 'sVisits_perP_all_runs.GG.csv');
% writetable(sVisits_perA_table, 'sVisits_perA_all_runs.GG.csv');
% writetable(sVisitsA_table, 'sVisitsA_all_runs.GG.csv');
% writetable(sVisitsP_table, 'sVisitsP_all_runs.GG.csv');

%to check order 
%if i == 1
 %   % Inspect initial data to verify species order
  %  disp('First simulation plants:');
   % disp(plantsf);
    %disp('First simulation animals:');
    %disp(animalsf);
%end

