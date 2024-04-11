function [M_V, sPolServ_perP, sN_extractj_perA, meansigma_perP, sVisits_perP, sVisitsP, meansigma_perA, sVisits_perA, sVisitsA]= calValMechs(Alpha,p,a,N,network_metadata)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Called by RunEfficiencies.m (i.e., no need to re-run simulations)
%
% NOTE: to get total visits, use RunHornVisits.m which runs Visits.m
% No need to adapt this code to get those here
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Last modification 3/6/2020 in SF, CA
% I added total and per-capita visits exerted by animals, and total visits
% received by each plant species (before was only per-capita visits)
% 
% Last modification 1/31/2020 in Ann Arbor, MI
% Output meansigma after finishing ms with Bobby. Sigma is a key variable
% for the species persistence of plants.
%
% Last modification 4/05/17 in Tucson, AZ
% Right efficiency variables destilled after talking with Neo
%
% What I thought was a per animal and plant efficiency was actually a per-
% abundance-weighted-link efficiency
% Folks want to know if animals and plants become more efficient when they
% specialize, so it makes more sense to plot foraging efficiency per-animal
% and pollination efficiency per plant.
%
% Pollination efficiency to a plant species is better calculated as the
% sum of pollination services that the plant species gets over the plant
% abundance.
% Foraging efficiency of a pollinator species is better calculated as the
% sum of rewards consumption from each of the pollinator's plants over the
% pollinator abundance.
%
% Last modification 3/27/17 in Tucson, AZ
% Calculates the mean efficiencies meansigma, pol_event_perPA and mN_extractj_perN
% OVER NON-ZERO ELEMENTS, i.e. over the actual interactions
% (ignoring zeros indicating no-interacting species)
%
% Calculates variables to find explanatory mechanisms
%(e.g. efficiency in getting resources)

indRemA=[];
indRemP=[];

tau     = network_metadata.tau ;
epsilon = network_metadata.epsilon ;
b       = full(network_metadata.b) ;

%% Plants
% Formula for visits: M_V=diag(p)*Alpha*diag(a.*tau);
M_V=diag(p)*Alpha*diag(a.*tau);
Visits_perP = Alpha * diag(a .*tau) ;
sVisits_perP=sum(Visits_perP,2);% Total visits received by each plant species (per-capita!)
VisitsP = diag(p)* Visits_perP;
sVisitsP=sum(VisitsP,2);% Total visits received by each plant species

sigma = diag(p.*epsilon) * Alpha ;
sigma = sigma * diag(1./(sum(sigma)+realmin)) ;

% Excluding removed species from the sigma matrix by making their cols and rows NaN
sigma2=sigma;
sigma2(:,indRemA)=NaN;
sigma2(indRemP,:)=NaN;
sigma2(sigma2==0)=NaN;% Making zeros equal to NaN
meansigma_perP = mean(sigma2,2, 'omitnan'); % visitation quality averaged over each plant species
                                              % CALCULATED OVER THE NON-ZERO ELEMENTS!!!
meansigma_perA = mean(sigma2, 'omitnan')'; % visitation quality averaged over each pollinator species
[maxsigma_perP, idA_maxsigma] = max(sigma,[],2);

% Excluding removed species from the pollination-events matrix by making their cols and rows NaN
pol_event = sigma .* Visits_perP ; % Matrix of pollination events per visit (includes animal abundance)
pol_event(:,indRemA)=NaN;
pol_event(indRemP,:)=NaN;
sPolServ_perP= sum(pol_event, 2,'omitnan');% sum of the pollination services to plant species i per plant (includes animal abundance)
sPolServ_perP(indRemP)=NaN;

% Calculating pollination events per plant and animal species (double per-capita)
tmp = Alpha * diag(tau) ; % Removing animal abundance!!
pol_event = sigma .* tmp ; % Matrix of pollination events per plant and animal species
pol_event(:,indRemA)=NaN;
pol_event(indRemP,:)=NaN;
pol_event(pol_event==0)=NaN;% Making zeros equal to NaN
mPolServ_perAbundWLink= mean(pol_event, 2, 'omitnan');% mean pollination efficiency per-capita of plant and pollinator sp
                                              % CALCULATED OVER THE NON-ZERO ELEMENTS!!!
mPolServ_perAbundWLink(indRemP)=NaN;

%% Animals
tmp = Alpha * diag(tau) ;
N_extractij_perA = (diag(N) * tmp) .* b; % per-capita resource consumption of each pollinator species from each plant species
N_extractij_perA(:,indRemA)=NaN;
N_extractij_perA(indRemP,:)=NaN;
sN_extractj_perA = sum(N_extractij_perA, 'omitnan')'; % sum of the resources that each individual extracts
sN_extractj_perA(indRemA)=NaN;

N_extractij_perN = N_extractij_perA./ ( diag(N)*ones(length(N), length(a)) );
N_extractij_perN(N_extractij_perN==0)=NaN; % Making zeros equal to NaN
mN_extractj_perN = mean(N_extractij_perN, 'omitnan'); % mean of the resources that each individual animal extracts per unit of rewards
                                              % CALCULATED OVER THE NON-ZERO ELEMENTS!
                                              
Visits_perA = diag(p)* Alpha * diag(tau) ; % Per-capita for the animals!
sVisits_perA=sum(Visits_perA)';% Total visits by each animal species (per-capita!) % check if I really want it per-capita, and also
                                % whether the per-capita is well-done. The per-capita is correct because is kept for
                                % each plant or animal species. It would be wrong to sum the per-capita values over all species.
VisitsA = diag(p)* Alpha * diag(a.*tau) ; % Per-capita for the animals!
sVisitsA=sum(VisitsA)';

end