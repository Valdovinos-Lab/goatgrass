function [Gamma, seed_produced, M_V, sPolServ_perP, sN_extractj_perA, meansigma_perP, sVisits_perP, sVisitsP, meansigma_perA, sVisits_perA, sVisitsA]= calValMechs(Alpha,p,a,N,metadata)

% Calculates variables to find explanatory mechanisms
%(e.g. efficiency in getting resources)

tau     = metadata.tau ;
epsilon = metadata.epsilon ;
b       = full(metadata.b) ;
g       = metadata.g ;
u       = metadata.u ;
w       = metadata.w ;
e       = metadata.e ;

%% Plants
% Recruitment rate (determined by a Lotka-Volterra model of competition)
Gamma = g .* (1 - u'*p - w.*p + u.*p); % non sparse

% Formula for visits: M_V=diag(p)*Alpha*diag(a.*tau);
M_V=diag(p)*Alpha*diag(a.*tau);
Visits_perP = Alpha * diag(a .*tau) ;
sVisits_perP=sum(Visits_perP,2);% Total visits received by each plant species (per-capita!)
VisitsP = diag(p)* Visits_perP;
sVisitsP=sum(VisitsP,2);% Total visits received by each plant species

sigma = diag(p.*epsilon) * Alpha ;
sigma = sigma * diag(1./(sum(sigma)+realmin)) ;

% Total seed produced by each plant species
tmp = Alpha * diag(sparse(a.*tau)) ; %tmp nxm sparse -> Per-plant visits
seed_produced = sum(e .* sigma .* tmp, 2);

% Calculating average sigma over non-zero elements
sigma2=sigma;
sigma2(sigma2==0)=NaN;% Making zeros equal to NaN
meansigma_perP = mean(sigma2,2, 'omitnan'); % visitation quality averaged over each plant species
                                              % CALCULATED OVER THE NON-ZERO ELEMENTS!!!
meansigma_perA = mean(sigma2, 'omitnan')'; % visitation quality averaged over each pollinator species

% Excluding removed species from the pollination-events matrix by making their cols and rows NaN
pol_event = sigma .* Visits_perP ; % Matrix of pollination events per visit (includes animal abundance)
sPolServ_perP= sum(pol_event, 2,'omitnan');% sum of the pollination services to plant species i per plant (includes animal abundance)


%% Animals
tmp = Alpha * diag(tau) ;
N_extractij_perA = (diag(N) * tmp) .* b; % per-capita resource consumption of each pollinator species from each plant species
sN_extractj_perA = sum(N_extractij_perA, 'omitnan')'; % sum of the resources that each individual extracts

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