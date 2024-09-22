% Last Modification 11/27/2022, Davis.
% Goatgrass located in the first row in the plant vector and in the adjacency
% matrix. Its equation is modified by making it seed production independent of
% animal pollination (see below), equal to the average seed production of
% the other plant species.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code created for simulating invasion of pollinator species into
% plant-pollinator networks, using Valdovinos et al. 2013, 2016 model
% Authors: Fernanda S. Valdovinos & Pablo Moisset de Espanes
% Last Modification: May 5, 2018
% Published: Nature Communications 2018 (see paper for more in detail
% explanation of parameters, variables, and simulation design)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Valdovinos et al. 2013 ODE's RHS. 
% New version. Avoids 0/0 cases to simplify simulations where species 
% are removed or added in the middle of the run

function dx = Valdovinos2013_rhs_goatgrass(t,x)

global network_metadata %indRemP indRemA

nz_pos  = network_metadata.nz_pos ;
e       = network_metadata.e ;
mu_p    = network_metadata.mu_p ;
mu_a    = network_metadata.mu_a ;
c       = network_metadata.c ;
b       = network_metadata.b ;
u       = network_metadata.u ;
%w       = network_metadata.w ; %%% uncomment w here to run the simulations
%using interspecific competition values form the network. 
Beta    = network_metadata.Beta ;
G       = network_metadata.G ;
g       = network_metadata.g ;
phi     = network_metadata.phi ;
tau     = network_metadata.tau ;
%epsilon = network_metadata.epsilon ;
In      = network_metadata.In ;

[p, N, a, Alpha] = unpack(x, network_metadata ) ;
%p(indRemP)=0;% We force extinct species to zero to avoid resucitation due to stiff integrator
%N(indRemP)=0;
%a(indRemA)=0;

w = readmatrix('w_6_26_24.csv'); 


%% Model's specific computation begins here

%sigma = diag(sparse(p.*epsilon)) * Alpha ; %sigma nxm sparse
sigma = diag(sparse(p)) * Alpha;
sigma = sigma * diag(sparse(1./(sum(sigma)+realmin))) ;

Gamma = g .* (1 - u'*p - w.*p + u.*p); % non sparse

tmp = Alpha * diag(sparse(a.*tau)) ; %tmp nxm sparse -> Per-plant visits
seed_produced = sum(e .* sigma .* tmp, 2);
dp = ( (Gamma .* seed_produced) - mu_p) .* p;

tmp = (diag(sparse(N)) * tmp) .* b ;
da = sum(c .* tmp, 1)' - mu_a .* a ;

dN = Beta .* p - phi.*N - sum(tmp, 2) ;

%% Adaptive dynamics starts here

% Fitness function
DH = diag(sparse(N)) * sparse(c.*b) ; %nxm sparse
DH(Alpha<0)=-DH(Alpha<0) ;

wavg = sum(Alpha.*DH) ; %Weights for average. nxm sparse

%This is the replicator equation
dAlpha = Alpha.*DH - Alpha*diag(sparse(wavg)) ;
dAlpha = dAlpha*diag(sparse(G)) ;

%% Modifications specific to the goatgrass project
% Population dynamic equation for goatgrass, which is a wind pollinated plant.
% Thus,its reproduction (seed_produced) is independent of the plant-pollinator
% network and animal abundances. To make the goatgrass' seed production
% similar to the other plants in the community (in absence of better
% information), it's assumed to be equal to that of the average plant of the
% network. Goatgrass is the first plant in the plant vector.
Gamma(1) = g(1) .* (1 - u'*p - w(1).*p(1) + u(1).*p(1));
dp(1) = ( ( Gamma(1) .* mean(nonzeros(seed_produced)) ) - mu_p(1)) .* p(1);

%goatgrass does not produce nectar
dN(1) = 0;

%Regardless, right now the effect of goatgrass on
%    Lasthenia via compatition for seed recruitment is set as 10 times higher
%    than the strength of competition with any other plant species, and
%    between any other pair of plant species. This can be easily changed by
%    changing u_21=u(1)*10 to u_21=u(1)*1.

% u(21) = u(1)*100 ;

%% Now pack the answer
dx = full([dp; dN; da; dAlpha(nz_pos)]) ;
