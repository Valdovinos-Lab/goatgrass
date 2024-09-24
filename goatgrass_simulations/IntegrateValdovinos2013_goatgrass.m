%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Developer: Fernanda S. Valdovinos
% Project: Goatgrass removal (Nelson et al 2024)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Modification 11/10/2023, Davis
% Adaptation of pior code to the specific case of the serpentine
% plant-pollinator network with goatgrass (first row)
%
% Modification 08/03/2019, Ann Arbor
% Cleaning up my codes
% Only run the dynamics, without species invasions or removals
% Runs the dynamics for only one matrix.
% Outputs the whole time-series for any variable as well as final values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [t, y, plantsf, nectarf, animalsf, alphasf]=IntegrateValdovinos2013_goatgrass(vectG,In,muAP)

global network_metadata J_pattern
tmax=3000;
%EUp=2e-2;
%EUa=1e-3;

[m, n]=size(In);
B=sparse(In);

% Parameters of the uniform distribution from where the parameters of the
% dynamic model are drawn:

varp=0.1;% variance of plant parameters. I'm changing this variance to 0 to
       % see more clearly the effects of the pollination network vs the
       % effect of goatgrass on Lastenia.

       % change to 0.1 to run simulations to get multiple replicates Summer
       % 2024

vara=0;% variance of animal parameters
mC=0.2; vC=vara;
mE=0.8; vE=varp;
mb=0.4; vb=vara;
mU=0.02; vU=varp;
mw=0.2; vw=varp;
mB=0.2; vB=varp;
mG=2; vG=vara;
mg=0.5; vg=varp;
mphi=0.04; vphi=varp;
mtau=1; vtau=vara;
mepsilon=1; vepsilon=0;
vmA=vara; vmP=varp;

if muAP==1
    mmA=0.05; mmP=0.001; % high pollinator mortality 
elseif muAP==2
    mmA=0.001; mmP=0.02; % high plant mortality
elseif muAP==3
    mmA=0.001; mmP=0.001; % low plant and animal mortality
elseif muAP==4
    mmA=0.03; mmP=0.005; % high plant and animal mortality
end


% Parameters are drawn from uniform distribution (see Valdovinos et al.
% 2013, Oikos for complete description of the model and
% parameter definition)

% (10%meanP)-meanP+(10%meanP); (0.01%meanA)-meanA+(0.01%meanA)
c=uniform_rand(mC,vC,m,n).*B;
e=uniform_rand(mE,vE,m,n).*B;
b=uniform_rand(mb,vb,m,n).*B;

u=uniform_rand(mU,vU,m,1);

Beta=uniform_rand(mB,vB,m,1);
G=uniform_rand(mG,vG,n,1).*vectG';
g=uniform_rand(mg,vg,m,1);
mu_a=uniform_rand(mmA,vmA,n,1);
mu_p=uniform_rand(mmP,vmP,m,1);
w=uniform_rand(mw,vw,m,1);
phi=uniform_rand(mphi,vphi,m,1);
epsilon=uniform_rand(mepsilon,vepsilon,m,1);

tau=uniform_rand(mtau,vtau,n,1);

%Create structure 
network_metadata = create_metadata(B, e, mu_p, mu_a, c, b, u, w, Beta, G, g, phi, tau, epsilon) ;

%Give initial state
%mz=0.005; vz=0;
vz=0; %making the variance among initial abundances zero

initial_plants=uniform_rand(0.4,vz,m,1);% mz=0.4 (initial plant densities equal 0.4)
initial_nectar=uniform_rand(0.4,vz,m,1);% mz=0.4 (initial reward densities equal 0.4)
initial_nectar(1)=0;% reward abundance of goatgrass equal to zero.
initial_animals=uniform_rand(0.1,vz,n,1);% mz=0.1 (initial animal densities equal 0.1)

% To remove goatgrass uncomment the following line:
 %initial_plants(1)=0; %uncomment to remove goatgrass.

initial_alphas=B;

%Normalization and packing.
initial_alphas=initial_alphas*diag(sum(initial_alphas).^(-1));

initial_alphas=initial_alphas(network_metadata.nz_pos) ;

% Combining all initial variables
initial_state=full([initial_plants;initial_nectar;initial_animals;initial_alphas]);
tspan = [0 tmax];

%% Integrating the dynamic model
options = odeset('JPattern', J_pattern,'NonNegative',1:2*m+n) ;
[t, y]=ode15s(@Valdovinos2013_rhs_goatgrass,tspan,initial_state, options) ;

yf = y(end,:)';

% Retriving final densities and foraging efforts
[plantsf, nectarf, animalsf, alphasf] = unpack(yf, network_metadata);

plantsf=full(plantsf);
nectarf=full(nectarf);
animalsf=full(animalsf);
alphasf=full(alphasf);

end
