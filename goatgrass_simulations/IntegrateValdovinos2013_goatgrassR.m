%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Developer: Fernanda S. Valdovinos
% Project: Goatgrass removal (Nelson et al 2024)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Last Modification 09/11/2024, Davis
% Adding an input to the function indicating whether the goatgrass is:
% 1. Present: ggPA=1
% 2. Absent: ggPA=0
%-------------------------------------------------------------------------
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

function [t, y, plantsf2, nectarf2, animalsf2, alphasf2]=IntegrateValdovinos2013_goatgrassR(plantsf, nectarf, animalsf, alphasf, ggPA)

global network_metadata J_pattern
tmax=3000;
%EUp=2e-2;
%EUa=1e-3;
[m, n]=size(alphasf);

%Give initial state (equal to the equilibrium of prior run
initial_plants=plantsf;
initial_nectar=nectarf;

initial_plants(1)=ggPA*plantsf(1);% abundance of goatgrass (equal to zero when removed)
initial_nectar(1)=0;% reward abundance of goatgrass equal to zero.

initial_animals=animalsf;% mz=0.1 (initial animal densities equal 0.1)
initial_alphas=alphasf(network_metadata.nz_pos);

% Combining all initial variables
initial_state=full([initial_plants;initial_nectar;initial_animals;initial_alphas]);
tspan = [0 tmax];

%% Integrating the dynamic model
options = odeset('JPattern', J_pattern,'NonNegative',1:2*m+n) ;
[t, y]=ode15s(@Valdovinos2013_rhs_goatgrass,tspan,initial_state, options) ;

yf2 = y(end,:)';

% Retriving final densities and foraging efforts
[plantsf2, nectarf2, animalsf2, alphasf2] = unpack(yf2, network_metadata);

plantsf2=full(plantsf2);
nectarf2=full(nectarf2);
animalsf2=full(animalsf2);
alphasf2=full(alphasf2);

end
