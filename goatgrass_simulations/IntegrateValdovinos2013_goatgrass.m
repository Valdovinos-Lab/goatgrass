%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Developer: Fernanda S. Valdovinos
% Project: Goatgrass removal (Nelson et al 2025)
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

function [plantsf, nectarf, animalsf, alphasf]=IntegrateValdovinos2013_goatgrass(metadata)

tmax=3000;
%EUp=2e-2;
%EUa=1e-3;

% Combining all initial variables
initial_state=full([metadata.p0;metadata.R0;metadata.a0;metadata.alphas0]);
tspan = [0 tmax];

%% Integrating the dynamic model
options = odeset('JPattern', metadata.J_pattern,'NonNegative',1:2*metadata.plant_qty+metadata.animal_qty) ;

rhs_with_metadata = @(t, y) Valdovinos2013_rhs_goatgrass(t, y, metadata);

[~, y]=ode15s(rhs_with_metadata, tspan, initial_state, options) ;

yf = y(end,:)';

% Retriving final densities and foraging efforts
[plantsf, nectarf, animalsf, alphasf] = unpack(yf, metadata);

plantsf=full(plantsf);
nectarf=full(nectarf);
animalsf=full(animalsf);
alphasf=full(alphasf);

end
