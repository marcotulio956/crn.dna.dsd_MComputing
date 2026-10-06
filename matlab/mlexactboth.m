function [V,M,N,t,Mtot,Ntot]=mlexactboth(tmax,Mtot,Ntot,doPlot,IappValue)
%function [V,M,Mtot,N,Ntot]=mlexactboth(tmax,Mtot,Ntot);
%
% Exact solution of Morris Lecar with discrete stochastic potassium channel
% (0<=N<=Ntot) and discrete stochastic calcium channel (0<=M<=Mtot).
% Using the random time change representation, we track four reactions:
%
% Rxn 1: calcium (m-gate) closed-> open (per capita rate alpha_m)
% Rxn 2: calcium (m-gate) open-> closed (per capita rate beta_m)
% Rxn 3: potassium (n-gate) closed-> open (per capita rate alpha_n)
% Rxn 4: potassium (n-gate) open-> closed (per capita rate beta_n)
%
% Default Mtot=40, Ntot=40, tmax=4000.
%
% Applied current "Iapp" set internally (default 100).
% Optional arguments:
%   doPlot (default true): enable output plots.
%   IappValue (default 100): constant applied current level.
%
% PJT June 2013, CWRU. Following Bard Ermentrout's "ml-rtc-exact.ode".
%% Use global variables to represent the channel state
% and random trigger for Poisson process
% Calcium
global Mcalcium_tot % total number of calcium channels
global Mcalcium % number of calcium channels in conducting state
global tau1 T1 % dummy variables for time to next Ca-opening event
global tau2 T2 % dummy variables for time to next Ca-closing event
% Potassium
global Npotassium_tot % total number of potassium channels
global Npotassium % number of potassium channels in conducting state
global tau3 T3 % dummy variables for time to next K-opening event
global tau4 T4 % dummy variables for time to next K-closing event
%% Set defaults for input arguments
if nargin < 3, Ntot=40; end
Npotassium_tot=Ntot;
if nargin < 2, Mtot=40; end
Mcalcium_tot=Mtot;
if nargin < 1, tmax=4e3; end
if nargin < 4, doPlot=true; end
if nargin < 5, IappValue=100; end
%% Parameters
phi_m=0.4;
va =-1.2; vb=18;
vc = 2; vd = 30;

phi_n = 0.04;
%% Functions for Morris-Lecar
global Iapp; Iapp=@(t)IappValue; % applied current
global xi_m; xi_m=@(v)(v-va)/vb; % scaled argument for m-gate input voltage
global minf; minf=@(v)0.5*(1+tanh(xi_m(v))); % m-gate activation function
global tau_m; tau_m=@(v)1./(phi_m*cosh(xi_m(v)/2)); % m-gate time constant
global alpha_m; alpha_m=@(v)(minf(v)./tau_m(v));
global beta_m; beta_m=@(v)((1-minf(v))./tau_m(v));
global xi_n; xi_n=@(v)(v-vc)/vd; % scaled argument for n-gate input
global ninf; ninf=@(v)0.5*(1+tanh(xi_n(v))); % n-gate activation function
global tau_n; tau_n=@(v)1./(phi_n*cosh(xi_n(v)/2)); % n-gate time constant
global alpha_n; alpha_n=@(v)(ninf(v)./tau_n(v));
global beta_n; beta_n=@(v)((1-ninf(v))./tau_n(v));
%% ODE options including reset
options=odeset('Events',@nextevent);
%% initialize
t0=0;
t=t0; % global "external" time
tau1=-log(rand); % time of next event on reaction stream 1 ("internal time")
tau2=-log(rand); % time of next event on reaction stream 2 ("internal time")
tau3=-log(rand); % time of next event on reaction stream 3 ("internal time")
tau4=-log(rand); % time of next event on reaction stream 4 ("internal time")
T1=0; % integrated intensity function for reaction 1
T2=0; % integrated intensity function for reaction 2
T3=0; % integrated intensity function for reaction 3
T4=0; % integrated intensity function for reaction 4
V0=-50; % start at an arbitrary middle voltage
V=V0; % initial voltage
M0=0; % start with calcium channels all closed
Mcalcium=M0; % initial state of calcium channel
M=M0; % use M to record the time course
N0=ceil(Ntot/2); % start with half potassium channels open
Npotassium=N0; % initial state of potassium channel
N=N0; % use N to record the time course
%% Loop over events
while t(end)<tmax
%% integrate ODE for voltage, until the next event is triggered
% State vector U for integration contains the following components
% 1 [voltage;

% 2 integral of (# closed)*alpha_m(v(t));
% 3 integral of (# open)*beta_m(v(t));
% 4 integral of (# closed)*alpha_n(v(t));
% 5 integral of (# open)*beta_n(v(t));
% 6 #open calcium channels;
% 7 #open potassium channels].
U0=[V0;0;0;0;0;M0;N0];
tspan=[t(end),tmax];
[tout,Uout,~,~,event_idx]=ode23(@dudtfunc,tspan,U0,options);
Vout=Uout(:,1); % voltage at time of next event
Mout=Uout(:,6); % number of calcium channels open at end of next event
Nout=Uout(:,7); % number of potassium channels open at end of next event
t=[t,tout'];
V=[V,Vout'];
M=[M,Mout'];
N=[N,Nout'];
%% Identify which reaction occurred, adjust state, and continue;
% and set trigger for next event.
mu=event_idx; % next reaction index
if mu==1 % next reaction is a calcium channel opening
M0=M0+1; % increment calcium channel state
tau1=tau1-log(rand); % increment in tau1 is exponentially distributed with mean 1
elseif mu==2 % next reaction is a calcium channel closing
M0=M0-1; % decrement calcium channel state
tau2=tau2-log(rand); % increment in tau2 is exponentially distributed with mean 1
elseif mu==3 % next reaction is a potassium channel opening
N0=N0+1;
% increment potassium channel state
tau3=tau3-log(rand); % increment in tau3 is exponentially distributed with mean 1
elseif mu==4 % next reaction is a potassium channel closing
N0=N0-1;
% decrement potassium channel state
tau4=tau4-log(rand); % increment in tau4 likewise
end
Mcalcium=M0;
Npotassium=N0;
if M0>Mcalcium_tot, error('M>Mtot'), end
if M0<0, error('M<0'), end
if N0>Npotassium_tot, error('N>Ntot'), end
if N0<0, error('N<0'), end
T1=T1+Uout(end,2);
T2=T2+Uout(end,3);
T3=T3+Uout(end,4);
T4=T4+Uout(end,5);
V0=V(end);

end % while t(end)<tmax
%% Plot output
if doPlot
figure
subplot(6,1,1),plot(t,M),ylabel('M','FontSize',20),set(gca,'FontSize',20)
subplot(6,1,2),plot(t,N),ylabel('N','FontSize',20),set(gca,'FontSize',20)
subplot(6,1,6),plot(t,V),xlabel('Time','FontSize',20)
ylabel('V','FontSize',20),set(gca,'FontSize',20)
subplot(6,1,4:6),plot3(V,M,N,'.-'),xlabel('V','FontSize',20)
ylabel('M','FontSize',20),zlabel('N','FontSize',20),set(gca,'FontSize',20)
grid on, rotate3d, shg
end
end % End of function mlexactboth
%% Define the RHS for Morris-Lecar
% state vector for integration is as follows:
% u(1) = voltage
% u(2) = integral of Ca-opening hazard function (Rxn 1)
% u(3) = integral of Ca-closing hazard function (Rxn 2)
% u(4) = integral of K-opening hazard function (Rxn 3)
% u(5) = integral of K-closing hazard function (Rxn 4)
% u(6) = M (number of open calcium channels)
% u(7) = N (number of open potassium channels)
function dudt=dudtfunc(t,u)
global Iapp % applied current
global Mcalcium Mcalcium_tot
global Npotassium Npotassium_tot
global alpha_m beta_m
global alpha_n beta_n
%% Parameters
vK =-84; vL =-60; vCa = 120;
gK =8; gL =2; C=20; gCa = 4.4;
%% calculate the RHS;
v=u(1); % extract the voltage from the input vector
dudt=[
(Iapp(t)-gCa*(Mcalcium/Mcalcium_tot)*(v-vCa)-gL*(v-vL)-gK*(Npotassium/Npotassium_tot)*(v-vK))/C;
alpha_m(v)*(Mcalcium_tot-Mcalcium);
% voltage
%-gK*(Npotassium/Npotassium_tot)*(v-vK))/C;
% Calcium chan. opening, internal time elapsed
% Calcium chan. closing, internal time elapsed
beta_m(v)*Mcalcium;
% Potassium chan. opening, internal time elapsed
alpha_n(v)*(Npotassium_tot-Npotassium);

% Potassium chan. closing, internal time elapsed
beta_n(v)*Npotassium;
0; % M is constant between events
0]; % N is constant between events
end
%% define behavior at threshold crossing
function [value,isterminal,direction] = nextevent(~,u)
global tau1 T1 % timing trigger for reaction 1 (calcium opening)
global tau2 T2 % timing trigger for reaction 2 (calcium closing)
global tau3 T3 % timing trigger for reaction 3 (potassium opening)
global tau4 T4 % timing trigger for reaction 4 (potassium closing)
value=[u(2)-(tau1-T1);u(3)-(tau2-T2);u(4)-(tau3-T3);u(5)-(tau4-T4)];
isterminal=[1;1;1;1]; % stop and restart integration at crossing
direction=[1;1;1;1]; % increasing value of the quantity at the trigger
end
