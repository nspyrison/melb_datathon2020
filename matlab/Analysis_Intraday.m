%% Load data
clear;clc;
load('maxmintemp.mat')  % Maximum and minimum daily temperatures in Victoria, obtained from BOM
load('Energydata.mat')  % Load electricity data taken from AEMO

state   =   5;          % 1 for NSW
                        % 2 for QLD
                        % 3 for SA
                        % 4 for TAS
                        % 5 for VIC
dates       =   data_all{state}.DT;                                     % Vector of dates and times
datev       =   floor( ( datenum( dates(1) ):datenum( dates(end) ) )' );% Vector containing only the date (not time)
datesdays   =   datetime(datestr(datev));                               % Convert datev into datetime format
demand      =   data_all{state}.TOTALDEMAND;                            % Demand for every half-hourly interval
demand_daily=   reshape( demand,48,[] );                                % Reshape demand so that each column corresponds to a day and each row corresponds to the same hour for each day

%% Find daily peak(s and trough)
DD          =   length( datev );
peaksind    =   zeros( DD,2 );  % Location of peak(s)
thepeaks    =   zeros( DD,2 );  % Demand at peak(s)
totaldem    =   zeros( DD,1 );  % Daily total demand
for t=1:DD
    demand_t        =   demand_daily(:,t);
% Smooth by 7-hour moving average
    demand_s        =   [0;0;0;(demand_t(1+end-6)+demand_t(2:end-5)+demand_t(3:end-4)+demand_t(4:end-3)+demand_t(5:end-2)+demand_t(6:end-1)+demand_t(7:end))/7;0;0;0];
% Indicator if smoothed curve fulfils my peaky criteria
 %- Higher than neighbours
 %- Higher than the average of three neighbours to the left
 %- Higher than the average of three neighbours to the right
    peakyes         =   [[0;0;0];...
                            (demand_s(4:end-3)>(demand_s(3:end-4)+demand_s(2:end-5)+demand_s(1:end-6))/3)...
                            .*...
                            (demand_s(4:end-3)>(demand_s(5:end-2)+demand_s(6:end-1)+demand_s(7:end))/3)...
                            .*...
                            (demand_s(4:end-3)>(demand_s(5:end-2)))...
                            .*...
                            (demand_s(4:end-3)>(demand_s(3:end-4)))...
                            .*...
                            (abs(demand_s(4:end-3)-(demand_s(3:end-4)))<=(2.5*abs(demand_s(2:end-5)-(demand_s(3:end-4)))))...
                            .*...
                            (abs(demand_s(4:end-3)-(demand_s(5:end-2)))<=(2.5*abs(demand_s(5:end-2)-(demand_s(6:end-1)))));...
                        [0;0;0]];
% Morning peak
    if sum( peakyes(8:24) )>0
    % If there is a peak between 4am and 12pm
        peaksind(t,1)   =   find( peakyes(8:24),1 )+7;
        thepeaks(t,1)   =   demand_t(peaksind(t,1));
    else
    % Otherwise no morning peak
        peaksind(t,1)   =   nan;
        thepeaks(t,1)   =   nan;
    end
    
% Evening peak
    if sum( peakyes(25:48) )>0
    % If there is a peak between 12.30pm and midnight
        d_tmp           =   demand_t(25:48);
        peaksind(t,2)   =   find( d_tmp==max( d_tmp(logical(peakyes(25:48))) ),1 )+24;
        thepeaks(t,2)   =   demand_t(peaksind(t,2));
    else
    % Otherwise no evening peak
        peaksind(t,2)   =   nan;
        thepeaks(t,2)   =   nan;
    end
    totaldem(t)     =   sum( demand_t );
end

% Some plots
difpeaks    =   thepeaks(:,2)-thepeaks(:,1);    % Difference between peaks (evening-morning)
nonnandif=(1:DD)';
nonnandif=nonnandif(~isnan(difpeaks));
% Plot the difference between peaks and a linear trend
diftrend    =   [ones(length(nonnandif),1) nonnandif]*([ones(length(nonnandif),1) nonnandif]\difpeaks(~isnan(difpeaks)));
plot(datesdays,difpeaks,datesdays(~isnan(difpeaks)),diftrend)
% plot(datesdays(~isnan(difpeaks)),difpeaks(~isnan(difpeaks))-diftrend)

%% Regressions
meantemp=   (maxtemp+mintemp)/2;    % Daily mean as defined by BOM
CDD     =   max(0,meantemp-18);     % Cooling degree days
HDD     =   max(0,18-meantemp);     % Heating degree days
% clf
% plot(datesdays,maxtemp-mintemp)
% yyaxis right
% plot(datesdays,thepeaks(:,1))

ser     =   1;                      % Choose which series to regress
dum     =   1;                      % Introduce break points or no? (Choose 0=no or 1=yes)
breakdt1=   datenum(2020,03,31);    % First break point (Stage 3)
breakdt2=   datenum(2020,08,03);    % Second break point (Stage 4)
if      ser==1; y=thepeaks(:,1);
elseif  ser==2; y=thepeaks(:,2);
end

breakpt1=   find( datev==breakdt1 )-1;
breakpt2=   find( datev==breakdt2 )-1;
trigvec =   2*pi*(1:DD)'/365;                                           % For long-term seasonality (next line)
trigmat =   [sin(trigvec) cos(trigvec) sin(2*trigvec) cos(2*trigvec)];  % Long-term seasonality
wdys    =   zeros( DD,6 );                                              % Day-of-week dummies
for i=1:6
    wdys(:,i)   =   weekday(datesdays)==i+1;
end
xxtemp  =   [HDD CDD];                  % Temperature RHS variables
xx      =   [ones( DD,1 ) wdys xxtemp]; % Form matrix of variables which are allowed to have structural breaks
N_x1    =   size( xx,2 );
    % Weekday dummies
    % Seasonality (trig curves)
    % Constant
    % Trend
    % Temperature variables

if dum==1       % Form full RHS matrix
    xx      =   [xx trigmat xx.*[zeros( breakpt1,1 );ones( DD-breakpt1,1 )] xx.*[zeros( breakpt2,1 );ones( DD-breakpt2,1 )]];
elseif dum==0
    xx      =   [xx trigmat];
else
    error('Choose dum=1 to check break point, or set dum=0.')
end

% Regress, compute test statistic and p-values
ind_ok  =   ~isnan(y);
y       =   y(ind_ok);
y       =   y/1000;                 % GW
T_y     =   length( y );
betas   =   xx(ind_ok,:)\y;         % Coefficient estimates
yfit    =   xx(ind_ok,:)*betas;     % Fitted values
errs    =   y-yfit;                 % Residuals
se      =   sqrt( diag( errs'*errs*(xx'*xx)^(-1)/(T_y-size(xx,2)) ) );  % Standard errors of estimates
tstats  =   betas./se;              % Test statistics
if dum==1       % Reshape for easier viewing and comparison
    N_x2    =   2*N_x1+4;
    tstats  =   [tstats(1:N_x2);zeros(4,1);tstats(N_x2+1:end);zeros(4,1)];
    betas   =   [betas(1:N_x2);zeros(4,1);betas(N_x2+1:end);zeros(4,1)];
    tstats  =   reshape(tstats,[],3);
    betas   =   reshape(betas,[],3);
end
pvals   =   (1-normcdf( abs( tstats ) ))*2;     % p-value
MSE     =   mean( y-yfit.^2 );




%% Plots
% Days with only one peak
figure(1)
clf
onepeak_mor     =   find(isnan(peaksind(:,1)));
onepeak_eve     =   find(isnan(peaksind(:,2)));
plot(.5:.5:24,demand_daily(:,onepeak_mor)/1000,peaksind(onepeak_mor,2)/2,thepeaks(onepeak_mor,2)/1000,'b.',[12 12],[3 10],'k-')
xlim([0.5 24])
xticks(2:2:24)
xlabel('Time of day')
ylabel('Demand (GWh)')
grid on


%%
clf
plot(.5:.5:24,demand_daily(:,onepeak_eve)/1000,peaksind(onepeak_eve,1)/2,thepeaks(onepeak_eve,1)/1000,'b.',[12 12],[3 10],'k-')
xlim([0.5 24])
xticks(2:2:24)
xlabel('Time of day')
ylabel('Demand (GWh)')
grid on

%%
% Show first peak
clf
plot(datesdays,thepeaks(:,1)/1000,datesdays,thepeaks(:,2)/1000)
ylim([0.5 11.5])
ylabel('Demand (GWh)')
yyaxis right
plot(datesdays,difpeaks/1000,'b-',datesdays([1 end]),[1 1]*nanmean(difpeaks/1000),'k--')
ylabel('Difference in demand (GWh)')
ylim([-1.5 9.5])
grid on
xlabel('Date')
p2={'Morning peak (L)','Evening peak (L)','Difference between peaks (R)','Mean of differences (R)'};
legend(p2,'location','n')
hold on
xticks(datesdays(1):calmonths(3):datesdays(end))
xtickangle(30)

%% Daily curves
figure(5);
clf
hold on
plot(0.5:0.5:24,demand_daily(:,datev==datenum(2019,01,01))/1000,'b:')
plot(0.5:0.5:24,demand_daily(:,datev==datenum(2019,01,06))/1000,'r:')
for ddd=2:6
    plot(0.5:0.5:24,demand_daily(:,and(weekday(datesdays)==ddd,and(datev>=datenum(2019,01,01),datev<=datenum(2019,12,31))))/1000,'b:')
end
for ddd=[1 7]
    plot(0.5:0.5:24,demand_daily(:,and(weekday(datesdays)==ddd,and(datev>=datenum(2019,01,01),datev<=datenum(2019,12,31))))/1000,'r:')
end
hold off
legend('Weekdays','Weekends','Orientation','Horizontal','Location','n')
ylabel('Demand (GWh)')
xlim([0.5 24])
xticks(0:24)
grid on
xlabel('Time of day')
