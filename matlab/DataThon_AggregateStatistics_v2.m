% Requires signal processing toolbox to run "findpeaks" function.
clear;

%Read input file
cd('VIC Data By Year');
inputdata = readtable('Data_20152020_Vic.xlsx');
cd ..

%Extract needed data as arrays 
year = year(inputdata.SETTLEMENTDATE); 
month = month(inputdata.SETTLEMENTDATE); 
day = day(inputdata.SETTLEMENTDATE); 
is_weekend = isweekend(inputdata.SETTLEMENTDATE); 
[h,m,s] = hms(inputdata.SETTLEMENTDATE);
electricity_demand = inputdata.TOTALDEMAND; 
electricity_price = inputdata.RRP; 
[rows cols] = size(inputdata);

%Variables for average calculation
count_2015 = 0;
count_2016 = 0;
count_2017 = 0;
count_2018 = 0;
count_2019 = 0;
count_2020 = 0;
sum_2015 = 0;
sum_2016 = 0;
sum_2017 = 0;
sum_2018 = 0;
sum_2019 = 0;
sum_2020 = 0;

%Arrays for activity by time of day
activitysum_2015 = zeros(24, 1);
activitysum_2016 = zeros(24, 1);
activitysum_2017 = zeros(24, 1);
activitysum_2018 = zeros(24, 1);
activitysum_2019 = zeros(24, 1);
activitysum_2020 = zeros(24, 1);

%Arrays for regressions
dummy_jan = zeros(rows, 1); 
dummy_feb = zeros(rows, 1);
dummy_mar = zeros(rows, 1);
dummy_apr = zeros(rows, 1);
dummy_may = zeros(rows, 1);
dummy_jun = zeros(rows, 1);
dummy_jul = zeros(rows, 1); 
dummy_aug = zeros(rows, 1);
dummy_sep = zeros(rows, 1);
dummy_oct = zeros(rows, 1);
dummy_nov = zeros(rows, 1);
dummy_dec = zeros(rows, 1);
years_from_start = zeros(rows,  1); 
constant = zeros(rows, 1); 

%Iterate through all observations
for i=1:rows
    %Isolate monthly data for chosen month
    if (month(i) == 4) && (year(i) == 2015)
        count_2015 = count_2015 + 1;
        sum_2015 = sum_2015 + electricity_demand(i);
    elseif (month(i) == 4) && (year(i) == 2016)
        count_2016 = count_2016 + 1;
        sum_2016 = sum_2016 + electricity_demand(i); 
    elseif (month(i) == 4) && (year(i) == 2017)
        count_2017 = count_2017 + 1;
        sum_2017 = sum_2017 + electricity_demand(i);        
    elseif (month(i) == 4) && (year(i) == 2018)
        count_2018 = count_2018 + 1;
        sum_2018 = sum_2018 + electricity_demand(i);
    elseif (month(i) == 4) && (year(i) == 2019)
        count_2019 = count_2019 + 1;
        sum_2019 = sum_2019 + electricity_demand(i);
    elseif (month(i) == 4) && (year(i) == 2020)
        count_2020 = count_2020 + 1;
        sum_2020 = sum_2020 + electricity_demand(i);
    end
    
    %In what hour is the highest amount of electricity used? 
    currenttime = h(i) ; 
    if (year(i) == 2015)
        activitysum_2015(currenttime + 1) = activitysum_2015(currenttime + 1) + electricity_demand(i);   
    elseif(year(i) == 2016)
        activitysum_2016(currenttime + 1) = activitysum_2016(currenttime + 1) + electricity_demand(i);   
    elseif(year(i) == 2017)
        activitysum_2017(currenttime + 1) = activitysum_2017(currenttime + 1) + electricity_demand(i);   
    elseif(year(i) == 2018)
        activitysum_2018(currenttime + 1) = activitysum_2018(currenttime + 1) + electricity_demand(i);   
    elseif(year(i) == 2019)
        activitysum_2019(currenttime + 1) = activitysum_2019(currenttime + 1) + electricity_demand(i);   
    elseif(year(i) == 2020)
        activitysum_2020(currenttime + 1) = activitysum_2020(currenttime + 1) + electricity_demand(i);   
    end

    %Run regression with annual and monthly trend
    constant(i) = 1; 
    if (year(i) == 2015)
        years_from_start(i) = 1; 
    elseif(year(i) == 2016)
        years_from_start(i) = 2;
    elseif(year(i) == 2017)
        years_from_start(i) = 3;
    elseif(year(i) == 2018)
        years_from_start(i) = 4; 
    elseif(year(i) == 2019)
        years_from_start(i) = 5;   
    elseif(year(i) == 2020)
        years_from_start(i) = 6;
    end
   
    if (month(i) == 1)
        dummy_jan(i) = 1;
    elseif(month(i) == 2)
        dummy_feb(i) = 1;
    elseif(month(i) == 3)
        dummy_mar(i) = 1;
    elseif(month(i) == 4)
        dummy_apr(i) = 1;
    elseif(month(i) == 5)
        dummy_may(i) = 1;
    elseif(month(i) == 6)
        dummy_jun(i) = 1;
    elseif(month(i) == 7)
        dummy_jul(i) = 1;
    elseif(month(i) == 4)
        dummy_aug(i) = 1;
    elseif(month(i) == 9)
        dummy_sep(i) = 1;
    elseif(month(i) == 10)
        dummy_oct(i) = 1;
    elseif(month(i) == 11)
        dummy_nov(i) = 1;
    elseif(month(i) == 12)
        dummy_dec(i) = 1;
    end
end
av_2015 = sum_2015 / count_2015;
av_2016 = sum_2016 / count_2016;
av_2017 = sum_2017 / count_2017;
av_2018 = sum_2018 / count_2018;
av_2019 = sum_2019 / count_2019;
av_2020 = sum_2020 / count_2020;
x = [1 2 3 4 5 6];
y = [av_2015 av_2016 av_2017 av_2018 av_2019 av_2020];
[size_activitysum_2015 cols] = size(activitysum_2015); 
[size_activitysum_2016 cols] = size(activitysum_2016);
[size_activitysum_2017 cols] = size(activitysum_2017);
[size_activitysum_2018 cols] = size(activitysum_2018);
[size_activitysum_2019 cols] = size(activitysum_2019);
[size_activitysum_2020 cols] = size(activitysum_2020);

activitysum_2015 = activitysum_2015 / size_activitysum_2015; 
activitysum_2016 = activitysum_2016 / size_activitysum_2016; 
activitysum_2017 = activitysum_2017 / size_activitysum_2017; 
activitysum_2018 = activitysum_2018 / size_activitysum_2018; 
activitysum_2019 = activitysum_2019 / size_activitysum_2019; 
activitysum_2020 = activitysum_2020 / size_activitysum_2020; 

%Calculating the hour which sees the most activity
[max2015 i_2015] = max(activitysum_2015); 
[max2016 i_2016] = max(activitysum_2016);
[max2017 i_2017] = max(activitysum_2017);
[max2018 i_2018] = max(activitysum_2018);
[max2019 i_2019] = max(activitysum_2019);
[max2020 i_2020] = max(activitysum_2020);

%Variance between times of day
std_2015 = std(activitysum_2015) / av_2015; 
std_2016 = std(activitysum_2016) / av_2016;
std_2017 = std(activitysum_2017) / av_2017;
std_2018 = std(activitysum_2018) / av_2018;
std_2019 = std(activitysum_2019) / av_2019;
std_2020 = std(activitysum_2020) / av_2020;

%Set up regression
X = [constant years_from_start dummy_feb dummy_mar dummy_apr dummy_may dummy_jun dummy_jul dummy_aug dummy_sep dummy_oct dummy_nov dummy_dec];
y = electricity_demand; 
% X = X(1:87647, :); Excludes 2020
% y = y(1:87647); Excludes 2020
[b,bint,r,rint,stats] = regress(y,X);

%Peak analysis
a = activitysum_2020; %Can change this to other years to compare
[~, peakLocations] = findpeaks( a );
%amplitude = arrayfun( @(location) a(location) - a(location-1), peakLocations );
amplitude1 = arrayfun( @(location)a(location), peakLocations(1)); 
amplitude2 = arrayfun( @(location)a(location), peakLocations(2)); 
diff_amplitude = amplitude2 - amplitude1; 
ratio_amplitude = amplitude2 / amplitude1; 
[pks,locs,w,p] = findpeaks(a); 

%Create a 1 x 48 vector for each day - store in a table with the number of
%unique days (i.e. a table which is n_uniquedays x 50 - add in column for month and then year) 
n_uniquedays = rows / 48; 
daily_movements = zeros(n_uniquedays, 50);
for i=1:rows
    if (rem(i, 48) ~= 0)
        currenttime_number = rem(i, 48); 
    else
        currenttime_number = 48;
    end
    currentdate_number = ceil(i / 48); 
    daily_movements(currentdate_number, currenttime_number) = electricity_demand(i); 
    current_month = month(i); 
    current_year = year(i); 
    daily_movements(currentdate_number, 49) = current_month; 
    daily_movements(currentdate_number, 50) = current_year; 
end

%Want to calculate mean and std of the peak ratios for 2015-2020 on a year
%by year basis 

%Array to store the month, year, ampitude1, amplitude2,
%ratioamplitudes, width1, width2, ratiowidths
store_peak_characteristics_daily = zeros(n_uniquedays, 8); 

for j=1:n_uniquedays
 a = daily_movements(j, :); 
[~, peakLocations] = findpeaks( a);
%amplitude = arrayfun( @(location) a(location) - a(location-1), peakLocations );
amplitude1 = arrayfun( @(location)a(location), peakLocations(1)); 
[npeaks x1] = size(peakLocations); 
[pks,locs,w,p] = findpeaks(a);
if (x1 > 1)
    amplitude2 = arrayfun( @(location)a(location), peakLocations(2));
    diff_amplitude = amplitude2 - amplitude1; 
    ratio_amplitude = amplitude2 / amplitude1;
    w1 = w(1); 
    w2 = w(2); 
    ratio_widths = w2 / w1; 
else
    amplitude2 = 0; 
    diff_amplitude = 0;
    ratio_amplitude = 1; 
    w1 = w(1); 
    w2 = 0;
    ratio_widths = 1; 
end

%Store in arrays
store_peak_characteristics_daily(j, 1) = daily_movements(j, 49); 
store_peak_characteristics_daily(j, 2) = daily_movements(j, 50);
store_peak_characteristics_daily(j, 3) = amplitude1; 
store_peak_characteristics_daily(j, 4) = amplitude2; 
store_peak_characteristics_daily(j, 5) = ratio_amplitude; 
store_peak_characteristics_daily(j, 6) = w1; 
store_peak_characteristics_daily(j, 7) = w2; 
store_peak_characteristics_daily(j, 8) = ratio_widths;
end

store_mean_heightratio = zeros(6, 1); 
store_std_heightratio = zeros(6, 1); 
store_mean_widthratio = zeros(6, 1); 
store_std_widthratio = zeros(6, 1); 

sum_heightratio_2015 = 0; 
count_heightratio_2015 = 0; 
sum_widthratio_2015 = 0;
count_widthratio_2015 =  0; 
sum_heightratio_2016 = 0; 
count_heightratio_2016 = 0; 
sum_widthratio_2016 = 0;
count_widthratio_2016 =  0;
sum_heightratio_2017 = 0; 
count_heightratio_2017 = 0; 
sum_widthratio_2017 = 0;
count_widthratio_2017 =  0; 
sum_heightratio_2018 = 0; 
count_heightratio_2018 = 0; 
sum_widthratio_2018 = 0;
count_widthratio_2018 =  0; 
sum_heightratio_2019 = 0; 
count_heightratio_2019 = 0; 
sum_widthratio_2019 = 0;
count_widthratio_2019 =  0; 
sum_heightratio_2020 = 0; 
count_heightratio_2020 = 0; 
sum_widthratio_2020 = 0;
count_widthratio_2020 =  0; 
for i=1:n_uniquedays
%Analyze August 2015
 if(store_peak_characteristics_daily(i, 1) == 8) && (store_peak_characteristics_daily(i, 2) == 2015)
    sum_heightratio_2015 = sum_heightratio_2015 + store_peak_characteristics_daily(i, 5);
    count_heightratio_2015 = count_heightratio_2015 + 1;
    sum_widthratio_2015 = sum_widthratio_2015 + store_peak_characteristics_daily(i, 8);
    count_widthratio_2015 = count_widthratio_2015 + 1;
 end
%Analyze August 2016
 if(store_peak_characteristics_daily(i, 1) == 8) && (store_peak_characteristics_daily(i, 2) == 2016)
    sum_heightratio_2016 = sum_heightratio_2016 + store_peak_characteristics_daily(i, 5);
    count_heightratio_2016 = count_heightratio_2016 + 1;
    sum_widthratio_2016 = sum_widthratio_2016 + store_peak_characteristics_daily(i, 8);
    count_widthratio_2016 = count_widthratio_2016 + 1;
 end
%Analyze August 2017
 if(store_peak_characteristics_daily(i, 1) == 8) && (store_peak_characteristics_daily(i, 2) == 2017)
    sum_heightratio_2017 = sum_heightratio_2017 + store_peak_characteristics_daily(i, 5);
    count_heightratio_2017 = count_heightratio_2017 + 1;
    sum_widthratio_2017 = sum_widthratio_2017 + store_peak_characteristics_daily(i, 8);
    count_widthratio_2017 = count_widthratio_2017 + 1;
 end
%Analyze August 2018
 if(store_peak_characteristics_daily(i, 1) == 8) && (store_peak_characteristics_daily(i, 2) == 2018)
    sum_heightratio_2018 = sum_heightratio_2018 + store_peak_characteristics_daily(i, 5);
    count_heightratio_2018 = count_heightratio_2018 + 1;
    sum_widthratio_2018 = sum_widthratio_2018 + store_peak_characteristics_daily(i, 8);
    count_widthratio_2018 = count_widthratio_2018 + 1;
 end
%Analyze August 2019
 if(store_peak_characteristics_daily(i, 1) == 8) && (store_peak_characteristics_daily(i, 2) == 2019)
    sum_heightratio_2019 = sum_heightratio_2019 + store_peak_characteristics_daily(i, 5);
    count_heightratio_2019 = count_heightratio_2019 + 1;
    sum_widthratio_2019 = sum_widthratio_2019 + store_peak_characteristics_daily(i, 8);
    count_widthratio_2019 = count_widthratio_2019 + 1;
 end
%Analyze August 2020
 if(store_peak_characteristics_daily(i, 1) == 8) && (store_peak_characteristics_daily(i, 2) == 2020)
    sum_heightratio_2020 = sum_heightratio_2020 + store_peak_characteristics_daily(i, 5);
    count_heightratio_2020 = count_heightratio_2020 + 1;
    sum_widthratio_2020 = sum_widthratio_2020 + store_peak_characteristics_daily(i, 8);
    count_widthratio_2020 = count_widthratio_2020 + 1;
 end
end

%Take the mean for each year
mean_heightratio_2015 = mean(sum_heightratio_2015) / count_heightratio_2015; 
mean_widthratio_2015 = mean(sum_widthratio_2015) / count_widthratio_2015; 
mean_heightratio_2016 = mean(sum_heightratio_2016) / count_heightratio_2016; 
mean_widthratio_2016 = mean(sum_widthratio_2016) / count_widthratio_2016; 
mean_heightratio_2017 = mean(sum_heightratio_2017) / count_heightratio_2017; 
mean_widthratio_2017 = mean(sum_widthratio_2017) / count_widthratio_2017;
mean_heightratio_2018 = mean(sum_heightratio_2018) / count_heightratio_2018; 
mean_widthratio_2018 = mean(sum_widthratio_2018) / count_widthratio_2018;
mean_heightratio_2019 = mean(sum_heightratio_2019) / count_heightratio_2019; 
mean_widthratio_2019 = mean(sum_widthratio_2019) / count_widthratio_2019; 
mean_heightratio_2020 = mean(sum_heightratio_2020) / count_heightratio_2020; 
mean_widthratio_2020 = mean(sum_widthratio_2020) / count_widthratio_2020; 

X1 = [mean_heightratio_2015 mean_heightratio_2016 mean_heightratio_2017 mean_heightratio_2018 mean_heightratio_2019 mean_heightratio_2020];
Y1 = [2015 2016 2017 2018 2019 2020];

X2 = [mean_widthratio_2015 mean_widthratio_2016 mean_widthratio_2017 mean_widthratio_2018 mean_widthratio_2019 mean_widthratio_2020];

%Detect outliers
A = [av_2015 av_2016 av_2017 av_2018 av_2019 av_2020];
isoutlier(A, 'median')
isoutlier(A, 'mean')
