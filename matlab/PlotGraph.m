%Plotting Average Half-Hourly Consumption
clear clf

%Hard code average half hourly energy consumption from previous results
X_August = [5.5047 5.4658 5.3981 5.2987 5.3278 5.1448];
X_April = [4.9836 4.8638 4.4886 4.7139 4.5261 4.4853];
X_August_Without2020 = [5.5047 5.4658 5.3981 5.2987 5.3278];
X_April_Without2020 = [4.9836 4.8638 4.4886 4.7139 4.5261];
years_without_2020 = [2015 2016 2017 2018 2019];

%Calculates metrics needed to determine mean deviation / standard deviation
%for robustness checks
August_Mean = median(X_August);
August_STD = std(X_August);
April_Mean = median(X_April);
April_STD = std(X_April); 
August_MeanDeviation = X_August - [August_Mean August_Mean August_Mean August_Mean August_Mean August_Mean];
April_MeanDeviation = X_April - [April_Mean April_Mean April_Mean April_Mean April_Mean April_Mean];

years = [2015 2016 2017 2018 2019 2020]; 
ones = [1 1 1 1 1 1];
years_and_constant = [transpose(years) transpose(ones)];


[b_apr,bint_apr,r_apr,rint_apr,stats_apr] = regress(transpose(X_April), years_and_constant);
[b_aug,bint_aug,r_aug,rint_aug,stats_aug] = regress(transpose(X_August),years_and_constant);

a1 = plot(years, X_August, 'color', 'red'); 
hold on; 
a2 = plot(years, X_April, 'color', 'blue'); 
hold on; 
ylabel('Electricity Demand (MWh) x 1000');
axis([2015 2020 0 6]);

legend([a2 a1],'April Electricity Demand','August Electricity Demand')