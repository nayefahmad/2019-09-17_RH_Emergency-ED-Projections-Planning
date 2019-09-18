use EDMART
declare @baseyear int,@projectionyear int,@yearcounter int,@mindateid int,@maxdateid int,@facilitylongname varchar(75),@maxgrowthrate float,@bcgrowthrate float
set @facilitylongname='richmond hospital'
set @projectionyear=2036
set @baseyear=2019 --use fiscalyearlong formnat in dim.[date]
set @yearcounter=@baseyear + 1
set @mindateid=(select min(dateid) from dim.[date] where fiscalyearlong=@baseyear)
set @maxdateid=(select max(dateid) from dim.[date] where fiscalyearlong=@baseyear)
set @maxgrowthrate=0.04

set @bcgrowthrate=(select sum(y.[Population])/sum(b.[Population]) as actualgrowthrate
from DSSI.[dbo].[PEOPLE2018Complete] y
left outer join DSSI.[dbo].[PEOPLE2018Complete] b on y.agegroup=b.agegroup and y.gender=b.gender and y.lhaid=b.lhaid
where y.[year]=@projectionyear  
and b.[year]=@baseyear)

select case when age = 0 then '<1'
when age between 1 and 4 then '1-4'
when age between 5 and 9 then '5-9'
when age between 10 and 14 then '10-14'
when age between 15 and 19 then '15-19'
when age between 20 and 24 then '20-24'
when age between 25 and 29 then '25-29' 
when age between 30 and 34 then '30-34'
when age between 35 and 39 then '35-39'
when age between 40 and 44 then '40-44'
when age between 45 and 49 then '45-49'
when age between 50 and 54 then '50-54'
when age between 55 and 59 then '55-59'
when age between 60 and 64 then '60-64'
when age between 65 and 69 then '65-69'
when age between 70 and 74 then '70-74'
when age between 75 and 79 then '75-79'
when age between 80 and 84 then '80-84'
when age between 85 and 89 then '85-89'
when age >= 90 then '90+'
else 'UNK'end as agegroup
,gendercode as gender
,TriageAcuityCode
,lhaid,1.0*count(*) as visits
into #visits
from [dbo].[vwEDVisitIdentifiedRegional]
left outer join dim.LHA on LocalHealthAuthority = LHAName
where FacilityLongName = @facilitylongname 
and startdateid between @mindateid and @maxdateid
--and gendercode<>'u' and lhaid not in (0,203,204,205)
group by case when age = 0 then '<1'
when age between 1 and 4 then '1-4'
when age between 5 and 9 then '5-9'
when age between 10 and 14 then '10-14'
when age between 15 and 19 then '15-19'
when age between 20 and 24 then '20-24'
when age between 25 and 29 then '25-29' 
when age between 30 and 34 then '30-34'
when age between 35 and 39 then '35-39'
when age between 40 and 44 then '40-44'
when age between 45 and 49 then '45-49'
when age between 50 and 54 then '50-54'
when age between 55 and 59 then '55-59'
when age between 60 and 64 then '60-64'
when age between 65 and 69 then '65-69'
when age between 70 and 74 then '70-74'
when age between 75 and 79 then '75-79'
when age between 80 and 84 then '80-84'
when age between 85 and 89 then '85-89'
when age >= 90 then '90+'
else 'UNK'end 
,gendercode,TriageAcuityCode
,lhaid
order by agegroup,gender,lhaid

select @projectionyear as[year],agegroup,gender,lhaid,1.00000 as growthrate
into #growth
from DSSI.[dbo].[PEOPLE2018Complete] 
where [year]=@baseyear

while @yearcounter<= @projectionyear
begin

update #growth
set #growth.growthrate=#growth.growthrate * a.adjustedgrowthrate
from #growth 
left outer join 
(select y.[year] as year2,b.[year] as year1,b.agegroup,b.gender,b.lhaid,y.[Population]/b.[Population] as actualgrowthrate
,case when y.[Population]/b.[Population] > 1 and y.[Population]/b.[Population] >= @maxgrowthrate + 1 then @maxgrowthrate + 1 
when y.[Population]/b.[Population] = 1 then 1
when y.[Population]/b.[Population] < 1 and y.[Population]/b.[Population] < 1-@maxgrowthrate then 1-@maxgrowthrate 
--when y.[Population]/b.[Population] < 1 then y.[Population]/b.[Population] 
else y.[Population]/b.[Population]
end as adjustedgrowthrate 
from DSSI.[dbo].[PEOPLE2018Complete] y
left outer join DSSI.[dbo].[PEOPLE2018Complete] b on y.agegroup=b.agegroup and y.gender=b.gender and y.lhaid=b.lhaid
where y.[year]=@yearcounter  
and b.[year]=@yearcounter-1) a on #growth.agegroup=a.agegroup and #growth.gender=a.gender and #growth.lhaid=a.lhaid

set @yearcounter = @yearcounter +1
end

update #visits 
set #visits.visits=#visits.visits * case when #growth.growthrate is null then @bcgrowthrate else #growth.growthrate end
from #visits
left outer join #growth on #growth.agegroup=#visits.agegroup and #growth.gender=#visits.gender and #growth.lhaid=#visits.lhaid

--select * from #growth order by agegroup,gender,lhaid
select @projectionyear as [year],#visits.* from #visits --where visits is not null
order by agegroup,gender,lhaid

drop table #growth
drop table #visits