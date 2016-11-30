set.seed(1)

day = function(){
	lambda = c(8,10,12,8,11)
window.s = c(9,10,11.5,13,15)
window.e = c(10,11.5,13,15,17)
lambda.int = window.e - window.s


an = rpois(length(lambda), lambda*lambda.int)
arrivals = 0
for(i in 1:length(lambda)){
window.arrive = sort(runif(an[i], window.s[i], window.e[i]) )
arrivals = c(arrivals,window.arrive)
}
arrivals = arrivals[-1]
arrivals


N = length(arrivals)
##first four departures
d = arrivals[1] + rbeta(1,1,2.3)

##event type   -1:departure, 1:arrivals, -2:teller return from lunch
e.t = c(rep(1,N),-1)

time = c(arrivals,d)
index = order(time)

run.tab = cbind(time,e.t)[index,]
time = run.tab[,1]
e.t = run.tab[,2]

##t.i = num: time teller next becomes available
##t.i = 0: no customer with teller
t.1 = d
t.2 = 0
t.3 = 0
t.4 = 0
queue = 0
new = 0
n.iter = 2*N + 4

##are they on lunch?
## 0 : no,   1:yes,   2:back from lunch
t1.lunch = 0
t2.lunch = 0
t3.lunch = 0
t4.lunch = 0
lunch.threshold = 11

queue.i = numeric(n.iter)
##need to determine if the next thing is an arrival or departure

for(i in 2:n.iter){

	if(e.t[i] == 1)
	{
		if(t.1[i-1] == 0)
		{
			t.1[i] = time[i] + rbeta(1,1,2.3)
			t.2[i] = t.2[i-1]
			t.3[i] = t.3[i-1]
			t.4[i] = t.4[i-1]
			new = c(t.1[i],-1)
		}
		else
		{
			t.1[i] = t.1[i-1]
			if(t.2[i-1] == 0)
			{
				t.2[i] = time[i] + rbeta(1,1,2.3)
				t.3[i] = t.3[i-1]
				t.4[i] = t.4[i-1]
				new = c(t.2[i],-1)
			}
			else
			{
				t.2[i] = t.2[i-1]
				if(t.3[i-1] == 0)
				{
					t.3[i] = time[i] + rbeta(1,1,2.3)
					t.4[i] = t.4[i-1]
					new = c(t.3[i],-1)
				}
				else
				{
					t.3[i] = t.3[i-1]
					if(t.4[i-1] == 0)
					{
						t.4[i] = time[i] + rbeta(1,1,2.3)
						new = c(t.4[i],-1)
					}
					else
					{
						t.4[i] = t.4[i-1]
						queue = queue + 1
					}
				}
			}
		}
	}

	if(e.t[i] == -1)
	{
	
		if(t.1[i-1] <= time[i])
		{
			if(time[i] >= lunch.threshold & t1.lunch == 0 & 
				t2.lunch != 1 & t3.lunch != 1 & t4.lunch != 1)
			{
				t1.lunch = 1
				t.1[i] = time[i] + .5
				new = c(t.1[i],2)
			}
			else 
			{	
				if(queue == 0) {t.1[i] = 0}
				else
				{
					t.1[i] = time[i] + rbeta(1,1,2.3)
					queue = queue - 1
					new = c(t.1[i],-1)
				}
			}
		}
		if(t.1[i-1] > time[i])
		{
			t.1[i] = t.1[i-1]
		}
		
		if(t.2[i-1] <= time[i])
		{
			if(time[i] >= lunch.threshold & t2.lunch == 0 &
				t1.lunch != 1 & t3.lunch != 1 & t4.lunch != 1)
			{
				t2.lunch = 1
				t.2[i] = time[i] + .5
				new = c(t.2[i],2)
			}
			
			else 
			{	
				if(queue == 0) {t.2[i] = 0}
				else
				{
					t.2[i] = time[i] + rbeta(1,1,2.3)
					queue = queue - 1
					new = c(t.2[i],-1)
				}	
			}
		}
		if(t.2[i-1] > time[i])
		{
			t.2[i] = t.2[i-1]
		}

		if(t.3[i-1] <= time[i])
		{
			if(time[i] >= lunch.threshold & t3.lunch == 0 &
				t1.lunch != 1 & t2.lunch != 1 & t4.lunch != 1)
			{
				t3.lunch = 1
				t.3[i] = time[i] + .5
				new = c(t.3[i],2)
			}
			else 
			{
				if(queue == 0) {t.3[i] = 0}
				else
				{
					t.3[i] = time[i] + rbeta(1,1,2.3)
					queue = queue - 1
					new = c(t.3[i],-1)
				}
			}
		}
		if(t.3[i-1] > time[i])
		{
			t.3[i] = t.3[i-1]
		}

		if(t.4[i-1] <= time[i])
		{
			if(time[i] >= lunch.threshold & t4.lunch == 0 &
				t1.lunch != 1 & t2.lunch != 1 & t3.lunch != 1)
			{
				t4.lunch = 1
				t.4[i] = time[i] + .5
				new = c(t.4[i],2)
			}
			else 
			{
				if(queue == 0) {t.4[i] = 0}
				else
				{	
					t.4[i] = time[i] + rbeta(1,1,2.3)
					queue = queue - 1
					new = c(t.4[i],-1)
				}
			}
		}
		if(t.4[i-1] > time[i])
		{
			t.4[i] = t.4[i-1]
		}

	}

	if(e.t[i] == 2)
	{
		if(t.1[i-1] == time[i])
		{
			t.1[i] = ifelse(queue == 0, 0, time[i]+rbeta(1,1,2.3))
			t1.lunch = 2
			t.2[i] = t.2[i-1]
			t.3[i] = t.3[i-1]
			t.4[i] = t.4[i-1]
			if(t.1[i] != 0)
			{
				new = c(t.1[i],-1)
				queue = queue -1
			}
		}
		if(t.2[i-1] == time[i])
		{
			t.2[i] = ifelse(queue == 0, 0, time[i]+rbeta(1,1,2.3))
			t2.lunch = 2
			t.1[i] = t.1[i-1]
			t.3[i] = t.3[i-1]
			t.4[i] = t.4[i-1]
			if(t.2[i] != 0)
			{
				new = c(t.2[i],-1)
				queue = queue -1
			}
		}
		if(t.3[i-1] == time[i])
		{
			t.3[i] = ifelse(queue == 0, 0, time[i]+rbeta(1,1,2.3))
			t3.lunch = 2
			t.1[i] = t.1[i-1]
			t.2[i] = t.2[i-1]
			t.4[i] = t.4[i-1]
			if(t.3[i] != 0)
			{
				new = c(t.3[i],-1)
				queue = queue -1
			}
			
		}
		if(t.4[i-1] == time[i])
		{
			t.4[i] = ifelse(queue == 0, 0, time[i]+rbeta(1,1,2.3))
			t4.lunch = 2
			t.1[i] = t.1[i-1]
			t.2[i] = t.2[i-1]
			t.3[i] = t.3[i-1]
			if(t.4[i] != 0)
			{
				new = c(t.4[i],-1)
				queue = queue -1
			}
		}
	}


	if(new[1] != 0)
	{
		t.new = c(time,new[1])
		e.t.new = c(e.t,new[2])
		index = order(t.new)
		run.tab = cbind(t.new,e.t.new)[index,]
		time = run.tab[,1]
		e.t = run.tab[,2]
		new = 0
	}

	
	queue.i[i] = queue

if(t.1[i] == 0 & t.2[i] == 0 & t.3[i] == 0 & t.4[i] == 0 & time[i] >= 17) break

}

cbind(time,e.t,t.1,t.2,t.3,t.4,queue.i)

if(table(e.t)[1] != table(e.t)[2]) print("#arrivals =/= #departures")

hours.t1=0
hours.t2=0
hours.t3=0
hours.t4=0

for(i in 1:(n.iter-1)){
	if(t.1[i] != 0)
	hours.t1=time[i+1]-time[i]+hours.t1
	if(t.2[i] != 0)
	hours.t2=time[i+1]-time[i]+hours.t2
	if(t.3[i] != 0)
	hours.t3=time[i+1]-time[i]+hours.t3
	if(t.4[i] != 0)
	hours.t4=time[i+1]-time[i]+hours.t4
}

busy=hours.t1+hours.t2+hours.t3+hours.t4-2
total=(max(time)-9+.25)*4
busy.perc = busy/total*100

return(list=c(total,busy.perc))
}


day()

output = matrix(0,nrow=10,ncol=2)

for(j in 1:10){
	output[j,]=day()
}

average=matrix(c(mean(output[,1]),mean(output[,2])),1,2)
colnames(average)=c("total","busy percent")
rownames(average)=""
average

sd=matrix(c(sd(output[,1]),sd(output[,2])),1,2)
colnames(sd)=c("total","busy percent")
rownames(sd)=""
sd

total.ci=c(average[1]-1.96*sd[1],average[1]+1.96*sd[1])
busy.ci=c(average[2]-1.96*sd[2],average[2]+1.96*sd[2])
total.ci
busy.ci
