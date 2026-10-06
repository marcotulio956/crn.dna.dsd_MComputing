# Morris-Lecar model Methods Chapter
dv/dt = ( I - gca*minf(V)*(V-Vca)-gk*w*(V-VK)-gl*(V-Vl)+s(t))/c
dw/dt = phi*(winf(V)-w)/tauw(V)
v(0)=-16
w(0)=0.014915
minf(v)=.5*(1+tanh((v-v1)/v2))
winf(v)=.5*(1+tanh((v-v3)/v4))
tauw(v)=1/cosh((v-v3)/(2*v4))
param vk=-84,vl=-60,vca=120
param i=0,gk=8,gl=2,c=20
param v1=-1.2,v2=18
# Uncomment the ones you like!!
par1-3 v3=2,v4=30,phi=.04,gca=4.4
set hopf {v3=2,v4=30,phi=.04,gca=4.4}
set snic {v3=12,v4=17.4,phi=.06666667,gca=4}
set homo {v3=12,v4=17.4,phi=.23,gca=4}
#par4-6 v3=12,v4=17.4,phi=.06666667,gca=4
#par7-8 v3=12,v4=17.4,phi=.23,gca=4
param s1=0,s2=0,t1=50,t2=55,t3=500,t4=550
# double pulse stimulus
s(t)=s1*heav(t-t1)*heav(t2-t)+s2*heav(t-t3)*heav(t4-t)
@ total=150,dt=.25,xlo=-75,xhi=75,ylo=-.25,yhi=.5,xp=v,yp=w
done