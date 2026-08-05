# Hodgkin huxley equations
init v=-65 m=.05 h=0.6 n=.317
par i0=0
par vna=50 vk=-77 vl=-54.4 gna=120 gk=36 gl=0.3 c=1 phi=1
par ip=0 pon=50 poff=150
is(t)=ip*heav(t-pon)*heav(poff-t)
am(v)=phi*.1*(v+40)/(1-exp(-(v+40)/10))
bm(v)=phi*4*exp(-(v+65)/18)
ah(v)=phi*.07*exp(-(v+65)/20)
bh(v)=phi*1/(1+exp(-(v+35)/10))
an(v)=phi*.01*(v+55)/(1-exp(-(v+55)/10))
bn(v)=phi*.125*exp(-(v+65)/80)
v’=(I0+is(t) - gna*h*(v-vna)*m^3-gk*(v-vk)*n^4-gl*(v-vl)-gsyn*s*(v-vsyn))/c
m’=am(v)*(1-m)-bm(v)*m
h’=ah(v)*(1-h)-bh(v)*h
n’=an(v)*(1-n)-bn(v)*n
s’=sinf(v)*(1-s)-s/tausyn
# track the currents
sinf(v)=alpha/(1+exp(-v/vshp))
par alpha=2,vshp=5,tausyn=20,gsyn=0,vsyn=0
aux ina=gna*(v-vna)*h*m^3
aux ik=gk*(v-vk)*n^4
aux il=gl*(v-vl)
# track the stimulus
aux stim=is(t)
@ bound=10000
done

# hh.ode equivalent potentials
init v=-65 vm=-65,vn=-65,vh=-65
par i0, vna=50 vk=-77 vl=-54.4 gna=120 gk=36 gl=0.3 c=1 phi=1
par eps=.1
am(v)=phi*.1*(v+40)/(1-exp(-(v+40)/10))
bm(v)=phi*4*exp(-(v+65)/18)
ah(v)=phi*.07*exp(-(v+65)/20)
bh(v)=phi*1/(1+exp(-(v+35)/10))
an(v)=phi*.01*(v+55)/(1-exp(-(v+55)/10))
bn(v)=phi*.125*exp(-(v+65)/80)
minf(v)=am(v)/(am(v)+bm(v))
ninf(v)=an(v)/(an(v)+bn(v))
hinf(v)=ah(v)/(ah(v)+bh(v))
km(v)=am(v)+bm(v)
kn(v)=an(v)+bn(v)
kh(v)=ah(v)+bh(v)
mp(v)=(minf(v+eps)-minf(v-eps))/(2*eps)
np(v)=(ninf(v+eps)-ninf(v-eps))/(2*eps)
hp(v)=(hinf(v+eps)-hinf(v-eps))/(2*eps)
v’=(I0 - gna*hinf(vh)*(v-vna)*minf(vm)^3-gk*(v-vk)*ninf(vn)^4-gl*(v-vl))/c
vm’=km(v)*(minf(v)-minf(vm))/mp(vm)
vn’=kn(v)*(ninf(v)-ninf(vn))/np(vn)
vh’=kh(v)*(hinf(v)-hinf(vh))/hp(vh)
aux n=ninf(vn)
aux h=hinf(vh)
done

# reduced HH equations using the rinzel reduction and n
# as the variable
init v=-65 n=.4
par i0=0
par vna=50 vk=-77 vl=-54.4 gna=120 gk=36 gl=0.3 c=1 phi=1
par ip=0 pon=50 poff=150
is(t)=ip*heav(t-pon)*heav(poff-t)
am(v)=phi*.1*(v+40)/(1-exp(-(v+40)/10))
bm(v)=phi*4*exp(-(v+65)/18)
ah(v)=phi*.07*exp(-(v+65)/20)
bh(v)=phi*1/(1+exp(-(v+35)/10))
an(v)=phi*.01*(v+55)/(1-exp(-(v+55)/10))
bn(v)=phi*.125*exp(-(v+65)/80)
v’=(I0+is(t) - gna*h*(v-vna)*m^3-gk*(v-vk)*n^4-gl*(v-vl))/c
m=am(v)/(am(v)+bm(v))
#h’=ah(v)*(1-h)-bh(v)*h
n’=an(v)*(1-n)-bn(v)*n
h=h0-n
par h0=.8
@ bound=10000
done
