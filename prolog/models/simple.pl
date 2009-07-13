%% test1 :- 
%% 	defActivity(a), defActivity(b), defActivity(c),
%% 	defWaitFor(b,a), defWaitFor(c,b),
%% 	defProcess(test1), 
%% 	defBelongsTo(a,test1), defBelongsTo(b, test1), defBelongsTo(c,test1),
%% 	defVariable(x), defAsInput().

%% cycle :- 
%% 	defActivity(a), defActivity(b), defActivity(c), 
%% 	defWaitFor(b,a), defWaitFor(c,b), defWaitFor(a,c).

:- 
        defProcess(service_operation),
        defActivity(a1),
        defBelongsTo(a1,service_operation),
        defActivity(a2),
        defBelongsTo(a2,service_operation),
        defActivity(a3),
        defBelongsTo(a3,service_operation),
        defWaitFor(a1,a2),
        defWaitFor(a2,a3),
        defProcess(addCache),
        defActivity(e1),
        defBelongsTo(e1,addCache),
        defActivity(e2),
        defBelongsTo(e2,addCache),
        defActivity(e3),
        defBelongsTo(e3,addCache).
