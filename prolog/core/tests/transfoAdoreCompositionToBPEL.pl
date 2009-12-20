%consult('/Users/mireilleblay-fornarino/Documents/ADORE/adore/prolog/src/tests/transfoAdoreCompositionToBPEL.pl').
loadFiles4Transfo :- 
	aw('%%%% Loading transfo Kernel'),
        ['/Users/mireilleblay-fornarino/Documents/ADORE/adore/prolog/src/tests/toBPEL-4.pl'], 
        ['/Users/mireilleblay-fornarino/Documents/ADORE/adore/prolog/src/tests/weave.pl'], 
        ['/Users/mireilleblay-fornarino/Documents/ADORE/adore/prolog/src/tests/dedicatedFunctions.pl'].
        
 
executeTransfo(Orchestration) :-
    process(Orchestration),
    weave(Orchestration,_Set),
    toBpel(Orchestration,P),write('\n==============\n'),write(P).
    