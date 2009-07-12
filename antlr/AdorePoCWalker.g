tree grammar AdorePoCWalker;

options {
  ASTLabelType = CommonTree;
  tokenVocab=AdoreParser;
}

@header { package fr.unice.i3s.modalis.adore.language; }
@member { import java.util.ArrayList; }

definitions returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	:	^(DEFINITIONS (definition {$facts.addAll($definition.facts);} )+);
	
definition returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	:	^(ORCHESTRATION s=IDENTIFIER o=IDENTIFIER behaviour[$s+"_"+$o]) 
			{ String name = $s.text+"_"+$o.text;
			  $facts.add("defProcess("+name+")");
			  $facts.addAll($behaviour.activitiesFacts);
			  $facts.addAll($behaviour.relationsFacts); }
	|	^(EVOLUTION n=IDENTIFIER behaviour[$n.text]) 
			{ $facts.add("defProcess("+$n.text+")");
			  //$facts.add("defEvolution("+$n.text+")"); 
			  $facts.addAll($behaviour.activitiesFacts);
			  $facts.addAll($behaviour.relationsFacts); };

behaviour [String owner] 
	returns [ArrayList<String> activitiesFacts, ArrayList<String> relationsFacts]
	@init{ $activitiesFacts = new ArrayList<String>(); 
	       $relationsFacts = new ArrayList<String>();}
	:	^(BEHAVIOUR activities[$owner]relations?) 
			{ $activitiesFacts.addAll($activities.facts);
			  if($relations.facts != null)
			  	$relationsFacts.addAll($relations.facts);
			 };

activities [String owner]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	:	^(ACTIVITIES (activity[$owner] 
			{$facts.addAll($activity.facts);} )+ );

activity [String owner] 
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	: 	^(ACTIVITY id=IDENTIFIER kind[$id.text] inputs[$id.text])
			{ $facts.add("defActivity("+$id.text+")");
			  $facts.add("defBelongsTo("+$id.text+","+$owner+")");
			  $facts.addAll($inputs.facts);  
			  $facts.add($kind.f); };

kind[String id]
	returns[String f]
	: ^(KIND RECEIVE) 			{ $f = "defKind("+$id+",receive)"; } 
	| ^(KIND REPLY) 			{ $f = "defKind("+$id+",reply)"; } 
	| ^(KIND THROW) 			{ $f = "defKind("+$id+",throw)"; } 
	| ^(KIND ASSIGNMENT fct=IDENTIFIER) 	{ $f = "defKind("+$id+",assign("+$fct.text+"))"; } 
	| ^(KIND INVOKE s=IDENTIFIER o=IDENTIFIER) 
		{$f = "defKind("+$id+",invoke("+$s.text+","+$o.text+"))";} ;

inputs[String id]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	:	^(INPUTS param_list)
			{ for(String ident: $param_list.identifiers)
				$facts.add("defAsInput("+ident+","+id+")"); };

param_list 
	returns [ArrayList<String> identifiers]
	@init{ $identifiers = new ArrayList<String>(); }
	: ^(PARAMETERS (i=IDENTIFIER { $identifiers.add($i.text); })*) ; 


relations
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	:	^(RELATIONS (relation { $facts.addAll($relation.facts); } )+);


relation
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	: ^(WAIT_FOR l=IDENTIFIER r=IDENTIFIER) {$facts.add("defWaitFor("+$l.text+","+$r.text+")");};
	
	
	
	
