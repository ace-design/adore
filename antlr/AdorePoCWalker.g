/** This file is part of ADORE [ www.adore-design.org ]
 *
 * Copyright (C) 2008-  Sebastien Mosser
 *
 * ADORE is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ADORE is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with jSeduite:DataCache; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * @author      Main S�bastien Mosser          [mosser@polytech.unice.fr]
 * @remarks: 	this is a QUICK AND DIRTY compiler ... really ...
 *		Do not do this at home ... !
 **/
tree grammar AdorePoCWalker;

options {
  ASTLabelType = CommonTree;
  tokenVocab=AdoreParser;
}

@header { 
package fr.unice.i3s.modalis.adore.language; 
import java.util.ArrayList;
import java.io.*;
}

@members {  

  private void safeAdd(ArrayList<String> target, ArrayList<String> source) {
    if(null != source)
      target.addAll(source);
  }

  private void trace(ArrayList<String> facts,String o, String n, String kind, String cxt) {
  	String f = "traceRename(" + kind + "," + o + "," + n + ",compile("+cxt+"))";
  	facts.add(f);
  }
  
  private void fragmentize(ArrayList<String> facts,String process) {
  	facts.add("createActivity("+process+"_preds)");
  	facts.add("setActivityKind("+process+"_preds,predecessors)");
  	facts.add("setContainment("+process+"_preds,"+process+")");
  	facts.add("createActivity("+process+"_succs)");
  	facts.add("setActivityKind("+process+"_succs,successors)");
  	facts.add("setContainment("+process+"_succs,"+process+")");
  }
}

  
definitions returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	:	^(DEFINITIONS 	( definition 		{ $facts.addAll($definition.facts); })+
		)
				
	;

definition returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	: ^(DEF ^(REQUIRE f=STR)) 		{ Compiler c = new Compiler();
						  try {
		 				    safeAdd($facts,c.run($f.text.substring(1,$f.text.length()-1)));
		 				  } catch(Exception e) {
		 				    System.err.println("\%\% " + e);
		 				  }
						} 
	| ^(DEF ^(ORCHESTRATION s=ID o=ID) core[$s+"_"+$o])	
						{ String name = $s.text + "_" + $o.text;
						  $facts.add("createProcess("+name+")");
						  $facts.add("setService("+name+","+$s.text+")");
						  $facts.add("setOperation("+name+","+$o.text+")");
						  safeAdd($facts, $core.facts);
						}
	| ^(DEF ^(FRAGMENT n=ID) core[$n.text])	{ $facts.add("createProcess("+$n.text+")");
						  $facts.add("setAsFragment("+$n.text+")");
						  fragmentize($facts, $n.text);
						  safeAdd($facts, $core.facts);
						}
						
	| ^(DEF merge)				{ safeAdd($facts,$merge.facts); }	
	;

core	[String cxt]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	:	vars[$cxt] acts[$cxt] rels[$cxt]?	{ safeAdd($facts, $vars.facts);
							  safeAdd($facts, $acts.facts);
						  	  safeAdd($facts, $rels.facts);
							} 
	;
	
vars	[String cxt]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	: ^(VARIABLES (decl_var[$cxt]	{ safeAdd($facts,$decl_var.facts); }  )* )	
	;

decl_var  [String cxt]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	:	^(VAR varname[$cxt] t=ID) 		{ safeAdd($facts, $varname.facts);
							  String name = $cxt + "_" + $varname.id;
						 	  $facts.add("setVariableType("+name+","+$t.text+")");
						 	  trace($facts, $varname.id, name, "variable", $cxt);
						 	}
	|	^(VAR varname[$cxt] t=ID v=STR)		{ 
							  safeAdd($facts, $varname.facts);
							  String name = $cxt + "_" + $varname.id;
						 	  $facts.add("setVariableType("+name+","+$t.text+")");
							  $facts.add("setInitValue("+name+","+$v.text+")");
							  trace($facts, $varname.id, name, "variable", $cxt);
							}
	|	^(CONST varname[$cxt] t=ID v=STR) 	{ safeAdd($facts, $varname.facts);
							  String name = $cxt + "_" + $varname.id;
						 	  $facts.add("setVariableType("+name+","+$t.text+")");
							  $facts.add("setInitValue("+name+","+$v.text+")");
							  $facts.add("setConstancy("+name+")");
							  trace($facts, $varname.id, name, "constant", $cxt);
							}
	;

varname	[String cxt]
	returns [ArrayList<String> facts, String id]
	@init{ $facts = new ArrayList<String>(); $id="";}
	:	^(SCALAR n=ID)			{ $id = $n.text;
						  $facts.add("createVariable("+ $cxt+"_"+ $id + ")"); }
	|	^(SET n=ID)			{ $id = $n.text+"_star";
						  $facts.add("createVariable("+$cxt+"_"+ $id+")");
						  $facts.add("flagAsSet("+$cxt+"_"+ $id+")");
						}
	;

acts	[String cxt]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	: ^(ACTIVITIES (activity[$cxt]		 { safeAdd($facts, $activity.facts); })+)
	;
	
activity [String cxt]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	: ^(ACT id=ID  ^(KIND kind[$cxt+"_"+$id]) act_io[$id.text, $cxt])	
						{ String name = $cxt + "_" + $id.text;
						  $facts.add("createActivity("+name+")");
						  safeAdd($facts,$kind.facts);
						  safeAdd($facts,$act_io.facts);
						  $facts.add("setContainment("+name+","+$cxt+")");
						  trace($facts,$id.text,name,"activity", $cxt);
						} 
	;

act_io	[String id, String cxt]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	: INS OUTS 				{}
	| ^(INS vlist[$id,$cxt]) OUTS 		{ for(String v: $vlist.identifiers)
						    $facts.add("addAsInput("+v+","+$cxt+"_"+$id+")");
						  safeAdd($facts,$vlist.facts);
						}
	| INS ^(OUTS vlist[$id,$cxt])		{ for(String v: $vlist.identifiers)
						    $facts.add("addAsOutput("+v+","+$cxt+"_"+$id+")");
						  safeAdd($facts,$vlist.facts);
						}
	| ^(INS i=vlist[$id,$cxt]) ^(OUTS o=vlist[$id,$cxt])	
						{ for(String v: $i.identifiers)
						    $facts.add("addAsInput("+v+","+$cxt+"_"+$id+")");
						  safeAdd($facts,$i.facts);
						  for(String v: $o.identifiers)
					  	    $facts.add("addAsOutput("+v+","+$cxt+"_"+$id+")");
						  safeAdd($facts,$o.facts);
						}
	; 

kind	[String actId]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	:	RECEIVE			{ $facts.add("setActivityKind("+$actId+",receive)"); }
	|	REPLY			{ $facts.add("setActivityKind("+$actId+",reply)");}
	|	THROW			{ $facts.add("setActivityKind("+$actId+",throw)");}
	|	HOOK			{ $facts.add("setActivityKind("+$actId+",hook)");}
	| 	INVOKE s=ID o=ID	{ $facts.add("setActivityKind("+$actId+",invoke)");
					  $facts.add("setInvokedService("+$actId+","+$s.text+")");
					  $facts.add("setInvokedOperation("+$actId+","+$o.text+")");
					}
	|	ASSIGNMENT		{ $facts.add("setActivityKind("+$actId+",assign)");
					  $facts.add("setFunction("+$actId+",id)");
					}
	| 	ASSIGNMENT fct=ID	{ $facts.add("setActivityKind("+$actId+",assign)");
					  $facts.add("setFunction("+$actId+","+$fct.text+")");
					}
	;
/**
vlist [String actId, String cxt]
 	returns [ArrayList<String> identifiers, ArrayList<String> facts]
	@init{ $identifiers = new ArrayList<String>(); $facts = new ArrayList<String>(); }
	: ((v=ID)		{ $identifiers.add($cxt+"_"+$v.text); }
	   |^(BIND v=ID m=ID) 	{ $identifiers.add($cxt+"_"+$v.text);
	   			  $facts.add("setMessageBinding("+$cxt+"_"+$actId+","+$m.text+","+$cxt+"_"+$v.text+")"); 
	   			})*;
**/
vlist 	[String actId, String cxt]
 	returns [ArrayList<String> identifiers, ArrayList<String> facts]
	@init{ $identifiers = new ArrayList<String>(); $facts = new ArrayList<String>(); }
	: ((v=varaccess[$cxt])			{ $identifiers.add($v.id); }
	   |^(BIND b=varaccess[$cxt] m=ID) 	{ $identifiers.add($b.id);
	   			  	  $facts.add("setMessageBinding("+$cxt+"_"+$actId+","+$m.text+","+$b.id+")"); 
	   				})*;
varaccess	[String cxt]
 		returns [String id]
 	:	^(SCALAR n=ID)			{ $id = $cxt +"_" + $n.text;         }
	|	^(SET n=ID)			{ $id = $cxt + "_" + $n.text+"_star"; }
 	;
	   			
rels	[String cxt]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	: ^(RELATIONS (rel[$cxt] 	{ safeAdd($facts,$rel.facts); })+) ;

ord	[String cxt]
	returns [String id]
	:	r=ID	{ $id = $cxt + "_" + $r.text; }
	| 	PREDS	{ $id = $cxt + "_preds"; }
	| 	SUCCS	{ $id = $cxt + "_succs"; }
	;

rel	[String cxt]
 	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	: ^(WAIT_FOR r=ord[$cxt] l=ord[$cxt])		{ $facts.add("defWaitFor("+$r.id+","+$l.id+")"); }
	| ^(COND_TRUE r=ord[$cxt] l=ord[$cxt] c=ID)	{ $facts.add("defGuard("+$r.id+","+$l.id+","+$cxt+"_"+$c.text+",true)");  }
	| ^(COND_FALSE r=ord[$cxt] l=ord[$cxt] c=ID)	{ $facts.add("defGuard("+$r.id+","+$l.id+","+$cxt+"_"+$c.text+",false)"); }
	;
	
merge 	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	: ^(UNIT s=ID o=ID (directive[$s.text,$o.text]	{ safeAdd($facts,$directive.facts); })+)
	;
/**	
idList	returns [ArrayList<String> identifiers]
	@init{ $identifiers = new ArrayList<String>(); }
	: (v=ID						{ $identifiers.add($v.text); })+
	;
**/	
directive [String s, String o]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	: ^(MERGE_FRAG e=ID a=ID)			{ $facts.add("defMergeOrder("+$s+","+$o+","+$a.text+","+$e.text+")"); }
	;
	
