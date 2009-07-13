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
 * @author      Main SŽbastien Mosser          [mosser@polytech.unice.fr]
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
}

  
definitions returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	:	^(DEFINITIONS (definition 		{ $facts.addAll($definition.facts); })+)
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
						  safeAdd($facts, $core.facts);
						}
	| ^(DEF ^(FRAGMENT n=ID) core[$n.text])	{ $facts.add("createProcess("+$n.text+")");
						  $facts.add("setAsFragment("+$n.text+")");
						  safeAdd($facts, $core.facts);
						}
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
	: ^(VARIABLES (decl_var[$cxt]	{ safeAdd($facts,$decl_var.facts); }  )+ )	
	;

decl_var  [String cxt]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	:	^(VAR n=ID t=ID) 		{ String name = cxt+"_"+$n.text;
						  $facts.add("createVariable("+name+")");
					 	  $facts.add("setVariableType("+name+","+$t.text+")");
					 	  trace($facts, $n.text, name, "variable", $cxt);
					 	}
	|	^(VAR n=ID t=ID v=STR)		{ String name = cxt+"_"+$n.text;
						  $facts.add("createVariable("+$n.text+")");
					 	  $facts.add("setVariableType("+$n.text+","+$t.text+")");
						  $facts.add("setInitValue("+$n.text+","+$v.text+")");
						  trace($facts, $n.text, name, "variable", $cxt);
						}
	|	^(CONST n=ID t=ID v=STR) 	{ String name = cxt+"_"+$n.text;
						  $facts.add("createVariable("+$n.text+")");
					 	  $facts.add("setVariableType("+$n.text+","+$t.text+")");
						  $facts.add("setInitValue("+$n.text+","+$v.text+")");
						  $facts.add("setConstancy("+$n.text+")");
						  trace($facts, $n.text, name, "constant", $cxt);
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
	: ^(ACT id=ID content[$id.text, $cxt])	{ String name = $cxt + "_" + $id.text;
						  $facts.add("createActivity("+name+")");
						  safeAdd($facts,$content.facts);
					 	  $facts.add("setContainment("+name+","+$cxt+")");
					 	  trace($facts,$id.text,name,"activity", $cxt);
						} 
	;

content	[String id, String cxt]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	: ^(KIND kind[$cxt+"_"+$id]) ^(INS vlist[$id,$cxt])		
							{ safeAdd($facts,$kind.facts) ;
							  for(String v: $vlist.identifiers)
							    $facts.add("addAsInput("+v+","+$cxt+"_"+$id+")");
							  safeAdd($facts,$vlist.facts);
							}
	| ^(KIND kind[$cxt+"_"+$id]) ^(INS i=vlist[$id,$cxt]) ^(OUTS o=vlist[$id,$cxt])	
							{ safeAdd($facts,$kind.facts) ;
							  for(String v: $i.identifiers)
							    $facts.add("addAsInput("+v+","+$cxt+"_"+$id+")");
							  safeAdd($facts,$i.facts);
							  for(String v: $o.identifiers)
							    $facts.add("addAsOutput("+v+","+$cxt+"_"+$id+")");
							  safeAdd($facts,$o.facts);
							}
	| ^(KIND kind[$cxt+"_"+$id]) INS ^(OUTS vlist[$id,$cxt])	
							{ safeAdd($facts,$kind.facts) ;
							  for(String v: $vlist.identifiers)
							    $facts.add("addAsOutput("+v+","+$cxt+"_"+$id+")");
							  safeAdd($facts,$vlist.facts);
							}
	;

kind	[String actId]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	:	RECEIVE			{ $facts.add("setActivityKind("+$actId+",receive)"); }
	|	REPLY			{ $facts.add("setActivityKind("+$actId+",reply)");}
	|	THROW			{ $facts.add("setActivityKind("+$actId+",throw)");}
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

vlist [String actId, String cxt]
 	returns [ArrayList<String> identifiers, ArrayList<String> facts]
	@init{ $identifiers = new ArrayList<String>(); $facts = new ArrayList<String>(); }
	: ((v=ID)		{ $identifiers.add($cxt+"_"+$v.text); }
	   |^(BIND v=ID m=ID) 	{ $identifiers.add($cxt+"_"+$v.text);
	   			  $facts.add("setMessageBinding("+$cxt+"_"+$actId+","+$m.text+","+$v.text+")"); 
	   			})*;
	
rels	[String cxt]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	: ^(RELATIONS (rel[$cxt] 	{ safeAdd($facts,$rel.facts); })+) ;
	
rel	[String cxt]
 	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	: ^(WAIT_FOR r=ID l=ID)		{ $facts.add("defWaitFor("+$cxt+"_"+$r.text+","+$cxt+"_"+$l.text+")"); }
	| ^(COND_TRUE r=ID l=ID c=ID)	{ $facts.add("defGuard("+$cxt+"_"+$r.text+","+$cxt+"_"+$l.text+","+$cxt+"_"+$c.text+",true)");  }
	| ^(COND_FALSE r=ID l=ID c=ID)	{ $facts.add("defGuard("+$cxt+"_"+$r.text+","+$cxt+"_"+$l.text+","+$cxt+"_"+$c.text+",false)"); }
	;
	
	
	
	
	
