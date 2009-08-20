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
  
  private ArrayList<String> compiled = new ArrayList<String>();
  public void setCompiled(ArrayList<String> l) {this.compiled = l; }
  public ArrayList<String> getCompiled() { return this.compiled; }
  private boolean stillCompiled(String file) {
    return this.compiled.indexOf(file) != -1;
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
  
  private static int ANONYMOUS_CPT = 0;
  private String generateAnonymousId() {
    return "lambda_" + ANONYMOUS_CPT++;
  }
  
  private String generateAnonymousApply(){
    return "apply_" + ANONYMOUS_CPT++;
  }

  private String generateFieldAccess(){
    return "field_" + ANONYMOUS_CPT++;
  }  

  private String generateBlock(){
    return "block_" + ANONYMOUS_CPT++;
  }  
  
  private String generateContext(){
    return "context_" + ANONYMOUS_CPT++;
  }  
}

  
definitions returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	:	^(DEFINITIONS 	( definition	{ $facts.addAll($definition.facts); })+
		)		
	;

definition returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	: ^(DEF ^(REQUIRE f=STR)) 		{ //System.err.println($f +" " + compiled);
						  
						  if (stillCompiled($f.text))
						    return $facts;
						  Compiler c = new Compiler(); c.setCompiled(compiled);
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
	| ^(DEF ^(FRAGMENT n=ID param[$n.text]) core[$n.text])	
						{ $facts.add("createProcess("+$n.text+")");
						  $facts.add("setAsFragment("+$n.text+")");
						  fragmentize($facts, $n.text);
						  safeAdd($facts, $core.facts);
						  safeAdd($facts, $param.facts); 
						}
						
	| ^(DEF merge) 				{ safeAdd($facts,$merge.facts); }	
	| ^(DEF ^(RAW p=DISENGAGE))		{ $facts.add($p.text);      }
	;

param [String cxt]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	: 	^(PARAMS (^(SCALAR i=ID) { $facts.add("setAsFragmentParameter("+$cxt+","+$i.text+")"); })*)				
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
	| ^(INS vlist[$id,$cxt]) OUTS 		{ safeAdd($facts,$vlist.facts);
						  for(String v: $vlist.identifiers)
						    $facts.add("addAsInput("+v+","+$cxt+"_"+$id+")");
						}
	| INS ^(OUTS vlist[$id,$cxt])		{ safeAdd($facts,$vlist.facts);
						  for(String v: $vlist.identifiers)
						    $facts.add("addAsOutput("+v+","+$cxt+"_"+$id+")");
						}
	| ^(INS i=vlist[$id,$cxt]) ^(OUTS o=vlist[$id,$cxt])	
						{ safeAdd($facts,$i.facts);
						  for(String v: $i.identifiers)
						    $facts.add("addAsInput("+v+","+$cxt+"_"+$id+")");
						  safeAdd($facts,$o.facts);
						  for(String v: $o.identifiers)
					  	    $facts.add("addAsOutput("+v+","+$cxt+"_"+$id+")");
						}
	; 

kind	[String actId]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	:	RECEIVE			{ $facts.add("setActivityKind("+$actId+",receive)"); }
	|	REPLY			{ $facts.add("setActivityKind("+$actId+",reply)");}
	|	THROW			{ $facts.add("setActivityKind("+$actId+",throw)");}
	|	HOOK			{ $facts.add("setActivityKind("+$actId+",hook)");}
	|	NOP			{ $facts.add("setActivityKind("+$actId+",nop)");}
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

vlist 	[String actId, String cxt]
 	returns [ArrayList<String> identifiers, ArrayList<String> facts]
	@init{ $identifiers = new ArrayList<String>(); $facts = new ArrayList<String>(); }
	: ((v=varaccess[$cxt,$actId])		{ safeAdd($facts,$v.facts);
						  $identifiers.add($v.id); }
	   |^(BIND b=varaccess[$cxt,$actId] m=ID) 	
	   					{ safeAdd($facts,$b.facts);
	   					  $identifiers.add($b.id);
	   			  	 	  $facts.add("setMessageBinding("+$cxt+"_"+$actId+","+$m.text+","+$b.id+")"); 
	   					})*;
varaccess	[String cxt, String actId]
 		returns [ArrayList<String> facts, String id]
 	@init{ $facts = new ArrayList<String>(); }
 	:	^(SCALAR n=ID)			{ $id = $cxt +"_" + $n.text;         }
 	|	^(SCALAR n=ID fields)	
 						{ $id = generateFieldAccess();
 						  $facts.add("createFieldAccess("+$id+","+$cxt+"_"+$n.text+","+$fields.access+")");
 						}
	|	^(SET n=ID)			{ $id = $cxt + "_" + $n.text+"_star"; }
	|	^(ANONYMOUS v=STR t=ID)		{ String id = generateAnonymousId();
						  $id = $cxt+"_" + id;
						  $facts.add("createVariable("+ $id + ")"); 
						  $facts.add("setVariableType("+ $id+","+$t.text+")");
						  $facts.add("setInitValue("+$id+","+$v.text+")");
						  $facts.add("setConstancy("+$id+")");
						  trace($facts, id, $id, "generated", $cxt);
						}
 	;
	   			
fields 	returns [String access]
	@init{ $access = ""; }
	: ^(FIELDS (i=ID { $access += $i +","; })* (i=ID STAR { $access += $i+"_star,";})?)	
						{ $access = "[" + $access.substring(0,access.length() -1)+"]"; }
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
	@init{ $facts = new ArrayList<String>();}
	: ^(WAIT_FOR r=ord[$cxt] l=ord[$cxt])		{ $facts.add("defWaitFor("+$r.id+","+$l.id+")"); }
	| ^(WEAK_WAIT r=ord[$cxt] l=ord[$cxt])		{ $facts.add("defWeakWait("+$r.id+","+$l.id+")"); }
	| ^(COND_TRUE r=ord[$cxt] l=ord[$cxt] c=ID)	{ $facts.add("defGuard("+$r.id+","+$l.id+","+$cxt+"_"+$c.text+",true)");  }
	| ^(COND_FALSE r=ord[$cxt] l=ord[$cxt] c=ID)	{ $facts.add("defGuard("+$r.id+","+$l.id+","+$cxt+"_"+$c.text+",false)"); }
	| ^(ON_FAIL r=ord[$cxt] l=ord[$cxt] 		{ String m = "'*'"; }
			(e=STR				{ m = $e.text;        })?  {$facts.add("defOnFail("+$r.id+","+$l.id+","+m+")"); }) 
	;
	
merge 	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); String targetId ="";}
	: ^(UNIT 					{ String id = generateContext(); 
							  $facts.add("defCompositionContext("+id+")"); 				}
		   	^(TARGET (s=ID 			{ targetId = $s.text; 							} 
		   		 	(o=ID 		{targetId += "_" + $o.text; 						})?
		   		 			{ $facts.add("setCompositionTarget("+id+","+targetId+")");		})?)
		   	^(OUTPUT (out=ID		{ $facts.add("setContextOutput("+id+","+$out.text+")");			})?)
		(directive[id]				{ safeAdd($facts,$directive.facts); 					})+) 
	;

	
directive [String cxt]
	returns [ArrayList<String> facts]
	@init{ $facts = new ArrayList<String>(); }
	: ^(MERGE_FRAG e=ID block[$cxt] 	{ String id = generateAnonymousApply();
						  $facts.add($block.fact);
					   	  $facts.add("defApply("+id+","+$cxt+","+$block.id+","+$e.text+")"); 
					   	 }
			(^(BIND v=STR x=ID)	{ $facts.add("setApplyParam("+id+","+$x.text+","+$v.text+")"); })*)
	| ^(SETIFY elemref[$cxt])		{ $facts.add("defSetify("+$cxt+","+$elemref.id+")");           }
	;

block [String cxt]
	returns [String fact, String id]
	@init{ String members = ""; }
	: ^(BLOCK (elemref[$cxt] { members += $elemref.id +","; })+) 	
							{ $id = generateBlock(); 
							  $fact = "defActivityBlock("+$cxt+","+$id+",["+members.substring(0,members.length()-1)+"])";
							}
	;

elemref [String cxt]
	returns [String id]
	:	^(ELEM_REF a=ID) 		{ id = "inferedReference("+$a.text+")"; }
	|	^(ELEM_REF f=ID a=ID)		{ id = "absoluteReference("+$f.text+","+$a.text+")";}
	| 	^(ELEM_REF s=ID o=ID a=ID) 	{ id="absoluteReference("+$s.text+","+$o.text+","+$a.text+")"; }
	;
