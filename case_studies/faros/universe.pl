:- style_check(-string).

:- ['../../prolog/src/tests/toBPEL-4.pl'],['../../prolog/src/tests/weave.pl'],
	['../../prolog/src/tests/dedicatedFunctions.pl'],
	['../../prolog/src/tests/transfoAdoreCompositionToBPEL.pl'].

%%%%
%% Link with Seduite Source File
%%%%

generate(seduite) :- 
	adore2bpel_genProcess(provider_entry,Code),
	writeSeduiteBpelCode(Code).

compose(seduite) :- 
	weave(provider_entry,_Set).

bpelFile(provider_entry,
	'../../../faros/DemoSeduite/Seduite4Faros-BPEL/src/v/Provider_v.bpel').

writeSeduiteBpelCode(Code) :- 
	bpelFile(provider_entry,F),
	open(F, write, Stream), write(Stream,Code), close(Stream).

%%%%
%% Bridging ADORE to Bpel entities
%%%%

bpelBridge(process,provider_entry,provider).

bpelBridge(activity,provider_entry_rec,receive).
bpelBridge(activity,provider_entry_sourceTimetablesxtimetable4Diploma0,timetable).
bpelBridge(activity,provider_entry_sourceNewsxnewsNow1,news).
bpelBridge(activity,provider_entry_concat2,concat).
bpelBridge(activity,provider_entry_menuxmenuToday3,menu).
bpelBridge(activity,provider_entry_concat4,concat).
bpelBridge(activity,provider_entry_truncate5,truncate).
bpelBridge(activity,provider_entry_fromProviderinfoSinkxid6,sink).
bpelBridge(activity,provider_entry_last,reply).

%% v1 & v2
bpelBridge(activity,provider_entry_fromProviderinfoSinkxid4,sink).
bpelBridge(activity,provider_entry_menuxmenuToday4,menu).
bpelBridge(activity,provider_entry_truncate3,truncate).
bpelBridge(activity,provider_entry_concat5,concat).
bpelBridge(activity,apply_2timeContract_e0,init_delay).
bpelBridge(activity,apply_2timeContract_timestart,wait).
bpelBridge(activity,apply_2timeContract_test,equalZero).
bpelBridge(activity,apply_2timeContract_o,timeViolation).
bpelBridge(activity,apply_5capacityContract_t,count).
bpelBridge(activity,apply_5capacityContract_test,capacityTest).
bpelBridge(activity,apply_5capacityContract_o,capacityViolation).

%% v3 (apply order matters ... crap :S )
bpelBridge(activity,apply_5timeContract_e0,init_delay).
bpelBridge(activity,apply_5timeContract_timestart,wait).
bpelBridge(activity,apply_5timeContract_test,equalZero).
bpelBridge(activity,apply_5timeContract_o,timeViolation).
bpelBridge(activity,apply_2cache_test,cacheValidity).
bpelBridge(activity,apply_2cache_r,readCache).
bpelBridge(activity,apply_2cache_e,memorizeCache).
bpelBridge(activity,apply_8capacityContract_t,count).
bpelBridge(activity,apply_8capacityContract_test,capacityTest).
bpelBridge(activity,apply_8capacityContract_o,capacityViolation).

%%%%
%% BPEL entities
%%%%

bpelTargetNamespace(provider,'http://seduite/4/faros/providers/bpel/v').

%% Namespaces
bpelNamespace(provider,'ns0','http://seduite/4/faros/providers/schema').
bpelNamespace(provider,'ns1',
	'http://seduite/4/faros/helpers/transform/schema').
bpelNamespace(concat,'ns2','http://seduite/4/faros/helpers/concat/schema').
bpelNamespace(truncate,'ns3','http://seduite/4/faros/helpers/truncate/schema').
bpelNamespace(provider,'ns4','http://seduite/4/faros/providers/wsdl/v').
bpelNamespace(provider,'ns5','http://seduite/4/faros/helpers/transform/wsdl').
bpelNamespace(timetable,'ns6','http://seduite/extract/faros/sources/timetable').
bpelNamespace(news,'ns7','http://seduite/extract/faros/sources/news').
bpelNamespace(concat,'ns8','http://seduite/4/faros/helpers/concat/wsdl').
bpelNamespace(truncate,'ns9','http://seduite/4/faros/helpers/truncate/wsdl').
bpelNamespace(menu,'ns10','http://seduite/extract/faros/sources/menu').

%% File Import
bpelImport(provider,'http://seduite/4/faros/providers/wsdl/v',
	'Provider_v.wsdl','http://schemas.xmlsoap.org/wsdl/').
bpelImport(provider,'http://seduite/4/faros/providers/schema',
	'../Provider.xsd','http://www.w3.org/2001/XMLSchema').
bpelImport(timetable,
	'http://enterprise.netbeans.org/bpel/TimeTableProviderWrapper',
	'../Partners/TimeTableProviderWrapper.wsdl',
	'http://schemas.xmlsoap.org/wsdl/').
bpelImport(timetable,'http://seduite/extract/faros/sources/timetable',
	'../Partners/TimeTableProvider.wsdl',
	'http://schemas.xmlsoap.org/wsdl/').
bpelImport(provider,'http://seduite/4/faros/helpers/transform/wsdl',
	'../Helpers/Transform.wsdl','http://schemas.xmlsoap.org/wsdl/').
bpelImport(news,'http://enterprise.netbeans.org/bpel/NewsProviderWrapper',
	'../Partners/NewsProviderWrapper.wsdl',
	'http://schemas.xmlsoap.org/wsdl/').
bpelImport(news,'http://seduite/extract/faros/sources/news',
	'../Partners/NewsProvider.wsdl','http://schemas.xmlsoap.org/wsdl/').
bpelImport(concat,'http://seduite/4/faros/helpers/concat/wsdl',
	'../Helpers/Concat.wsdl','http://schemas.xmlsoap.org/wsdl/').
bpelImport(truncate,'http://seduite/4/faros/helpers/truncate/wsdl',
	'../Helpers/Truncate.wsdl','http://schemas.xmlsoap.org/wsdl/').
bpelImport(wait,'http://enterprise.netbeans.org/bpel/TimerWrapper',
	'../Partners/TimerWrapper.wsdl','http://schemas.xmlsoap.org/wsdl/').
bpelImport(wait,'http://seduite/extract/faros/helpers/timer',
	'../Partners/Timer.wsdl','http://schemas.xmlsoap.org/wsdl/').
bpelImport(menu,'http://enterprise.netbeans.org/bpel/MenuProviderWrapper',
	'../Partners/MenuProviderWrapper.wsdl',
	'http://schemas.xmlsoap.org/wsdl/').
bpelImport(menu,'http://seduite/extract/faros/sources/menu',
	'../Partners/MenuProvider.wsdl','http://schemas.xmlsoap.org/wsdl/').
bpelImport(readCache,'http://enterprise.netbeans.org/bpel/DataCacheWrapper',
	'../Partners/DataCacheWrapper.wsdl','http://schemas.xmlsoap.org/wsdl/').
bpelImport(readCache,'http://seduite/extract/faros/helpers/datacache',
	'../Partners/DataCache.wsdl','http://schemas.xmlsoap.org/wsdl/').

  
%% PartnerLinks
bpelPnlk(timetable,'sourceTimetables',
	'http://enterprise.netbeans.org/bpel/TimeTableProviderWrapper',
	'tns:TimeTableProviderLinkType','TimeTableProviderRole','').
bpelPnlk(news,'news',
	'http://enterprise.netbeans.org/bpel/NewsProviderWrapper',
	'tns:NewsProviderLinkType','NewsProviderRole','').
bpelPnlk(menu,'menu',
	'http://enterprise.netbeans.org/bpel/MenuProviderWrapper',
	'tns:MenuProviderLinkType','MenuProviderRole','').
bpelPnlk(provider,'transform','http://seduite/4/faros/helpers/transform/wsdl',
	'tns:Transform','TransformPortTypeRole','').
bpelPnlk(concat,'concat',
	'http://seduite/4/faros/helpers/concat/wsdl',
	'tns:Concat','ConcatPortTypeRole','').
bpelPnlk(truncate,'truncate','http://seduite/4/faros/helpers/truncate/wsdl',
	'tns:Truncate','TruncatePortTypeRole','').
bpelPnlk(wait,'timer', 'http://enterprise.netbeans.org/bpel/TimerWrapper',
	'tns:TimerLinkType','TimerRole','').
bpelPnlk(readCache,'cache',
	'http://enterprise.netbeans.org/bpel/DataCacheWrapper',
	'tns:DataCacheLinkType','DataCacheRole','').
bpelPnlk(provider,'ext', 'http://seduite/4/faros/providers/wsdl/v',
	'tns:Provider_v','','Provider_vPortTypeRole').

%% Variables

bpelVariable(provider_entry_idVar,'ns0:informationList').
bpelVariable(provider_entry_diplomaVar,'xsd:string').
bpelVariable(provider_entry_timetableInfoListVar,'ns0:informationList').
bpelVariable(provider_entry_newsListVar,'ns0:informationList').
bpelVariable(provider_entry_infos2troncateVar,'ns0:informationList').
bpelVariable(provider_entry_infoNumberVar,'xsd:int').
bpelVariable(provider_entry_infosVar,'ns0:informationList').
bpelVariable(provider_entry_infosresultVar,'ns0:informationList').
bpelVariable(provider_entry_menuInfoListVar,'ns0:informationList').
bpelVariable(provider_entry_newsAndTimetableInfoListVar,'ns0:informationList').

%% v1 & v2
bpelVariable(apply_2timeContract_delay,'xsd:int').
bpelVariable(apply_2timeContract_duration,'xsd:int').
bpelVariable(apply_2timeContract_timeout,'xsd:boolean').
bpelVariable(apply_2timeContract_timerxerror,'xsd:string').
bpelVariable(apply_2timeContract_zero,'xsd:int').
bpelVariable(apply_5capacityContract_capacityError,'xsd:string').
bpelVariable(apply_5capacityContract_listSize,'xsd:int').
bpelVariable(apply_5capacityContract_ok,'xsd:boolean').
bpelVariable(apply_5capacityContract_size,'xsd:int').

%% v3
bpelVariable(apply_2cache_valid,'xsd:boolean').
bpelVariable(apply_5timeContract_delay,'xsd:int').
bpelVariable(apply_5timeContract_duration,'xsd:int').
bpelVariable(apply_5timeContract_timeout,'xsd:boolean').
bpelVariable(apply_5timeContract_timerxerror,'xsd:string').
bpelVariable(apply_5timeContract_zero,'xsd:int').
bpelVariable(apply_8capacityContract_capacityError,'xsd:string').
bpelVariable(apply_8capacityContract_listSize,'xsd:int').
bpelVariable(apply_8capacityContract_ok,'xsd:boolean').
bpelVariable(apply_8capacityContract_size,'xsd:int').

%%%%
%% Activities Template (aka Really Ugly Hack)
%%%%

bpelTemplate(receive,Id,[],[Diploma,InfoNumberVar],Code) :- 
	swritef(Code,'<scope name="%w"><variables><variable name="EntryIn" xmlns:tns="http://seduite/4/faros/providers/wsdl/v" messageType="tns:EntryRequest"/></variables><sequence name="rec_core"><receive name="%w_tech" createInstance="yes" partnerLink="ext" operation="Entry" xmlns:tns="http://seduite/4/faros/providers/wsdl/v" portType="tns:Provider_vPortType" variable="EntryIn"/><assign name="%w_post"><copy><from>$EntryIn.in/ns0:diploma</from><to variable="%w"/></copy><copy><from>$EntryIn.in/ns0:treshold</from><to variable="%w"/></copy></assign></sequence></scope>',[Id,Id,Id,Diploma,InfoNumberVar]).

bpelTemplate(timetable,Id,[Diploma],[TimetableInfoListVar],Code) :- 
	swritef(Code,'<scope name="%w"><variables><variable name="GetTTForDiplomaOut" xmlns:tns="http://seduite/extract/faros/sources/timetable" messageType="tns:getTTForDiplomaResponse"/><variable name="GetTTForDiplomaIn" xmlns:tns="http://seduite/extract/faros/sources/timetable" messageType="tns:getTTForDiploma"/></variables><sequence name="%w_core"><assign name="%w_pre"><copy><from variable="%w"/><to>$GetTTForDiplomaIn.parameters/diploma</to></copy></assign><invoke name="sourceTimetablesxtimetable4Diploma0_tech" partnerLink="sourceTimetables" operation="getTTForDiploma" xmlns:tns="http://seduite/extract/faros/sources/timetable" portType="tns:TimeTableProvider" inputVariable="GetTTForDiplomaIn" outputVariable="GetTTForDiplomaOut"/><scope name="%w_transform"><variables><variable name="RunOut" xmlns:tns="http://seduite/4/faros/helpers/transform/wsdl" messageType="tns:RunResponse"/><variable name="RunIn" xmlns:tns="http://seduite/4/faros/helpers/transform/wsdl" messageType="tns:RunRequest"/></variables><sequence name="%w_transform_core"><assign name="sourceTimetablesxtimetable4Diploma0_transform_pre"><copy><from>$GetTTForDiplomaOut.parameters/return</from><to>$RunIn.in/ns1:unit/ns1:data</to></copy><copy><from>\'event\'</from><to>$RunIn.in/ns1:unit/ns1:kind</to></copy></assign><invoke name="%w_transform_tech" partnerLink="transform" operation="Run" xmlns:tns="http://seduite/4/faros/helpers/transform/wsdl" portType="tns:TransformPortType" inputVariable="RunIn" outputVariable="RunOut"/><assign name="sourceTimetablesxtimetable4Diploma0_post"><copy><from>$RunOut.out/ns1:list</from><to variable="%w"/></copy></assign></sequence></scope></sequence></scope>',[Id,Id,Id,Diploma,Id,Id,Id,TimetableInfoListVar]).


bpelTemplate(news,Id,[],[L],Code) :- 
	swritef(Code,'<scope name="%w"><variables><variable name="GetNewsNowOut" xmlns:tns="http://seduite/extract/faros/sources/news" messageType="tns:getNewsNowResponse"/><variable name="GetNewsNowIn" xmlns:tns="http://seduite/extract/faros/sources/news" messageType="tns:getNewsNow"/></variables><sequence name="%w_core"><assign name="%w_pre"><copy><from>\'\'</from><to variable="GetNewsNowIn" part="parameters"/></copy></assign><invoke name="%w_tech" partnerLink="news" operation="getNewsNow" xmlns:tns="http://seduite/extract/faros/sources/news" portType="tns:NewsProvider" inputVariable="GetNewsNowIn" outputVariable="GetNewsNowOut"/><scope name="%w_transform"><variables><variable name="RunOut" xmlns:tns="http://seduite/4/faros/helpers/transform/wsdl" messageType="tns:RunResponse"/><variable name="RunIn" xmlns:tns="http://seduite/4/faros/helpers/transform/wsdl" messageType="tns:RunRequest"/></variables><sequence name="%w_transform_core" xmlns:tns="http://seduite/4/faros/helpers/transform/wsdl"><assign name="%w_pre"><copy><from>$GetNewsNowOut.parameters/return</from><to>$RunIn.in/ns1:unit/ns1:data</to></copy><copy><from>\'news\'</from><to>$RunIn.in/ns1:unit/ns1:kind</to></copy></assign><invoke name="%w_transform_tech" partnerLink="transform" operation="Run" xmlns:tns="http://seduite/4/faros/helpers/transform/wsdl" portType="tns:TransformPortType" inputVariable="RunIn" outputVariable="RunOut"/><assign name="%w_post"><copy><from>$RunOut.out/ns1:list</from><to variable="%w"/></copy></assign></sequence></scope></sequence></scope>',[Id,Id,Id,Id,Id,Id,Id,Id,Id,L]).

bpelTemplate(menu,Id,[],[L],Code) :- 
	swritef(Code,'<scope name="%w"><variables><variable name="GetTodayMenuOut" messageType="ns10:getTodayMenuResponse"/><variable name="GetTodayMenuIn" messageType="ns10:getTodayMenu"/></variables><sequence name="%w_core"><assign name="%w_pre"><copy><from>\'\'</from><to variable="GetTodayMenuIn" part="parameters"/></copy></assign><invoke name="%w_tech" partnerLink="menu" operation="getTodayMenu" portType="ns10:MenuProvider" inputVariable="GetTodayMenuIn" outputVariable="GetTodayMenuOut"/><scope name="%w_transform"><variables><variable name="RunOut" messageType="ns5:RunResponse"/><variable name="RunIn" messageType="ns5:RunRequest"/></variables><sequence name="%w_transform_core"><assign name="%w_transform_pre"><copy><from>\'menu\'</from><to>$RunIn.in/ns1:unit/ns1:kind</to></copy><copy><from>$GetTodayMenuOut.parameters/return</from><to>$RunIn.in/ns1:unit/ns1:data</to></copy></assign><invoke name="%w_transform_tech" partnerLink="transform" operation="Run" portType="ns5:TransformPortType" inputVariable="RunIn" outputVariable="RunOut"/><assign name="%w_post"><copy><from>$RunOut.out/ns1:list</from><to variable="%w"/></copy></assign></sequence></scope></sequence></scope>',[Id,Id,Id,Id,Id,Id,Id,Id,Id,L]).



bpelTemplate(concat,Id,[L1,L2],[L],Code) :- 
	swritef(Code,'<scope name="%w"><variables><variable name="RunOut" xmlns:tns="http://seduite/4/faros/helpers/concat/wsdl" messageType="tns:RunResponse"/><variable name="RunIn" xmlns:tns="http://seduite/4/faros/helpers/concat/wsdl" messageType="tns:RunRequest"/></variables><sequence name="%w_core"><assign name="%w_pre"><copy><from variable="%w"/><to>$RunIn.in/ns2:list_1</to></copy><copy><from variable="%w"/><to>$RunIn.in/ns2:list_2</to></copy></assign><invoke name="%w_tech" partnerLink="concat" operation="Run" xmlns:tns="http://seduite/4/faros/helpers/concat/wsdl" portType="tns:ConcatPortType" inputVariable="RunIn" outputVariable="RunOut"/><assign name="%w_post"><copy><from>$RunOut.out/ns2:list</from><to variable="%w"/></copy></assign></sequence></scope>',[Id,Id,Id,L1,L2,Id,Id,L]).

bpelTemplate(truncate,Id,[L,N],[LPrime],Code) :- 
	swritef(Code,'<scope name="%w"><variables><variable name="RunOut" xmlns:tns="http://seduite/4/faros/helpers/truncate/wsdl" messageType="tns:RunResponse"/><variable name="RunIn" xmlns:tns="http://seduite/4/faros/helpers/truncate/wsdl" messageType="tns:RunRequest"/></variables><sequence name="%w_core"><assign name="%w_pre"><copy><from variable="%w"/><to>$RunIn.in/ns3:list</to></copy><copy><from variable="%w"/><to>$RunIn.in/ns3:treshold</to></copy></assign><invoke name="%w_tech" partnerLink="truncate" operation="Run" xmlns:tns="http://seduite/4/faros/helpers/truncate/wsdl" portType="tns:TruncatePortType" inputVariable="RunIn" outputVariable="RunOut"/><assign name="%w_post"><copy><from>$RunOut.out/ns3:list</from><to variable="%w"/></copy></assign></sequence></scope>',[Id,Id,Id,L,N,Id,Id,LPrime]).

bpelTemplate(sink,Id,[In],[Out],Code) :- 
	swritef(Code,'<scope name="%w"><sequence name="%w_core"><assign name="fromProviderinfoSinkxid4_tech"><copy><from variable="%w"/><to variable="%w"/></copy></assign></sequence></scope>',[Id,Id,In,Out]).

bpelTemplate(reply,Id,[L],[],Code) :-
	swritef(Code,'<scope name="%w"><variables><variable name="EntryOut" xmlns:tns="http://seduite/4/faros/providers/wsdl/v" messageType="tns:EntryResponse"/></variables><sequence name="rep_core"><assign name="%w_pre"><copy><from variable="%w"/><to>$EntryOut.out/ns0:result</to></copy></assign><reply name="%w_tech" partnerLink="ext" operation="Entry" xmlns:tns="http://seduite/4/faros/providers/wsdl/v" portType="tns:Provider_vPortType" variable="EntryOut"/><exit name="%w_post" /></sequence></scope>',[Id,Id,L,Id,Id]).

bpelTemplate(init_delay,Id,[_],[Delay],Code) :- 
	swritef(Code,'<scope name="%w"><sequence name="%w_core"><assign name="%w_tech"><copy><from>0</from><to variable="%w"/></copy></assign></sequence></scope>',[Id,Id,Id,Delay]).

bpelTemplate(wait,Id,[_],[Delay],Code) :- 
	swritef(Code,'<scope name="%w"><variables><variable name="WaitOut" xmlns:tns="http://seduite/extract/faros/helpers/timer" messageType="tns:waitResponse"/><variable name="WaitIn" xmlns:tns="http://seduite/extract/faros/helpers/timer" messageType="tns:wait"/></variables><sequence name="%w_core"><assign name="%w_pre"><copy><from>5</from><to>$WaitIn.parameters/length</to></copy></assign><invoke name="%w_tech" partnerLink="timer" operation="wait" xmlns:tns="http://seduite/extract/faros/helpers/timer" portType="tns:Timer" inputVariable="WaitIn" outputVariable="WaitOut"/><assign name="%w_post"><copy><from>$WaitOut.parameters/return</from><to variable="%w"/></copy></assign></sequence></scope>',[Id,Id,Id,Id,Id,Delay]).

bpelTemplate(equalZero,Id,[Delay,_],[Timeout],Code) :- 
	swritef(Code,'<scope name="%w"><sequence name="%w_core"><assign name="%w_tech"><copy><from>0 != $%w</from><to variable="%w"/></copy></assign></sequence></scope>',[Id,Id,Id,Delay,Timeout]).

bpelTemplate(timeViolation,Id,[_],[],Code) :- 
	swritef(Code,'<scope name="%w"><variables><variable name="Fault1FaultVar" messageType="ns4:EntryFault"/></variables><sequence name="%w_core"><assign name="%w_pre"><copy><from>\'RESPONSE TIME VIOLATION\'</from><to variable="Fault1FaultVar" part="fault"/></copy></assign><reply name="%w_tech" partnerLink="ext" operation="Entry" portType="ns4:Provider_vPortType" faultName="ns4:fault1" variable="Fault1FaultVar"/><exit name="%w_post"/></sequence></scope>',[Id,Id,Id,Id,Id]).

bpelTemplate(cacheValidity,Id,[Diploma],[Valid],Code) :-
	swritef(Code,'<scope name="%w"><variables><variable name="IsValidOut" xmlns:tns="http://seduite/extract/faros/helpers/datacache" messageType="tns:isValidResponse"/><variable name="IsValidIn" xmlns:tns="http://seduite/extract/faros/helpers/datacache" messageType="tns:isValid"/></variables><sequence name="%w_core"><assign name="%w_pre"><copy><from variable="%w"/><to>$IsValidIn.parameters/key</to></copy></assign><invoke name="%w_tech" partnerLink="cache" operation="isValid" xmlns:tns="http://seduite/extract/faros/helpers/datacache" portType="tns:DataCache" inputVariable="IsValidIn" outputVariable="IsValidOut"/><assign name="%w_post"><copy><from>$IsValidOut.parameters/return</from><to variable="%w"/></copy></assign></sequence></scope>',[Id,Id,Id,Diploma,Id,Id,Valid]).

bpelTemplate(readCache,Id,[Diploma],[List],Code) :- 
	swritef(Code,'<scope name="%w"><variables> <variable name="GetOut" xmlns:tns="http://seduite/extract/faros/helpers/datacache" messageType="tns:getResponse"/><variable name="GetIn" xmlns:tns="http://seduite/extract/faros/helpers/datacache" messageType="tns:get"/></variables><sequence name="%w_core"><assign name="cache_r_pre"><copy><from variable="%w"/><to>$GetIn.parameters/key</to></copy></assign><invoke name="%w_tech" partnerLink="cache" operation="get" xmlns:tns="http://seduite/extract/faros/helpers/datacache" portType="tns:DataCache" inputVariable="GetIn" outputVariable="GetOut"/><assign name="%w_post"><copy><from>$GetOut.parameters/return</from><to variable="%w"/></copy></assign></sequence></scope>',[Id,Id,Diploma,Id,Id,List]).

bpelTemplate(memorizeCache,Id,[List,Key],[],Code) :- 
	swritef(Code,'<scope name="%w"><variables><variable name="MemorizeIn" xmlns:tns="http://seduite/extract/faros/helpers/datacache" messageType="tns:memorize"/></variables><sequence name="%w_core"><assign name="%w_pre"><copy><from variable="%w"/><to>$MemorizeIn.parameters/data</to></copy><copy><from variable="%w"/><to>$MemorizeIn.parameters/key</to></copy></assign><invoke name="%w_tech" partnerLink="cache" operation="memorize" xmlns:tns="http://seduite/extract/faros/helpers/datacache" portType="tns:DataCache" inputVariable="MemorizeIn"/></sequence></scope>',[Id,Id,Id,List,Key,Id]).

bpelTemplate(count,Id,[L],[Count],Code) :- 
	swritef(Code,'<scope name="%w"><sequence name="%w_core"><assign name="%w_tech"><copy><from>count($%w/ns0:item)</from><to variable="%w"/></copy></assign></sequence></scope>',[Id,Id,Id,L,Count]).

bpelTemplate(capacityTest,Id,[Count,Max],[R],Code) :- 
	swritef(Code,'<scope name="%w"><sequence name="%w_core"><assign name="%w_tech"><copy><from>$%w &lt;= $%w</from><to variable="%w"/></copy></assign></sequence></scope>',[Id,Id,Id,Count,Max,R]).

bpelTemplate(capacityViolation,Id,[_],[],Code) :- 
	swritef(Code,'<scope name="%w"><variables><variable name="Fault1FaultVar" messageType="ns4:EntryFault"/></variables><sequence name="%w_core"><assign name="%w_pre"><copy><from>\'Capacity Violation\'</from><to variable="Fault1FaultVar" part="fault"/></copy></assign><reply name="%w_tech" partnerLink="ext" operation="Entry" portType="ns4:Provider_vPortType" faultName="ns4:fault1" variable="Fault1FaultVar"/><exit name="%w_post"/></sequence></scope>',[Id,Id,Id,Id,Id]).