
Step 0: Case Study Overview

  - Car Crash Crisis Management System?
    - TAOSD Journal, Special Issue on Aspect Oriented Modeling
    - Common case study to compare AOM approaches
    - Waiting for reviews ... ^_^. 

Step 1: Technological Context                [process.adore]

  - What is an orchestration?
  - How do we model it in ADORE?
  - CaptureWitnessReport Example:
    - Textual DSL
    - Logical model
    - Graphical Representation

Step 2: Evolution Need                       [fragments.adore]

  - Scenario Extensions
  - Realizing extensions as fragments
    - callDisconnected: Hook, P & S
    - fakeCrisisDetected: guards (if/then/else)
    - requestVideo: fragments parameters  

Step 3: Weaving a Fragment                   [simple_weaving.adore]

  - Expressing composition in ADORE:
    - composition unit declaration 
    - meaning in terms of algorithms calls
      - Computing actions, then executing the action set
      - Reasonning on Set principles => no ordering!
    - weaving target:
      - simple activities
      - block of activities
  ==> output: figs/3.simple_weaving.png

Step 4: Merging fragments                    [merge_then_weave.adore]

  - Shared join points (so-called)
  - Automatic merge:
    - Reasonning on set => order-independent
    - Visualization of the merged artifact
    ==> output: figs/4__merged_on_a4.png 
  ==> output: figs/4.merge_then_weave.png

Step 5: Fragment on Fragment                 [fragment_on_fragment.adore]

  - enhancing fragments on demand
  - Viualization of the enhanced artifact
    ==> output: figs/5__requestVideo_enhanced.png
  ==> output: figs/5.fragment_on_fragment.png

Step 6: The all together                     [the_all_together.adore]

  - composition unit including ALL busines extensions
    - hiding the temporary artifact for simplification reasons
  ==> output: figs/6.the_all_together.png
  - Inconsistency checking:
    => e.g., model incompletness


Step 7: Including Non-functional concerns    [with_nf.adore]

  - Generating weave directives using the logical layer
    ==> pointcus.pl, 'displayCuts.'
  - Huge composition 'at the end'
  ==> output: figs/7.including_nf_concerns.png
