<?xml version="1.0" encoding="UTF-8"?>
<uml:Model xmi:version="2.0" xmlns:xmi="http://www.omg.org/XMI" xmlns:ecore="http://www.eclipse.org/emf/2002/Ecore" xmlns:uml="http://www.eclipse.org/uml2/3.0.0/UML" xmi:id="idModel" name="main">
  <packagedElement xmi:type="uml:Package" xmi:id="idPackage" name="DefaultNamePackage">
    <packagedElement xmi:type="uml:Collaboration" xmi:id="idCollaboration" name="DefaultNameCollaboration">
      <ownedBehavior xmi:type="uml:Interaction" xmi:id="idInteraction">
        <ownedComment xmi:type="uml:Comment" xmi:id="_YC3eAAjhEeSWNoVLtxf4XQ" annotatedElement="_FTE-EgjhEeSWNoVLtxf4XQ">
          <body>Analyze File 1</body>
        </ownedComment>
        <ownedComment xmi:type="uml:Comment" xmi:id="_exfSIgjhEeSWNoVLtxf4XQ" annotatedElement="_kLVEogjhEeSWNoVLtxf4XQ">
          <body>Analyze File N</body>
        </ownedComment>
        <ownedComment xmi:type="uml:Comment" xmi:id="_oDawggjhEeSWNoVLtxf4XQ" annotatedElement="_UV8gUgjhEeSWNoVLtxf4XQ">
          <body>...</body>
        </ownedComment>
        <ownedComment xmi:type="uml:Comment" xmi:id="_Xzqu8gjiEeSWNoVLtxf4XQ" annotatedElement="_FTE-EgjhEeSWNoVLtxf4XQ">
          <body>Analyze File 1</body>
        </ownedComment>
        <ownedComment xmi:type="uml:Comment" xmi:id="_YEDUUgjiEeSWNoVLtxf4XQ" annotatedElement="_UV8gUgjhEeSWNoVLtxf4XQ">
          <body>............</body>
        </ownedComment>
        <ownedComment xmi:type="uml:Comment" xmi:id="_YTGc8gjiEeSWNoVLtxf4XQ" annotatedElement="_kLVEogjhEeSWNoVLtxf4XQ">
          <body>Analyze File N</body>
        </ownedComment>
        <ownedComment xmi:type="uml:Comment" xmi:id="_azbrsgjiEeSWNoVLtxf4XQ" annotatedElement="_xlTWcgjhEeSWNoVLtxf4XQ">
          <body>Analyze File 1</body>
        </ownedComment>
        <ownedComment xmi:type="uml:Comment" xmi:id="_bOP_cAjiEeSWNoVLtxf4XQ" annotatedElement="_J9-xggjiEeSWNoVLtxf4XQ">
          <body>............</body>
        </ownedComment>
        <ownedComment xmi:type="uml:Comment" xmi:id="_bcaXQgjiEeSWNoVLtxf4XQ" annotatedElement="_Ks6UEgjiEeSWNoVLtxf4XQ">
          <body>Analyze File N</body>
        </ownedComment>
        <lifeline xmi:type="uml:Lifeline" xmi:id="_def1sAjgEeSWNoVLtxf4XQ" name="AbstractAnalysisJob" coveredBy="_4wPTJQjhEeSWNoVLtxf4XQ _PN-rhgjiEeSWNoVLtxf4XQ _BLonkgjhEeSWNoVLtxf4XQ"/>
        <lifeline xmi:type="uml:Lifeline" xmi:id="_klHTEgjgEeSWNoVLtxf4XQ" name="AbstractAnalyzer" coveredBy="_tlAw1gjhEeSWNoVLtxf4XQ _PN-rhQjiEeSWNoVLtxf4XQ _MSr_FgjiEeSWNoVLtxf4XQ _4wYdEAjhEeSWNoVLtxf4XQ _xlTWeAjhEeSWNoVLtxf4XQ __LL9VQjhEeSWNoVLtxf4XQ _BvPq8gjhEeSWNoVLtxf4XQ"/>
        <lifeline xmi:type="uml:Lifeline" xmi:id="_rYDmggjgEeSWNoVLtxf4XQ" name="AbstractEvaluation" coveredBy="_tlAw1QjhEeSWNoVLtxf4XQ _MSr_FQjiEeSWNoVLtxf4XQ __LL9VgjhEeSWNoVLtxf4XQ _xlTWeQjhEeSWNoVLtxf4XQ _FTE-EgjhEeSWNoVLtxf4XQ _UV8gUgjhEeSWNoVLtxf4XQ _kLVEogjhEeSWNoVLtxf4XQ _xlTWcgjhEeSWNoVLtxf4XQ _J9-xggjiEeSWNoVLtxf4XQ _Ks6UEgjiEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_PN-rhQjiEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification11" covered="_klHTEgjgEeSWNoVLtxf4XQ" event="_PN-rhwjiEeSWNoVLtxf4XQ" message="_PN-riAjiEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_PN-rhgjiEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification12" covered="_def1sAjgEeSWNoVLtxf4XQ" event="_PN-rhwjiEeSWNoVLtxf4XQ" message="_PN-riAjiEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_MSr_FQjiEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification9" covered="_rYDmggjgEeSWNoVLtxf4XQ" event="_MSr_FwjiEeSWNoVLtxf4XQ" message="_MSr_GAjiEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_MSr_FgjiEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification10" covered="_klHTEgjgEeSWNoVLtxf4XQ" event="_MSr_FwjiEeSWNoVLtxf4XQ" message="_MSr_GAjiEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_xlTWeAjhEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification5" covered="_klHTEgjgEeSWNoVLtxf4XQ" event="_xlTWegjhEeSWNoVLtxf4XQ" message="_xlTWewjhEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="__LL9VQjhEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification2" covered="_klHTEgjgEeSWNoVLtxf4XQ" event="__LL9VwjhEeSWNoVLtxf4XQ" message="__LL9WAjhEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="__LL9VgjhEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification8" covered="_rYDmggjgEeSWNoVLtxf4XQ" event="__LL9VwjhEeSWNoVLtxf4XQ" message="__LL9WAjhEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_4wPTJQjhEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification1" covered="_def1sAjgEeSWNoVLtxf4XQ" event="_4wYdEQjhEeSWNoVLtxf4XQ" message="_4wYdEgjhEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_4wYdEAjhEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification7" covered="_klHTEgjgEeSWNoVLtxf4XQ" event="_4wYdEQjhEeSWNoVLtxf4XQ" message="_4wYdEgjhEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_xlTWeQjhEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification6" covered="_rYDmggjgEeSWNoVLtxf4XQ" event="_xlTWegjhEeSWNoVLtxf4XQ" message="_xlTWewjhEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_tlAw1QjhEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification3" covered="_rYDmggjgEeSWNoVLtxf4XQ" event="_tlAw1wjhEeSWNoVLtxf4XQ" message="_tlAw2AjhEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_tlAw1gjhEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification4" covered="_klHTEgjgEeSWNoVLtxf4XQ" event="_tlAw1wjhEeSWNoVLtxf4XQ" message="_tlAw2AjhEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:BehaviorExecutionSpecification" xmi:id="_BLonkgjhEeSWNoVLtxf4XQ" name="BehaviorExecutionSpecification1" covered="_def1sAjgEeSWNoVLtxf4XQ" start="_4wPTJQjhEeSWNoVLtxf4XQ" finish="_PN-rhgjiEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:BehaviorExecutionSpecification" xmi:id="_BvPq8gjhEeSWNoVLtxf4XQ" name="BehaviorExecutionSpecification2" covered="_klHTEgjgEeSWNoVLtxf4XQ" start="_4wYdEAjhEeSWNoVLtxf4XQ" finish="_PN-rhQjiEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:BehaviorExecutionSpecification" xmi:id="_FTE-EgjhEeSWNoVLtxf4XQ" name="Analyze File 1" covered="_rYDmggjgEeSWNoVLtxf4XQ" start="__LL9VgjhEeSWNoVLtxf4XQ" finish="__LL9VgjhEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:BehaviorExecutionSpecification" xmi:id="_UV8gUgjhEeSWNoVLtxf4XQ" name="BehaviorExecutionSpecification4" covered="_rYDmggjgEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:BehaviorExecutionSpecification" xmi:id="_kLVEogjhEeSWNoVLtxf4XQ" name="BehaviorExecutionSpecification3" covered="_rYDmggjgEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:BehaviorExecutionSpecification" xmi:id="_xlTWcgjhEeSWNoVLtxf4XQ" name="BehaviorExecutionSpecification5" covered="_rYDmggjgEeSWNoVLtxf4XQ" start="_xlTWeQjhEeSWNoVLtxf4XQ" finish="_xlTWeQjhEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:BehaviorExecutionSpecification" xmi:id="_J9-xggjiEeSWNoVLtxf4XQ" name="BehaviorExecutionSpecification6" covered="_rYDmggjgEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:BehaviorExecutionSpecification" xmi:id="_Ks6UEgjiEeSWNoVLtxf4XQ" name="BehaviorExecutionSpecification7" covered="_rYDmggjgEeSWNoVLtxf4XQ" start="_MSr_FQjiEeSWNoVLtxf4XQ" finish="_MSr_FQjiEeSWNoVLtxf4XQ"/>
        <message xmi:type="uml:Message" xmi:id="_tlAw2AjhEeSWNoVLtxf4XQ" name="Sends Results" messageSort="reply" receiveEvent="_tlAw1gjhEeSWNoVLtxf4XQ" sendEvent="_tlAw1QjhEeSWNoVLtxf4XQ"/>
        <message xmi:type="uml:Message" xmi:id="_xlTWewjhEeSWNoVLtxf4XQ" name="Call Evaluation N" receiveEvent="_xlTWeQjhEeSWNoVLtxf4XQ" sendEvent="_xlTWeAjhEeSWNoVLtxf4XQ"/>
        <message xmi:type="uml:Message" xmi:id="_4wYdEgjhEeSWNoVLtxf4XQ" name="Launch Analysis" receiveEvent="_4wYdEAjhEeSWNoVLtxf4XQ" sendEvent="_4wPTJQjhEeSWNoVLtxf4XQ"/>
        <message xmi:type="uml:Message" xmi:id="__LL9WAjhEeSWNoVLtxf4XQ" name="Call Evaluation 1" receiveEvent="__LL9VgjhEeSWNoVLtxf4XQ" sendEvent="__LL9VQjhEeSWNoVLtxf4XQ"/>
        <message xmi:type="uml:Message" xmi:id="_MSr_GAjiEeSWNoVLtxf4XQ" name="Sends Results" messageSort="reply" receiveEvent="_MSr_FgjiEeSWNoVLtxf4XQ" sendEvent="_MSr_FQjiEeSWNoVLtxf4XQ"/>
        <message xmi:type="uml:Message" xmi:id="_PN-riAjiEeSWNoVLtxf4XQ" name="Sends all results" messageSort="reply" receiveEvent="_PN-rhgjiEeSWNoVLtxf4XQ" sendEvent="_PN-rhQjiEeSWNoVLtxf4XQ"/>
      </ownedBehavior>
    </packagedElement>
    <packagedElement xmi:type="uml:CallEvent" xmi:id="_tlAw1wjhEeSWNoVLtxf4XQ" name="CallEvent3"/>
    <packagedElement xmi:type="uml:CallEvent" xmi:id="_xlTWegjhEeSWNoVLtxf4XQ" name="CallEvent4"/>
    <packagedElement xmi:type="uml:CallEvent" xmi:id="_4wYdEQjhEeSWNoVLtxf4XQ" name="CallEvent1"/>
    <packagedElement xmi:type="uml:CallEvent" xmi:id="__LL9VwjhEeSWNoVLtxf4XQ" name="CallEvent2"/>
    <packagedElement xmi:type="uml:CallEvent" xmi:id="_MSr_FwjiEeSWNoVLtxf4XQ" name="CallEvent5"/>
    <packagedElement xmi:type="uml:CallEvent" xmi:id="_PN-rhwjiEeSWNoVLtxf4XQ" name="CallEvent6"/>
  </packagedElement>
  <profileApplication xmi:type="uml:ProfileApplication" xmi:id="idProfileApplication">
    <eAnnotations xmi:type="ecore:EAnnotation" xmi:id="idProfileAnnotation" source="http://www.eclipse.org/uml2/2.0.0/UML">
      <references xmi:type="ecore:EPackage" href="pathmap://SysMLActivityExtensionsProfile.uml#ProfileContentId"/>
    </eAnnotations>
    <appliedProfile xmi:type="uml:Profile" href="pathmap://SysMLActivityExtensionsProfile.uml#ActivityProfileId"/>
  </profileApplication>
</uml:Model>
