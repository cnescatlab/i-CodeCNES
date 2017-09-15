<?xml version="1.0" encoding="UTF-8"?>
<uml:Model xmi:version="2.0" xmlns:xmi="http://www.omg.org/XMI" xmlns:ecore="http://www.eclipse.org/emf/2002/Ecore" xmlns:uml="http://www.eclipse.org/uml2/3.0.0/UML" xmi:id="idModel" name="main">
  <packagedElement xmi:type="uml:Package" xmi:id="idPackage" name="HandlerSequenceDiagramPackage">
    <packagedElement xmi:type="uml:Collaboration" xmi:id="idCollaboration" name="HandlerSequenceDiagramCollaboration">
      <ownedBehavior xmi:type="uml:Interaction" xmi:id="idInteraction">
        <ownedComment xmi:type="uml:Comment" xmi:id="_YuqFogjkEeSWNoVLtxf4XQ" annotatedElement="__pIysgjjEeSWNoVLtxf4XQ">
          <body>See Analyzer Sequence Diagram for details</body>
        </ownedComment>
        <lifeline xmi:type="uml:Lifeline" xmi:id="_MrttogjjEeSWNoVLtxf4XQ" name="User" coveredBy="_-Pk-xQjjEeSWNoVLtxf4XQ _mkyvCQjkEeSWNoVLtxf4XQ _7TircgjjEeSWNoVLtxf4XQ _mkyvAgjkEeSWNoVLtxf4XQ"/>
        <lifeline xmi:type="uml:Lifeline" xmi:id="_NDsv0gjjEeSWNoVLtxf4XQ" name="AbstractAnalysisHandler" coveredBy="_-Pk-xgjjEeSWNoVLtxf4XQ _GLPNxQjkEeSWNoVLtxf4XQ _D0AQpgjkEeSWNoVLtxf4XQ _AWIblQjkEeSWNoVLtxf4XQ _9Q_2ogjjEeSWNoVLtxf4XQ"/>
        <lifeline xmi:type="uml:Lifeline" xmi:id="_1QkcYgjjEeSWNoVLtxf4XQ" name="AbstractAnalysisJob" coveredBy="_AWIblgjkEeSWNoVLtxf4XQ _D0AQpQjkEeSWNoVLtxf4XQ __pIysgjjEeSWNoVLtxf4XQ"/>
        <lifeline xmi:type="uml:Lifeline" xmi:id="_4sgXwgjjEeSWNoVLtxf4XQ" name="AbstractView" coveredBy="_GLPNxgjkEeSWNoVLtxf4XQ _4axSFQjkEeSWNoVLtxf4XQ _3AnoSAjkEeSWNoVLtxf4XQ _mkyvCAjkEeSWNoVLtxf4XQ _Fdy48gjkEeSWNoVLtxf4XQ"/>
        <lifeline xmi:type="uml:Lifeline" xmi:id="_vLqeMAjkEeSWNoVLtxf4XQ" name="Converter" coveredBy="_3AnoSQjkEeSWNoVLtxf4XQ _4axSFAjkEeSWNoVLtxf4XQ _3AnoQgjkEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_4axSFAjkEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification13" covered="_vLqeMAjkEeSWNoVLtxf4XQ" event="_4axSFgjkEeSWNoVLtxf4XQ" message="_4axSFwjkEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_4axSFQjkEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification14" covered="_4sgXwgjjEeSWNoVLtxf4XQ" event="_4axSFgjkEeSWNoVLtxf4XQ" message="_4axSFwjkEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_3AnoSAjkEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification11" covered="_4sgXwgjjEeSWNoVLtxf4XQ" event="_3AnoSgjkEeSWNoVLtxf4XQ" message="_3AnoSwjkEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_3AnoSQjkEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification12" covered="_vLqeMAjkEeSWNoVLtxf4XQ" event="_3AnoSgjkEeSWNoVLtxf4XQ" message="_3AnoSwjkEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_mkyvCAjkEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification9" covered="_4sgXwgjjEeSWNoVLtxf4XQ" event="_mkyvCgjkEeSWNoVLtxf4XQ" message="_mkyvCwjkEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_mkyvCQjkEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification10" covered="_MrttogjjEeSWNoVLtxf4XQ" event="_mkyvCgjkEeSWNoVLtxf4XQ" message="_mkyvCwjkEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_GLPNxQjkEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification7" covered="_NDsv0gjjEeSWNoVLtxf4XQ" event="_GLPNxwjkEeSWNoVLtxf4XQ" message="_GLPNyAjkEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_GLPNxgjkEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification8" covered="_4sgXwgjjEeSWNoVLtxf4XQ" event="_GLPNxwjkEeSWNoVLtxf4XQ" message="_GLPNyAjkEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_D0AQpQjkEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification5" covered="_1QkcYgjjEeSWNoVLtxf4XQ" event="_D0AQpwjkEeSWNoVLtxf4XQ" message="_D0AQqAjkEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_D0AQpgjkEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification6" covered="_NDsv0gjjEeSWNoVLtxf4XQ" event="_D0AQpwjkEeSWNoVLtxf4XQ" message="_D0AQqAjkEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_AWIblQjkEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification3" covered="_NDsv0gjjEeSWNoVLtxf4XQ" event="_AWIblwjkEeSWNoVLtxf4XQ" message="_AWIbmAjkEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_AWIblgjkEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification4" covered="_1QkcYgjjEeSWNoVLtxf4XQ" event="_AWIblwjkEeSWNoVLtxf4XQ" message="_AWIbmAjkEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_-Pk-xQjjEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification1" covered="_MrttogjjEeSWNoVLtxf4XQ" event="_-Pk-xwjjEeSWNoVLtxf4XQ" message="_-Pk-yAjjEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:MessageOccurrenceSpecification" xmi:id="_-Pk-xgjjEeSWNoVLtxf4XQ" name="MessageOccurrenceSpecification2" covered="_NDsv0gjjEeSWNoVLtxf4XQ" event="_-Pk-xwjjEeSWNoVLtxf4XQ" message="_-Pk-yAjjEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:BehaviorExecutionSpecification" xmi:id="_7TircgjjEeSWNoVLtxf4XQ" name="BehaviorExecutionSpecification1" covered="_MrttogjjEeSWNoVLtxf4XQ" start="_-Pk-xQjjEeSWNoVLtxf4XQ" finish="_-Pk-xQjjEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:BehaviorExecutionSpecification" xmi:id="_9Q_2ogjjEeSWNoVLtxf4XQ" name="BehaviorExecutionSpecification2" covered="_NDsv0gjjEeSWNoVLtxf4XQ" start="_-Pk-xgjjEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:BehaviorExecutionSpecification" xmi:id="__pIysgjjEeSWNoVLtxf4XQ" name="BehaviorExecutionSpecification3" covered="_1QkcYgjjEeSWNoVLtxf4XQ" start="_AWIblgjkEeSWNoVLtxf4XQ" finish="_D0AQpQjkEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:BehaviorExecutionSpecification" xmi:id="_Fdy48gjkEeSWNoVLtxf4XQ" name="BehaviorExecutionSpecification4" covered="_4sgXwgjjEeSWNoVLtxf4XQ" start="_GLPNxgjkEeSWNoVLtxf4XQ" finish="_mkyvCAjkEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:BehaviorExecutionSpecification" xmi:id="_3AnoQgjkEeSWNoVLtxf4XQ" name="BehaviorExecutionSpecification6" covered="_vLqeMAjkEeSWNoVLtxf4XQ" start="_3AnoSQjkEeSWNoVLtxf4XQ" finish="_4axSFAjkEeSWNoVLtxf4XQ"/>
        <fragment xmi:type="uml:BehaviorExecutionSpecification" xmi:id="_mkyvAgjkEeSWNoVLtxf4XQ" name="BehaviorExecutionSpecification5" covered="_MrttogjjEeSWNoVLtxf4XQ" start="_mkyvCQjkEeSWNoVLtxf4XQ" finish="_mkyvCQjkEeSWNoVLtxf4XQ"/>
        <message xmi:type="uml:Message" xmi:id="_-Pk-yAjjEeSWNoVLtxf4XQ" name="Click on &quot;Run Analysis&quot;" receiveEvent="_-Pk-xgjjEeSWNoVLtxf4XQ" sendEvent="_-Pk-xQjjEeSWNoVLtxf4XQ"/>
        <message xmi:type="uml:Message" xmi:id="_AWIbmAjkEeSWNoVLtxf4XQ" name="Launch Analysis" receiveEvent="_AWIblgjkEeSWNoVLtxf4XQ" sendEvent="_AWIblQjkEeSWNoVLtxf4XQ"/>
        <message xmi:type="uml:Message" xmi:id="_D0AQqAjkEeSWNoVLtxf4XQ" name="Send Analysis Results" messageSort="reply" receiveEvent="_D0AQpgjkEeSWNoVLtxf4XQ" sendEvent="_D0AQpQjkEeSWNoVLtxf4XQ"/>
        <message xmi:type="uml:Message" xmi:id="_GLPNyAjkEeSWNoVLtxf4XQ" name="Sends results to View" receiveEvent="_GLPNxgjkEeSWNoVLtxf4XQ" sendEvent="_GLPNxQjkEeSWNoVLtxf4XQ"/>
        <message xmi:type="uml:Message" xmi:id="_mkyvCwjkEeSWNoVLtxf4XQ" name="Update view with new results" messageSort="reply" receiveEvent="_mkyvCQjkEeSWNoVLtxf4XQ" sendEvent="_mkyvCAjkEeSWNoVLtxf4XQ"/>
        <message xmi:type="uml:Message" xmi:id="_3AnoSwjkEeSWNoVLtxf4XQ" name="Sends results" receiveEvent="_3AnoSQjkEeSWNoVLtxf4XQ" sendEvent="_3AnoSAjkEeSWNoVLtxf4XQ"/>
        <message xmi:type="uml:Message" xmi:id="_4axSFwjkEeSWNoVLtxf4XQ" name="Return converted results" messageSort="reply" receiveEvent="_4axSFQjkEeSWNoVLtxf4XQ" sendEvent="_4axSFAjkEeSWNoVLtxf4XQ"/>
      </ownedBehavior>
    </packagedElement>
    <packagedElement xmi:type="uml:CallEvent" xmi:id="_-Pk-xwjjEeSWNoVLtxf4XQ" name="CallEvent1"/>
    <packagedElement xmi:type="uml:CallEvent" xmi:id="_AWIblwjkEeSWNoVLtxf4XQ" name="CallEvent2"/>
    <packagedElement xmi:type="uml:CallEvent" xmi:id="_D0AQpwjkEeSWNoVLtxf4XQ" name="CallEvent3"/>
    <packagedElement xmi:type="uml:CallEvent" xmi:id="_GLPNxwjkEeSWNoVLtxf4XQ" name="CallEvent4"/>
    <packagedElement xmi:type="uml:CallEvent" xmi:id="_mkyvCgjkEeSWNoVLtxf4XQ" name="CallEvent5"/>
    <packagedElement xmi:type="uml:CallEvent" xmi:id="_3AnoSgjkEeSWNoVLtxf4XQ" name="CallEvent6"/>
    <packagedElement xmi:type="uml:CallEvent" xmi:id="_4axSFgjkEeSWNoVLtxf4XQ" name="CallEvent7"/>
  </packagedElement>
  <profileApplication xmi:type="uml:ProfileApplication" xmi:id="idProfileApplication">
    <eAnnotations xmi:type="ecore:EAnnotation" xmi:id="idProfileAnnotation" source="http://www.eclipse.org/uml2/2.0.0/UML">
      <references xmi:type="ecore:EPackage" href="pathmap://SysMLActivityExtensionsProfile.uml#ProfileContentId"/>
    </eAnnotations>
    <appliedProfile xmi:type="uml:Profile" href="pathmap://SysMLActivityExtensionsProfile.uml#ActivityProfileId"/>
  </profileApplication>
</uml:Model>
