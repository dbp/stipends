<apply template="_base">

  <dfForm>
    <dfChildErrorList ref=""/>

    <dfLabel ref="amount">
      Paid this amount:
      <dfInput ref="amount" />
    </dfLabel>
    <dfLabel ref="period">
      for this period of time
      <dfInputSelect ref="period" />
    </dfLabel>
    <br/>

    <br/>

    <dfLabel ref="academic_year">
      Academic Year for this stipend
      <dfInputSelect ref="academic_year" />
    </dfLabel>
    <br/>

    <dfLabel ref="summer_guarantee">
      Is summer funding guaranteed?
      <dfInputSelect ref="summer_guarantee" />
    </dfLabel>
    <br/>

    <dfLabel ref="year_in_program">
      What year in the program is this stipend for?
      <dfInputSelect ref="year_in_program" />
    </dfLabel>
    <br/>

    <dfLabel ref="department">
      What department is this for?
      <dfInputSelect ref="department" />
    </dfLabel>
    <br/>

    <dfLabel ref="saw_document">
      Have you seen the offer letter that confirms this amount (if this is your own stipend, answer yes!)
      <dfInputSelect ref="saw_document" />
    </dfLabel>
    <br/>

    <dfLabel ref="notes">
      Any other notes (including department if we didn't have it above).
      <br/>
      <dfInputTextArea rows="5" cols="90" ref="notes">

      </dfInputTextArea>
    </dfLabel>
    <br/>
    
    <dfInputSubmit />
  </dfForm>

  <hr/>

  <departments>
    <div>
      <strong><department/></strong><br/>
      <ul>
        <years>
          <li>
            <year/>:
            <stipends>
              <is-verified><strong>$<amount/></strong></is-verified>
              <not-verified>$<amount/></not-verified>
            </stipends>
          </li>
        </years>
      </ul>
    </div>
  </departments>
</apply>
