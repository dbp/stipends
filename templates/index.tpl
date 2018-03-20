<bind tag="is-home">sel</bind>
<bind tag="is-about"></bind>
<bind tag="is-who"></bind>
<apply template="_base">

  <h2>Add a stipend (will be reviewed before publishing)</h2>
  
  <dfForm>
    <dfChildErrorList ref=""/>

    <dfLabel ref="amount">
      Paid this amount:
      <dfInput ref="amount" />
    </dfLabel>
    <dfLabel ref="period">
      as a 
      <dfInputSelect ref="period" /> stipend
    </dfLabel>
    <br/>

    <br/>

    <dfLabel ref="academic_year">
      Academic Year for this stipend
      <dfInputSelect ref="academic_year" />
    </dfLabel>
    <br/>

    <dfLabel ref="summer_typical">
      Is summer funding typically provided (or guaranteed)?
      <dfInputSelect ref="summer_typical" />
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

  <h2>All public data</h2>

  <departments>
    <div>
      <strong><department/></strong><br/>
      <ul>
        <years>
          <li>
            <year/>:
            <stipends>
              <div class="amount">
                <span title="${amount-note}">$<amount/> (<length-paid/>)</span><br/>
                <verified>
                  <span title="${note}">trusted: <bullets/></span>
                </verified>
              </div>
            </stipends>
          </li>
        </years>
      </ul>
    </div>
  </departments>
</apply>
