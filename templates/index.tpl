<bind tag="is-home">sel</bind>
<bind tag="is-about"></bind>
<bind tag="is-who"></bind>
<apply template="_base">

  <div id="add-box" class="hidden">
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

      <hr/>

      <dfLabel ref="academic_year">
        Academic Year for this stipend
        <dfInputSelect ref="academic_year" />
      </dfLabel>
      
      <hr/>

      <dfLabel ref="summer_typical">
        Is summer funding typically provided (or guaranteed)?
        <dfInputSelect ref="summer_typical" />
      </dfLabel>

      <hr/>

      <dfLabel ref="year_in_program">
        What year in the program is this stipend for?
        <dfInputSelect ref="year_in_program" />
      </dfLabel>

      <hr/>

      <dfLabel ref="department">
        What department is this for?
        <dfInputSelect ref="department" />
      </dfLabel>

      <hr/>

      <dfLabel ref="saw_document">
        Have you seen the offer letter confirming this amount (if this is your own stipend, answer yes!)
        <dfInputSelect ref="saw_document" />
      </dfLabel>

      <hr/>

      <dfLabel ref="notes">
        Any other notes, including department if you put "Other" above.
        (won't ever be posted publically, so you can include contact info if you are okay with us following up).
        <br/>
        <dfInputTextArea rows="5" cols="90" ref="notes">

        </dfInputTextArea>
      </dfLabel>

      <hr/>

      After submitting, you can attach a document (likely, an offer letter) that confirms the stipend. This will be securely encrypted so only the site <a href="/who" target="_blank">curators</a> can view it.

      <p style="text-align: right;">
        <dfInputSubmit value="Submit Stipend for Review" />
      </p>
    </dfForm>
  </div>

  <p id="add-button">
    <span onclick="document.getElementById('add-box').className = ''; document.getElementById('add-button').className = 'hidden';">Add a stipend</span>
  </p>
  
  <h2>All existing stipend data</h2>

  <departments>
    <div class="department">
      <strong><department/></strong><br/>
      <ul>
        <years>
          <li class="year">
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
