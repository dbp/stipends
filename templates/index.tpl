<bind tag="is-home">sel</bind>
<bind tag="is-about"></bind>
<bind tag="is-who"></bind>
<apply template="_base">

  <div id="add-box" class="hidden">
    <h2>Add a stipend (will be reviewed before publishing)</h2>
    
    <dfForm>
      <dfChildErrorList class="error" ref=""/>

      <dfLabel ref="amount">
        Paid:
        $<dfInput ref="amount" />
      </dfLabel>
      <dfLabel ref="period">
        every
        <dfInputSelect ref="period" />
        as a stipend
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
  <script type="text/javascript">
   if (document.getElementsByClassName("error").length > 0) {
     document.getElementById("add-button").childNodes[1].onclick()
   }
  </script>
  
  <h2>All existing stipend data</h2>

  <div class="data">
    <div id="legend" class="legend shortened" onclick="document.getElementById('legend').className = 'legend shortened'; arguments[0].stopPropagation();">
      <div class="overlay" onclick="document.getElementById('legend').className = 'legend'; arguments[0].stopPropagation();">↓expand↓</div>
      <h3>Legend</h3>
      <div class="row">
        <div class="col"><span class="summer">summer</span></div>
        <div class="col">salary paid over 12 months</div>
      </div><!-- .row -->
      <div class="row">
        <div class="col"><span class="summer guarantee8mo">summer</span></div>
        <div class="col">salary shown is paid over 8 month academic year: over summer, grad must find other funding, which may be impossible because of visa work requirements.</div>
      </div><!-- .row -->
      <div class="row">
        <div class="col">●●●</div>
        <div class="col">Offer letter confirming this stipend uploaded and verified by the <a href="/who">site curators</a>.</div>
      </div><!-- .row -->
      <div class="row">
        <div class="col">●●○</div>
        <div class="col">Offer letter seen by organizer with the grad union.</div>
      </div><!-- .row -->
      <div class="row">
        <div class="col">●○○</div>
        <div class="col">Self-reported or by organizer.</div>
      </div><!-- .row -->
      <div class="row">
        <div class="col">○○○</div>
        <div class="col">Reported secondhand.</div>
      </div><!-- .row -->

      <div class="collapse">↑close↑</div>
    </div>
    <br/>
    <departments>
      <h3><department/></h3>
      <div class="department">
        <div class="years">
          <years>
            <div class="year">
              <div class="title"><year/></div>
              <div class="stipends">
                <stipends>
                  <div class="stipend">
                    <div class="row1">
                      <div class="amount" title="${amount-note}">$<amount/></div>
                      <div class="summer guarantee${length-paid}">summer</div>
                    </div><!-- .row1 -->
                    <div class="row2">
                      <verified>
                        <div class="trusted" title="${note}"><bullets/></div>
                      </verified>
                      <div class="cohort">
                        <year-in-program/> yr PhD
                      </div>
                    </div><!-- .row2 -->
                  </div> <!-- .stipend -->
                </stipends>
              </div> <!-- .stipends -->
            </div> <!-- .year -->
          </years>
        </div><!-- .years -->
      </div> <!-- .department -->
    </departments>
  </div>
</apply>
