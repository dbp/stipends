Created: <created-at/><br/>

Amount: <strong><amount/></strong> (<period/>, <summer-typical/>)<br/>
Academic Year: <academic-year/><br/>
Year in Program: <year-in-program/><br/>
Department: <strong><department/></strong><br/>

<not-verified>
  <is-curator>
    <br/>
    Not verified: <a href="/stipend/${id}/verify">verify</a><br/>
    Reported by:
    <reporter>
      <is-trusted>
        <name/> (organizer)
      </is-trusted>
      <not-trusted>
        <a href="/reporter/${id}/stipends" target="_blank">Anonymous</a> w/ fingerprint <fingerprint/>
      </not-trusted>
    </reporter>
    <br/>
    <a href="/stipend/${id}/delete" onclick="return confirm('Are you sure?');">Delete this stipend</a><br/> 
  </is-curator>
</not-verified>

<br/>
Notes: <notes/>

<br/><br/>
Supporting documents:<br/>
<documents>
  <a href="/document/${id}" target="_blank">Document #<counter/></a>
  <verified>
    Verified on <strong><verified-at/></strong>
  </verified>
  <not-verified>
    <is-curator>
      <a href="/document/${id}/verify">verify</a>
    </is-curator>
  </not-verified>
  <br/>
</documents>
