<apply template="_base">

  Created: <created-at/><br/>

  Amount: <strong><amount/></strong> (<period/>, <summer-guarantee/>)<br/>
  Academic Year: <academic-year/><br/>
  Year in Program: <year-in-program/><br/>
  Department: <strong><department/></strong><br/><br/>
  Notes: <notes/>

  <br/><br/>
  Supporting documents:<br/>
  <documents>
    <a href="/document/${id}">Document #<counter/></a><br/>
  </documents>
  <a href="/document/add?stipend=${token}">Add encrypted document</a>
 
</apply>
