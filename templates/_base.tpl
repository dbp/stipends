<!doctype html>
<meta charset="utf-8">
<html>
    <head>
        <css path="/static/%cache%/style.css"/>
    </head>
    <body>
      <h1 class="title">Northeastern University Grad Stipends</h1>
      <h2 class="subtitle">by GENU-UAW, a Union for RAs and TAs</h2>
      <ul class="menu">
        <li class="${is-home}"><a href="/">Home</a></li>
        <li class="${is-about}"><a href="/about">How it works</a></li>
        <li class="${is-who}"><a href="/who">Who we are</a></li>
        <is-organizer>
          <li class="organizer">Organizer: <name/></li>
        </is-organizer>
      </ul>
      
      <div class="message"><render-message/></div>
      
      <apply-content/>

      <div class="footer">
        This site is open source: <a href="https://github.com/dbp/stipends">https://github.com/dbp/stipends</a>
      </div>
    </body>
</html>
