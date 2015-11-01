<!doctype html>

<html>
  <head>
    <meta name="viewport" content="width=350px, user-scalable=no">
    <title>HouseTab</title>
    <link rel="stylesheet" href="/static/bootstrap/css/bootstrap.min.css"/>
    <link rel="stylesheet" href="/static/pikaday.css"/>

    <script type="text/javascript" src="/static/jquery-2.1.4.min.js"></script>
    <script type="text/javascript" src="/static/bootstrap/js/bootstrap.min.js"></script>
    <script type="text/javascript" src="/static/bootstrap3-typeahead.min.js"></script>
    <script type="text/javascript" src="/static/moment.min.js"></script>
    <script type="text/javascript" src="/static/pikaday.js"></script>

    <link rel="stylesheet" href="/static/codemirror.css">
    <script src="/static/codemirror.js"></script>

    <script type="text/javascript" src="/static/site.js"> </script>
    <link rel="stylesheet" href="/static/site.css"/>
  </head>

  <body>
    <div class="container-fluid">

      <div class="row row-no-padding navigation">
        <div class="col-xs-3">
          <add-class url-ends-with="add" class="selected"><a href="add" class="btn btn-block"><span class="glyphicon glyphicon-header"></span></a></add-class>
        </div>
        <div class="col-xs-3">
          <add-class url-ends-with="reg" class="selected"><a href="reg" class="btn btn-block"><span class="glyphicon glyphicon-th-list"></span></a></add-class>
        </div>
        <div class="col-xs-3">
          <add-class url-ends-with="src" class="selected"><a href="src" class="btn btn-block"><span class="glyphicon glyphicon-console"></span></a></add-class>
        </div>
        <div class="col-xs-3">
          <add-class url-ends-with="settings" class="selected"><a href="settings" class="btn btn-block"><span class="glyphicon glyphicon-cog"></span></a></add-class>
        </div>
      </div>

      <apply-content/>


    </div>


  </body>
</html>
