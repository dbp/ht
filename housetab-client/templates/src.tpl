<apply template="base">

  <div class="row row-no-padding">

    <div class="col-xs-12">
      <textarea id="ledger">
<source/>
      </textarea>
    </div>
  </div>
    <script type="text/javascript">
     var editor = CodeMirror.fromTextArea(document.getElementById("ledger"), {
       lineNumbers: true,
       viewportMargin: Infinity
     });
    </script>

</apply>
