<apply template="base">

  <div class="row row-no-padding">

    <div class="col-xs-12">
      <textarea id="ledger">

2015/07/05 price chopper
    expenses:personal:groceries          7.85
    liabilities:credit:chase            -7.85

2015/07/06 compare
    liabilities:credit:chase           -42.90
    expenses:personal:groceries         42.90

2015/07/07 compare
    liabilities:credit:chase           -54.78
    expenses:personal:groceries         54.78

2015/07/11 compare
    liabilities:credit:chase           -30.61
    expenses:personal:groceries         30.61

2015/07/12 compare
    liabilities:credit:chase           -35.86
    expenses:personal:groceries         35.86

2015/07/13 compare
    liabilities:credit:chase           -26.91
    expenses:personal:groceries         26.91

2015/07/14 compare
    liabilities:credit:chase           -41.32
    expenses:personal:groceries         41.32

2015/07/14 wine
    liabilities:credit:chase           -16.32
    expenses:personal:groceries         16.32

2015/07/20 compare
    liabilities:credit:chase           -21.33
    expenses:personal:groceries         21.33

2015/07/21 compare
    liabilities:credit:chase           -33.18
    expenses:personal:groceries         33.18

2015/07/22 compare
    liabilities:credit:chase           -17.74
    expenses:personal:groceries         17.74

2015/07/23 brocolli farm
    expenses:personal:groceries          6.48
    liabilities:credit:chase            -6.48

2015/07/25 tamarind paste
    expenses:personal:groceries          6.55
    liabilities:credit:chase            -6.55

2015/07/26 compare
    liabilities:credit:chase           -33.24
    expenses:personal:groceries         33.24

2015/07/29 jovial pasta
    expenses:personal:groceries         24.00
    liabilities:credit:chase           -24.00

2015/07/29 compare
    liabilities:credit:chase           -25.50
    expenses:personal:groceries         25.50

2015/07/31 stop and shop
    liabilities:credit:chase           -16.72
    expenses:personal:groceries         16.72

2015/08/15 tea
    assets:paypal                      -56.95
    expenses:personal:groceries         56.95

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
