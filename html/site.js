var picker = new Pikaday({ field: document.getElementById('date'),
                           defaultDate: new Date(),
                           setDefaultDate: true,
                           format: "YYYY/MM/DD"
                         });


var substringDefaultMatcher = function(strs, def) {
  return function findMatches(q, cb) {
    if (q === '') {
      cb(def);
    } else {
      var matches, substringRegex;

      // an array that will be populated with substring matches
      matches = [];

      // regex used to determine if a string contains the substring `q`
      substrRegex = new RegExp(q, 'i');

      // iterate through the pool of strings and for any string that
      // contains the substring `q`, add it to the `matches` array
      $.each(strs, function(i, str) {
        if (substrRegex.test(str)) {
          matches.push(str);
        }
      });

      cb(matches);
    }
  };
};

function typeahead(selector, values, defaults) {

  $(selector).typeahead({
    minLength: 0,
    source: substringDefaultMatcher(values, defaults),
    items: 4
  });

  $(selector).on("focusin", function () {
    window.setTimeout(function () {
      if ($(selector).val() === "") {
        $(selector).trigger("keyup");
      }
    }, 200);
  });
}

typeahead("#from",
          ["assets:bank",
           "assets:giftcard",
           "assets:paypal",
           "assets:reimbursements:atm",
           "assets:reimbursements:position",
           "assets:vanguard",
           "liabilities:credit"],
          ["assets:bank",
           "liabilities:credit"]);

typeahead("#to",
          ["housetab:amy:cash",
           "housetab:amy,daniel:groceries",
           "housetab:amy,daniel:misc",
           "expenses:business:hardware",
           "expenses:business:meetings",
           "expenses:business:office",
           "expenses:business:services",
           "expenses:business:training",
           "expenses:personal:cash",
           "expenses:personal:clothing",
           "expenses:personal:discretionary",
           "expenses:personal:donations",
           "expenses:personal:gifts",
           "expenses:personal:groceries",
           "expenses:personal:healthcare",
           "expenses:personal:household",
           "expenses:personal:misc",
           "expenses:personal:pet",
           "expenses:personal:rent",
           "expenses:personal:restaurants",
           "expenses:personal:utilities",
           "expenses:personal:utilities:phone",
           "expenses:personal:vacation",
           "expenses:taxes:federal",
           "expenses:taxes:ny",
           "expenses:taxes:ri",
           "expenses:taxes:services",
           "expenses:taxes:vt",
           "expenses:transit"],
          ["housetab:amy,daniel:groceries",
           "expenses:personal:misc"]);
