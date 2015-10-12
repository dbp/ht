# Better

What if every user has their own ledger, and there is an application
that reports balances to a central server which responds with their
totals.

Everyone should have a housetab account. So you record
transactions like:

2015/10/11 Groceries ; people:amy&daniel
    assets:bank -120
    housetab

2015/10/11 Paying cash ; people:daniel
    assets:bank -20
    housetab

Or we could do even simpler and have accounts for who shares:

2015/10/11 Groceries
    assets:bank -120
    housetab:amy/daniel

2015/10/11 Paying cash
    assets:bank -20
    housetab:daniel

If we do it that way, hledger can report balances to us with
`hledger bal housetab -O json` (if we add a json output format), and
we can just format them and send them off. The only thing the client
needs to know is who we are.


Assume we simplify the application and say everyone has the same
number of shares, then all we have to report is what the totals are
for each sharing situation:

{ "from": "amy",
  "balances": { "amy&daniel": 120,
                "daniel": 20 }
}

The server has most recent values from each person, and presumably
from that can calculate the balance for each person. So it just
returns balances for each person:

{ "amy": 140,
  "daniel": -140
}

To pull this off, the server needs almost no state - no accounts, no
logins, just a unique identifier that is for the account. For ex, we
would post to api.housetab.org/F4080BFB-737E-4BDE-A5C2-7EEAD5DAE7A3
and get back out results; it would figure out who the people were
based on inputs in got (ie, if it ever recieves from a name or with a
name referenced in the balances, it will include that name). If you
wanted to reset, just pick a new endpoint (the server doesn't even
need to know about the endpoint a priori - getting a post to it would
create it).

If we do that, then almost _all_ our task is building a good input
system and probably online storage for hledger.

We can abandon previous features (history, shares) and leave the old
system up if people want to keep using it. Maybe we should add some
feature to migrate data over, or possibly don't even worry about it.
