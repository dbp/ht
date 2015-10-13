## api.housetab.org

POST /UUID
body: JSON { from: string, balances: [[[person,...], NUMBER],...] }
response: JSON { balances: [[person, NUMBER],..] }

State: postgres has mapping from UUID to data structure that has
current responses from each person. That's it. When it recieves a
post, it updates the state, computes balances, and stores the state
(RACE? or just use DB transaction...).

Note that none of this data is particularly critical - it can be
recreated by having everyone edit their ledger.

## client.housetab.org

GET / - prompt to put in client UUID or create a new account w/ email
GET /UUID - entry form, prompt to put in name & server UUID if missing
POST /UUID - post entry
GET /UUID/edit - codemirror on ledger file
POST /UUID/edit - update whole file, provided it validates
GET /UUID/download - download ledger file stripped of housetab parts

State: The client has a postgres instance with UUIDs mapping to state for
the client. This includes the user name, server name, and email
address (which is persisted to S3 as described below), but also has a
a queue of operations to carry out on the ledger file. The operations
run in the background, but only one will run at a time (a central
queue has the UUID put in when a job in added to the UUID's
queue. When the job finishes, if the UUID queue still has jobs, the
UUID is added back to the central queue; if you add a job and the UUID
queue was not previously empty, no job needs to be added to the
central queue). Redis also has a mapping back from email to UUID, for
a 'forgot UUID' function.

There is also a cached version of ledger files. Note that the
important part of all of this is that none of this data is
particularly critical, and it can all be recreated from S3.

## s3.housetab.org

GET /UUID/file - download raw ledger file
GET /UUID/meta - download any user info (email address, user name, server name)

This is both where the application reads from / writes to, and it is
public (though unguessable).


# High level idea

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

These aren't right, because they lose the information about what the
money went to. For example, the groceries above should be able to be
reported under expenses:groceries, but it isn't.

Maybe prefix everything? Then we can use aliases to collapse?

2015/10/11 Groceries ;
    assets:bank -120
    housetab:amy,daniel:expenses:groceries

2015/10/11 Paying cash ;
    assets:bank -20
    housetab:daniel:expenses:reimbursements

If we do that, we can get the balances with:
`hledger bal housetab --flat --depth=2 -O json`

And we can get "regular" reporting with:

--alias "/housetab:([a-z,]+:)?/="

---


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
