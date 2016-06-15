# postgresql-simple-query-validator
Validate SQL statements embedded in postgresql-simple's quasiquoter against a PostgreSQL server.

# Why this exists

So I don't have to wait till an integration test or run-time to figure out
whether a query I wrote or modified is valid (syntax and structure).

Also, swapping '?'s and actual data is a pain.

**No more SQL syntax errors or incorrect column/table names :)**

# Usage

`./extractSql.py src/App/Queries.hs | ./validateSql.hs postgresql://user:password@host/dbname`

Errors, if any, will be printed to stdout and the exit code will be non-zero.
