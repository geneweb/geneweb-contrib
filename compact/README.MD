# geneweb-compact

A tool to remove unused values from your database.

It only support old legacy gwbd format.

## Why?

When modifiying a text, deleting a person or a family, GeneWeb does
not actually delete data, it just remove the links to this data,
instead.

It can be a security problem, if you though you erased sensitive data
from a note which is still present in the database.

Also, as the time goes by, leftover data can grow waste some space en
resources.

## What?

Current stage of the tool only report and dump leftover data. Next
step is to actually perfom a database cleanup.
