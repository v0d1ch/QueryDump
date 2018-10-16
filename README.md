# QueryDump
Enjoy your raw sql queries

This thing works only on select queries for now.

So if you have a query that looks something like this

```
  select (from $
    \(user `LeftOuterJoin` products `LeftOuterJoin` things) -> do
    on (things ?. ThingId ==. product ^.ProductThing)
    on (user ?. UserProduct ==. product ^. ProductId)
    return user)

```

You can dump it if instead of `select` you put in `dumpQuery` like so:

```
  dumpQuery Nothing (from $
    \(user `LeftOuterJoin` products `LeftOuterJoin` things) -> do
    on (things ?. ThingId ==. product ^.ProductThing)
    on (user ?. UserProduct ==. product ^. ProductId)
    return user)

```
You also need to add a `RunDbM m` constraint to your function in order to get this working.
