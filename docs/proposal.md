Datatype driven Rest interface for PostgreSQL
======================================================

Providing a restful interface to a PostgreSQL database can involve a
lot of boilerplate code. Required operations for such an interface are
`GET`, `UPDATE`, `REMOVE`, `CREATE` and `LIST`. A resource in this
sense is a database table. This lines up nicely with an ADT and should
be possible to generalise using generics. This project aims to do
this.

Frontend
---------

The frontend is responsible for providing the rest interface.

### Rest framework

There are multiple rest server libraries in Haskell. The ones that
seemed to fit best were *servant* and *rest*. Both *servant* and
*rest* offer all the standard operations. Both can also be
parametarised by the return/request type.

An important difference are the supported web frameworks. Both
*servant* and *rest* are designed to be agnostic in this
respect. *Servant* however has only one such adaptor, namely for
*scotty*. *Rest* on the other hand supports *snap*, *happstack* and
*wai*. *rest* also seems to be a more actively developed project.

*Rest* will also generate client code for Haskell and JavaScript
 automatically using the defined Api.

### Marshalling/Unmarshalling

The rest interface should communicate via JSON. Using Aeson for this
is common, but requires instances for `FromJSON` and
`ToJSON`. `generic-aeson` uses an existing (derivable) instance for
`GHC.Generic` for to use for such an instance. Similar things are
possible for `JSONSchema`.

```
data Post = Post
  { postTitle :: Text
  , postBody :: Text
  } deriving (Generic, Typeable)
instance Model Post
instance JSONSchema Post where
  schema = gSchema
instance ToJSON Post where
instance FromJSON Post where
```

Backend
--------

There are 2 packages on hackage which handle accessing PostgreSQL
through generics. `postgresql-simple-sop` and
`postgresql-orm`. `postgresql-simple-sop` only supplies generic
`FromRow` and `ToRow` methods. Queries are still fully
manual. `postgresql-orm` however offers a more complete
interface. This includes predefined methods for all our required
operations.

Proof of Concept
-------------------

A working full draft can be found at
<https://gist.github.com/tvh/7a9e4dbd8e2e291c0867>.

This used *rest* and *postgresql-orm* and *aeson-generic*.

The implementation is independent from the datatype and is only driven
by derivable typeclasses. Selecting the correct instance is done in
the router.

```
testRouter :: forall m . (Applicative m, MonadIO m) => Router (ReaderT Connection m) (ReaderT Connection m)
testRouter = root -/ post
  where
    post = route (resource :: GenericResource m tr Post)
```

The web framework used to serve the resource is *snap*. This choice is
arbitrary and can be exchanged by any of the other supported
frameworks.
