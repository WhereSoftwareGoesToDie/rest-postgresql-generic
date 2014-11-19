Notifications
=============

APIs implemented with the `rest-postgresql-generic` allows clients to register
a callback URL to receive notifications when resources change. This is a quick
description of this feature.

This document describes **version 0.0.1** of the notifications API.

Overview
--------

A client can create a `Watch` on a resource. Each `Watch` describes a resource
to be monitored, a callback to be notified, and some metadata. When a resource
is changed (i.e. updated or deleted), a `Notification` is created for each
`Watch` on that resource. `Notification`s are created while API operations are
being performed, but are delivered in the background.

API
---

### Create a new watch

`POST` on `$version/_notifications/watch` with a JSON document as described
below.

The response will redirect to the new `Watch` resource.

### Modify an existing watch

`PUT` on `$version/_notifications/watch/$id` with a JSON document as described
below.

### Delete an existing watch

`DELETE` on `$version/_notifications/watch/$id`

### List resources

`GET` on `$version/_notifications/watch/$id`

Documents
---------

### `Watch`

- `resource` is a text value, a valid URI, internal to the API; represents
a resource in the API.

- `callback` is a text value, a valid URI, external to the API.

- `created` is an integer value, a POSIX timestamp; when this `Watch` was
created.

- `count_triggered` is an integer value, the total number of times this `Watch` has
been triggered.

- `count_delivered` is an integer value, the total number of times this `Watch`
has resulted in a `Notification` which was delivered successfully.

- `count_errored` is an integer value, the total number of times this `Watch`
resulted in a `Notifcation` which could not be delivered.

### `Notification`

- `resource` is a text value, a valid URI, internal to the API; represents
a resource in the API.

- `callback` is a text value, a valid URI, external to the API.

- `event` is a text value, one of `updated` or `deleted`.

- `timestamp` is an integer value, a POSIX timestamp of the event according to
the API server's local clock. Default: now.

- `delivery_attempts` is an integer value; the number of times an attempt has
been made to deliver this `Notifcation`. Used together with `timestamp` to
implement retry of failed deliveries with back-off and eventual abandonment.
Default: 0.

Dispatch
--------

The generic handlers which implement "ordinary" resources run in a monad
transformer stack which includes a `ReaderT` of the current request URI
(including scheme, host, and path). When such a handler has successfully
performed an update operation it will invoke a `dispatchNotifications` handler.
This handler will use the URI information of the current resource stored in the
monad state and create `Notification`s for any corresponding `Watch`es.

The process of creating `Notification`s is done largely in the database with
something like:

````{.sql}
BEGIN TRANSACTION;
INSERT INTO notifications (resource, callback, event)
    SELECT resource, callback, ?
    FROM watches AS w
    WHERE w.resource = ?;
UPDATE notifications
    SET count_triggered = count_triggered + 1
    WHERE resource = ?;
COMMIT TRANSACTION;
````

NB: The `ReaderT` approach was suggested by one of the `rest` maintainers in
response to a [pull request][pr90]

[pr90]: https://github.com/silkapp/rest/pull/90

Delivery
--------

Once `Notification`s are recorded in the database, they wait for delivery by
the delivery agent.

The delivery agent will run as one or more threads or, potentially, processes
which periodically poll the `Notification`s database table for records which
either:

- Have never had an attempted delivered; or

- Are due for another delivery attempt.

A `Notification` is due for a delivery attempt when the current time stamp is
not less than `created + (2 ^ (attempts - 1) * retry_period)`. This will wait
`retry_period` for the first retry attempt, `2 * retry_period` (i.e. an
additional `1 * retry_period`) for the second retry, `4 * retry_period` (i.e.
an additional `2 * retry_period`) for the third retry, `8 * retry_period` (i.e.
an additional `4 * retry_period`) for the fourth, etc.

The delivery process takes the following configuration parameters:

- `retry_period` is a time span; defaults to 3600 seconds.

- `retry_attempts` is the maximum number of delivery attempts to be made;
defaults to 5. When this is exceeded the `Notification` is discarded and the
`count_errored` field on the `Watch` is incremented.

Delivery occurs like this:

1. Prepare a JSON document containing the details of the `Notification`.

2. `POST` the document to the `callback` URL.

3. If the response is a redirect, do the needful from (2).

4. If the response is `200 OK`, delivery is successful. Increment the
`count_delivered` field for the `Watch` and delete the `Notification` from the
database.

5. Otherwise, delivery is failed.

