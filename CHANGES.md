### 2013-06-21 Version 0.88.5

Fixed a bug in `encoding the boolean`. `true` should be `01` and false should be `00`. Now fixed

### 2013-06-21 Version 0.88.4

Fixed a bug in `Bson.encode_string`. The bug leads Bson to not accepting empty strings. Now fixed.
