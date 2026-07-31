---
title: "Collections API"
api-name: "Collections API"
source: docs/upgrading/upgrade_4_5_collect.html
---
## Collections API

The changes to the Collections API are compatible with prior releases, with one exception: the Iterator object returned by the StoredCollection.iterator() method can no longer be explicitly cast to StoredIterator because a different implementation class is now used for iterators. If you depend on the StoredIterator class, you must now call StoredCollection.storedIterator() instead. Note the StoredIterator.close(Iterator) static method is compatible with the new iterator implementation, so no changes are necessary if you are using that method to close iterators.
