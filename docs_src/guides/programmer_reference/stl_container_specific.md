---
title: "Dbstl container specific notes"
api-name: "Dbstl container specific notes"
source: docs/programmer_reference/stl_container_specific.html
---
## Dbstl container specific notes

<span class="sect2"> [db_vector specific notes](stl_container_specific.md#idp51492808) </span>

<span class="sect2"> [Associative container specific notes](stl_container_specific.md#idp51561456) </span>

### db_vector specific notes

- Set the <a href="../../api/c/dbset_flags.md#dbset_flags_DB_RENUMBER" class="olink">DB_RENUMBER</a> flag in the database handle if you want `db_vector<>` to work like `std::vector` or `std::deque`. Do not set <a href="../../api/c/dbset_flags.md#dbset_flags_DB_RENUMBER" class="olink">DB_RENUMBER</a> if you want `db_vector<>` to work like `std::list`. Note that without <a href="../../api/c/dbset_flags.md#dbset_flags_DB_RENUMBER" class="olink">DB_RENUMBER</a> set, `db_vector<>` can work faster.

  For example, to construct a fast std::queue/std::stack object, you only need a `db_vector<>` object whose database handle does not have <a href="../../api/c/dbset_flags.md#dbset_flags_DB_RENUMBER" class="olink">DB_RENUMBER</a> set. Of course, if the database handle has <a href="../../api/c/dbset_flags.md#dbset_flags_DB_RENUMBER" class="olink">DB_RENUMBER</a> set, it still works for this kind of scenario, just not as fast.

  `db_vector` does not check whether <a href="../../api/c/dbset_flags.md#dbset_flags_DB_RENUMBER" class="olink">DB_RENUMBER</a> is set. If you do not set it, `db_vector<>` will not work like std::vector\<\>/std::deque\<\> with regard to operator\[\], because the indices are not maintained in that case.

  You can find example code showing how to use this feature in the `StlAdvancedFeaturesExample::queue_stack()` method.

- Just as is the case with `std::vector`, inserting/deleting in the middle of a `db_vector` is slower than doing the same action at the end of the sequence. This is because the underlying DB_RECNO DB (with the <a href="../../api/c/dbset_flags.md#dbset_flags_DB_RENUMBER" class="olink">DB_RENUMBER</a> flag set) is relatively slow when inserting/deleting in the middle or the head — it has to update the index numbers of all the records following the one that was inserted/deleted. If you do not need to keep the index ordered on insert/delete, you can use `db_map` instead.

  `db_vector` also contains methods inherited from `std::list` and `std::deque`, including `std::list<>'s` unique methods `remove()`, `remove_if()`, `unique()`, `merge()`, `sort()`, `reverse()`, and `splice()`. These use the identical semantics/behaviors of the `std::list<>` methods, although pushing/deleting at the head is slower than the `std::deque` and `std::list` equivalent when there are quite a lot of elements in the database.

- You can use `std::queue`, `std::priority_queue` and `std::stack` container adapters with `db_vector`; they work with db_vector even without <a href="../../api/c/dbset_flags.md#dbset_flags_DB_RENUMBER" class="olink">DB_RENUMBER</a> set.

### Associative container specific notes

`db_map` contains the union of method set from `std::map` and `hash_map`, but there are some methods that can only be called on containers backed by `DB_BTREE` or `DB_HASH` databases. You can call `db_map<>::is_hash()` to figure out the type of the backing database. If you call unsupported methods then an InvalidFunctionCall exception is thrown.

These are the `DB_BTREE` specific methods: `upper_bound()`, `lower_bound()`, `key_comp()`, and `value_comp()`. The `DB_HASH` specific methods are `key_eq()`, `hash_funct()`.
