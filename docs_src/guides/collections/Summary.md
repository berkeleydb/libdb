---
title: "Chapter 7.  Summary"
api-name: "Chapter 7.  Summary"
source: docs/collections/tutorial/Summary.html
---
## Chapter 7.  Summary

In summary, the DB Java Collections API tutorial has demonstrated how to create different types of bindings, as well as how to use the basic facilities of the DB Java Collections API: the environment, databases, secondary indices, collections, and transactions. The final approach illustrated by the last example program, Serializable Entity, uses tuple keys and serial entity values. Hopefully it is clear that any type of object-to-data binding may be implemented by an application and used along with standard Java collections.

The following table summarizes the differences between the examples in the tutorial.

| Example | Key | Value | Entity | Comments |
|----|----|----|----|----|
| <a href="BasicProgram.md" class="xref" title="Chapter 2.  The Basic Program">The Basic Program</a> | Serial | Serial | No | The shipment program |
| <a href="UsingSecondaries.md" class="xref" title="Chapter 3.  Using Secondary Indices">Using Secondary Indices</a> | Serial | Serial | No | Secondary indices |
| <a href="Entity.md" class="xref" title="Chapter 4.  Using Entity Classes">Using Entity Classes</a> | Serial | Serial | Yes | Combining the key and value in a single object |
| <a href="Tuple.md" class="xref" title="Chapter 5.  Using Tuples">Using Tuples</a> | Tuple | Serial | Yes | Compact ordered keys |
| <a href="SerializableEntity.md" class="xref" title="Chapter 6.  Using Serializable Entities">Using Serializable Entities</a> | Tuple | Serial | Yes | One serializable class for entities and values |

Having completed this tutorial, you may want to explore how other types of bindings can be implemented. The bindings shown in this tutorial are all <span class="emphasis">*external bindings*</span>, meaning that the data classes themselves contain none of the binding implementation. It is also possible to implement <span class="emphasis">*internal bindings*</span>, where the data classes implement the binding.

Internal bindings are called <span class="emphasis">*marshalled bindings*</span> in the DB Java Collections API, and in this model each data class implements a marshalling interface. A single external binding class that understands the marshalling interface is used to call the internal bindings of each data object, and therefore the overall model and API is unchanged. To learn about marshalled bindings, see the <span class="pdf;html"> `marshal` and `factory` examples that came with your DB distribution (you can find them in `<INSTALL_DIR>/examples_java/src/com/sleepycat/examples/collections/ship` where `<INSTALL_DIR>` is the location where you unpacked your DB distribution). </span> These examples continue building on the example programs used in the tutorial. The Marshal program is the next program following the Serializable Entity program, and the Factory program follows the Marshal program. The source code comments in these examples explain their differences.
