---
title: "Chapter 29.  Dbstl Exception Classes"
api-name: "Chapter 29.  Dbstl Exception Classes"
source: docs/api_reference/STL/Exception_classes_group.html
---
## Chapter 29.  Dbstl Exception Classes

dbstl throws several types of exceptions on several kinds of errors, the exception classes form a class hiarachy.

First, there is the <a href="DbstlException.md" class="link" title="Chapter 30.  DbstlException">DbstlException</a> , which is the base class for all types of dbstl specific concrete exception classes. <a href="DbstlException.md" class="link" title="Chapter 30.  DbstlException">DbstlException</a> inherits from the class DbException of Berkeley DB C++ API. Since DbException class inherits from C++ STL exception base class std::exception, you can make use of all Berkeley DB C++ and dbstl API exceptions in the same way you use the C++ std::exception class.

Besides exceptions of <a href="DbstlException.md" class="link" title="Chapter 30.  DbstlException">DbstlException</a> and its subclasses, dbstl may also throw exceptions of DbException and its subclasses, which happens when a Berkeley DB call failed. So you should use the same way you catch Berkeley DB C++ API exceptions when you want to catch exceptions throw by Berkeley DB operations.

When an exception occurs, dbstl initialize an local exception object on the stack and throws the exception object, so you should catch an exception like this:

try { dbstl operations } catch(DbstlException ex){ Exception handling throw ex; // Optionally throw ex again }

#### Public Members

| Member | Description |
|----|----|
| <a href="DbstlException.md" class="link" title="Chapter 30.  DbstlException">DbstlException</a> | DbstlException |
| <a href="NotEnoughMemoryException.md" class="link" title="Chapter 35.  NotEnoughMemoryException">NotEnoughMemoryException</a> | NotEnoughMemoryException |
| <a href="InvalidIteratorException.md" class="link" title="Chapter 37.  InvalidIteratorException">InvalidIteratorException</a> | InvalidIteratorException |
| <a href="InvalidCursorException.md" class="link" title="Chapter 33.  InvalidCursorException">InvalidCursorException</a> | InvalidCursorException |
| <a href="InvalidDbtException.md" class="link" title="Chapter 31.  InvalidDbtException">InvalidDbtException</a> | InvalidDbtException |
| <a href="FailedAssertionException.md" class="link" title="Chapter 32.  FailedAssertionException">FailedAssertionException</a> | FailedAssertionException |
| <a href="NoSuchKeyException.md" class="link" title="Chapter 34.  NoSuchKeyException">NoSuchKeyException</a> | NoSuchKeyException |
| <a href="InvalidArgumentException.md" class="link" title="Chapter 39.  InvalidArgumentException">InvalidArgumentException</a> | InvalidArgumentException |
| <a href="NotSupportedException.md" class="link" title="Chapter 36.  NotSupportedException">NotSupportedException</a> | NotSupportedException |
| <a href="InvalidFunctionCall.md" class="link" title="Chapter 38.  InvalidFunctionCall">InvalidFunctionCall</a> | InvalidFunctionCall |

#### Group

None
