### Demonstrating why comparison dunders should check type and return NotImplemented

```python
In [17]: class Foo:
    ...:     def __init__(self, x):
    ...:         self.x = x
    ...:     def __eq__(self, other):
    ...:         return self.x == other.x
    ...: 

In [18]: class Bar:
    ...:     def __init__(self, x):
    ...:         self.x = x
    ...:     def __eq__(self, other):
    ...:         if isinstance(other, Bar):
    ...:             return self.x == other.x
    ...:         return NotImplemented
    ...: 

In [19]: class Baz:
    ...:     def __init__(self, y):
    ...:         self.y = y
    ...:     def __eq__(self, other):
    ...:         if isinstance(other, Baz):
    ...:             return self.y == other.y
    ...:         if isinstance(other, (Foo, Bar)):
    ...:             return self.y == other.x
    ...:         return NotImplemented
    ...: 

In [20]: foo, bar, baz = Foo(1), Bar(1), Baz(1)

In [21]: foo == 1
---------------------------------------------------------------------------
AttributeError                            Traceback (most recent call last)
Cell In[21], line 1
----> 1 foo == 1

Cell In[17], line 5, in Foo.__eq__(self, other)
      4 def __eq__(self, other):
----> 5     return self.x == other.x

AttributeError: 'int' object has no attribute 'x'

In [22]: bar == 1
Out[22]: False

In [23]: baz == bar
Out[23]: True

In [24]: baz == foo
Out[24]: True

In [27]: foo == baz
---------------------------------------------------------------------------
AttributeError                            Traceback (most recent call last)
Cell In[27], line 1
----> 1 foo == baz

Cell In[17], line 5, in Foo.__eq__(self, other)
      4 def __eq__(self, other):
----> 5     return self.x == other.x

AttributeError: 'Baz' object has no attribute 'x'

In [28]: bar == baz
Out[28]: True
```
