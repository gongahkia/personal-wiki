> Continue making notes from the documentation from [here](https://dart.dev/language) and read more about variables!

# The [Dart](https://dart.dev/language) programming language

![](https://dart.dev/assets/shared/dart-logo-for-shares.png?2)

Dart is a **statically-typed**, **compiled** programming language, most commonly used to build *cross-platform* mobile, desktop and web apps using the [Fluttr framework](https://flutter.dev/).

> This document assumes a rudimentary understanding of the structure of programming languages.

---

## Basic introductions

Dart is ...

* a *semi-colon* language
* similar to `C`, `C++` and `Rust` in its calling of a *main function* and *function type declaration*

## Hello world

* `print()`
    * Print statement similar to `Python`.

```dart
void main() {
    print("Hello world!");
}
// printing to the console
```

## [Variables](https://dart.dev/language/variables)

* `var`
    * Variables in dart can be declared with `var` *without explicitly specifying their type*.

```dart
var name = "Shit ass";
var year = 1997;
var antennaDiameter = 3.7;
var flyByObjects = [ 'Jupiter', 'Saturn', 'Uranus', 'Neptune' ];
var image = {
    'tags': ['saturn'],
    'url': '//path/to/saturn'
};
```

However, we didn't come here to enjoy some weakly typed shit, so let's learn about Dart's data types!

### Data types && data structures

* `int`
* `double`
* `String`
* `bool`
* `List`
* `Map`

```dart
int num1 = 1;
double dec1 = 1.2;
String name1 = "gongkiasai";

List list1 = List(3);
list1[0] = 'Shit';
list1[1] = 'ok';
list2[2] = 'Dart';

Map map1 = Map();
map1['key1'] = 'value 1';
map2['key2'] = 'value 2';
map3['key3'] = 'value 3';
```

### [Null safety](https://dart.dev/null-safety)

In general, null safety is very sexy.

> *"The Dart language enforces sound null safety."*  
> ~ Dart lang docs

### Default value
