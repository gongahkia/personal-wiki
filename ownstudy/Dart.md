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

However, we didn't come here to enjoy some weakly typed shit, so let's learn Dart's data types!

## Data types

* `int`
* `double`
* `String`
* `bool`
* `List`
* `Map`

```dart
int num1 = 1;
double dec1 = 1.2;
String name = "gongkiasai";
```

## [Null safety](https://dart.dev/null-safety)

In general, null safety is very sexy.

> *"The Dart language enforces sound null safety."*  
> ~ Dart lang docs

