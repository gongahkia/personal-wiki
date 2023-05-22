> *Continue from [here](https://www.youtube.com/watch?v=5OdVJbNCSso) @ `1:02:17`.*

# MySQL

![](https://1000logos.net/wp-content/uploads/2020/08/MySQL-Logo.jpg)

---

## SQL: Structured Query Language

* A **database** is a *`folder`* that holds *`files`*, which are the tables within the database.
* Used to **CRUD** (Create/Update/Retrieve/Delete) data from a database.
* There are *2 types of databases*:
    * Relational database 
        * MySQL
    * Non-relational database
        * NoSQL
* MySQL has **no case-sensitivity** for its keywords.
* `Sys` is the internal database that MySQL uses.

---

## Databases

### KEYWORDS
* `create database {name}`
* `use {name}`
* `drop database {name}`
* changing **read only** mode
    * `alter database {name} read only = 1`
    * `alter database {name} read only = 0`

#### Creating a database

```sql
create database myDB; 
/* Creates a database of name `myDB` */
```

#### Using a database

```sql
use myDB;
/* indicates to MySQL to use the database of the given name `myDB` */
```

#### Dropping a database

```sql
drop database myDB;
/* drops the given database of name `myDB` */
```

#### Setting and Unsetting database to `read only`

```sql
alter database myDB read only = 1;
/* sets database `myDB` to read only mode as true, 1 */

alter database myDB read only = 0;
/* sets database `myDB` to read only mode as false, 0 */
```

---

## Tables

### KEYWORDS
* `create table {name} ({column names and data type, comma-separated})`
* `select * from {table name}`
    * `where {conditional check related to a table field value}`
* `rename table {original name} to {new name}`
* `drop table {name}`
* `alter table {name}`
    * `add {column name and data type}`
    * `rename column {original name} to {new name}`
    * `modify column {name} {new data type}`
    * `modify column {name and existing data type}; after {other column name}`
    * `modify column {name and existing data type}; first`
    * `drop column {name}`
* `insert into {table name}`
    * `values ()`
    * `values (), (), ()`
* `insert into {table name} {table columns to fill up}`
    * `values ({table columns specified above})`
* `update {table name}`
    * `set {column name} = {updated value}`
    * `set {column name} = NULL`
    * `set {column name} = {updated value} where {conditional check}`
    * `set {column name} = {updated value}, {column name} = {updated value} where {conditional check}`
* `delete from {table name}`
* `delete from {table name} where {conditional check}`

#### Creating a table

```sql
create table employees (
    employee_id INT,
    first_name VARCHAR(50),
    last_name VARCHAR(50),
    hourly_pay DECIMAL(5, 2),
    hire_date DATE,
);
/* creates a table named `employees` with the following 5 columns and their values' data types */
```

#### Select a table

```sql
select * from employees
/* selects the entire table named `employees` and displays it */
```

#### Rename a table

```sql
rename table employees to workers;
/* renames the table `employees` to `workers` */
```

#### Drop a table

```sql
drop table employees;
/* drops the table `employees` */
```

#### Alter a table

```sql
alter table employees;
/* specifies table to alter as `employees`, required prelude for any of the following alter instructions */

add phone_number VARCHAR(15);
/* calls the add operation to add another column to the table */

rename column phone_number to email;
/* renames the specified column */

modify column email VARCHAR(100);
/* modifies the data type of a specific column */

modify column email VARCHAR(100)
after last_name;
/* moves the specified column to after the other specified column */

modify column email VARCHAR(100)
first;
/* moves the specified column to the leftmost first column in the table */

drop column email;
/* drops the specified column within a table */
```

#### Insert rows

```sql
insert into employees
values (1, "Eugene", "Krabs", 25.50, "2023-01-02");
/* inserts all required data, comma-separated, to create a row in the table */
/* date format is "YEAR-MONTH-DAY" */

insert into employees
values (2, "Squidward", "Tentacles", 15.00, "2023-01-03"),
       (3, "Spongebob", "Squarepants", 12.50, "2023-01-04"),
       (4, "Patrick", "Star", 12.50, "2023-01-05");
/* can add multiple entry rows at once with multiple parantheses, comma-separated */

insert into employees (employee_id, first_name, last_name)
values (6, "Sheldon", "Plankton");
/* allows us to add only specified fields to a table */
```

#### Select/Query data

```sql
select * from employees;
/* previously we used * to query and display all field values, displaying the entire table */

select first_name, last_name from employees;
/* select specific columns depending on what you're looking for */

select * from employees where employee_id = 1;
select * from employees where hourly_pay >= 15;
select * from employees where hire_date <= "2023-01-03";
select * from employees where employee_id != 1;
/* where sets up conditional check to find a field based on a specific condition */

select * from employees where employee_id IS NULL;
/* regarding an empty NULL value, we have to use the IS operator */

select * from employees where employee_id IS NOT NULL;
/* IS NOT is also a valid logical operator */
```

#### Update && Delete data 

```sql
update employees set hourly_pay = 10.25;
/* updates all field values in the column `hourly_pay` indiscriminately */

update employees set hourly_pay = 10.25 where employee_id = 6;
/* allows us to set and update a field value in the table */

update employees set hourly_pay = 10.50, hire_date = "2023-01-07" where employee_id = 6;
/* we can also update multiple fields at once, separated by a comma */

update employees set hire_date = NULL where employee_id = 6;
/* sets a field to NULL */

delete from employees;
/* deletes all entry rows in table indiscriminately */

delete from employees where employee_id = 6;
/* conditionally checks for the where operator condition before deleting the specified row */
```

---

## Autocommit, Default, Rollback

* Functions similarly to the Git *add*, *commit* and *push* commands.
* By default, **autocommit** is enabled, and any transactions are immediately saved. 
* **Autocommit** can be toggled off to enable manual saving using the `commit` keyword, allowing us to create save points we can easily return to later.

### KEYWORDS
* `set autocommit = off`
* `set autocommit = on`
* `commit`
* `rollback`

#### Toggling autocommit

```sql
set autocommit = off;
/* forces us to manually save each transaction with the COMMIT command to prevent rapid deletion of whole tables */
```

#### Commit

```sql
commit
/* creates a save point that we can then ROLLBACK to should we proceed with a dumb mistake */
```

#### Rollback

```sql
rollback;
/* restores your current transaction to the previous save point when we called commit */
```

---

## Constraints

Constraints are conditional statements **enforced** on a table's columns or fields, preventing certain types of data from being inputted.

### KEYWORDS
* `unique`
    * `{column name and data type} unique`
    * `alter table {table name} add constraint unique({column we want to add the unique constraint to})`
* `not NULL`
    * `{column name and data type} not NULL`
    * `alter table {table name} modify {column name and data type} not NULL`
* `check`
    * `constraint {check constraint name} check ({conditional check for the constraint})`
    * `alter table {table name} add constraint {check constraint name} check ({conditional check for the constraint})`
    * `alter table {table name} drop check {check constraint name}`
* `default`
    * `{column name and data type} default {deafult value}`
    * `alter table {table name} alter {column name} set default {default value}`

#### Unique

Applies the **unique** constraint on the specified column's values, ensuring there will be no repeats.

```sql
create table products (
    product_id INT,
    product_name VARCHAR(25) unique,
    price DECIMAL(4, 2)
);
/* ensures that the field `product_name` will only have unique values */

alter table products add constraint unique(product_id);
/* allows us to alter table fields after creation to apply the unique constraint */
```

#### Not NULL

Applies the **not NULL** constraint on the specified column's value, ensuring there is not empty NULL value in said field.

```sql
create table groceries (
    grocery_id INT,
    grocery_name VARCHAR(25),
    grocery_price DECIMAL(4,2) not NULL
);
/* ensures that the specified field, `grocery_price` will not have an empty NULL value */

alter table groceries modify grocery_price DECIMAL(4,2) not NULL;
/* allows us to alter table fields after creation to apply the not NULL constraint */
```

#### Check

**Check** constraint limits what values can be placed within a column.

```sql
create table employees (
    employee_id INT,
    first_name VARCHAR(50),
    last_name VARCHAR(50),
    hourly_pay DECIMAL(5, 2),
    hire_date DATE,
    constraint check_hourly_pay check (hourly_pay >= 10.00)
);
/* adds a check constraint to the table, which runs whenever a new value is added to the specified field to be checked */

alter table employees add constraint check_hourly_pay check (hourly_pay >= 10.00);
/* allows us to alter table fields after creation to apply the check constraint to the specified field */ 

alter table employees drop check check_hourly_pay;
/* drops the specified check constraint */
```

#### Default 

Should we insert a row without certain field values in a table, the **default** constraint fills those fields up with default values.

```sql
create table products (
    product_id INT,
    product_name VARCHAR(25) unique,
    price DECIMAL(4, 2) default 0.00
);
/* adds the default constraint to the created table */

alter table products alter price set default 0.00 
/* allows us to alter table fields after creation to apply the default constraint to the specified field */ 
```

---

## Other shit

#### KEYWORDS
* `current_date()`
* `current_time()`
* `now()`

#### Current date and time

> A bunch of default fuctions that return the current date and time.  

Basic arithmetic operations (`+`/`-`) can be performed on the data or time values to tweak the values as needed.

* `current_date()`
    * returns the current date in the specified MySQL format
* `current_time()`
    * returns the current time in the specified MySQL format
* `now()`
    * returns the current date and time in the specified MySQL format

---
