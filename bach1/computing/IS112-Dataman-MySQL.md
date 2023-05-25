> *Continue from [here](https://www.youtube.com/watch?v=5OdVJbNCSso) @ `1:58:40`.*  
> *MySQL download available [here](https://dev.mysql.com/downloads/).*

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
* `primary key`
    * `{column name and data type} primary key`
    * `alter table {table name} add constraint primary key({column we want to add the primary key constraint to})`
        * `{column name and data type} primary key auto_increment`
        * `alter table {table name} auto_increment = {value we want primary key column field to begin at}`
* `foreign key`
    * `foreign key({foreign key column name in home table}) references {other table name}({primary key column name in other table})`
    * `alter table {table name} drop foreign key {foreign key constraint name in MySQL GUI software}`
    * `alter table {table name} foreign key({foreign key column name in home table}) references {other table name}({primary key column name in other table})`
    * `alter table {table name} add constraint {desired foreign key constraint name} foreign key({foreign key column name in home table}) references {other table name}({primary key column name in other table})`
    * `join`
        * `select * from {table 1 name} inner join {table 2 name} on {table 1 name}.{table 1 shared column name} = {table 2 name}.{table 2 shared column name}`
        * `select * from {table 1 name} left join {table 2 name} on {table 1 name}.{table 1 shared column name} = {table 2 name}.{table 2 shared column name}`
        * `select * from {table 1 name} right join {table 2 name} on {table 1 name}.{table 1 shared column name} = {table 2 name}.{table 2 shared column name}`

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

#### Primary Key

* The *primary key* constraint ensures that the given column's values must hold values that are **unique** and **not NULL**.  
* A table can only have one *primary key* constraint.

```sql
create table transactions (
    transaction_id INT primary key,
    amount DECIMAL(5, 2)
);
/* adds the primary key constraint to the created table */

alter table transactions add constraint primary key(transaction_id);
/* allows us to alter table fields after creation to apply the primary key constraints to the specified column */ 
```

##### Auto_increment attribute

* *Auto_increment* is an attribute that can be applied to a column that is set as a key, and it **automatically increments** the value of the primary key column's fields each row.
* By default, *auto_increment* will start the primary key field value at 1.

```sql
create table transactions (
    transaction_id INT primary key auto_increment,
    amount DECIMAL(5, 2)
);
/* adds the auto_increment attribute to the primary key constraint when creating the table */

insert into transactions (amount) values (4.99);
/* note that we no longer need to indicate a transaction_id when adding entry rows to the table, the auto_increment attribute does the work for us */

alter table transactions auto_increment = 1000;
/* allows us to begin the primary key column field values at a different value other than 1 */
```

#### Foreign key

* A *foreign key constraint* is a **foreign key** within one table that can be found as a **primary key** inside another table, allowing us to establish a **link** between two tables.

```sql
create table customers (
    customer_id INT primary key auto_increment,
    first_name VARCHAR(50),
    last_name VARCHAR(50)
);
/* creating the `customers` table */

insert into customers (first_name, last_name)
values ("Fred", "Fish"),
       ("Larry", "Lobster"),
       ("Bubble", "Bass");
/* inserting entry row values into the `customers` table */

create table transactions (
    transaction_id INT primary key auto_increment,
    amount DECIMAL(5, 2),
    customer_id INT,
    foreign key(customer_id) references customers(customer_id)
);
/* the last line here uses the foreign key constraint to link the customer_id column in transactions table with the customer_id column in the customers table */

alter table transactions drop foreign key transaction_ibfk_1;
/* to drop a foreign key constraint, we enter MySQL GUI software, see under the specified table name, Foreign Keys/{foreign key names}, and enter that foreign key accordingly */

alter table transactions foreign key(customer_id) references customers(customer_id);
/* allows us to alter an existing table by adding a foreign key constraint that we don't want to manually name */

alter table transactions add constraint fk_customer_id foreign key(customer_id) references customers(customer_id)
/* allows us to rename our foreign key, or name our foreign key constraint when adding it to our table */
```

##### Joins

* A *join* is an operation we can apply on 2 tables that share a **related column** (like a *foreign key constraint*) to combine both tables into one.
* There are 3 kinds of *joins* we will cover:
    * `inner join`
        * join together any matching row from **both tables** based on a common shared column
    * `left join`
        * join together any matching row from **both tables** based on a common shared column, and display all shared rows AND any unmatched rows on the **left** table
    * `right join`
        * join together any matching row from **both tables** based on a common shared column, and display all shared rows AND any unmatched rows on the **right** table

```sql
select * from transactions inner join customers on transactions.customer_id = customers.customer_id;
/* creates an INNER JOIN based on the shared foreign key `customer_id` column, it excludes fields that are not shared in terms of value as well! */

select * from transactions left join customers on transactions.customer_id = customers.customer_id;
/* creates a LEFT JOIN based on the shared foreign key `customer_id` column, it excludes fields that are not shared BUT displays unmatched rows from the LEFT table*/

select * from transactions right join customers on transactions.customer_id = customers.customer_id;
/* creates a RIGHT JOIN based on the shared foreign key `customer_id` column, it excludes fields that are not shared BUT displays unmatched rows from the RIGHT table*/
```

---

## [Logical operators](https://www.w3schools.com/mysql/mysql_and_or.asp)

Functions similarly as in any other low and high-level programming language.

### KEYWORDS
* `where`
    * used to scope out **conditional checks** for a given field, row or column in MySQL
* `and`
* `or`
* `not`

---

## [Functions](https://www.w3schools.com/mysql/mysql_ref_functions.asp)

### KEYWORDS
* `as`
    * used to define **aliases** for column names in MySQL
* `count()`
* `max()`
* `min()`
* `avg()`
* `sum()`
* `concat()`

> See the [MySQL website](https://www.w3schools.com/mysql/mysql_ref_functions.asp) for other functions.

---

## Clauses

Clauses are keywords we use to **tweak the displayed data** after we have queried it.

### KEYWORDS
* `order by` 
    * `select {query selection} from {table name} order by {column name we want to order the entries by}`
    * `select {query selection} from {table name} order by {column name we want to order the entries by} desc`
* `limit`
    * `select {query selection} from {table name} limit {number of entry rows we want returned}`
    * `select {query selection} from {table name} limit {number of entry rows we want offset before displaying the next value}, {number of entry rows we want returned}`
* `union`
    * `select {query selection} from {table 1 name} union select {query selection} from {table 2 name}`
    * `select {query selection} from {table 1 name} union all select {query selection} from {table 2 name}`
* self join
    * `select {query selection} from {table 1 name} as {table 1 alias} inner join {table 2 name} as {table 2 alias} on {table 1 alias}.{shared column name} = {table 2 alias}.{shared column name}`
    * `select {query selection} from {table 1 name} as {table 1 alias} left join {table 2 name} as {table 2 alias} on {table 1 alias}.{shared column name} = {table 2 alias}.{shared column name}`
    * `select {query selection} from {table 1 name} as {table 1 alias} right join {table 2 name} as {table 2 alias} on {table 1 alias}.{shared column name} = {table 2 alias}.{shared column name}`

#### Order by

The *order by* clause sorts the results of a query in **ascending** or **descending** order.
* *Order by* orders the fields by default in **ascending order** (`ASC`).
* **Descending order** (`DESC`) has to be explicitely stated.

```sql
select * from employees order by last_name;
/* by default the fields will be ordered in ASCENDING order */

select * from employees order by last_name desc;
/* the `desc` will order the results in DESCENDING order */
```

#### Limit

The *limit clause* limits the number of **entry rows displayed** from a given number of records.

```sql
select * from customers limit 1;
/* this sets a limit clause of 1, displaying a single row of field value entries */

select * from customers limit 2, 1;
/* this sets an OFFSET of 2 before displaying the next one entry row, essentially displaying the THIRD row entry */
```

The following are *technically not clauses*, but...

#### Union operator

The *union operator* **combines the results** of two or more `select` statements.

```sql
select * from income union select * from expenses;
/* REMOVES all duplicates, creates a shared table that displays both the income and expenses data at one go */

select * from income union all select * from expenses;
/* INCLUDES all duplicates, creates a shared table that displays both the income and expenses data at one go */
```

#### Self join

* A *self join* **joins together** another copy of a table to itself, to allow for easier comparison of rows from the same table.
* There are 3 kinds of *self joins* we will cover:
    * `inner join`
        * join together any matching row from **both copies of the table** based on a common shared column
    * `left join`
        * join together any matching row from **both copies of the table** based on a common shared column, and display all shared rows AND all columns on the **left copy** of the table
    * `right join`
        * join together any matching row from **both tables of the table** based on a common shared column, and display all shared rows AND all columns on the **right copy** of the table

```sql
alter table customers add referral_id INT;
update customers set referral_id = 1 where customer_id = 2;
update customers set referral_id = 2 where customer_id = 3;
update customers set referral_id = 2 where customer_id = 4;

select * from customers as a inner join customers as b on a.referral_id = b.customer_id;
/* inner join functions similarly to the join discussed previously, except this common shared column is taken from a copy of the same table */

select customer_id, a.first_name, a.last_name, b.first_name, b.last_name from customers as a inner join customers as b on a.referral_id = b.customer_id;
/* we can similarly use the dot notation syntax to identify which table each field we want displayed in query selection refers to */

select * from customers as a left join customers as b on a.referral_id = b.customer_id;
/* left join functions similarly to the join discussed previously, except this common shared column is taken from a copy of the same table, and it displays all columns from the LEFT COPY of the table */

select * from customers as a right join customers as b on a.referral_id = b.customer_id;
/* right join functions similarly to the join discussed previously, except this common shared column is taken from a copy of the same table, and it displays all columns from the RIGHT COPY of the table *
```

---

## Other shit

### KEYWORDS
* `current_date()`
* `current_time()`
* `now()`
* `%`
* `_`
* View
    * `create view {view name} as select {column names from desired table, comma-separated} from {desired table name}`
    * `drop view {view name}`
* Index
    * `show indexes from {table name}`
    * `create index {index name} on {table name}({column name we want to create index from})`
    * `create index {multi-column index name} on {table name}({column names we want to create the multi-column index from, comma-separated})`
    * `alter table {table name} drop index {index / multi-column index name}`

#### Current date and time

> A bunch of default fuctions that return the current date and time.  

Basic arithmetic operations (`+`/`-`) can be performed on the data or time values to tweak the values as needed.

* `current_date()`
    * returns the current date in the specified MySQL format
* `current_time()`
    * returns the current time in the specified MySQL format
* `now()`
    * returns the current date and time in the specified MySQL format

#### Wild card characters

* *Wild card characters* are used to substitute **one or more characters** in a string.
    * `like` operator: replacs the *=* operator when comparing VARCHAR with a wild card operator
    * `%` wild card operator: **any number** of random characters
    * `_` wild card operator: **one** random character

```sql
select * from employees where job like "sp%";
/* this searches for words that start with a "sp" */

select * from employees where first_name like "_";
/* this searches for words that are just any singular character */ 

select * from employees where job like "_a%";
/* BOTH wild card operators can be combined */ 
/* this searches for words that have an 'a' as their second character, and have an undefined number of characters after the 'a', like "janitor" */

select * from employees where hire_date like "____-__-03";
/* wild card operators work on DATES as well */
```

#### Views

* *Views* are virtual tables **created from the result-set of SQL statements**, that are made up of fields and columns from actual tables in the database.
* *Views* are not real tables, but can be interacted with as if they were.

> One main benefit of using *views* is that the view's data **automatically updates** when the original table has data altered *(similar to a pointer and memory address)*.

```sql
create view employee_attendance as select first_name, last_name from employees;
/* creates a new view titled `employee_attendance` from the first_name last_name columns from the `employees` table */

select * from employee_attendance order by last_name ASC;
/* views can be interacted with as normal tables, so all previously-established SQL actions are valid */

drop view employee_attendance;
/* drops the view `employee_attendance` */
```

#### [Indexes](https://www.w3schools.com/sql/sql_create_index.asp)

* *Indexes* allow us to find field values within a column much quicker, similar to a **binary tree data structure**.
* Multi-column *indexes* are also possible to allow for rapid querying of field values across multiple columns.

```sql
show indexes from customers;
/* shows the current indexes for the table `customers` */

create index last_name_index on customers(last_name);
/* creates the index `last_name_index` on the table `customers` column `last_name` */

select * from customers where last_name = "Puff";
/* this search now runs much quicker due to the creation of the index `last_name_index` */

create index last_name_first_name_index on customers(last_name, first_name);
/* creates the mulit-column index `last_name_first_name_index` on the table `customers` column `last_name` and `first_name` */

alter table customers drop index last_name_first_name_index;
/* drops the previously created multi-column index */
```

---
