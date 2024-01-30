# `SQL`

SQL is a standardised language for interacting with relational database management systems (MySQL, Oracle, Sybase).

## Quickstart

```sql
-- ---------- QUICKSTART ---------
    -- note this quickstart covers the structure of databases, SQL syntax is covered further below

-- ----------- GENERIC DEFINITION ----------
    -- data => stored representation of bits, forming integers, floats, strings
    -- metadata => properties of data that give data contextual significance
    -- information => processed data that increases knowledge in users (this is mostly semantics so ignore this definition)
    -- database => structured collection of logically-related data 
    -- redundancy => good for cybersecurity, but bad in databases where repetition might render a database inconsistent if one field has to be manually altered multiple times to reflect a single change

-- ---------- DATABASE ENVIRONMENT -----------
    -- database => storehouse for data
    -- repository => storehouse for metadata
    -- database management system (DBMS) => software to manage a database
    -- application programs => apps that use data from the database
    -- user interface => UI for end users
    -- end users => application and database users
    -- system developers => app developers
    -- data administrators => database engineers

-- ---------- DATABASE LIFECYCLE ----------
    -- 1. PLANNING
        -- business provides business rules for database engineers
            -- business rule => atomic statements that specify constraints on the way the business should work
    -- 2. ANALYSIS
        -- database engineers carry out data modelling
    -- 3. DATABASE DESIGN 
    -- 4. DATABASE IMPLEMENTATION
    -- 5. DATABASE MAINTENANCE

-- ---------- E-R DIAGRAM NOTATION ---------
    -- STANDARD E-R DIAGRAM NOTATION
        -- CANNOT copy identifiers around
        -- 1. entity symbols
            -- entity => building block of E-R diagram used to model a basic unit in an organisation
                -- entity type => class and its class attributes
                    -- strong entity => entity type that exists idependently of other entity types, has its own unique identifier and is represented with a single-lined rectangle
                    -- weak entity => entity type that cannot form a unique identifier by its own attributes alone, and is identified by a combination of its attributes and an identifier from another entity (owner of the weak entity), represented with a double-lined rectangle
                    -- weak entities and their owners are connected by identifying relationships, a double line
                -- entity instance => instance object and its instance attributes
                -- associative entity => special name assigned for relationship attributes that we convert into an entity (to prevent overly complex cardinality rules), represented with a rounded-corner rectangle
                    -- ternary relationships must ALWAYS be modelled as an associative entity
                    -- ternary relationships are to be read from the origin entity type to the next directly-connected entity type via cardinality, then looks beyond to other connected entity types and examines their cardinality
                    -- a unary or binary relationship can be modelled as an associative entity if it is many-to-many and has AT LEAST ONE attribute
                    -- associative entities can have other independent relationships, but an associative entity always has a one-to-one going into the other connected entity types and has a zero-to-many or one-to-many going into the associated entity
        -- 2. attribute symbols
            -- attribute => property of an entity type
                -- simple => basic attribute, just list each attribute within the entity type
                -- composite => an attribute comprised of multiple simple attributes, declared within () brackets and comma-delimited, an example being an address being comprised of street address, city, state and postal code as its composite attributes, and note that we can have a multi-valued attribute that consists of composite attributes
                -- multi-valued => an attribute that in memory, is represented as a list that contains one or more possible values for the attribute, declared within {} curly braces
                -- derived => attributes that don't need to be explicitly assigned and can be calculated based on other assigned values within the database, declared within [] square brackets, and example being able to compute number of years employed for an employee since we have their year of employment recorded as a stored attribute
                -- identifier => attribute that uniquely distinguishes an entity instance from all other entity instances, only one default identifier can be chosen and the identifier cannot be null or a mutable value, declared by being underlined, an example being a studentID
                    -- complete simple identifier => complete simple attribute assigned as an identifier, declared by being underlined once, can be part of a strong entity
                    -- partial identifier => double underlined, can be part of a weak entity
                    -- composite identifier => either a composite attribute that is assigned as an identifier underlined directly, or multiple simple attributes forming one composite identifier then having each composite attribute that comprise the composite identifier being underlined
        -- 3. relationship symbols
            -- relationship type => relationship line between entity types
            -- relationship instance => relationship lines between entity instances
            -- relationship attributes => field attributes applied on a relationship instance, specified in a box connected to the relationship line with a dashed line
            -- relationship degree => number of entities participating in a relationship, don't just assume the degree when you see an associative entity, check the individual cardinality of each connected entity
                -- unary relationship => 1 entity related to an entity of the SAME entity type
                -- binary relationship => 2 different entity types related to each other
                -- ternary relationship => 3 different entity types related to each other
                -- more than ternary 
            -- relationship cardinality => number of instances one entity that can or must be associated with each instance of another entity 
                -- one-to-one => each entity in a relationship has ONE related entity
                -- one-to-many => an entity on ONE side has MANY related entities, but an entity on the other side has a maximum of ONE related entity
                -- many-to-many => entities on BOTH sides of the relationship have MANY related entities on the other side
                -- cardinality constraints => number of instances one entity must be associated with each instance of another entity, and in the E-R diagram, cardinality symbols are represented with O for 0, | for 1 and > < for many with the minimum cardinality specified on the left and maximum cardinality specified on the right
                    -- cardinality is read with regard to the target entity type
                    -- minimum cardinality => min number of related entity instances, if zero is optional, one or more mandatory
                    -- maximum cardinality => max number of related entity instances
    -- ENHANCED E-R DIAGRAM NOTATION
        -- supertype and subtype
            -- relationship between supertype and its subtypes specified with a circle at the intersection between lines connecting the supertype and its subtypes
            -- no cardinality symbols to specify relationship between the supertype and subtype since cardinality symbols specify verb-interaction relationships between two distinct entity types, and subtypes inherit the existing relationships of their supertypes but don't share an explicit verb-interaction relationship with their supertype, alongside being able to define their own respective verb-interaction relationships
            -- supertype => parent class entity type that contains generic shared attributes
            -- subtype => child class entity type that inherits the shared attributes (and their values) and relationships of the supertype, while also specifying any attributes and relationships unique to the subtype 
                -- note that each instance of a subtype is also an instance of its supertype by nature of inheritance
                -- never draw a subtype that has no unique attributes, those will be categorised under the supertype implicitly
        -- generalization and specialization
            -- generalization => process of defining a more general entity type from a set of more specific entity types by forming subtype-supertype relationships BOTTOM-UP
            -- specialization => process of defining one or more subtypes off of a given supertype by forming supertype-subtype relationships TOP-DOWN
        -- constraints in supertype
            -- 1. completeness constraint => whether an instance of a supertype MUST also be a member of at least one of the specified subtypes
                -- total specialization rule => yes, represented with double line, where the specified subtypes are the only possible variations of the supertype
                -- partial specialization rule => no, represented with single line, where the specified subtypes are not the only possible variations of the supertype and the supertype has other unspecified subtypes including within the supertype itself
            -- 2. disjointness constraint => whether an instance of a supertype CAN simultaneously be a member of TWO or MORE subtypes
                -- disjoint rule => an instance of a supertype can be only ONE of the subtypes at once, represented by a 'd' within the aforementioned circle specifying the relationship between supertypes and their subtypes
                -- overlap rule => an instance of the supertype could be more than one of the subtypes at once, represented by a 'o' within the aforementioned circle specifying the relationship between supertypes and their subtypes
            -- 3. subtype discriminators constraint => attribute of the supertype whose value determines the target subtype
                -- disjoint rule => simple attribute with alternate values to indicate possible subtypes, where the simple attribute and the = assignment operator is specified on the line running from the supertype to the aforementioned circle AND the possible attribute values are specified on the lines running from the aforementioned circle to the subtypes
                    -- allows for mutually exclusive subtypes only
                -- overlap rule => composite attribute whose subparts pertain to different subtypes, each subpart contains a Boolean value that indicates whether the instance belongs to the associated subtype, where the composite attribute and the : colon operator is specified on the line running from the supertype to the aforementioned circle AND the possible subpart values are specified on the lines running from the aforementioned circle to the subtypes
                    -- allows for overlap in the specified composite attributes, creating many subtypes that are not neccesarily mutually exclusive

-- ---------- RELATION ---------
    -- Relation => a generic table that corresponds to an entity type and with a many-to-many relationship types
    -- Table ROWS => correspond to entity instance OR any instance with a many-to-many relationship instances
    -- Table COLUMNS => correspond to attribute
    -- Primary Key => attribute or combination of attributes of unique value that uniquely identifies a ROW in a relation, cannot be NULL, usually the identifier within the context of E-R diagrams, specified by underlining it 
        -- Simple primary key => single field, unique identifier
        -- Composite primary key => multiple fields, composite identifier
        -- Each relation MUST HAVE ONE primary key, it is used as index to speed up user querying
    -- Relational database => consists of any number of relations
    -- Schema => description of database structure, textually represented by a capitalised uppercase word and its comma-delimited components within brackets
    -- Foreign Key => attribute in a relation that is primary key to another relation
    -- Integrity constraints
        -- Referential Integrity => any foreign key value MUST either match a primary key value in the relation it shares a relationship with or be NULL
            -- represented with an arrow, wherein the arrow direction flows FROM the dependant child relation table with the FOREIGN KEY to the related parent relation table with the PRIMARY KEY
            -- impacts data deletion based on the following specifications 
                -- restrict: as long as a relation table row contains a primary or foreign key linked with another relation table row, it cannot be deleted 
                -- cascade: deletion of a relation table row that contains a foreign or primary key will result in any linked rows being deleted as well
                -- set-to-null: related affected relation table row will have its foreign key value set to NULL
        -- Domain constraints 
            -- permitted data types for a given field
        -- Entity integrity
            -- No primary key attribute may be NULL and all primary key fields must have data 
    -- EER Diagrams into Relations
        -- when determining cardinalities in relations, we are reflecting the MAX CARDINALITY of a given relationship in ER-diagram land
        -- 1. Map REGULAR entity types to relations
            -- SIMPLE attributes map directly onto columns of a relation table
            -- COMPOSTIE attributes ONLY have their simple component attributes mapped as columns of the relation table
            -- each MULTI-VALUED attribute becomes a separate relation with a foreign key taken from its related parent entity and both its identifying primary key and the multi-valued attribute is underlined
                -- where a composite attribute is also multi-valued, we must actively determine whether we need to underline all its composite attributes to distinguish each entity instance of a multivalued composite attribute
            -- DERIVED attributes are NOT stored and so do not need to be displayed within the relations diagram
        -- 2. Map WEAK entities
            -- WEAK ENTITY becomes a separate relation with a foreign key taken from the strong entity, and added as an additional column within the relation table
            -- the PRIMARY KEY is comprised of the partial identifier of weak entity AND primary key of the strong entity
        -- 3. Map BINARY relationships
            -- keep in mind that based on the specific business rules, there might be a need to underline additional fields to distinguish a relation instance from another instance
            -- one-to-many => PRIMARY KEY on one side becomes FOREIGN KEY on many side
            -- many-to-many => create a NEW RELATION with PRIMARY KEYS of two entities as its primary key
            -- one-to-one => PRIMARY key on mandatory one side becomes FOREIGN key on the optional one side, but if both sides are of exact same cardinality then the arrow direction does not matter
        -- 4. Map ASSOCIATIVE ENTITIES
            -- identifier NOT ASSIGNED => default primary keys for association relation composed of PRIMARY KEYS of the two entities
            -- identifier ASSIGNED => assigned identifier becomes the PRIMARY KEY of the relation table
        -- 5. Map UNARY relationships
            -- ONE-to-ONE or ONE-to-MANY relationships are mapped as a recursive foreign key within the same one relation table
            -- MANY-to-MANY are mapped as two relations, with one as the entity type and one for the associative relation in which the PRIMARY KEY has two attributes both taken from the primary key of the entity
        -- 6. Map TERNARY and more relationships
            -- one relation for each entity
            -- one relation for the assoicative entity
                -- associative entity has FOREIGN KEYS to each entity type in the relationship
        -- 7. Map SUPERTYPE and SUBTYPE relationships
            -- one relation for each supertype
                -- supertype attributes including unique identifier and subtype discriminators go into supertype relation
            -- one relation for each subtype
                -- subtype attributes go into each subtype
            -- primary key of supertype relation table ALSO becomes primary key of subtype relation table
            -- one to one relationship between supertype and each subtype where supertype is the PRIMARY relation table
```

## Comments

```sql
-- --------- COMMENT ----------

-- single-line comments

-- there is no special syntax for multi-line comments
```

## Commands

* semi-colon language
* keywords are not case-sensitive, but are capitalised by convention
* database, table and column names are case-sensitive
* a database is a collection of tables

```sql
-- ---------- COMMAND ----------
    -- database commands
    -- table commands

-- ---------- DATABASE ----------
    -- CREATE DATABASE {database name} => creates specified database
    -- DROP DATABASE {database name} => deletes specified database
    -- SHOW DATABASES => lists all available databases
    -- USE {database name} => choose an existing database

CREATE DATABASE parklaneCaiFan;
DROP DATABASE parklaneCaiFan;
SHOW DATABASES;
USE parklaneCaiFan;

-- ---------- TABLE ----------
    -- CREATE TABLE {table name} ({column name(s)}) => creates specified table with specified columns
    -- INSERT INTO {table name} VALUES({row value(s)}) => creates an entry in a specified table
    -- UPDATE {table name} SET {value to be updated} WHERE {predicate to enforce value to be updated} => updates an entry in a specified table
    -- DELETE FROM {table name} => deletes rows from the specified table
    -- DROP TABLE {table name} => deletes the specified table
    -- SELECT {column name(s)} FROM {table name} {augmentation(s)} => general syntax to specify which row or column to select from in a table

CREATE TABLE employees (first_name VARCHAR(20), last_name VARCHAR(20), employee_name VARCHAR(20), employee_num BIGINT); -- creates a table called employees that has the column names of first_name, last_name, employee_name and employee_num and their specified data types
INSERT INTO employees VALUES('John', 'Piper', 'John Piper', 101); -- inserts a row of data entry into the employees tablee, and assumes the values and table have been correctly assigned
UPDATE employees SET first_name='Watermelon' WHERE last_name='Piper' -- updates a first_name value in a row to 'Watermelon' where the last_name value was 'Piper' in the employees table
DELETE FROM employees WHERE last_name LIKE 'P%'; -- deletes rows from the employees table where the last_name begins with 'P', deleting the 'Watermelon Piper' row data entry
DELETE FROM employees; -- deletes all rows from employees table, leaving an empty table
DROP TABLE employeesl -- deletes the entire employes table

-- AUGMENTATION
    -- * => wildcard all operator
    -- LIMIT => limits selection
    -- WHERE => specifies a predicate
    -- LIKE => similar to a partial equality check operator
    -- % => wildcard string operator to represent any string of characters with indeterminate count
        -- 'abc%' specifies a field that starts with 'abc'
        -- '%abc' specifies a field that ends with 'abc'
        -- '%abc%' specifies a field that contains the substring 'abc' in the middle of it
    -- _ => catch-all operator to represent any character with a specific count
    -- DISTINCT => show unique entries only with no duplicates
    -- ORDER BY => sorts values by their title (case-sensitive)
    -- COUNT() => returns the count of whatever field specified within brackets
    -- JOIN => combines rows from two or more tables based on a related column between them
        -- INNER JOIN => selects only matched rows in both tables
        -- LEFT JOIN => selects all rows from left table and matched rows from right table
        -- RIGHT JOIN => selects all rows from right table and matched rows from left table
        -- FULL JOIN => selects all rows from left or right table as long as there is a match in either
    -- ON => specifies a predicate for the JOIN clause

SELECT * FROM employees; -- select all rows and columns from parklaneCaiFan database's employees table
SELECT working_hrs, employee_name FROM employees; -- select only the working_hrs and employee_name column from employees table

SELECT * FROM employees LIMIT 5; -- selects all columns from employees table but only the first 5 rows

SELECT working_hrs FROM employees WHERE employee_name LIKE '%auntie%' -- selects all working_hrs column values from employees table where the employee_name contains the substring 'auntie'
SELECT * FROM employees WHERE employee_name LIKE 'S____'; -- selects all columns values from employees table where the employee_name starts with an S and has exactly 4 characters after it

SELECT DISTINCT working_hrs FROM employees; -- selects working_hr values from the employees table but doesn't show duplicates
SELECT DISTINCT working_hrs FROM employees ORDER BY employee_name; -- selects working_hr values from the employees table but doesn't show duplicates, then sorts them  by employee name case-sensitive

SELECT COUNT(*) FROM employees; -- selects and shows the number of rows in the employees table
SELECT COUNT(*) FROM employees WHERE employee_name LIKE '%auntie%'; -- similar to above, selects and shows the number of rows in the employees table with 'auntie' as a substring of the employee_name value

SELECT employees.first_name, employees.last_name FROM employees INNER JOIN payslip ON employees.employee_num = payslip.employee_num LIMIT 10; -- inner join the employees table column values of first name and last name when the payslip table's employee_num value is the same as employees table employee_num value, limited to 10 rows

SELECT * FROM INFORMATION_SCHEMAD.TABLES WHERE TABLE_TYPE='BASE TABLE'; -- list all tables in a database, though different implementations provide their own shortcut command to achieve this
```

## More on

* [modern database management textbook](https://library.lol/main/1E408DAED98F12A01C57A47CE5E28396)
* [mysql documentation](https://dev.mysql.com/doc/)
* [different sql implementations](https://troels.arvin.dk/db/rdbms/)
* [learn sql in y minutes](https://learnxinyminutes.com/docs/sql/)
