## 内表的基本介绍
ABAP (Advanced Business Application Programming) 是SAP（System, Applications, and Products in Data Processing）系统的主要编程语言。<br />在ABAP中，内表（Internal Tables）是用于存储和处理数据的一种重要数据结构。
### ABAP内表的基本概念
内表可以理解为一个二维的数据结构，它由行和列组成，每一行都可以被视为一个结构（Structure），而每一列则是这个结构中的一个字段。内表可以用来存储多个相同类型的记录，这使得它非常适合于处理大量数据的场景。
### ABAP内表的特点

1. **灵活性**：与静态数组相比，内表提供了更多的灵活性，因为它的大小可以在运行时动态改变。这意味着你可以在程序执行过程中根据需要添加或删除行。
2. **索引和排序**：内表支持索引和排序操作，这使得查找特定的记录变得非常高效。你可以创建一个主键来唯一标识每条记录，也可以对内表进行排序以满足特定的业务需求。
3. **数据访问**：ABAP提供了丰富的函数和语句来访问和操作内表中的数据，如LOOP AT、READ TABLE、SORT等，这些使数据处理更加方便。
4. **内存管理**：内表存储在内存中，因此对于需要快速访问和处理大量数据的应用来说，它是一个理想的选择。然而，这也意味着内表的使用可能会受到系统可用内存的限制。
5. **类型**：ABAP内表有三种主要类型：标准内表（Standard）、排序内表（Sorted）和哈希内表（Hashed）。标准内表是最基本的类型，排序内表和哈希内表分别在排序和查找操作上提供了优化。
6. **多维性**：虽然基本的内表是一维的，但通过嵌套结构，可以实现多维数据的存储和处理。
## 行类型
在ABAP中，行类型（Line Type）定义了内表每一行的数据结构。行类型可以是结构、数据库表、CDS视图或基本数据类型。以下是几种定义行类型的方法：
#### 1. 使用本地定义的结构类型
```abap
TYPES: BEGIN OF ls_loc,
         key_field TYPE i,
         char1     TYPE c LENGTH 10,
         char2     TYPE c LENGTH 10,
         num1      TYPE i,
         num2      TYPE i,
       END OF ls_loc.

" 定义一个标准内表类型，行类型为本地定义的结构类型
TYPES tt_loc_str TYPE TABLE OF ls_loc WITH NON-UNIQUE KEY key_field.
```

1. **2. 使用全局结构类型**
```abap
" 假设在DDIC中有一个全局结构类型 zsome_global_struc_type
TYPES tt_gl_str TYPE TABLE OF zsome_global_struc_type WITH NON-UNIQUE KEY key_field.
```
#### 3. 使用数据库表
```abap
" 使用数据库表 zdemo_abap_fli 作为行类型
TYPES tt_gl_tab TYPE TABLE OF zdemo_abap_fli WITH NON-UNIQUE KEY carrid.
```
#### 4. 使用基本数据类型
```abap
" 定义一个行类型为基本整数类型的内表
TYPES tt_el_type TYPE TABLE OF i.
```
### 示例：创建内表并使用行类型
```abap
" 使用本地定义的表类型创建内表
DATA: itab_a1 TYPE tt_loc_str,
      itab_a2 TYPE tt_gl_str,
      itab_a3 TYPE tt_gl_tab,
      itab_a4 TYPE tt_el_type.

" 使用全局定义的表类型创建内表
DATA itab_a5 TYPE string_table.

" 结合数据对象和表类型定义创建内表
DATA itab_a6 TYPE TABLE OF ls_loc WITH NON-UNIQUE KEY key_field.

" 使用 LIKE 关键字基于已存在的内表创建内表
DATA itab_a7 LIKE TABLE OF itab_a6.
```
### 示例：使用内联声明创建内表
```abap
" 在赋值的上下文中内联声明内表
DATA(it_inline1) = itab_a1.
DATA(it_inline2) = it_inline1.

" 使用 FINAL 创建不可变变量
FINAL(it_final) = it_inline1.

" 使用 VALUE 操作符和内表类型创建内表
DATA(it_inline3) = VALUE tt_loc_str( ( key_field = 1 char1 = 'A' char2 = 'B' num1 = 10 num2 = 20 ) ).

" 未提供任何表行意味着表是初始的
DATA(it_inline4) = VALUE tt_loc_str( ).

" 在选择语句中内联声明内表
SELECT * FROM zdemo_abap_fli INTO TABLE @DATA(it_inline5).
```
通过定义行类型，可以灵活地管理内表的数据结构，并根据需求创建和操作内表。
## 表类别（Table Category）
在ABAP中，表类别决定了内表的管理和存储方式，以及如何访问特定的表条目。主要的表类别包括标准表、排序表和哈希表。每种表类别在性能和用法上都有不同的特点。
#### 1. 标准表（Standard Table）

- **特点**:
   - 行在内表中按顺序存储。
   - 行是按插入顺序排列的。
   - 每次访问的时间复杂度为O(n)，即线性时间，适合在小型表中使用。
   - 不能为标准表提供唯一键约束；所有的键都是非唯一的。
- **使用场景**:
   - 当你需要顺序访问表的内容，或表的条目较少时。
   - 对于插入、删除操作频繁且数据量不大的情况。
- **示例**:
```abap
DATA: lt_standard TYPE TABLE OF string WITH EMPTY KEY.
```
#### 2. 排序表（Sorted Table）

- **特点**:
   - 内表的行按排序顺序存储。
   - 支持按键进行排序访问。
   - 可以指定唯一或非唯一的键。
   - 支持自动排序，当你插入新行时，它会按照指定的排序规则插入。
- **使用场景**:
   - 当你需要按特定顺序访问表内容时。
   - 对于需要频繁进行键访问或查询的情况。
- **示例**:
```abap
DATA: lt_sorted TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
```
#### 3. 哈希表（Hashed Table）

- **特点**:
   - 使用哈希算法来管理和存储行。
   - 支持通过键进行快速访问，时间复杂度为O(1)。
   - 内表的行不是按任何顺序存储的。
   - 每个键必须是唯一的。
- **使用场景**:
   - 当你需要通过键快速访问数据，并且行的顺序不重要时。
   - 对于需要高效查找和检索的情况。
- **示例**:
```abap
DATA: lt_hashed TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
```
### 访问方式

- **通过表索引访问**:
   - 通过行号直接访问内表的行（仅适用于标准表）。这种方式最快，但只能在标准表中使用。
- **通过表键访问**:
   - 通过在特定列中查找值来访问行。适用于排序表和哈希表。需要指定键来进行检索。
### 选择合适的表类别

- **标准表**:
   - 当数据量较小且主要按插入顺序进行访问时使用。
- **排序表**:
   - 当数据需要按照某种排序顺序进行访问或查询时使用。
- **哈希表**:
   - 当数据量较大且需要快速通过键进行查找时使用。

选择合适的表类别可以大大提高程序的性能和效率，特别是在处理大量数据时。
## 键属性
 在ABAP中，键属性用于定义内表的访问方式。键可以帮助优化内表的存储和访问方式。主要的键属性包括主键、次键 。<br />下面是对这些键属性的详细说明：
### 1. 主键（Primary Key）

- **定义**:
   - 主键是内表的主要标识，用于唯一标识每一行。
   - 每个内表都有一个主键。
   - 主键可以是自定义的键或标准键。
- **特点**:
   - 在排序表和哈希表中，主键是排序和查找的基础。
   - 在标准表中，主键是非唯一的，主要用于数据的访问和管理。
   - 主键字段在排序表和哈希表中是只读的，即不能修改。
- **定义示例**:
```abap
DATA itab TYPE TABLE OF zsome_structure WITH UNIQUE KEY key_field.
```
### 2. 次键（Secondary Key）

- **定义**:
   - 次键是附加的键，用于加速对内表的查询和访问。
   - 可以定义一个或多个次键。
   - 次键可以是唯一的或非唯一的。
- **特点**:
   - 次键的创建会增加额外的内存消耗，但可以显著提高查询性能。
   - 适用于排序表和哈希表。
   - 次键可以使用别名来增加代码的可读性。
- **定义示例**:
```abap
DATA itab TYPE SORTED TABLE OF zsome_structure
    WITH UNIQUE KEY key1
    WITH NON-UNIQUE SORTED KEY key2 COMPONENTS field1 field2.
```
### 键属性的使用场景

- **主键**: 用于唯一标识内表中的每一行。适用于所有表类别，但在排序表和哈希表中用于确定排序和查找方式。
- **次键**: 用于提高查询性能。适用于排序表和哈希表，通过增加额外的索引来加速特定列的查找。

选择合适的键属性可以显著提升内表的性能，特别是在处理大量数据时。确保键属性与内表的使用场景相匹配，以实现最优的性能和效率。
## 表键类型
在ABAP中，内表的键用于优化数据存取，主要有以下几种类型：
### 1. 排序键（Sorted Keys）

- **定义**:
   - 排序键用于排序表（Sorted Tables），可以是主键或次键。
   - 排序键用于维持表中数据的排序顺序。
- **特点**:
   - 排序键支持唯一和非唯一两种类型。
   - 在插入数据时，系统会自动根据排序键的定义将数据插入到正确的位置。
- **示例**:
```abap
DATA itab TYPE SORTED TABLE OF zsome_structure
    WITH UNIQUE KEY key1
    WITH NON-UNIQUE SORTED KEY key2 COMPONENTS field1 field2.
```
### 2. 哈希键（Hashed Keys）

- **定义**:
   - 哈希键用于哈希表（Hashed Tables），主要是主键。
   - 哈希键通过哈希算法来快速定位数据。
- **特点**:
   - 哈希键必须是唯一的。
   - 数据的插入和查找都依赖于哈希算法，因此查找操作速度很快。
- **示例**:
```abap
DATA itab TYPE HASHED TABLE OF zsome_structure
    WITH UNIQUE KEY key1.
```
### 3. 标准键（Standard Keys）

- **定义**:
   - 标准键是默认的主键类型，适用于标准表（Standard Tables）。
   - 标准键通常由所有非数字字段组成。
- **特点**:
   - 在标准表中，所有非数字字段自动成为标准键的一部分。
   - 标准键在性能方面不如排序键和哈希键，特别是在处理大量数据时。
- **示例**:
```abap
DATA itab TYPE TABLE OF zsome_structure WITH DEFAULT KEY.
```
### 4. 空键（Empty Keys）

- **定义**:
   - 空键是指没有指定任何键字段的主键。
   - 仅适用于标准表。
- **特点**:
   - 使用空键表示内表没有主键。
   - 当内表需要初始化时，可以使用空键，所有行均未指定特定的键标识。
- **示例**:
```abap
DATA itab TYPE TABLE OF zsome_structure WITH EMPTY KEY.
```
### 键类型的比较
| 键类型 | 适用表类型 | 唯一性 | 数据排序 | 查找效率 | 主要特点 |
| --- | --- | --- | --- | --- | --- |
| **排序键** | 排序表（Sorted Tables） | 唯一或非唯一 | 自动排序 | 高 | 根据键的定义自动排序，适合对数据进行排序和查询。 |
| **哈希键** | 哈希表（Hashed Tables） | 必须唯一 | 无排序 | 极高 | 使用哈希算法快速查找，必须唯一，不能重复。 |
| **标准键** | 标准表（Standard Tables） | 可重复 | 无排序 | 较低 | 默认主键类型，所有非数字字段组成键，适合简单场景。 |
| **空键** | 标准表（Standard Tables） | 无键字段 | 无排序 | 无效 | 主键为空，仅用于初始化或特定场景，所有行未指定键。 |

### 详细说明

- **排序键（Sorted Keys）**
   - **适用表类型**: 排序表（Sorted Tables）
   - **唯一性**: 可以是唯一的（UNIQUE）或非唯一的（NON-UNIQUE）
   - **数据排序**: 系统自动维护数据的排序顺序
   - **查找效率**: 高（基于排序顺序）
   - **主要特点**: 适合需要排序和高效查找的场景。数据插入时会自动排序。
- **哈希键（Hashed Keys）**
   - **适用表类型**: 哈希表（Hashed Tables）
   - **唯一性**: 必须唯一
   - **数据排序**: 无排序
   - **查找效率**: 极高（基于哈希算法）
   - **主要特点**: 通过哈希算法进行快速查找，适合需要高效查找的数据处理。
- **标准键（Standard Keys）**
   - **适用表类型**: 标准表（Standard Tables）
   - **唯一性**: 可以重复
   - **数据排序**: 无排序
   - **查找效率**: 较低
   - **主要特点**: 默认键类型，所有非数字字段组成键。适合简单的数据处理场景。
- **空键（Empty Keys）**
   - **适用表类型**: 标准表（Standard Tables）
   - **唯一性**: 无键字段
   - **数据排序**: 无排序
   - **查找效率**: 无效
   - **主要特点**: 主要用于初始化时的空表，没有指定主键字段。

 选择合适的键类型可以显著提高数据处理的效率。排序键适合需要排序的数据处理，哈希键适合快速查找，标准键适合简单的数据管理，而空键用于不需要键的场景。  
