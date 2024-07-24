## 检查内表中行的存在性
当你不需要获取表行的内容，只是想知道是否存在匹配指定索引或键的行时，可以使用 `READ TABLE` 语句和 `TRANSPORTING NO FIELDS` 附加项。该附加项表示不实际读取内容。如果搜索成功并且存在条目，系统字段 `sy-subrc` 将被设置为 0。<br />另一种较新的检查行存在性的方法是使用函数 `line_exists()`。该函数支持一个内表表达式作为参数。不过，表表达式不会设置系统字段。
#### 通过键读取
```abap
READ TABLE it WITH KEY b = 2 TRANSPORTING NO FIELDS.

IF sy-subrc = 0.
  " 行存在，执行相应的逻辑
ENDIF.
```
#### 通过索引读取
```abap
READ TABLE it INDEX 1 TRANSPORTING NO FIELDS.

IF sy-subrc = 0.
  " 行存在，执行相应的逻辑
ENDIF.
```
#### 使用函数 `line_exists()` 通过键检查
```abap
IF line_exists( it[ b = 2 ] ).
  " 行存在，执行相应的逻辑
ENDIF.
```
#### 使用谓词函数 `line_exists()` 通过索引检查
```abap
IF line_exists( it[ 1 ] ).
  " 行存在，执行相应的逻辑
ENDIF.
```
### 代码示例
下面是一个完整的代码示例，展示了如何使用这两种方法检查内表中的行是否存在：
```abap
DATA: itab TYPE TABLE OF i WITH UNIQUE KEY table_line,
      wa TYPE i.

" 填充内表
itab = VALUE #( ( 1 ) ( 2 ) ( 3 ) ).

" 通过键读取
DATA: itab TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line,
IF sy-subrc = 0.
  WRITE: / '行存在，键 = 2'.
ELSE.
  WRITE: / '行不存在，键 = 2'.
ENDIF.

" 通过索引读取
READ TABLE itab INDEX 1 TRANSPORTING NO FIELDS.
IF sy-subrc = 0.
  WRITE: / '行存在，索引 = 1'.
ELSE.
  WRITE: / '行不存在，索引 = 1'.
ENDIF.

" 使用 line_exists() 通过键检查
IF line_exists( itab[ table_line = 2 ] ).
  WRITE: / '行存在，键 = 2 (使用 line_exists)'.
ELSE.
  WRITE: / '行不存在，键 = 2 (使用 line_exists)'.
ENDIF.

" 使用 line_exists() 通过索引检查
IF line_exists( itab[ 1 ] ).
  WRITE: / '行存在，索引 = 1 (使用 line_exists)'.
ELSE.
  WRITE: / '行不存在，索引 = 1 (使用 line_exists)'.
ENDIF.
```
通过这些示例，可以看到如何检查内表中的行是否存在，无论是通过键还是通过索引。
### 示例输出
![image.png](https://cdn.nlark.com/yuque/0/2024/png/488204/1721361354184-db4a9055-71f1-4ccf-9a45-ee9dba8acd01.png#averageHue=%23f9f9f9&clientId=u37fadd14-1ff4-4&from=paste&height=172&id=uc61866d5&originHeight=215&originWidth=697&originalType=binary&ratio=1.25&rotation=0&showTitle=false&size=11280&status=done&style=none&taskId=u8e47fb12-2523-4b90-8245-7b747555325&title=&width=557.6)
## 检查内表中行的索引
如果你想了解内表中某行的索引，可以使用 `READ TABLE` 语句。如果找到该行，系统字段 `sy-tabix` 将被设置为索引号。另外，你还可以使用内置函数 `line_index()`。该函数返回找到行的索引，如果行不存在则返回 0。
### 示例代码
```abap
DATA: itab TYPE TABLE OF string WITH EMPTY KEY,
      tabix TYPE i,
      idx TYPE i.

" 填充内表
itab = VALUE #( ( `aaa` ) ( `bbb` ) ).

" 使用 READ TABLE 语句获取行的索引
READ TABLE itab WITH KEY table_line = `bbb` TRANSPORTING NO FIELDS.
tabix = sy-tabix. " 结果：2

" 使用 line_index() 获取行的索引
idx = line_index( itab[ table_line = `aaa` ] ). " 结果：1
```
### 详细解释

1. **通过 **`READ TABLE`** 语句获取索引**：
   - `READ TABLE itab WITH KEY table_line = 'bbb' TRANSPORTING NO FIELDS.` 这行代码试图在 `itab` 中查找键为 `bbb` 的行。
   - 如果找到该行，系统字段 `sy-tabix` 将被设置为该行的索引。
   - `tabix = sy-tabix.` 将 `sy-tabix` 的值赋给变量 `tabix`，结果是 2，因为 `bbb` 是 `itab` 中的第二行。
2. **通过 **`line_index()`** 函数获取索引**：
   - `idx = line_index( itab[ table_line = 'aaa' ] ).` 这行代码使用 `line_index()` 函数查找键为 `aaa` 的行的索引。
   - 结果是 1，因为 `aaa` 是 `itab` 中的第一行。
### 示例输出
假设内表 `itab` 中有以下内容：

| Index | Value |
| --- | --- |
| 1 | `aaa` |
| 2 | `bbb` |

运行上述代码后，`tabix` 的值将为 2，而 `idx` 的值将为 1。

## 检查内表中的行数
使用内置函数 `lines()` 可以检查内表中存在多少行。该函数返回一个整数值，表示内表中的行数。
### 示例代码
```abap
DATA: itab TYPE TABLE OF string WITH EMPTY KEY,
      number_of_lines TYPE i.

" 填充内表
itab = VALUE #( ( `a` ) ( `b` ) ( `c` ) ( `d` ) ( `e` ) ).

" 使用 lines() 函数获取内表中的行数
number_of_lines = lines( itab ). " 结果：5
```
### 详细解释

1. **定义内表 **`itab`：
   - `itab` 被定义为一个字符串类型的内表，并填充了五个字符串值：`a`，`b`，`c`，`d` 和 `e`。
2. **使用 **`lines()`** 函数**：
   - `number_of_lines = lines( itab ).` 使用 `lines()` 函数来获取内表 `itab` 中的行数。
   - 函数返回值为 5，因为内表中有五行数据。
### 示例输出
假设内表 `itab` 中有以下内容：

| Index | Value |
| --- | --- |
| 1 | `a` |
| 2 | `b` |
| 3 | `c` |
| 4 | `d` |
| 5 | `e` |

运行上述代码后，`number_of_lines` 的值将为 5。
### 在运行时获取内表（类型）信息
使用运行时类型识别（Runtime Type Identification，RTTI），你可以在运行时获取内表和表类型的信息。这在动态编程中非常有用，可以帮助你处理各种类型的数据结构。
### 示例代码
以下代码演示了如何使用 RTTI 获取内表的类型信息：
```abap
DATA: itab TYPE TABLE OF string WITH EMPTY KEY,
      type_descr TYPE REF TO cl_abap_tabledescr,
      line_descr TYPE REF TO cl_abap_structdescr.

" 填充内表
itab = VALUE #( ( `a` ) ( `b` ) ( `c` ) ( `d` ) ( `e` ) ).

" 获取内表的描述符
type_descr ?= cl_abap_typedescr=>describe_by_data( itab ).

" 输出内表的类型信息
IF type_descr IS BOUND.
  WRITE: / 'Table Kind: ', type_descr->table_kind,
         / 'Table Line Type: ', type_descr->get_table_line_type( )->absolute_name.
ENDIF.
```
### 详细解释

1. **定义内表 **`itab`：
   - `itab` 被定义为一个字符串类型的内表，并填充了一些数据。
2. **获取内表的描述符**：
   - `type_descr ?= cl_abap_typedescr=>describe_by_data( itab ).` 使用 `cl_abap_typedescr=>describe_by_data` 方法来获取内表的描述符。
3. **输出内表的类型信息**：
   - 如果描述符存在，使用 `type_descr->table_kind` 获取表的类型（如标准表、排序表或哈希表）。
   - 使用 `type_descr->get_table_line_type( )->absolute_name` 获取内表行的类型名称。
### 示例输出
假设内表 `itab` 是一个标准表，并且内表行的类型是字符串。运行上述代码后，输出结果：<br />![image.png](https://cdn.nlark.com/yuque/0/2024/png/488204/1721362232668-592dbdf8-5be1-40ee-bbe6-d6685e8e8921.png#averageHue=%23f9f9f9&clientId=u37fadd14-1ff4-4&from=paste&height=116&id=ud14783ce&originHeight=145&originWidth=426&originalType=binary&ratio=1.25&rotation=0&showTitle=false&size=4660&status=done&style=none&taskId=u09b28751-a21b-4cb4-b4c3-fe52343e3b2&title=&width=340.8)

