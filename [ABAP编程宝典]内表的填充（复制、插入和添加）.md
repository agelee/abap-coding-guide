# 复制内表
简单赋值没有构造表达式的情况下，会复制另一个内表的内容（注意，现有的内容将被删除）。以下示例假定源表和目标表具有兼容的行类型。使用内联声明有助于避免为适当类型的额外内表声明。
```abap
itab = itab2.

DATA(itab3) = itab.
FINAL(itab4) = itab.
```
> 💡 注意
> 内表只能分配给内表。
> 如果内表的行类型兼容或可转换，内表之间可以相互分配。
> 如果目标表被分配了一个唯一主键或辅助键的重复项，赋值操作可能会触发不可捕获的异常。

# 使用 INSERT 和 APPEND 语句填充内表
可以使用 ABAP 关键字 `INSERT` 和 `APPEND` 向内表添加行。<br />**使用 APPEND 和 INSERT 的注意事项**

- **APPEND**
   - 总是在内表的末尾添加行。
   - 对于标准表，使用索引管理行时没有问题。使用该语句时，系统字段 `sy-tabix` 会设置为最近添加行的索引。`sy-tabix` 总是相对于主表索引设置索引。
   - 无法用于哈希表。对于排序表，仅在匹配排序顺序且未创建唯一主表键的重复条目时，才追加行。因此，向排序表添加行时应使用 `INSERT`。
- **INSERT**
> 💡 注意: 对于唯一主表键，表不能有重复键的条目。如果插入重复条目，插入失败并且系统字段 `sy-subrc` 设置为 4。

   - 可以用于在表的特定位置添加行（通过指定目标索引）。这样，所有后续行会向下移动一个位置。
   - 未指定位置时，在标准表中添加行到表末尾。然而，使用 `INSERT` 时，`sy-tabix` 不会像 `APPEND` 那样设置。在排序表中，行会自动插入到正确的位置。
   - 使用 `INSERT` 时，可以在 `INTO` 后指定行插入的位置：
      - `... INTO TABLE itab ...`: 行插入到...
         - 标准表的最后一行（即追加）
         - 按主键值排序的排序表
         - 通过哈希管理的哈希表按主键值
      - `... INTO itab INDEX n`: 适用于索引表。行插入到主表索引中行号为 n 的行之前。

**应该使用什么？**<br />	推荐使用 `INSERT` 语句。它涵盖所有表和键类型。当您更改表/键类型时，考虑使用 `APPEND` 可能引起的问题。
## 向内表添加一行
示例显示了使用VALUE运算符创建的结构以及添加的现有结构。
```abap
APPEND VALUE #( comp1 = a comp2 = b ... ) TO itab.
APPEND lv_struc TO itab.

INSERT VALUE #( comp1 = a comp2 = b ... ) INTO TABLE itab.
INSERT lv_struc INTO TABLE itab.
```
 向内表添加初始行而不提供任何字段值：  
```abap
APPEND INITIAL LINE TO itab.

INSERT INITIAL LINE INTO TABLE itab.
```
## 向内表添加一行并将添加的行分配给字段符号或数据引用变量
### 使用字段符号：
```abap
"声明内表
DATA: lt_table TYPE TABLE OF zdemo_abap_fli.

"声明字段符号
FIELD-SYMBOLS: <fs_row> TYPE zdemo_abap_fli.

"使用APPEND添加一行并将其分配给字段符号
APPEND INITIAL LINE TO lt_table ASSIGNING <fs_row>.

"为添加的行赋值
<fs_row>-carrid = 'LH'.
<fs_row>-connid = '400'.
```
### 使用数据引用变量：
```abap
"声明内表
DATA: lt_table TYPE TABLE OF zdemo_abap_fli.

"声明数据引用变量
DATA: lr_row TYPE REF TO zdemo_abap_fli.

"使用INSERT添加一行并将其分配给数据引用变量
INSERT INITIAL LINE INTO TABLE lt_table REFERENCE INTO lr_row.

"为添加的行赋值
lr_row->carrid = 'LH'.
lr_row->connid = '400'.
```
上述示例展示了如何使用 `APPEND` 和 `INSERT` 语句将初始行添加到内表，并将新添加的行分配给字段符号或数据引用变量。这样可以方便地对新添加的行进行赋值。
## 将另一内表中的所有行添加到当前内表中
将另一内表中的所有行添加到当前内表中，可以使用 `APPEND LINES OF` 或 `INSERT LINES OF` 语句。以下是示例：
### 示例：使用 `APPEND LINES OF`
```abap
"声明两个内表
DATA: lt_source TYPE TABLE OF zdemo_abap_fli,
      lt_target TYPE TABLE OF zdemo_abap_fli.

"向源内表添加数据
APPEND VALUE #( carrid = 'LH' connid = '400' ) TO lt_source.
APPEND VALUE #( carrid = 'AA' connid = '200' ) TO lt_source.

"将源内表的所有行添加到目标内表
APPEND LINES OF lt_source TO lt_target.
```
### 示例：使用 `INSERT LINES OF`
```abap
"声明两个内表
DATA: lt_source TYPE TABLE OF zdemo_abap_fli,
      lt_target TYPE TABLE OF zdemo_abap_fli.

"向源内表添加数据
APPEND VALUE #( carrid = 'LH' connid = '400' ) TO lt_source.
APPEND VALUE #( carrid = 'AA' connid = '200' ) TO lt_source.

"将源内表的所有行插入到目标内表
INSERT LINES OF lt_source INTO TABLE lt_target.
```
### 注意事项

- 使用 `APPEND LINES OF` 时，所有源内表的行将被添加到目标内表的末尾。
- 使用 `INSERT LINES OF` 时，源内表的所有行将被插入到目标内表中。如果目标内表是一个排序表或哈希表，行将按照相应的键进行排序或哈希处理。
### 示例：将所有行添加到排序表中
```abap
"声明两个排序内表
DATA: lt_source TYPE SORTED TABLE OF zdemo_abap_fli WITH UNIQUE KEY carrid,
      lt_target TYPE SORTED TABLE OF zdemo_abap_fli WITH UNIQUE KEY carrid.

"向源内表添加数据
APPEND VALUE #( carrid = 'LH' connid = '400' ) TO lt_source.
APPEND VALUE #( carrid = 'AA' connid = '200' ) TO lt_source.

"将源内表的所有行插入到目标排序内表
INSERT LINES OF lt_source INTO TABLE lt_target.
```
以上代码展示了如何将一个内表的所有行添加到另一个内表中，包括标准表和排序表的情况。
##  将另一内表的多行添加到当前内表
可以使用 `INSERT LINES OF` 语句和指定索引范围来将另一内表的多行添加到当前内表。以下是一个示例，展示如何从指定索引范围添加多行：
### 示例：从指定索引范围添加多行
```abap
"声明两个内表
DATA: lt_source TYPE TABLE OF zdemo_abap_fli,
      lt_target TYPE TABLE OF zdemo_abap_fli.

"向源内表添加数据
APPEND VALUE #( carrid = 'LH' connid = '400' ) TO lt_source.
APPEND VALUE #( carrid = 'AA' connid = '200' ) TO lt_source.
APPEND VALUE #( carrid = 'BA' connid = '300' ) TO lt_source.
APPEND VALUE #( carrid = 'SQ' connid = '500' ) TO lt_source.

"将源内表索引2到3（包含）的行插入到目标内表
INSERT LINES OF lt_source FROM 2 TO 3 INTO TABLE lt_target.
```
### 注意事项

- 使用 `INSERT LINES OF ... FROM ... TO ...` 语句时，行从指定的起始索引（包含）到结束索引（包含）被添加到目标内表。
- 起始索引和结束索引必须在源内表的有效范围内，否则将会引发运行时错误。
### 示例：从指定索引范围添加多行到排序表中
```abap
"声明两个排序内表
DATA: lt_source TYPE SORTED TABLE OF zdemo_abap_fli WITH UNIQUE KEY carrid,
      lt_target TYPE SORTED TABLE OF zdemo_abap_fli WITH UNIQUE KEY carrid.

"向源内表添加数据
APPEND VALUE #( carrid = 'LH' connid = '400' ) TO lt_source.
APPEND VALUE #( carrid = 'AA' connid = '200' ) TO lt_source.
APPEND VALUE #( carrid = 'BA' connid = '300' ) TO lt_source.
APPEND VALUE #( carrid = 'SQ' connid = '500' ) TO lt_source.

"将源内表索引1到2（包含）的行插入到目标排序内表
INSERT LINES OF lt_source FROM 1 TO 2 INTO TABLE lt_target.
```
### 示例：从指定索引范围添加多行到哈希表中
```abap
"声明两个哈希内表
DATA: lt_source TYPE HASHED TABLE OF zdemo_abap_fli WITH UNIQUE KEY carrid,
      lt_target TYPE HASHED TABLE OF zdemo_abap_fli WITH UNIQUE KEY carrid.

"向源内表添加数据
APPEND VALUE #( carrid = 'LH' connid = '400' ) TO lt_source.
APPEND VALUE #( carrid = 'AA' connid = '200' ) TO lt_source.
APPEND VALUE #( carrid = 'BA' connid = '300' ) TO lt_source.
APPEND VALUE #( carrid = 'SQ' connid = '500' ) TO lt_source.

"将源内表索引1到3（包含）的行插入到目标哈希内表
INSERT LINES OF lt_source FROM 1 TO 3 INTO TABLE lt_target.
```
以上示例展示了如何使用 `INSERT LINES OF` 语句和指定索引范围将另一内表的多行添加到目标内表，包括标准表、排序表和哈希表的情况。
