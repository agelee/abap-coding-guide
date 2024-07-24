在ABAP程序中使用 `TYPES`和 `DATA`语句来声明内表和内表类型。<br />内表的相关语法元素包括 `TABLE OF`后跟`TYPE`或 `LIKE` 的组合。
```abap
TYPES itab_type1 TYPE STANDARD TABLE OF data_type ...   "标准表类型
TYPES itab_type2 LIKE SORTED TABLE OF data_object ... "排序表类型

DATA  itab1      TYPE          TABLE OF data_type ...   "默认标准表
DATA  itab2      TYPE HASHED   TABLE OF data_type ...   "哈希表
DATA  itab3      TYPE                   itab_type1 ...  "基于现有内表类型
DATA  itab4      LIKE                   itab1 ...       "基于现有内表

```
> 💡 注意  
> 如果未指定表类别（例如 ...` TYPE TABLE OF` ...），则默认会使用 ... `TYPE STANDARD TABLE OF` ...。

以下代码片段包含了各种内表的声明示例。它旨在展示前面章节中提到的多种可能的内表类型，例如在《内表中的表键》部分中的例子。<br />在这些示例中，内表是使用`DDIC`中的演示数据库表的结构化类型创建的。在定义内表时，会自动使用数据库表的行类型。
```abap
"------------------ 标准表键 ------------------

"标准表没有显式指定主表键。注意，STANDARD 没有被明确指定。
"隐式地使用标准键；所有非数值表字段构成主表键。
DATA it1 TYPE TABLE OF zdemo_abap_fli.

"显式指定标准表的 STANDARD。显式指定标准表键。与 it1 相同。
DATA it2 TYPE STANDARD TABLE OF zdemo_abap_fli WITH DEFAULT KEY.

"带有唯一标准表键的散列表
DATA it3 TYPE HASHED TABLE OF zdemo_abap_fli WITH UNIQUE DEFAULT KEY.

"带有非唯一标准表键的排序表
DATA it4 TYPE SORTED TABLE OF zdemo_abap_fli WITH NON-UNIQUE DEFAULT KEY.

"基本行类型；整个表行作为标准表键
DATA it5 TYPE TABLE OF i.

"------------------ 主表键 ------------------

"指定主表键
"标准表：只能使用非唯一键
"以下两个示例是相同的。NON-UNIQUE 可以省略，但会隐式添加。
DATA it6 TYPE TABLE OF zdemo_abap_fli WITH NON-UNIQUE KEY carrid.
DATA it7 TYPE TABLE OF zdemo_abap_fli WITH KEY carrid.

"排序表：可以使用唯一和非唯一键
DATA it8 TYPE SORTED TABLE OF zdemo_abap_fli WITH UNIQUE KEY carrid connid.
DATA it9 TYPE SORTED TABLE OF zdemo_abap_fli WITH NON-UNIQUE KEY carrid connid cityfrom.

"哈希表：必须指定唯一键
DATA it10 TYPE HASHED TABLE OF zdemo_abap_fli WITH UNIQUE KEY carrid.

"显式指定预定义名称 `primary_key` 并列出组件。
"该示例与 it6 和 it7 相同。
DATA it11 TYPE TABLE OF zdemo_abap_fli WITH KEY primary_key COMPONENTS carrid.

"以下示例与 it9 相同。
DATA it12 TYPE SORTED TABLE OF zdemo_abap_fli 
  WITH NON-UNIQUE KEY primary_key COMPONENTS carrid connid cityfrom.

"为主表键指定别名。
"仅对排序表/哈希表有效。
DATA it13 TYPE SORTED TABLE OF zdemo_abap_fli
  WITH NON-UNIQUE KEY primary_key
  ALIAS p1 COMPONENTS carrid connid cityfrom.

"使用预定义的 `table_line` 组成的整个行指定主表键。
"在示例中，为主表键定义了一个别名。
DATA it14 TYPE HASHED TABLE OF zdemo_abap_fli
  WITH UNIQUE KEY primary_key
  ALIAS p2 COMPONENTS table_line.

"------------------ 空键 ------------------

"空键仅对标准表有效
DATA it15 TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.

"附带说明：在 SELECT 语句中的内联声明生成一个标准表，具有空键。
SELECT * FROM zdemo_abap_fli INTO TABLE @DATA(it16).

"------------------ 副表键 ------------------

"以下示例演示了所有表类别中可能的副表键。
DATA it17 TYPE TABLE OF zdemo_abap_fli                      "标准表
  WITH NON-UNIQUE KEY carrid connid                         "主表键
  WITH UNIQUE SORTED KEY cities COMPONENTS cityfrom cityto. "副表键

DATA it18 TYPE HASHED TABLE OF zdemo_abap_fli               "散列表
  WITH UNIQUE KEY carrid connid
  WITH NON-UNIQUE SORTED KEY airports COMPONENTS airpfrom airpto.

DATA it19 TYPE SORTED TABLE OF zdemo_abap_fli              "排序表
  WITH UNIQUE KEY carrid connid
  WITH UNIQUE HASHED KEY countries COMPONENTS countryfr countryto.

"多个副表键是可能的
DATA it20 TYPE TABLE OF zdemo_abap_fli
  WITH NON-UNIQUE KEY primary_key COMPONENTS carrid connid
  WITH NON-UNIQUE SORTED KEY cities COMPONENTS cityfrom cityto
  WITH UNIQUE HASHED KEY airports COMPONENTS airpfrom airpto.

"副表键（以及示例中的主表键）的别名
DATA it21 TYPE SORTED TABLE OF zdemo_abap_fli
  WITH NON-UNIQUE KEY primary_key ALIAS k1 COMPONENTS carrid connid city
  WITH NON-UNIQUE SORTED KEY cities ALIAS s1 COMPONENTS cityfrom cityto
  WITH UNIQUE HASHED KEY airports ALIAS s2 COMPONENTS airpfrom airpto.

"使用表键和别名的示例，通过 LOOP AT 语句。
"下面的所有语句都是可能的。
"注意，如果未指定副表键（并且示例中未使用 USING KEY 附加项），则默认尊重主表键。
"可以使用其他 ABAP 语句，如 READ 或 MODIFY，显式指定键以处理内表。
LOOP AT it21 INTO DATA(wa) USING KEY primary_key.
"LOOP AT it21 INTO DATA(wa) USING KEY k1.
"LOOP AT it21 INTO DATA(wa) USING KEY cities.
"LOOP AT it21 INTO DATA(wa) USING KEY s1.
"LOOP AT it21 INTO DATA(wa) USING KEY airports.
"LOOP AT it21 INTO DATA(wa) USING KEY s2.
  ...
ENDLOOP.

```
如前所述，上述示例展示了如何使用`DDIC`中数据库表的结构化类型来创建内表。<br />以下示例展示了通过包括结构化数据和内表类型的本地定义来声明内表和类型的模式及各种示例，用于演示目的。<br />步骤：

1. 定义一个结构化数据类型（本地或全局）。如果使用数据库表或CDS视图的名称进行内表声明，则不需要此步骤。在这些情况下，会自动使用它们的行类型。
2. 定义内表类型。
3. 创建内表，即使用该类型的数据对象。

你还可以通过以下方式创建内表：

- 将数据对象的创建和表类型的定义合并为一步。
- 使用内联声明。如果操作数类型可以完全确定，例如，通过使用 DATA 语句（或用于不可变变量的 FINAL），则可以在适当的声明位置进行此类内联声明。

```abap
"1. 本地定义行类型

TYPES: BEGIN OF ls_loc,
        key_field TYPE i,
        char1     TYPE c LENGTH 10,
        char2     TYPE c LENGTH 10,
        num1      TYPE i,
        num2      TYPE i,
      END OF ls_loc.

"2. 定义内表类型
"所有示例使用简短形式：

TYPES:
  "基于本地定义的结构类型的标准表类型。
  tt_loc_str TYPE TABLE OF ls_loc WITH NON-UNIQUE KEY key_field,

  "基于全局结构类型
  tt_gl_str TYPE TABLE OF zsome_global_struc_type WITH NON-UNIQUE KEY key_field,

  "基于数据库表（也可以是，例如CDS视图）
  "在这种情况下，表的行类型会自动使用。
  tt_gl_tab TYPE TABLE OF zdemo_abap_fli WITH NON-UNIQUE KEY carrid,

  "基于基本类型
  tt_el_type TYPE TABLE OF i.

"3. 创建内表 ...
"... 从本地定义的表类型
DATA: itab_a1 TYPE tt_loc_str,
      itab_a2 TYPE tt_gl_str,
      itab_a3 TYPE tt_gl_tab,
      itab_a4 TYPE tt_el_type.

"... 从全局内表类型
DATA itab_a5 TYPE string_table.

"结合数据对象和表类型定义
DATA itab_a6 TYPE TABLE OF ls_loc WITH NON-UNIQUE KEY key_field.

"基于已存在的内表使用 LIKE 创建内表。
"在这里，创建了一个包含 itab_a6 类型的内表。
DATA itab_a7 LIKE TABLE OF itab_a6.

"通过内联声明创建内表

"在赋值上下文中内联声明的表
"示例显示了在一个步骤中复制表及其内容
"并创建表。声明变量的数据类型由右侧确定。
DATA(it_inline1) = itab_a1.
DATA(it_inline2) = it_inline1.

"使用 FINAL 创建不可变变量
FINAL(it_final) = it_inline1.

"使用 VALUE 操作符和内表类型
DATA(it_inline3) = VALUE tt_loc_str( ( ... ) ).

"不提供任何表行意味着表是初始的
"其效果与上述 itab_a1 的声明相同。
DATA(it_inline4) = VALUE tt_loc_str( ).

"在 SELECT 语句的上下文中内联声明的表；
"无需事先额外声明内表。
SELECT * FROM zdemo_abap_fli INTO TABLE @DATA(it_inline5).

"代替
DATA it_sel TYPE TABLE OF zdemo_abap_fli WITH EMPTY KEY.
SELECT * FROM zdemo_abap_fli INTO TABLE @it_sel.

"使用 FINAL
SELECT * FROM zdemo_abap_fli INTO TABLE @FINAL(it_inline6).

```
